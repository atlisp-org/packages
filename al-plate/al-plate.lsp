(@::define-config 'al-plate:al-layer "A-铝板" "铝构件框线图层")
(@::define-config 'al-plate:thikness-layer "A-板厚线" "铝构件厚线图层")
(@::define-config 'al-plate:thikness 0.5 "铝板厚度值")
(@::define-config 'al-plate:length 200 "铝构件绘制长度")
(@::define-config 'al-plate:symmetry 1 "铝构件是否转角构件，0: 非转角，1:一端转角，2:两端转角")
(@::define-config 'al-plate:symmetry 1 "铝构件是否对称")


(@::add-menus
 '("铝板"
   ("铝板配置" (al-plate:setup))
   ("铝构展开" (al-plate:draw1))
   ))

(defun al-plate:setup ()
  (setq @::tmp-search-str "al-plate")
  (@::edit-config-dialog))

(defun al-plate:draw-expand (lst)
  (setq s 0)
  (setq lst-diso
	(mapcar '(lambda(x)
		  (+ s x)
		  (setq s (+ s x)))
		lst))
  (setq pts (cons '(0 0 0) (mapcar '(lambda(x)
				     (list x 0 0))
				   lst-diso)))
  (setq ents
	(cons
	 ;;端线
	 (entity:putdxf
		    (entity:make-lwpolyline
		     pts nil 0 0 0)
		    8 "A-展开线")
		   (mapcar '(lambda(x)
			     (entity:putdxf
			      (entity:make-line
			       x
			       (polar x (* 0.5 pi) (@::get-config 'al-plate:length)))
			      8  "A-折弯线")
			     )
			   pts)))
  (ui:dyndraw ents '(0 0 0))
  )
(defun al-plate:draw1 ()
  (@:prompt "选择多段线")
  (if (setq lwpl (car (pickset:to-list (ssget ":S:E" (list '(0 . "lwpolyline")
							   (cons 8 (@::get-config 'al-plate:al-layer)))
					      ))))
      (progn
	(setq pts (curve:get-points lwpl))
	(setq pt1 (car pts))
	(setq pts (cdr pts))
	(setq pt2 (car pts))
	(setq pts (cdr pts))
	(setq flag-start 0)
	(setq segs nil)
	(while (setq pt3 (car pts))
	  ;; 判断内折或外折
	  (setq flag-end
		(if (ssget "f"
			   (list
			    (setq pt-mid (point:mid pt1 pt2))
			    (polar pt-mid (angle pt2 pt3)  2))
			   (list '(0 . "line,lwpolyline")
			     (cons 8 (@::get-config 'al-plate:thikness-layer))))
		    (@::get-config 'al-plate:thikness) 0.0))
	  (setq segs
		(cons
		 (- (distance  pt1 pt2)
		    flag-start
		    flag-end)
		 segs))
	  (setq flag-start flag-end)
	  (setq pt1 pt2)
	  (setq pt2 pt3)
	  (setq pts (cdr  pts))
	  )
	(setq segs
	      (cons
	       (- (distance  pt1 pt2)
		  flag-start)
	       segs))
	(princ segs)
	;;绘制
	(al-plate:draw-expand segs)
	)))

