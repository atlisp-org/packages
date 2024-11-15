;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@:define-config 'saw-blade:outer-diameter 700.0 "圆盘外径")
(@:define-config 'saw-blade:inner-diameter 100.25 "圆盘内径")
(@:define-config 'saw-blade:hole-diameter 250.0 "圆盘孔的分布圆直径")

(@:define-config 'saw-blade:radius 1.5 "自动圆角的半径")
(@:define-config 'saw-blade:maxlength-corner 3.0 "自动圆角的原始倒角最大线长")
(@:define-config 'saw-blade:max-angle "80.0" "自动圆角的转角最大角度")
(@:define-config 'saw-blade:min-angle "20.0" "自动圆角的转角最小角度")
(@:define-config 'saw-blade:hole-number 6 "圆盘上的孔洞数")
(@:define-config 'saw-blade:hole-d 19.0  "圆盘上的孔径")
(@:define-config 'saw-blade:tooth-number 280 "圆盘上的锯齿数")
(@:define-config 'saw-blade:tooth-height 10 "圆盘上的锯齿深度")
(@:define-config 'saw-blade:tooth-step 10 "直锯的齿距")

(@:add-menus
 '(("锯片"
    ("锯片设置" "(saw-blade:setup)")
    ("绘圆锯片" "(saw-blade:draw-circle)")
    ("绘直锯齿" "(saw-blade:draw-sawtooth)")
    )))

(defun saw-blade:setup (/ res)
  (setq @::tmp-search-str "saw-blade")
  (@::edit-config-dialog))
(defun saw-blade:calc-fillet-pts (n1 n2 n3 n4 / O pt1 pt2 pt3 pt4 pt-n2 pt-n3)
    (if(and (setq O (inters
		     (setq pt1 (polar (car n1)
				      (+ (angle (car n1)(car n2))
					 (* -1
					    (m:sign (geometry:turn-right-p
						     (car n1)(car n2)(car n3)))
				      (* 0.5 pi)))
				   rad
				   ))
		  (setq pt2 (polar pt1
				   (angle (car n1)(car n2))
				   (distance (car n1)(car n3))))
		  (setq pt3 (polar (car n4)
				   (+ (angle (car n3)(car n4))
				      (* -1
					 (m:sign (geometry:turn-right-p
						  (car n2)(car n3)(car n4)))
					 (* 0.5 pi)))
				   rad))
	       (setq pt4 (polar pt3 (angle (car n4)(car n3))
				(distance (car n4)(car n2))))))
	   (setq pt-n2 
    		 (polar O (+ (angle (car n1)(car n2))
			     (* (m:sign (geometry:turn-right-p
					 (car n1)(car n2)(car n3)))
				(* 0.5 pi))
			     )
			rad))
	   (setq pt-n3  (polar O (+ (angle (car n3)(car n4))
				    (* (m:sign (geometry:turn-right-p
						(car n2)(car n3)(car n4)))
				       (* 0.5 pi))
				    )
			       rad)))
	(list pt-n2 pt-n3 O)
      ))
(defun saw-blade:calc-fillet-3pts (n1 n2 n3 / O pt1 pt2 pt3 pt4 pt-n2 pt-n3)
    (if(and (setq O
		  (inters
		   (setq pt1 (polar (car n1)
				    (+ (angle (car n1)(car n2))
				       (* -1
					  (m:sign (geometry:turn-right-p
						   (car n1)(car n2)(car n3)))
					  (* 0.5 pi)))
				    rad
				    ))
		   (setq pt2 (polar pt1
				    (angle (car n1)(car n2))
				    (distance (car n1)(car n2))))
		   (setq pt3 (polar (car n3)
				    (+ (angle (car n2)(car n3))
				       (* -1
					  (m:sign (geometry:turn-right-p
						   (car n1)(car n2)(car n3)))
					  (* 0.5 pi)))
				    rad))
		   (setq pt4 (polar pt3 (angle (car n3)(car n2))
				    (distance (car n3)(car n2))))))
	    (setq pt-n2 
    		  (polar O (+ (angle (car n1)(car n2))
			      (* (m:sign (geometry:turn-right-p
					  (car n1)(car n2)(car n3)))
				 (* 0.5 pi))
			      )
			 rad))
	    (setq pt-n3  (polar O (+ (angle (car n2)(car n3))
				    (* (m:sign (geometry:turn-right-p
						(car n1)(car n2)(car n3)))
				       (* 0.5 pi))
				    )
				rad)))
       (list pt-n2 pt-n3 O)
       ))
(defun saw-blade:sawtooth (pts / segs res n1 n2 n3 n4 fuzz rad max-angle min-angle maxlength-corner ang)
  "路口圆角"
  (setq rad (@::get-config 'saw-blade:radius)
      maxlength-corner  (@::get-config 'saw-blade:maxlength-corner)
      max-angle (angtof (@::get-config 'saw-blade:max-angle))
      min-angle (angtof (@::get-config 'saw-blade:min-angle))
      )
  (setq segs (mapcar (quote (lambda (x)
			      (cons x 0)))
		     pts))
  (setq n1 (car segs))
  (setq res (cons n1 nil))
  (setq segs (cdr segs))
  
  (setq n2 (car segs))
  (setq res (cons n2 res))
  (setq segs (cdr segs))
  
  (setq n3 (car segs))
  (setq res (cons n3 res))
  (setq segs (cdr segs))
  (cond
   ((and (= 0 (cdr n1))
	 (= 0 (cdr n2))
	 (> (distance (car n1)
		      (car n2))
	    (* 2 maxlength-corner))
	 (> (distance (car n2)
		      (car n3))
	    (* 2 maxlength-corner))
	 (< min-angle
	    (progn
	      (setq ang
		    (m:fix-angle
		     (- (angle (car n2)
			       (car n1))
			(angle (car n2)
			       (car n3)))))
	      (if (> ang pi)
		  (setq ang (- (* 2 pi) ang)))
	      ang)
	    max-angle)
	 )
    (and (setq pts-fillet (saw-blade:calc-fillet-3pts n1 n2 n3))
	 (setq res (cons
		    (cons (car pts-fillet)
			  (apply 'curve:o2bulge pts-fillet))
		    (cddr res)))
	 (setq res (cons
		    (cons (cadr pts-fillet)
			  (cdr n3))
		    res))
	 (setq res (cons n3 res))
	 )))
  (while (setq n4 (car segs))
    (cond
     ((and (= 0 (cdr n2))
	   (= 0 (cdr n3))
	   (> (distance (car n2)
		      (car n3))
	      (* 2 maxlength-corner))
	   (> (distance (car n3)
			(car n4))
	      (* 2 maxlength-corner))
	   (< min-angle
	      (progn
		(setq ang
		      (m:fix-angle
		       (- (angle (car n2)
				 (car n1))
			  (angle (car n3)
				 (car n4)))))
		(if (> ang pi)
		    (setq ang (- (* 2 pi) ang)))
		ang)
	      max-angle)
	   )
      (and (setq pts-fillet (saw-blade:calc-fillet-3pts n2 n3 n4))
	   (setq res (cons
		      (cons (car pts-fillet)
			    (apply 'curve:o2bulge pts-fillet))
		      (cdr res)))
	   (setq res (cons
		      (cons (cadr pts-fillet)
			    (cdr n3))
		      res))
	   ))
     ((and (= 0 (cdr n1))
	   (= 0 (cdr n3))
	   ;; n2 短
	   (< (distance (car n2)
			(car n3))
	      maxlength-corner) ;; ;; 角度
	   (< min-angle
	      (progn
		(setq ang
		      (m:fix-angle
		       (- (angle (car n2)
				 (car n1))
			  (angle (car n3)
				(car n4)))))
	       (if (> ang pi)
		   (setq ang (- (* 2 pi) ang)))
	       ang)
	     max-angle)
	   )
      (and (setq pts-fillet (saw-blade:calc-fillet-pts n1 n2 n3 n4))
	   (setq res (cons
		      (cons (car pts-fillet)
			    (apply 'curve:o2bulge pts-fillet))
		      (cddr res)))
	   (setq res (cons
		      (cons (cadr pts-fillet)
			    (cdr n3))
		      res))
	   )))
    (setq res (cons n4 res))
    (setq n1 (caddr res))
    (setq n2 (cadr res))
    (setq n3 (car res))
    (setq segs (cdr segs)))
  
  (setq res (reverse res))
  (entity:make-lwpline-bold (mapcar (quote car)
				    res)
			    (mapcar (quote cdr)
				    res)
			    0
			    0
			    0)
  )
(defun saw-blade:draw-circle-by-params (pt-O m n)
  "pt 位置，m 定位孔数，n齿数"
  ;; 三个同心圆
  (setq 2pi (* pi 2))
  (entity:make-circle
   pt-O
   (mapcar '(lambda(x)(* 0.5 x))
	   (list
	    (@::get-config 'saw-blade:inner-diameter)
	    (@::get-config 'saw-blade:hole-diameter)
	    (@::get-config 'saw-blade:outer-diameter))))
  ;; m个定位孔
  (entity:make-circle
   (mapcar '(lambda(x)(polar pt-O x 125.0)) (list:range 0 2pi (/ 2pi m)))
   (* 0.5 (@::get-config 'saw-blade:hole-d)))
  ;; 剧齿n
  (mapcar '(lambda(x / pt0)
	    (saw-blade:sawtooth
	     (reverse
	     (list (setq pt0 (polar pt-O x (* 0.5 (@::get-config 'saw-blade:outer-diameter))))
	      (polar pt0 (- x pi) (@::get-config 'saw-blade:tooth-height))
	      (polar pt-O (+ x (/ 2pi n)) (* 0.5 (@::get-config 'saw-blade:outer-diameter)))))))
	  (list:range 0 2pi (/ 2pi n)))
  (princ))
(defun saw-blade:draw-circle(/ pt-O m n)
  (if (null pt-O)(setq pt-O (getpoint  "绘制圆心位置:")))
  (if pt-O
      (saw-blade:draw-circle-by-params
       pt-O
       (@::get-config 'saw-blade:hole-number)
       (@::get-config 'saw-blade:tooth-number)
       )))
(defun saw-blade:draw-sawtooth (/ pt-start pt-end)
  (if (and (setq pt-start (getpoint "起点:"))
	   (setq pt-end  (getpoint pt-start "终点:")))
      (progn
	(setq ang (angle pt-start pt-end))
	(mapcar '(lambda(i)
		  (saw-blade:sawtooth
		   (list (setq pt0
				(polar pt-start
				       ang
				       (* i (@::get-config 'saw-blade:tooth-step))))
			  (polar pt0
				 (+ (* 1.5 pi)
				    ang)
				 (@::get-config 'saw-blade:tooth-height))
			  (polar pt0
				 ang
				 (@::get-config 'saw-blade:tooth-step)))))
		(list:range 0
			    (fix (/ (distance pt-start pt-end)
				    (@::get-config 'saw-blade:tooth-step)))
			    1)))))
