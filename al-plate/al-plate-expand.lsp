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
  (setq ents (cons (entity:make-lwpolyline
		    pts nil 0 0 0)
		   (mapcar '(lambda(x)
			     (entity:make-line
			      x
			      (polar x (* 0.5 pi) leng))
			   pts)))
  (ui:dyndraw ents '(0 0 0))
  )
(defun al-plate:select-draw ()
  (@:prompt "选择多段线")
  (if (setq lwpl (car (pickset:to-list (ssget ":S:E" '((0 . "lwpolyline"))))))
      (progn
	(setq pts (curve:get-points lwpl))
	(setq pt1 (car pts))
	(setq pts (cdr pts))
	(setq pt2 (car pts))
	(setq pts (cdr pts))
	(setq flag-start 0)
	(setq segs nil)
	(while (setq pt3 (car pts))
	      (setq flag-end 
		    (if (< (geometry:turn-right-p pt1 pt2 pt3) 0)
			0 0.5))
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

	(lvban:draw-expand segs)

	)))
  
	  
	  
