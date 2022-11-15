
(setq rad 30.0
      maxlength-corner 80.0
      max-angle (* (/ 12.0 18.0) pi)
      min-angle (* (/  6.0 18.0) pi)
      )
(defun at-curve:fillet (ent / segs res n1 n2 n3 n4 fuzz)
  (setq segs (mapcar (quote (lambda (x y)
          (cons x y)))
      (curve:pline-3dpoints ent)
      (curve:pline-convexity ent)))
  (setq n1 (car segs))
  (setq res (cons n1 nil))
  (setq segs (cdr segs))
  
  (setq n2 (car segs))
  (setq res (cons n2 res))
  (setq segs (cdr segs))
  
  (setq n3 (car segs))
  (setq res (cons n3 res))
  (setq segs (cdr segs))
  (princ "test")
  (while (setq n4 (car segs))
    (cond
     ((and (= 0 (cdr n1))
	   (= 0 (cdr n3))
	   ;; ;; n2 短
	   (< (distance (car n2)
			(car n3))
	      maxlength-corner)	   ;; ;; 角度
	   (< min-angle
	      (progn(setq ang
			  (abs (- (angle (car n1)
					 (car n2))
				  (angle (car n3)
					 (car n4)))))
		    (princ ang)
		    (if (> ang  pi)
		  	(setq ang (- ang pi))
		      ang))
	      max-angle)
	   )
      (princ "test")
      (setq O (inters
	       (setq pt1 (polar (car n1)
				(+ (angle (car n1)(car n2))
				   (* (m:sign (geometry:turn-right-p
					       (car n1)(car n2)(car n3)))
				      (* 0.5 pi)))
				rad
				))
	       (setq pt2 (polar pt1 (angle (car n1)(car n2))
				(distance (car n1)(car n3))))
	       (setq pt3 (polar (car n3)
				(+ (angle (car n3)(car n4))
				   (* (m:sign (geometry:turn-right-p
					       (car n2)(car n3)(car n4)))
				      (* 0.5 pi)))
				rad))
	       (setq pt4 (polar pt1 (angle (car n4)(car n3))
				(distance (car n4)(car n2))))))
      (princ "test")(princ O)
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
			  rad))
      (setq res (cons
		 (cons pt-n2
		       (curve:o2bulge pt-n2 pt-n3 O))
		 (cddr res)))
      (setq res (cons
		 (cons pt-n3
		       (cdr n3))
		 res))
      ))
    (setq res (cons n4 res))
    (setq n1 (caddr res))
    (setq n2 (cadr res))
    (setq n3 (car res))
    (setq segs (cdr segs)))
 
  ;; (if (= 1 (entity:getdxf ent 70))
  ;;     )
  (setq res (reverse res))
  (entity:make-lwpline-bold (mapcar (quote car)
      res)
    (mapcar (quote cdr)
      res)
    0 (entity:getdxf ent 70)
    0))

(defun at-curve:fillet-road ()
  (@:help '("根据设定的转弯半径和道路转角范围，平滑道路的转角。"))
  (setq lwpls (pickset:to-list(ssget '((0 . "lwpolyline")))))
  (mapcar 'at-curve:fillet  lwpls))

