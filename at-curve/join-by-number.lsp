(defun at-curve:join-by-number (/ dist)
  (setq dist 10)
  (setq pts (pickset:to-list (ssget  '((0 . "point")))))
  (setq pts
	(vl-sort pts
		 '(lambda(x y)
		   (if (and
			(setq nox
			      (car (pickset:to-list
				    (ssget
				     "c"
				     (entity:getdxf x 10)
				     (polar (entity:getdxf x 10) (* 0.25 pi) dist)
				     '((0 . "text")(1 . "#,##,###"))))))
	      		(setq noy
			      (car (pickset:to-list
				    (ssget
				     "c"
				     (entity:getdxf y 10)
			      (polar (entity:getdxf y 10) (* 0.25 pi) 10)
			      '((0 . "text")(1 . "#,##,###")))))))
		       (< (atoi (entity:getdxf nox 1))
			  (atoi (entity:getdxf noy 1)))))))
  (setq pts (mapcar '(lambda(x)(entity:getdxf x 10)) pts))
  (entity:make-lwpolyline pts nil 0 1 0))

