(defun @math:mark-in-zone () 
  (@:help '("筛分多段线区域中的最大最小数，大数用红圈，小数用绿圈。"))
  (setq zone (pickset:to-list (ssget (list '(0 . "lwpolyline")
					   (cons 8 (@:get-config '@math:layer-of-zone))))))
  (foreach
   lwpl zone
   (if (and 
	(setq numbers (pickset:to-list 
                       (ssget "wp" (curve:get-points lwpl) '((0 . "text")))))
	(setq numbers (vl-remove-if-not 
                       '(lambda (x) 
			 (string:numberp 
			  (entity:getdxf x 1)))
		       numbers))
	(setq numbers (vl-sort 
                       numbers
                       '(lambda (x y) 
			 (>= 
			  (atof (entity:getdxf x 1))
			  (atof (entity:getdxf y 1))))))
	(setq numbers (list:group-by 
                       numbers
                       '(lambda (x y) 
			 (= 
			  (atof (entity:getdxf x 1))
			  (atof (entity:getdxf y 1)))))))
       (progn
	 (foreach nmax (car numbers) 
		  (entity:putdxf 
		   (entity:make-circle 
		    (point:centroid 
		     (setq box (entity:getbox nmax 0)))
		    (* 0.5 (apply 'distance box)))
		   62
		   1))
	 (foreach nmin (last numbers) 
		  (entity:putdxf 
		   (entity:make-circle 
		    (point:centroid 
		     (setq box (entity:getbox nmin 0)))
		    (* 0.5 (apply 'distance box)))
		   62
		   3))))))
