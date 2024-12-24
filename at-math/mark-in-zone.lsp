(defun @math:mark-in-ss (ent-nums)
  (if (and (setq ent-nums (vl-remove-if-not 
			  '(lambda (x) 
			    (string:numberp 
			     (entity:getdxf x 1)))
			  ent-nums))
	   (setq ent-nums (vl-sort 
			  ent-nums
			  '(lambda (x y) 
			    (>= 
			     (atof (entity:getdxf x 1))
			     (atof (entity:getdxf y 1))))))
	   (setq ent-nums (list:group-by 
			  ent-nums
			  '(lambda (x y) 
			    (= 
			     (atof (entity:getdxf x 1))
			     (atof (entity:getdxf y 1)))))))
      (progn
	(foreach nmax (car ent-nums) 
		 (entity:putdxf 
		  (entity:make-circle 
		   (point:centroid 
		    (setq box (entity:getbox nmax 0)))
		   (* 0.5 (apply 'distance box)))
		  62
		  1))
	(foreach nmin (last ent-nums) 
		 (entity:putdxf 
		  (entity:make-circle 
		   (point:centroid 
		    (setq box (entity:getbox nmin 0)))
		   (* 0.5 (apply 'distance box)))
		  62
		  3)))))

(defun @math:mark-in-lwpl (/ ent-nums) 
  (@::prompt '("区域标数：筛分多段线区域中的最大最小数，大数用红圈，小数用绿圈。"))
  (setq zone (pickset:to-list (ssget (list '(0 . "lwpolyline")
					   (cons 8 (@:get-config '@math:layer-of-zone))))))
  (foreach
   lwpl zone
   (if (setq ent-nums (pickset:to-list 
                      (ssget "wp" (curve:get-points lwpl) '((0 . "text")))))
       (@math:mark-in-ss  ent-nums))))
(defun @math:mark-in-w ()
  (@::prompt '("筛分框选的最大最小数，大数用红圈，小数用绿圈。"))
  (if (setq ent-nums (pickset:to-list 
                      (ssget '((0 . "text")))))
      (@math:mark-in-ss ent-nums))
  )
