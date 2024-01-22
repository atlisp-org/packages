(defun @curve:link-obj ()
  (@:help  '("连接原物体到目标物体。"))
  (@:prompt "选择源物体:")
  (setq obj-src (pickset:sort-by-box (ssget )"Yx" 10))
  (@:prompt "选择目标物体:")
  (setq obj-target  (pickset:sort-by-box (ssget )"Yx" 10))
  
  ;; 1 to n
  (cond
    ((= (length obj-src) 1)
     (mapcar '(lambda(x)
	       (entity:make-lwpolyline
		(list
		 (point:centroid
		  (entity:getbox (car obj-src) 0))
		 (point:centroid
		  (entity:getbox x 0)))
		nil 0 0 0))
	     obj-target))
    ((= (length obj-target) 1)
     (mapcar '(lambda(x)
	       (entity:make-lwpolyline
		(list
		 (point:centroid
		  (entity:getbox (car obj-target) 0))
		 (point:centroid
		  (entity:getbox x 0)))
		nil 0 0 0))
	     obj-src))
    ((= (length obj-target)(length obj-src))
     (mapcar '(lambda(x y)
	       (entity:make-lwpolyline
		(list
		 (point:centroid
		  (entity:getbox x 0))
		 (point:centroid
		  (entity:getbox y 0)))
		nil 0 0 0))
	     obj-src
	     obj-target))))
		 
  
