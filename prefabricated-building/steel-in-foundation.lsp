(@:define-config 'prefabricated-building:layer-of-steelfoundation "基础插筋" "基础插筋图层")
(defun prefabricated-building:make-steel-in-foundation ()
  ;; 从构件生成基础插筋
  ;; 选择构件
  ;; 逐个在指定图层生成点
  (layer:make (@:get-config 'prefabricated-building:layer-of-steelfoundation) 1 nil nil)
  (setq components-of-wall (pickset:to-list(ssget '((0 . "insert")(2 . "*Q@##0")))))
  (foreach
   component components-of-wall
   (setq name (entity:getdxf component 2))
   (cond
     ((wcmatch name "*Q@600")
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (entity:getdxf component 50)
	 220))
       8 (@:get-config 'prefabricated-building:layer-of-steelfoundation))
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (+ pi
	    (entity:getdxf component 50))
	 220))
       8 (@:get-config 'prefabricated-building:layer-of-steelfoundation)))
     ((wcmatch name "*Q@750")
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (entity:getdxf component 50)
	 295))
       8 (@:get-config 'prefabricated-building:layer-of-steelfoundation))
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (+ pi
	    (entity:getdxf component 50))
	 295))
       8 (@:get-config 'prefabricated-building:layer-of-steelfoundation))))))
	
(defun prefabricated-building:draw-steel-in-foundation ()
  (@:help (list "点位绘筋"))
  (setq pts
	(mapcar '(lambda(x)
		  (entity:getdxf x 10))
		(pickset:to-list
		 (ssget (list '(0 . "point")
			      (cons 8 (@:get-config 'prefabricated-building:layer-of-steelfoundation)))))))
  (mapcar '(lambda(x)
	    (entity:putdxf
	     x 8
	     (@:get-config 'prefabricated-building:layer-of-steelfoundation)))
	  (entity:make-circle
	   pts 20)))
  
(defun prefabricated-building:delete-steel-in-foundation ()
  (mapcar 'entdel
	  (pickset:to-list
	   (ssget (list '(0 . "point,circle")
			(cons 8 (@:get-config 'prefabricated-building:layer-of-steelfoundation))))))

  )
(defun prefabricated-building:dim-steel-in-foundation ()
  (@:help (list "定位插筋"))
  (@:prompt "请选择一行或一列插筋点:")
  (setq ss (ssget (list '(0 . "point")
			(cons 8 (@:get-config 'prefabricated-building:layer-of-steelfoundation)))))
  (command "qdim" ss )
  )
