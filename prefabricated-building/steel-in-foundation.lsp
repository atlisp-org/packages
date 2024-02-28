(defun prefabricated-building:make-steel-in-foundation ()
  ;; 从构件生成基础插筋
  ;; 选择构件
  ;; 逐个在指定图层生成点
  (layer:make "steel-in-foundation" 1 nil nil)
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
       8 "steel-in-foundation")
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (+ pi
	    (entity:getdxf component 50))
	 220))
       8 "steel-in-foundation"))
     ((wcmatch name "*Q@750")
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (entity:getdxf component 50)
	 295))
       8 "steel-in-foundation")
      (entity:putdxf
       (entity:make-point
	(polar
	 (entity:getdxf component 10)
	 (+ pi
	    (entity:getdxf component 50))
	 295))
       8 "steel-in-foundation")))))
	
