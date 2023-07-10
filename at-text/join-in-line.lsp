(defun @text:join-in-line ()
  (@:help '("合并同一行的多个单行文本为一个"))
  (setq txts (pickset:to-list (ssget '((0 . "text")))))
  ;; 按行分组
  (setq txts
	(pickset:sort txts
		      "Yx"
		      (list
		       (* 0.5 (entity:getdxf (car txts) 40))
		       0)))
  (setq txts
	(list:group-by
	 txts
	 '(lambda(x y)
	    (equal 
	     (cadr (entity:getdxf x 10))
	     (cadr (entity:getdxf y 10))
	     (* 0.5 (entity:getdxf x 40))))))
  (foreach txts% txts
	   (entity:putdxf (car txts%)
			  1
			  (string:from-list
			   (mapcar '(lambda(x)(entity:getdxf x 1)) txts%)
			   ""))
	   ;; 删除后面的
	   (mapcar 'entdel (cdr txts%)))
  (princ)
  )

	    
  
