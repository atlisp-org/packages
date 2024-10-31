(defun at-select:select-closed-lwpl (/ s1 filters)
  (@::help '("选择闭合的多段线"))
  (setq filters '((0 . "lwpolyline")(70 . 1)))
  (if @:*auto-mode*
      (setq s1 (ssget "x" filters))
    (progn
      (prompt "回车或右键则为所有多段线")
      (setq s1 (ssget filters))
      (if (null s1)
	  (setq s1 (ssget "x" filters)))))
  (sssetfirst nil s1))
(defun at-select:select-unclosed-lwpl (/ s1 filters)
  (@::help '("选择未闭合的多段线"))
  (setq filters '((0 . "LWPOLYLINE")(70 . 0)))
  (if @:*auto-mode*
      (setq s1 (ssget "x" filters))
    (progn
      (prompt "回车或右键则为所有多段线")
      (setq s1 (ssget filters))
      (if (null s1)
	  (setq s1 (ssget "x" filters)))))
  (sssetfirst nil s1))
(defun at-select:select-sametype (/ ent1 s1 filters)
  (@::help '("选择同类型的图形"))
  (@:prompt "请点选一个图形:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" ))))
  (setq filters (list
		 (cons 0
		       (entity:getdxf ent1 0))))
  (setq s1 (ssget "x" filters))
  (sssetfirst nil s1))
(defun at-select:select-shortlines (/ ent1 s1 filters)
  (@::help '("选择短线，即小于给定长度的线"))
  (if (or (null shortline-value)
	  (not(numberp shortline-value))
	  (< shortline-value 0)
	  )
      (setq shortline-value (getreal "请输入短线限值:")))
	    
		    
  (if (@::get-config 'curve:types)
      (setq filters (list
		     (cons 0
			   (@::get-config 'curve:types))))
      (setq filters '((0 . "*polyline,line"))))

  
  (setq s1
	(vl-remove-if
	 '(lambda(x)
	   (> (vla-get-length (e2o x))
	    shortline-value))
	 (pickset:to-list (progn
			    (prompt "回车或右键则为所有曲线")
			    (setq s1 (ssget filters))
			    (if (null s1)
				(setq s1 (ssget "x" filters)))
			    s1))))
  (if s1
      (sssetfirst nil (pickset:from-list s1))))
(defun at-select:select-samelayer (/ ent1 s1 filters)
  (@::help '("选择同层的图形"))
  (@:prompt "请点选一个图形:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" ))))
  (setq filters (list
		 (cons 8
		       (entity:getdxf ent1 8))))
  (setq s1 (ssget "x" filters))
  (sssetfirst nil s1))
(defun at-select:select-samecolor (/ ent1 s1 filters)
  (@::help '("选择同色的图形"))
  (@:prompt "请点选一个图形:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" ))))
  (setq filters (entity:get-color ent1))
  (setq s1
	(vl-remove-if-not
	 '(lambda(x)
	   (= filters
	    (entity:get-color x)))
	 (pickset:to-list (ssget "x"))))
  (sssetfirst nil (pickset:from-list s1)))
(defun at-select:select-samelinetype (/ ent1 s1 filters)
  (@::help '("选择同线型的图形"))
  (@:prompt "请点选一个图形:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" ))))
  (setq filters (entity:get-linetype ent1))
  (setq s1
	(vl-remove-if-not
	 '(lambda(x)
	   (= filters
	    (entity:get-linetype x)))
	 (pickset:to-list (ssget "x"))))
  (sssetfirst nil (pickset:from-list s1)))


(defun at-select:select-similar (/ ent1 s1 filters)
  (@::help '("选择相似曲线"))
  (if (@::get-config '@curve:types)
      (setq filters (list
		     (cons 0
			   (@::get-config '@curve:types))))
      (setq filters '((0 . "*POLYLINE,circle,arc,ellipse,spline,region,line"))))
  (@:prompt "请点选一个曲线:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" filters))))
  (setq s1
	(vl-remove-if-not
	 '(lambda(x)
	   (curve:similar-p ent1 x))
	 (pickset:to-list (ssget "x" filters))))
  (sssetfirst nil (pickset:from-list s1)))


(defun at-select:select-samelens-lines (/ ent1 s1 filters lengths)
  (@::help '("选择定长线，即给定的固定长度的线。"))
  (while (null
	  (and 
	   (setq lengths (getstring t "请输入线长度值(以空格或,号分隔多值):"))
	   (setq lengths (vl-remove nil (string:split lengths '(" " "," "，"))))
	   (apply 'and (mapcar 'string:numberp lengths))
	   (setq lengths (mapcar 'read lengths))
	   ))
    (prompt "输入错误，请重新输入!\n"))
  k		    
  (if (@::get-config 'curve:types)
      (setq filters (list
		     (cons 0
			   (@::get-config 'curve:types))))
      (setq filters '((0 . "*polyline,line"))))

  
  (setq s1
	(vl-remove-if-not
	 '(lambda(x)
	   (list:member (vla-get-length (e2o x))
	    lengths (* 0.001 (vla-get-length (e2o x)))))
	 (pickset:to-list (progn
			    (prompt "回车或右键则为所有曲线")
			    (setq s1 (ssget filters))
			    (if (null s1)
				(setq s1 (ssget "x" filters)))
			    s1))))
  (if s1
      (sssetfirst nil (pickset:from-list s1))))
