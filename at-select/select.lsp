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

