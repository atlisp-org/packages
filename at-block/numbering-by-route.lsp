(defun @block:numbering-by-route (/ num1 start ss-list ss1 fuzz)
  (@:help "按曲线路顺序对块进行递增编号")
  (@:prompt "请选择一条曲线:")
  (if (setq lwpl (ssname (ssget ":S" '((0 . "*line"))) 0))
      (progn
	(setq pts (curve:get-points lwpl))
	(if (= "" (@:get-config '@block:block-name))
	    (@block:setup))
	(if (progn
	      (setq ss1 (ssget "F" pts (list '(0 . "insert")'(66 . 1)
				     '(-4 . "<or")
				     (cons 2 (@:get-config '@block:block-name))
				     (cons 2 "`**")
				     '(-4 . "or>"))
			 ))
	      (setq ss-list
		    (vl-remove-if-not
		     '(lambda(x)
		       (= (block:get-effectivename x)
			(@:get-config '@block:block-name)))
		     (pickset:to-list ss1))))
	    (progn
	      ;; 排序
	      (sssetfirst nil (pickset:from-list ss-list))

	      (setq ss-list
		    (vl-sort ss-list
			     '(lambda(x y)
			       (<
				(vlax-curve-getDistAtPoint (e2o lwpl)
				 (vlax-curve-getClosestPointTo
				  (e2o lwpl) (entity:getdxf x 10)))
				(vlax-curve-getDistAtPoint (e2o lwpl)
				 (vlax-curve-getClosestPointTo
				  (e2o lwpl) (entity:getdxf y 10)))))))
	      
	      (setq start (getint "请输入块起始编号<1>:"))
	      (if (null start) (setq start 1))
	      (setq num1 0)
	      (foreach en0 ss-list
		       (block:set-attributes
		  en0
		  (list (cons (@:get-config '@block:attribute-name)
			      (strcat
			       (@:get-config '@block:attribute-prefix)
			       (if (< (+ num1 start) 10) "0" "")
			       (itoa (+ num1 start))
			       (@:get-config '@block:attribute-suffix)
			       ))))
		 (setq num1 (1+ num1))
		 ))
      (progn
	(alert "未选中设置的图块。请设置要进行操作的图块。")
	(@block:setup))))))
