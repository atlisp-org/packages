(defun at-curve:stat (/ curves res classname)
  (@:help "分类汇总曲线的长度。")
  (setq curves (pickset:to-list (ssget '((0 . "line,lwpolyline,arc,circle")))))
  (setq classname (ui:select "请选择分类条目" '("图层""颜色号" "线型")))
  (if classname
      (progn
	(setq lst-class '(("图层" . entity:get-layer)
			  ("颜色号" . entity:get-color)
			  ("线型" . entity:get-linetype)
			  ))
	(setq res (mapcar '(lambda(x)(cons
				      ((eval (cdr (assoc classname lst-class))) x)
				      (vla-get-length (e2o x))))
			  curves))
	(if res
	    (table:make (getpoint (@:speak "请点击表格插入点:"))
			"长度汇总表"
			(list classname "总长")
			(mapcar '(lambda(x)
				   (list (car x)(cdr x)))
				(vl-sort (stat:classify res)
					 '(lambda(x y)
					    (> (cdr x)
					       (cdr y))))
				))))))
					      