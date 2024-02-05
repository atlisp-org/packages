(defun at-curve:stat (/ curves res classname dxfno)
  (@:help "分类汇总曲线的长度。")
  (setq curves (pickset:to-list (ssget '((0 . "*line,lwpolyline,arc,circle")))))
  (setq classname (ui:select "请选择分类条目" '("图层""颜色号" "线型" "多线比例" "指定组码")))
  (if (= classname "指定组码")
      (setq dxfno (cdr (assoc "组码号" (ui:input "请输入要统计的 dxf 组码号" '(("组码号" 8 "要进行分类统计的组码号")))))))
  (print dxfno)
  (if classname
      (progn
	(setq lst-class (list '("图层" . entity:get-layer)
			  '("颜色号" . entity:get-color)
			  '("线型" . entity:get-linetype)
			  '("多线比例" . (lambda(x)(entity:getdxf x 40)))
			  (cons "指定组码" '(lambda(x)(entity:getdxf x dxfno)))
			  ))
	(setq res (mapcar '(lambda(x)(cons
				      ((eval (cdr (assoc classname lst-class))) x)
				      (curve:length x)))
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
					      
