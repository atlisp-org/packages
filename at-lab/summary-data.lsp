(defun @lab:summary-data (/ tbl-summary get-data)
  (@::prompt (strcat "汇总图框及图框内文本信息\n形成表格"))
  (defun get-data (ent / data box mt)
    (if ent
	(progn
	  (setq data (block:get-attributes ent))
	  (setq box (entity:getbox ent 0))
	  (@:cmd "zoom" "w" (car box)(cadr box))
	  (setq mt (pickset:to-list
		    (ssget "w" (car box)(cadr box)
			   '((0 . "mtext")))))
	  (if mt
	      (progn
		(setq data-mt
		      (mapcar 'car
			      (vl-remove-if '(lambda(x)(cdr x))
					    (apply 'append
						   (mapcar '(lambda(x)
							      (text:parse-mtext
							       (text:get-mtext x)))
							   mt)))))
		(foreach
		 txt data-mt
		 (foreach handle '("材料""下料尺寸""表面处理""数量")
			  (if (setq res (member handle
						(mapcar '(lambda(x)
							   (vl-string-trim " " x))
							(string:parse-by-lst txt '("、""：""；")))))
			      (setq data (cons (cons handle
						     (cadr res)) 
					       data))))))
	    )
	  data
	  )))
  (setq mapsheet (car (entsel)))
  (prompt "请选择要提取数据的图框")
  (if (and (setq ss-tk (ssget (list '(0 . "insert")
				    (assoc 2 (entget mapsheet)))))
	   (setq tbl-data (mapcar 'get-data (pickset:to-list ss-tk)))
	   (setq tbl-header '("物料编码" "图纸编号" "单元名称/零部件名称" "材料""下料尺寸""表面处理""数量" "备注"))
	   (setq tbl-data (mapcar '(lambda(x / data)
				     (foreach hd tbl-header
					      (if (assoc hd x)
						  (setq data (cons (cdr (assoc hd x)) data))
						(setq data (cons "" data)))
					      )
				     (reverse data))
				  tbl-data))
	   (listp tbl-data)
	   (> (length tbl-data) 0))
      (table:make (getpoint "请输入表格插入位置:") "表格" tbl-header (reverse tbl-data)))
  )

