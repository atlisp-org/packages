(@:add-menu "@试验室" "统计光缆" '(@lab:stat-line))
(@:add-menu "@试验室" "统计文数" '(@lab:stat-line2))
(@:define-config '@lab:stat-item "*+*" "分类统计文字数字文字数字")
(defun @lab:stat-line (/ res res1 to-pair get-slave)
  (@::prompt (list "统计汇总 `文字数字文字数字文字数字...' 格式的单行文本字符串"
		"文字须是全汉字，或全英文，且文字之间及文字与数字之间不能有空格."
		"数字须是半角数字字符，目前不支持带正负号的数字。"
		))
  (defun to-pair(str / pair res)
    (foreach a (string:auto-split str)
	     (if (string:numberp a)
		 (setq res
		       (cons 
			(cons
			 pre-a (atof a))
			res))
	       (setq pre-a a)))
    res)
  (defun get-slave (title%)
    (mapcar (function
	     (lambda (x)
	       (to-pair
		(entity:getdxf x 1))))
	    (pickset:to-list
	     (ssget "C"
		    (polar (entity:getdxf title% 10)
			   pi
			   (entity:getdxf title% 40))
		    (polar (entity:getdxf title% 10)
			   (* 0.5 pi)
			   (* 1.5
			      (entity:getdxf title% 40)))
		    '((0 . "text")(1 . "*管道*,*架空*,*引上*"))))))
    
  (push-var nil)
  (setvar "osmode" 0)
  (setq res nil)
  (setq titles (pickset:to-list(ssget '((0 . "text")(1 . "*-*")))))
  (foreach title% titles
	   (if (assoc (entity:getdxf title% 1) res)
	       (setq res
		     (subst
		      (cons (entity:getdxf title% 1)
			    (append (cdr (assoc (entity:getdxf title% 1) res))
				    (get-slave title%)
				    ))
		      (assoc (entity:getdxf title% 1) res)
		      res))
	     (setq res
		 (cons 
		  (cons (entity:getdxf title% 1)
			(get-slave title%))
		  res))
	     ))
  ;; summary
  (setq res
	(mapcar (function
		 (lambda(x / res1)
		   (cons (car x)
			 (progn
			   (foreach subj (apply 'append (cdr x))
				    (if (assoc (car subj) res1)
					(setq res1
					      (subst
					       (cons (car subj)
						     (+ (cdr subj)
							(cdr (assoc (car subj) res1))))
					       (assoc (car subj) res1)
					       res1))
				      (setq res1
					    (cons
					     subj
					     res1))))
			   res1))))
		res))
  (setq pt (getpoint "表格绘制位置点："))
  (foreach data res
	   (table:make pt
		       (car data)
		       (list "名称" "数量")
		       (append 
			(setq data
			      (mapcar '(lambda(x)
					 (list (car x)(cdr x)))
				      (cdr data)))
			(list (list "合计"
				    (apply '+ (mapcar 'cadr data))))))
			
	   (setq pt (polar pt 0 100))
	   )
					
  (pop-var)
  (princ)
  )
(defun @lab:stat-line2 (/ res1 to-pair )
  (@::prompt (list "统计汇总 `文字数字文字数字...' 格式的单行文本字符串"
		"文字须是全汉字，或全英文，且文字之间及文字与数字之间不能有空格."
		"数字须是半角数字字符，目前不支持带正负号的数字。"
		))
  (defun to-pair(str / pair res)
    (foreach a (string:auto-split str)
	     (if (string:numberp a)
		 (setq res
		       (cons 
			(cons
			 pre-a (atof a))
			res))
	       (setq pre-a a)))
    res)
  (defun get-slave (title%)
    (mapcar (function
	     (lambda (x)
	       (to-pair
		(entity:getdxf x 1))))
	    (pickset:to-list
	     (ssget "C"
		    (polar (entity:getdxf title% 10)
			   pi
			   (entity:getdxf title% 40))
		    (polar (polar (entity:getdxf title% 10)
				  (* 0.5 pi)
				  (* 2
				     (entity:getdxf title% 40)))
			   0
			   (entity:getdxf title% 40))
		    (list '(0 . "text")(cons 1 (@:get-config '@lab:stat-item)))))))
    
  (push-var nil)
  (setvar "osmode" 0)
  (setq res nil)
  (setq titles (pickset:to-list(ssget '((0 . "text")(1 . "*-*")))))
  (foreach title% titles
	   (if (assoc (entity:getdxf title% 1) res)
	       (setq res
		     (subst
		      (cons (entity:getdxf title% 1)
			    (append (cdr (assoc (entity:getdxf title% 1) res))
				    (get-slave title%)
				    ))
		      (assoc (entity:getdxf title% 1) res)
		      res))
	     (setq res
		 (cons 
		  (cons (entity:getdxf title% 1)
			(get-slave title%))
		  res))
	     ))
  ;; summary
  (if (and (setq res
		 (mapcar (function
			  (lambda(x / res1)
			    (cons (car x)
				  (progn
				    (foreach subj (apply 'append (cdr x))
					     (if (assoc (car subj) res1)
						 (setq res1
						       (subst
					       (cons (car subj)
						     (+ (cdr subj)
							(cdr (assoc (car subj) res1))))
					       (assoc (car subj) res1)
					       res1))
				      (setq res1
					    (cons
					     subj
					     res1))))
			   res1))))
			 res))
	   (setq pt (getpoint "表格绘制位置点："))
	   (setq item-name (list:remove-duplicates (mapcar 'car (apply 'append (mapcar 'cdr res))))))
      (progn
	(setq i 0)
	(table:make pt
		    "汇总表"
		    (cons "序号" (cons "类型" item-name))
		    (mapcar
		     '(lambda (x)
			(cons
			 (itoa (setq i (1+ i)))
			 (cons (car x)
			       (mapcar
				'(lambda(y)
				   (cdr (assoc y (cdr x))))
				item-name))))
		     res))))
  	
  (pop-var)
  (princ)
  )
