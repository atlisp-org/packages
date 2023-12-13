(defun @math:unit-number (/ res lst-str)
  (@:help  "从文字中提取有物理量的数据,结果保存至变量@:tmp-result")
  (setq txts (mapcar 'text:get-mtext (pickset:to-list(ssget '((0 . "*text"))))))
  (setq lst-str
	(apply 'append 
	       (mapcar '(lambda(x)
			 (string:auto-split
			  (text:remove-fmt (string:subst-all "," "\\P"  x))))
		       txts)))
  (setq lst-str (vl-remove-if '(lambda(x)(= "" (vl-string-trim " " x))) lst-str))
  (while (and  lst-str (> (length lst-str) 1))
    (if(and (string:numberp (car lst-str))
	    (p:phyunitp (cadr lst-str)))
       (setq res
	     (cons
	      (cons (read-from-string (car lst-str))
		    (cadr lst-str))
	      res)))
    (setq lst-str (cdr lst-str)))
  (setq @:tmp-result res))
