(defun at-math:sumtxt (/ res1 ents txts data textnumpair)
  (@::help '("统计文本中的项与数据，对不同的项汇总求和。"))
  
  (defun to-pair(str / pair res)
    (if (p:stringp  str)
	(foreach a
		 (vl-remove "" (mapcar '(lambda(x)(vl-string-trim ";； ,，:：" x))
				       (string:auto-split str)))
		 (if (string:numberp a)
		     (setq res
			   (cons 
			    (cons
			     pre-a (atof a))
			    res))
		   (setq pre-a a))))
    res)
  (if 
      (and (setq ents (pickset:to-list (ssget '((0 . "*text")))))
	   (setq txts 
		 (mapcar '(lambda(x)
			    (text:remove-fmt (text:get-mtext x)))
			 ents))
	   (setq textnumpair (vl-remove nil (mapcar 'to-pair txts)))
           (setq data (apply 'append textnumpair)))
      (progn
	;; 项目
        (setq res1 nil)
	
        (foreach subj data
		 (if (assoc (car subj) res1)
		     (setq res1
			   (subst (cons (car subj) (+ (cdr subj)
						      (cdr (assoc (car subj) res1))))
				  (assoc (car subj) res1)
				  res1))
		   (setq res1 (cons  subj res1))))
	(if (setq @:tmp-stat-result  res1)
	    (stat:draw))
	)))

