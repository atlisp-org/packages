(defun at-math:sumtxt ()
  (@:help '("统计文本中的项与数据，对不同的项汇总求和。"))

  (defun to-pair(str / pair res)
    (if (p:stringp  str)
     (foreach a (string:auto-split str)
	     (if (string:numberp a)
		 (setq res
		       (cons 
			(cons
			 pre-a (atof a))
			res))
	       (setq pre-a a))))
    res)
  (if 
      (and (setq txts (pickset:to-list (ssget '((0 . "*text")))))
	      (setq txts 
				   (mapcar 'car
					   (vl-remove-if '(lambda(x)(cdr x))
							 (apply 'append
								(mapcar '(lambda(x)
									   (text:parse-mtext
									    (text:get-mtext x)))
									txts)))))
			  
	      (setq textnumpair (vl-remove nil (mapcar 'to-pair txts)))
        (setq data (mapcar 'car textnumpair)))
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
        res1)))

