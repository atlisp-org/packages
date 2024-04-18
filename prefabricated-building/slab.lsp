(defun prefabricated-building:stat-slab ()
  "统计梁长"
  (setq slabs (pickset:to-list(block:ssget nil "slab*" nil)))
  

  (setq statdata (stat:stat
		  (mapcar
		   '(lambda(x)
		     (fix
		      (* 10
		       (round
			(/ (cdr(assoc "距离1" (block:get-dynamic-prop-cons-name-value x))) 10)))))
		   slabs)))

  (setq table-data
	(mapcar '(lambda(x)
		  (list
		   (+ 20 (car x))
		   (car x)
		   (cdr x)
		   ""))
		statdata))
  (setq ent-tbl
	(table:make '(0 0 0)
		    "板统计表"
		    '("板跨La(mm)""板净跨L""个数""备注")
		    table-data))
  (ui:dyndraw ent-tbl '(0 0 0)))
  
