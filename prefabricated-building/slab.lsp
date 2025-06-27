(defun prefabricated-building:stat-slab ()
  "统计形及板长"
  (setq slabs (pickset:to-list(block:ssget nil "slab*" nil)))
  
  ;;按板宽分组
  ;; (setq slabs (list:sort
  ;; 	       slabs
  ;; 	       '(lambda(x y)
  ;; 		  (<
  ;; 		   (block:get-dynprop x "查寻1")
  ;; 		   (block:get-dynprop y "查寻1")
  ;; 		   ))))
  ;; (setq slabs
  ;; 	(apply 'append
  ;; 	(list:group-by
  ;; 	       slabs
  ;; 	       '(lambda(x y)
  ;; 		  (=
  ;; 		   (block:get-dynprop x "查寻1")
  ;; 		   (block:get-dynprop y "查寻1")
  ;; 		   )))))
  (setq statdata
	(stat:stat (mapcar '(lambda(x)
			      (strcat "W"(itoa (fix(block:get-dynprop x "距离2")))
				      "-"
				      (itoa
				       (fix
					(* 10
					   (round
					    (/ (block:get-dynprop x  "距离1") 10.0)))))))
			   slabs)))
  ;; (setq statdata
  ;; 	(mapcar '(lambda(w)
  ;; 		   (stat:stat
  ;; 		    (mapcar
  ;; 		     '(lambda(x)
  ;; 			(list
  ;; 			 (block:get-dynprop x "查寻1")
  ;; 			 (fix
  ;; 			  (* 10
  ;; 			     (round
  ;; 			      (/ (block:get-dynprop x  "距离1") 10.0))))))
  ;; 		     w)))
  ;; 		slabs))
  
  (setq table-data
	(mapcar '(lambda(x / slab)
		   (setq slab (string:to-list (car x) "-"))
		   (list
		    (car x)
		    (car slab)
		    (+ 20 (atoi (cadr slab)))
		    (cadr slab)
		    (cdr x)
		    ""))
		statdata))
  (setq ent-tbl
	(table:make '(0 0 0)
		    "板统计表"
		    '("型号""板宽W" "板跨La(mm)""板净跨L""个数""备注")
		    table-data))
  (ui:dyndraw ent-tbl '(0 0 0)))
  
