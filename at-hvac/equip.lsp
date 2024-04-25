(defun @hvac:equip-balance ()
  (@::help '("分集水器平衡分析"))
  (setq hequips (pickset:to-list (ssget '((0  . "TCH_DBHEQUIPMENT")))))
  (setq hvac-jfsqs
	(vl-remove-if-not '(lambda(x)
			    (equal (vlax-get (e2o x) 'hvac_s5)
			     "采暖分集水器"))
			  hequips))
  (@:prompt (strcat "选中"(itoa(length hvac-jfsqs))"个采暖分集水器"))
  (setq i 0)
  (mapcar '(lambda(x / box lens)
	    (setq box (entity:getbox x 100))
	    (prompt (strcat "分集水器 " (itoa (setq i (1+ i)))" . "))
	    (setq pipes (pickset:to-list(ssget "c" (car box)(cadr box)'((0 . "lwpolyline")(8 . "*地暖*")))))
	    (setq lens (mapcar '(lambda(x)(* 0.001(curve:length  x))) pipes))
	    (prompt (strcat "回路数: "(itoa (length lens))" 个; "))
	    (prompt (strcat "总长: "(rtos (apply '+ lens) 2 1)"m; "))
	    (prompt (strcat "最长: "(rtos (setq pmax (apply 'max lens)) 2 1)"m; "))
	    (prompt (strcat "最短: "(rtos (setq pmin (apply 'min lens)) 2 1)"m; "))
	    (prompt (strcat "最大差值: "(rtos (setq diff (- pmax pmin)) 2 1)"m; "))
	    (if (> diff 15)
		(progn
		  (@::prompt " 已亮显差值超过15m的管道组")
		  (sssetfirst nil (pickset:from-list pipes)))
		(prompt "\n"))
	    )
	  hvac-jfsqs))
    
    
    
(defun @hvac:make-equip-bom ()
  (@::help '("生成设备表"))
  (setq hequips (pickset:to-list (ssget '((0  . "TCH_DBHEQUIPMENT")))))
  (setq hvac-jfsqs
	(vl-remove-if-not '(lambda(x)
			    (equal (vlax-get (e2o x) 'hvac_s5)
			     "采暖分集水器"))
			  hequips))
  (@:prompt (strcat "选中"(itoa(length hvac-jfsqs))"个采暖分集水器"))
  (setq i 0)
  (if (setq bom
	(stat:stat
	 (mapcar '(lambda(x / box lens)
		   (setq box (entity:getbox x 100))
		   (setq pipes (pickset:to-list(ssget "c" (car box)(cadr box)'((0 . "lwpolyline")(8 . "*地暖*")))))
		   (setq lens (mapcar '(lambda(x)(* 0.001(curve:length  x))) pipes))
		   (strcat "FPQ-" (itoa (length lens)))
		   )
		 hvac-jfsqs)
	 ))
      (progn
	(setq tbl 
	      (table:make
	       '(0 0 0)
	       "主要设备表"
	       '("编号""名称""规格型号""单位""数量""备注")
	       (mapcar 'list
		       '(1 2 3 4 5)
		       '("分、集水器""分、集水器""分、集水器""分、集水器""分、集水器")
		       (mapcar 'car bom)
		       '("个""个""个""个""个")
		       (mapcar 'cdr bom)
		 '("铜制""铜制""铜制""铜制""铜制""铜制"))))
	(ui:dyndraw tbl '(0 0 0)))))
  
    
  
