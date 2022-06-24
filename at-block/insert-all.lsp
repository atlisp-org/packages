(defun @block:insert-all (/ pt0 pt1 ang dist blk)
  (@:help (strcat "块操作 -> 插所有块\n"
		  " 将当前 dwg 中的所有块插入到从指定点和指定距离排列。"))
  (if (and (setq pt0 (getpoint "请输入插入点:"))
	   (setq pt1 (getpoint pt0 "请输入相对插入点的距离和方向点:"))
	   (setq ang (angle pt0 pt1)
		 dist (distance pt0 pt1)))
      (progn 
	(setq blk (tblnext "block" t))
	(while blk
	  (block:insert (cdr (assoc 2 blk)) "" pt0 0 1)
	  (setq pt0 (polar pt0 ang dist))
	  (setq blk (tblnext "block"))))))

  
