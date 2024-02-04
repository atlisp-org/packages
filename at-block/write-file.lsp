(defun @block:write-file ()
  (@:help "将当前dwg中的块导出到库文件夹")
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq path (@:path-win-format (@:get-config '@block:lib)))
  
  (if (/= path nil)
      (progn
	(if (= (substr path (strlen path) 1) "\\")
	    (setq path (substr path 1 (1- (strlen path))))
	    )
	(if(null (findfile path))
	   (@:mkdir (@:path path))
	  )
	(@:prompt "请选择要写出的块:")
	(setq blk-lst
	      (mapcar 
	       '(lambda(x)
		  (entity:getdxf x 2))
	       (pickset:to-list (ssget '((0 . "insert"))))))
	(setq lst
	      (vl-remove-if  '(lambda(x)
			       (or 
				(wcmatch x "`**")
				(wcmatch x "_*")))
			     blk-lst))
	;; TODO: 可以列出选择
	(foreach blk lst
		 (setq fn (strcat path (chr 92) blk))
		 (if (findfile (strcat fn ".dwg"))
		     (command "_.WBLOCK" fn "_Y" blk)
		     (command "_.WBLOCK" fn blk)
		     )
		 )
	)
      )
  
  (setvar "CMDECHO" cmdecho)
  (princ)
  )
