(defun @block:overblocks (/ *error* blkname ss-blk blks blkent blkname box)
  (@:help "定位相互重叠的同名块")
  (defun *error* (msg)
    ;; 重启动处理 
    (if (= 'file (type dcl_fp))
	(close (dcl_fp)))
    (princ (strcat msg ))
    (princ))
  (prompt  "请选择一个块:")
  (while (null(and (setq blkent (ssget "_:S:E" '((0 . "insert"))))
		   (setq blkent (ssname blkent 0))))
    (@:prompt "\n未选中块，请选择一个块:"))
  (if blkent
      (progn
	(setq blks (pickset:to-list (ssget "x" '((0 . "INSERT")))))
	(setq blkname (block:get-effectivename blkent))
	(setq blks (vl-remove-if '(lambda (x) (/= blkname
					       (block:get-effectivename x)))
				 blks))
	(while (and (setq blk (car blks))
		    (setq box (@block:get-corner blk))
		    (setq ss-blk
			  (ssget "c" (car box)(cadr box) (list '(0 . "insert")
							       (cons 2 blkname))))
		    (<= (sslength ss-blk)  1))
	  (setq blks (cdr  blks)))
	(if (and ss-blk (> (sslength ss-blk) 1))
	    (progn
	      (setq corner (pickset:getbox ss-blk 10))
	      (command "zoom" "w" (car corner) (cadr corner))
	      (sssetfirst nil (ssadd (ssname ss-blk 0))))
	    (princ (@:speak "没有发现重叠块。"))
	    )))
  (princ))
  
    