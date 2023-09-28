(defun @block:overblocks (/ *error* blkname ss-blk blks blkent blkname box)
  (@:help "定位相互重叠的同名块")
  (defun *error* (msg)
    ;; 重启动处理 
    (if (= 'file (type dcl_fp))
	(close (dcl_fp)))
    (princ (strcat msg ))
    (princ))
  (setq blkent (car (entsel "请选择一个块:")))
  (setq blks (pickset:to-list (ssget "x" '((0 . "INSERT")))))
  (setq blkname (block:get-effectivename blkent))
  (setq blks (vl-remove-if '(lambda (x) (/= blkname
					 (block:get-effectivename x)))
			   blks))
  ;; (progn
  ;;   (setq blkname  (@:get-config '@block:block-name))
  ;;   (setq blks (pickset:to-list (ssget "x" (list '(0 . "insert")
  ;; 						 (cons 2 blkname)))))
  ;;   )
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
      )
  (princ))
  
    
