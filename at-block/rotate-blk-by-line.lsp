(defun @block:rotate-blk-by-line()
  (@:prompt '("选择块内一条直线，按该直线水平旋转块."
	      "输入Y旋转所有同名块。"
	      ))
  (if (and 
       (setq lineblk (nentsel "请选择块内一条直线:"))
       (setq line (car lineblk))
       (setq blkref (car (last lineblk)))
       (= "INSERT" (entity:getdxf blkref 0))
       (wcmatch (entity:getdxf line 0) "LINE,*POLYLINE"))
      (progn
	;;计算角度
	(setq ang
	      (cond
	       ((= "LINE" (entity:getdxf line 0 ))
		(angle(entity:getdxf line 10)
		      (entity:getdxf line 11)))
	       ((= "LWPOLYLINE" (entity:getdxf line 0))
		;;最近的线段seg
		(setq selpt-in-blk (block:wcs2bcs (cadr lineblk)
						  (cdr
						   (assoc 10
							  (tblsearch
							   "block"
							   (entity:getdxf blkref 2))))
						  (entity:getdxf blkref 10)
						  (entity:getdxf blkref 50)
						  (entity:getdxf blkref 41))
		      )
		;; (setq v (mapcar '- (cadr lineblk)
		;; 		(last (caddr lineblk))))
		;; (setq m (reverse(cdr(reverse (caddr lineblk)))))
		;; (setq selpt-in-blk (matrix:mxv m v))
		;;求内部点到多段线的最近线段
		;; (setq pt-in-lwpl (curve:pickclosepointto (e2o line) selpt-in-blk))
		(setq pts (curve:subsegment-picked-points(e2o line) selpt-in-blk ))
		(angle (car pts)(cadr pts))
		)))
	;; (princ  (m:fix-angle (* -1 ang)))
	;; 选择同名块
	(entity:putdxf
	 (pickset:to-list (ssget "x" (list '(0 . "insert")
					   (cons 2 (entity:getdxf blkref 2)))))
	 50
	 (m:fix-angle (* -1 ang))
	 )
	)))
  
