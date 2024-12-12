(defun @curve:inters-number ()
  (@::prompt "一条曲线上与其他多段线的交点进行递增编号")
  (@:prompt "请选择一条曲线:")
  (if (setq lwpl (ssname (ssget ":S" '((0 . "*line"))) 0))
      (progn
	(setq box (entity:getbox lwpl 0))
	(setq lwpls (ssget "c" (car box)(cadr box) '((0 . "*line"))))
	(setq lwpls (ssdel lwpl lwpls))
	;;(setq lwpls (vl-remove lwpl lwpls))
	;; 多段线的相交点
	
	(setq pts (curve:inters lwpl lwpls acExtendNone))
	;; 对交点排序
	(setq pts
	      (vl-sort pts
		       '(lambda(x y)
			 (< (vlax-curve-getDistAtPoint (e2o lwpl) x)
			  (vlax-curve-getDistAtPoint (e2o lwpl) y)))))
	;; 标序号
	(setq i 0)
	(foreach pt pts
		 (entity:putdxf
		  (entity:make-text
		   (itoa (setq i (1+ i)))
		   pt
		   (@:scale 2.5) 0 1 0 "LB")
		  62 1)
		  )
	)))
  
  
