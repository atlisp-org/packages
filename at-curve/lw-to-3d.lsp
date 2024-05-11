(defun @curve:lw-to-3d (lwpl / closed pl pts points)
  "LWpolyline 转 3D polyline"
  ;; 取lwpl 顶点
  (setq closed (entity:getdxf lwpl 70))
  (setq pts (mapcar 'point:2d->3d(curve:get-points lwpl)))
  (setq points (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 3 (length pts))))))
  (vlax-safearray-fill points (apply 'append pts))
  (setq pl (vla-add3dpoly *MS* points))
  (if (= 1 closed)
      (vla-put-closed pl
		      :vlax-true))
  pl
  )
(defun @curve:menu-lw2pl ()
  (@::help '("将二维多段线转化为三维多段线"
	     "对于有凸度的二维线将将行拉直"))
  (mapcar '@curve:lw-to-3d
	  (pickset:to-list
	   (ssget '((0 . "LWpolyline"))))))
