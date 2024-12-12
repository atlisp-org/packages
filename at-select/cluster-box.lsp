(@:define-config '@select:cluster-gap 1000 "分堆时不同堆的图形之间的最小间距")
(@:define-config '@select:clusterbox-layer "cluster" "分堆结果包围盒所在的图层")
(defun at-select:cluster-box(/ )
  (@::prompt '("将所选图形进行分堆，并为每个堆群加包围矩形框"))
  (if (null(member (@:get-config '@select:clusterbox-layer)(layer:allname)))
      (layer:make (@:get-config '@select:clusterbox-layer) 210 nil nil))
  (@::prompt "请选择要进行分堆的图形:")
  (setq res (pickset:cluster(ssget)(@:get-config '@select:cluster-gap)))
  (mapcar '(lambda(x)
	     (entity:putdxf 
	      (apply 'entity:make-rectangle
		     (pickset:getbox
		      (ssget "w" (car x)(cadr x))
		      0)
		     )
	      8
	      (@:get-config '@select:clusterbox-layer)
	      ))
	  res)
  )
  
  
