;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@:add-menus
 '("曲线工具"
   ("曲线配置" (@curve:setup))
   ("双线互连" (at-curve:join))
   ("优化多段线" (at-curve:optimize-lwpl))
   ("平滑路口" (at-curve:fillet-road))
   ("曲线面积" (at-curve:area))
   ("曲线长度" (at-curve:length))
   ("每段长度" (at-curve:per-length))
   ("垂线缺口" (at-curve:notch))
   ("连线端点" (at-curve:link-end))
   ("统计线长" (at-curve:stat))
   ("单线变双" (at-curve:dualline))
   ("交点编号" (@curve:inters-number))
   ("排线相连" (@curve:link-obj))
   ("样条转多段" (@curve:spline2lwpl))
   ))
(@:define-config
    '@curve:types
    "*POLYLINE,circle,arc,ellipse,spline,region"
  "可操作的曲线的图元类型")
(@:define-config '@curve:dualline-width 120.0 "单线变双线的默认宽度")
(defun @curve:setup (/ res)
  (setq @::tmp-search-str "@curve")
  (@::edit-config-dialog))
(defun at-curve:join (/ l1 l2 pts1 pts2)
  (@:help "选择两条线，从最近端点连接成一条.")
  (setq curves (pickset:to-list (ssget '((0 . "*line")))))
  (setq pts1 (curve:pline-3dpoints (car curves)))
  (setq pts2 (curve:pline-3dpoints (cadr curves)))
  (if
      (<
       (distance (car pts1) (car pts2))
       (distance (last pts1) (car pts2)))
      (setq pts1 (reverse pts1)))
  (if
      (>
       (distance (last pts1) (car pts2))
       (distance (last pts1) (last pts2)))
      (setq pts2 (reverse pts2)))
  (entdel (car curves))
  (entdel (cadr curves))
  (entity:make-lwpline-bold
   (append pts1 pts2)
   nil
   nil
   0
   0))


(defun at-curve:area (/ lst-curve pts)
  (@:help '("标注曲线的的闭合面积"))
  (@:prompt "请选择闭合曲线:")
  (setq lst-curve (pickset:to-list
                   (ssget (list (cons 0 (@:get-config '@curve:types))))))
  (foreach curve lst-curve
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@:draw-scale))
             0
             0.72
             0
             "mm")
            62
            1))
  (princ))
(defun at-curve:length (/ lst-curve pts)
  (@:help '("在曲线的中点,标注曲线的长度"))
  (@:prompt "请选择曲线:")
  (setq lst-curve (pickset:to-list
                   (ssget (list (cons 0 (@:get-config '@curve:types))))))
  (foreach curve lst-curve
           (entity:putdxf
            (entity:make-text
             (rtos (curve:length (e2o curve)) 2 3)
             (point:2d->3d (curve:midpoint curve))
             (* 2.5 (@:get-config '@:draw-scale))
             0
             0.72
             0
             "mb")
            62
            1))
  (princ))
(defun at-curve:per-length (/ lst-curve pts)
  (@:help '("标注曲线的每段长度"))
  (@:prompt "请选择曲线:")
  (setq lst-curve (pickset:to-list
                   (ssget (list (cons 0 (@:get-config '@curve:types))))))
  (foreach curve lst-curve
	   (cond
	    ((= "MLINE" (entity:getdxf curve 0))
	     (setq pts (curve:get-points curve))
	     (if (equal (car pts)(cadr pts))
		 (setq pts (cdr pts)))
	     (while (> (length pts) 1)
	       (entity:putdxf
		(entity:make-text
		 (rtos (distance (car pts)(cadr pts)) 2 3)
		 (point:2d->3d (point:mid (car pts)(cadr pts)))
		 (* 2.5 (@:get-config '@:draw-scale))
		 (angle (car pts)(cadr pts))
		 0.72
		 0
		 "mb")
		62
		1)
	       (setq pts (cdr pts))
	       ))
	    ((= "LWPOLYLINE" (entity:getdxf curve 0))
	     ;; 少闭合曲线最后段
	     (setq i 0)
	     (setq bulges (curve:pline-convexity curve))
	     (repeat (curve:subsegments curve)
		     (entity:putdxf
		      (entity:make-text
		       (rtos (curve:subsegment-length
			      curve
			      (car (curve:subsegment-points curve i))
			      (cadr (curve:subsegment-points curve i)))
			     2 3)
		       (point:2d->3d
			(if (= 0 (nth i bulges))
			    (point:mid
			     (car (curve:subsegment-points curve i))
			     (cadr (curve:subsegment-points curve i)))
			  (polar
			   (point:mid
			    (car (curve:subsegment-points curve i))
			    (cadr (curve:subsegment-points curve i)))
			   (-(angle  (car (curve:subsegment-points curve i))
				     (cadr (curve:subsegment-points curve i)))
			     (* 0.5 pi)
			     )
			   (* (nth i bulges) 0.5
			      (distance 
			       (car (curve:subsegment-points curve i))
			       (cadr (curve:subsegment-points curve i)))))
			  ))
		       
		       (* 2.5 (@:get-config '@:draw-scale))
		       (angle  (car (curve:subsegment-points curve i))
			       (cadr (curve:subsegment-points curve i)))
		       0.72
		       0
		       "mb")
		      62
		      1)
		     (setq i (1+ i))
		     ))
	    (t
             (entity:putdxf
              (entity:make-text
               (rtos (curve:length (e2o curve)) 2 3)
               (point:2d->3d (curve:midpoint curve))
               (* 2.5 (@:get-config '@:draw-scale))
               0
               0.72
               0
               "mb")
              62
              1)

	     )))
  (princ))
(defun at-curve:dualline ()
  (@:help '("将单线双向偏移成双线。"))
  (if (null (member "DASHDOT" (tbl:list "linetype")))
      (vla-load *LTS* "DASHDOT" (findfile "acadiso.lin")))
  (setq dualline-width (getdist (strcat"\n"(@:speak"输入双线宽度")"<"(rtos (@:get-config '@curve:dualline-width) 2 3)">：")))
  (if dualline-width (@:set-config @curve:dualline-width dualline-width))
  (setq lst-curve (pickset:to-list
                   (ssget (list (cons 0 (@:get-config '@curve:types))))))
  (foreach curve lst-curve
	   (vla-offset (e2o curve) (* 0.5 (@:get-config '@curve:dualline-width)))
	   (vla-offset (e2o curve) (* 0.5 (@:get-config '@curve:dualline-width) -1))
	   (entity:putdxf curve 6 "DASHDOT")
	   (entity:putdxf curve 62 1)
    )
  (princ)
  )
