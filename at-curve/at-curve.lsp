;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@:add-menus 
  '("曲线工具"
    ("双线互连" (at-curve:join))
    ("优化多段线" (at-curve:optimize-lwpl))
    ("平滑路口" (at-curve:fillet-road))
    ("曲线面积" (at-curve:area))
    ("曲线长度" (at-curve:length))
    ("垂线缺口" (at-curve:notch))
    ("连线端点" (at-curve:link-end))
    ("统计线长" (at-curve:stat))))
(@:define-config 
  '@curve:types
  "*POLYLINE,circle,arc,ellipse,spline,region"
  "可操作的曲线的图元类型")
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
        "mm")
      62
      1))
  (princ))
