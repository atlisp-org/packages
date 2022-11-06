;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@:add-menus
 '("曲线工具"
   ("双线互连" (at-curve:join))
   ("曲线面积" (at-curve:area))))

(defun at-curve:join (/ l1 l2 pts1 pts2)
  (@:help "选择两条线，从最近端点连接成一条.")
  (setq curves (pickset:to-list(ssget '((0 . "*line")))))
  (setq pts1 (curve:pline-3dpoints (car curves)))
  (setq pts2 (curve:pline-3dpoints (cadr curves)))
  (if (< (distance (car pts1) (car pts2))
	 (distance (last pts1) (car pts2)))
      (setq pts1 (reverse pts1)))
  (if (> (distance (last pts1) (car pts2))
	 (distance (last pts1) (last pts2)))
      (setq pts2 (reverse pts2)))
  (entdel (car curves))(entdel (cadr curves))
  (entity:make-lwpline-bold
   (append pts1 pts2)
   nil
   nil
   0 0))


(defun at-curve:area (/ lst-curve pts)
  (@:help '("标注曲线的的闭合面积"))
  (@:prompt "请选择闭合曲线:")
  (setq lst-curve (pickset:to-list (ssget '((0 . "*POLYLINE,circle,ellipse,spline,region")))))
  (foreach
   curve lst-curve
   (entity:make-text
    (rtos (vla-get-area (e2o curve)) 2 3)
    (point:2d->3d (point:centroid (curve:get-points curve)))
    (* 2.5 (@:get-config '@:draw-scale))
    0 0.72 0 "mm"))
  (princ)
  )
