;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-3d:first 用于 应用包 at-3d 的 第一个配置项 first 
(@:define-config 'at-3d:first "我是配置项 at-3d:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-3d:first) ;; 获取配置顶的值
;; (@:set-config 'at-3d:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "3D相关" "选点剖切" "(at-3d:slice-by-pts)")
(@:add-menu "3D相关" "路径剖切" "(at-3d:slice-by-route)")

(defun at-3d:slice-by-pts ()
  (@:help '("将一个曲面用过多个点的平面剖切"))
  (@:prompt "请选择需要剖切的曲面:")
  (setq surfaces (ssget '((0 . "surface"))))
  
  (@:prompt "请选择用于剖切的控制的点:")
  (setq pts (pickset:to-list(ssget '((0 . "point")))))
  (setq slicesurfaces (mapcar 'read (ui:select-multi "请选择切面方式"
				      '("1 平行于xy面 " "2 平行于yz面" "3 平行于zx面" "4 垂直于导向线"))))
  ;;取点所在的曲线，求点到曲线起点的距离，求点处的切线，求点处的垂面。(vlax-curve-getDistAtPoint curve-obj point)
  ;; (if (and (setq route (ssget pt1))
  ;; 	   (setq firstdiv (vlax-curve-getfirstderiv (e2o route) (vlax-curve-getDistAtPoint (e2o route) pt1))))
  ;;     (setq surface-vr (list pt1

  ;; 			     ))
  ;;   (setq surface-vr nil)
  ;;   )
  (push-var)
  (foreach
   pt% pts
   (setq pt1 (entity:getdxf pt% 10))
   (setq surface-xy (list pt1
			  (mapcar '+ pt1 '(100 0 0))
			  (mapcar '+ pt1 '(0 100 0))))
   (setq surface-yz (list pt1
			  (mapcar '+ pt1 '(0 100 0 ))
			  (mapcar '+ pt1 '(0 0 100 ))))
   (setq surface-zx (list pt1
			  (mapcar '+ pt1 '(0 0 100))
			  (mapcar '+ pt1 '(100 0 0))))
   ;;取点所在的曲线，求点到曲线起点的距离，求点处的切线，求点处的垂面。(vlax-curve-getDistAtPoint curve-obj point)
   (if (and (setq route (car (pickset:to-list (ssget "c"(polar pt1 0 10) (polar pt1 pi 10) '((0 . "*line"))))))
    	    (setq firstdiv (vlax-curve-getfirstderiv (e2o route) (vlax-curve-getDistAtPoint (e2o route) pt1))))
       (setq surface-vr (list pt1
			      (polar pt1 (+ (* 0.5 pi)(atan (cadr firstdiv)(car firstdiv)))  100)
			      (mapcar '+ pt1 '(0 0 100 ))
    			     ))
      (setq surface-vr nil)
      )
   (setq ent-last (entity:make-point '(0 0 0)))
   (setvar "osmode" 0)
   (if (member 1 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-xy)
		  (cadr surface-xy)
		  (caddr surface-xy)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   (if (member 2 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-yz)
		  (cadr surface-yz)
		  (caddr surface-yz)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   
   (if (member 3 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-zx)
		  (cadr surface-zx)
		  (caddr surface-zx)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   (if (member 4 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-vr)
		  (cadr surface-vr)
		  (caddr surface-vr)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   
   )
  (entdel ent-last)
  (pop-var)
  (princ)
  )

(defun at-3d:slice-by-route ()
  (@:help '("将一个曲面用曲线路径剖切"))
  (@:prompt "请选择需要剖切的曲面:")
  (setq surfaces (ssget '((0 . "surface"))))
  
  (@:prompt "请选择用于剖切的路径曲线:")
  (setq route  (car (pickset:to-list (ssget ":S" '((0 . "*line"))))))
  (setq n (getint (@:prompt "请输入分段数:")))
  (setq len-pre (/ (curve:length route) n))
  (setq slicesurfaces (mapcar 'read (ui:select-multi "请选择切面方式"
   				      '("1 平行于xy面 " "2 平行于yz面" "3 平行于zx面" "4 垂直于导向线"))))
  (push-var)
  (setq i 0)
  (repeat
   (1- n)
   (setq pt1 (vlax-curve-getpointatdist (e2o route) (* (setq i (1+ i)) len-pre)))
   (setq surface-xy (list pt1
			  (mapcar '+ pt1 '(100 0 0))
			  (mapcar '+ pt1 '(0 100 0))))
   (setq surface-yz (list pt1
			  (mapcar '+ pt1 '(0 100 0 ))
			  (mapcar '+ pt1 '(0 0 100 ))))
   (setq surface-zx (list pt1
			  (mapcar '+ pt1 '(0 0 100))
			  (mapcar '+ pt1 '(100 0 0))))
   ;;取点所在的曲线，求点到曲线起点的距离，求点处的切线，求点处的垂面。(vlax-curve-getDistAtPoint curve-obj point)
   (if (and (setq firstdiv (vlax-curve-getfirstderiv (e2o route) (vlax-curve-getDistAtPoint (e2o route) pt1))))
       (setq surface-vr (list pt1
			      (polar pt1 (+ (* 0.5 pi)(atan (cadr firstdiv)(car firstdiv)))  100)
			      (mapcar '+ pt1 '(0 0 100 ))
    			      ))
      (setq surface-vr nil)
      )
   (setq ent-last (entity:make-point '(0 0 0)))
   (setvar "osmode" 0)
   (if (member 1 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-xy)
		  (cadr surface-xy)
		  (caddr surface-xy)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   (if (member 2 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-yz)
		  (cadr surface-yz)
		  (caddr surface-yz)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   
   (if (member 3 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-zx)
		  (cadr surface-zx)
		  (caddr surface-zx)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   (if (member 4 slicesurfaces)
       (progn
	 (command "slice" surfaces "" "3"
		  (car surface-vr)
		  (cadr surface-vr)
		  (caddr surface-vr)
		  )
	 (while (> (getvar "cmdactive") 0)
	   (command "")
	   )
	 (if (= "SURFACE" (entity:getdxf (entlast) 0))
	     (setq surfaces (ssadd (entlast) surfaces)))))
   
   )
  (entdel ent-last)
  (pop-var)
  (princ)
  )

