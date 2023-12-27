;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-3d:first 用于 应用包 at-3d 的 第一个配置项 first 
(@:define-config 'at-3d:first "我是配置项 at-3d:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-3d:first) ;; 获取配置顶的值
;; (@:set-config 'at-3d:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "3D相关" "多点剖切" "(at-3d:slice-by-pts)" )

(defun at-3d:slice-by-pts ()
  (@:help '("将一个曲面用过多个点的平面剖切"))
  (@:prompt "请选择曲面")
  (setq surfaces (ssget '((0 . "surface"))))
  
  (@:prompt "请选择剖切面上的点")
  (setq pts (pickset:to-list(ssget '((0 . "point")))))
  (foreach
   pt% pts
   (push-var)
   (setvar "osmode" 0)
   (command "slice" surfaces "" "3"
	    (setq pt1 (entity:getdxf pt% 10))
	    (mapcar '+ pt1 '(0 100.0 0))
	    (mapcar '+ pt1 '(0 0 100.0))
	    )
   (while (> (getvar "cmdactive") 0)
     (command "")
     )
   (pop-var)
   (if (= "SURFACE" (entity:getdxf (entlast) 0))
       (setq surfaces (ssadd (entlast) surfaces)))
   ))

   ;;(foreach
   ;; surface% surfaces
   ;; (setq pt1 (entity:getdxf pt% 10))
    ;; (setq new-surface
    ;; 	  (vla-slicesolid
    ;; 	   surface%
    ;; 	   (point:to-ax pt1)
    ;; 	   (point:to-ax (mapcar '+ pt1 '(0 100.0 0)))
    ;; 	   (point:to-ax (mapcar '+ pt1 '(0 0 100.0)))
    ;; 	   :vlax-true))
   ;; (setq surfaces (cons new-surface surfaces)))))
     
  
