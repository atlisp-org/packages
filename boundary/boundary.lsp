;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'boundary:first 用于 应用包 boundary 的 第一个配置项 first 
(@::define-config 'boundary:blkname "gc124" "要生成边界的块名")
(@::define-config 'boundary:layer "tmp-boundary" "生成的边界所在的图层")
(@::define-config 'boundary:color 2 "生成的边界所在的图层的颜色")
;; (@:get-config 'boundary:first) ;; 获取配置顶的值
;; (@:set-config 'boundary:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "边界" "生成边界" "(boundary:make-by-blk)" )
(@:add-menu "边界" "填充边界" "(boundary:hatch)" )
(@:add-menu "边界" "删除边界" "(boundary:remove-boundary)" )
(@:add-menu "边界" "删除边界填充" "(boundary:remove-hatch)" )
(@:add-menu "边界" "显示块" "(boundary:show-ent)" )

(defun boundary:make-by-blk (/ *error*)
  "由特定的块生成区域"
  (defun *error*(msg)
    (pop-var)
    )
  (setq filter (list '(0 . "insert")(cons 2 (@::get-config 'boundary:blkname))))
  (setq blks (pickset:to-list(ssget filter)))
  ;; 缩放
  (pickset:zoom blks)
  (layer:make (@::get-config 'boundary:layer)
	      (@::get-config 'boundary:color)
	      nil nil)
  (setvar "clayer" (@::get-config 'boundary:layer))
  (push-var "pickbox")
  (std:osmode-off)
  (setvar "pickbox" 1)
  (while (car blks)
    (setq zone (entity:boundary (car blks)))
    (if zone (entity:set-visible (car blks) nil))
    (setq blks (cdr blks))
    (if zone
	(progn
	  (setq blks-in-zone
		(pickset:to-list
		 ;;可见的情况下才能选中
		 (ssget "wp" (curve:get-points zone) filter)))
	  (setq blks (list:difference blks blks-in-zone))
	  (mapcar '(lambda(x) (entity:set-visible x nil))
		  blks-in-zone)
	  )
	))
  (std:osmode-on)
  (pop-var)
  )

(defun boundary:hatch (/ boundarys)
  "填充边界"
  (setq boundarys
	(ssget"x"
	      (list '(0 . "lwpolyline")
		    (cons 8  (@::get-config 'boundary:layer)))))
  (command "hatch""s" boundarys "" "solid" "")
  (princ)
  )
(defun boundary:remove-hatch ()
  "删除填充边界"
  (mapcar 'entdel
	  (pickset:to-list
	   (ssget
	    "x"
	    (list '(0 . "hatch")
		  (cons 8  (@::get-config 'boundary:layer))))))
  (princ)
  )
(defun boundary:remove-boundary ()
  "删除边界"
  (mapcar 'entdel
	  (pickset:to-list
	   (ssget
	    "x"
	    (list '(0 . "lwpolyline")
		  (cons 8  (@::get-config 'boundary:layer))))))
  (princ)
  )
(defun boundary:show-ent ()
  "显示块"
  (setq filter (list '(0 . "insert")(cons 2 (@::get-config 'boundary:blkname))))
  (mapcar '(lambda(x)(entity:set-visible x t))
	  (pickset:to-list (ssget "x" filter))))
