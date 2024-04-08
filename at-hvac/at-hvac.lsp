;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-hvac:first 用于 应用包 at-hvac 的 第一个配置项 first 
;; (@:define-config 'at-hvac:first "我是配置项 at-hvac:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-hvac:first) ;; 获取配置顶的值
;; (@:set-config 'at-hvac:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menus
 '("@暖通"
   ("插入说明" (@hvac:draw-readme))
   ("平面图样例" (@hvac:draw-plan-example))
   ))
(defun @hvac:draw-readme ()
  (@:help '("插入暖通说明。"
	    ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (if (findfile (strcat @::*prefix* "packages/at-hvac/readme-hvac.dwg"))
      (block:insert
       "readme-hvac"
       (strcat @::*prefix* "packages/at-hvac/")
       (getpoint "请点击插入位置:")
       0 1))
  )
(defun @hvac:draw-plan-example ()
  (@:help '("插入暖通平面图样例。"
	    ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (if (findfile (strcat @::*prefix* "packages/at-hvac/example-hvac.dwg"))
      (block:insert
       "example-plan-hvac"
       (strcat @::*prefix* "packages/at-hvac/")
       (getpoint "请点击插入位置:")
       0 1))
  )
