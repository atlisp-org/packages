;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'composing:first 用于 应用包 composing 的 第一个配置项 first 
(@:define-config 'composing:first "我是配置项 composing:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'composing:first) ;; 获取配置顶的值
;; (@:set-config 'composing:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "排版" "第一个功能" "(composing:hello)" )
(defun composing:hello ()
  (@:help (strcat "这里的内容用于在运行这个功能开始时，对用户进行功能提示。\n"
		  "如怎么使用，注意事项等。\n当用户设置了学习模式时，会在命令行或弹窗进行提示。\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (alert (strcat "排版 的第一个功能.\n"
		 "创建了一个配置项 composing:first .\n"
		 "这个配置项的值为: " (@:get-config 'composing:first)
		 ))
  (princ)
  )
