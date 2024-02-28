;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'prefabricated-building:first 用于 应用包 prefabricated-building 的 第一个配置项 first 
;; (@:get-config 'prefabricated-building:first) ;; 获取配置顶的值
;; (@:set-config 'prefabricated-building:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(defun prefabricated-building:hello ()
  (@:help (strcat "这里的内容用于在运行这个功能开始时，对用户进行功能提示。\n"
		  "如怎么使用，注意事项等。\n当用户设置了学习模式时，会在命令行或弹窗进行提示。\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (alert (strcat "装配建筑 的第一个功能.\n"
		 "创建了一个配置项 prefabricated-building:first .\n"
		 "这个配置项的值为: " (@:get-config 'prefabricated-building:first)
		 ))
  (princ)
  )
