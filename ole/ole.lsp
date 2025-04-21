;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'ole:first 用于 应用包 ole 的 第一个配置项 first 
;;(@:define-config 'ole:size "我是配置项 ole:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'ole:first) ;; 获取配置顶的值
;; (@:set-config 'ole:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "ole图像" "批量插入" "(ole:multi-insert)" )
(defun ole:hello ()
  (@:help (strcat "这里的内容用于在运行这个功能开始时，对用户进行功能提示。\n"
		  "如怎么使用，注意事项等。\n当用户设置了学习模式时，会在命令行或弹窗进行提示。\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (alert (strcat "ole图像 的第一个功能.\n"
		 "创建了一个配置项 ole:first .\n"
		 "这个配置项的值为: " (@:get-config 'ole:first)
		 ))
  (princ)
  )
(defun ole:multi-insert ()
  (@::help '("将文件夹中的图像文件jpg/png,插入到当前dwg中"))
  (if (setq folder (system:get-folder "请选择要插入的图片文件所有在文件夹"))
      (progn
	(setq pt-ins (getpoint "插入点:"))
	(foreach imgfile
		 (vl-sort
		  (append
		   (vl-directory-files folder "*.jpg" 1)
		   (vl-directory-files folder "*.png" 1))
		  '<)
		 ;; 图片数据入剪贴板
		 (@::cmd"shell-bg"
			(strcat "py oleimage '"
				folder"\\"
				imgfile
				"'"))
		 ;;等待完成
		 (sleep 5)
		 (setq box (entity:getbox(entlast) 0)
		 (setq pt-ins (polar pt-ins
				     0
				     (- (cadr box)
					(car box))))
		 ))
	))
  (princ))
  
	     
