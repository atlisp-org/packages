;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-sidebar:first 用于 应用包 at-sidebar 的 第一个配置项 first 
;; (@:define-config 'at-sidebar:first "我是配置项 at-sidebar:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-sidebar:first) ;; 获取配置顶的值
;; (@:set-config 'at-sidebar:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "@侧边栏" "加载侧边栏" "(at-sidebar:load)" )
(defun at-sidebar:load ()
  (setq netdll
	(strcat (@::package-path  "at-sidebar")
		"net/at-sidebar-"
		(cond
		 (is-zwcad
		  "ZWCAD")
		 (is-gstarcad
		  "GstarCAD")
		 (t
		 "AutoCAD"))
		".dll"))
		  
  (if (findfile netdll)
      (progn
	(command "netload" (findfile netdll))
	(sleep 5)
	(command "@Pallete"))
    (progn
      (@::@log "INFO" "编译 at-sidebar")
      (if (or (system:which "dotnet")
	      (findfile "C:\\Program Files\\dotnet\\dotnet.exe"))
	  (command "start"
		   (findfile (strcat (@::package-path  "at-sidebar")
				     "compile.bat"))
		   )
	(progn
	  (@::@log "WARN" "没有发现 .NET SDK开发环境")
	  ))
      )))
