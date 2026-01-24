;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-sidebar:first 用于 应用包 at-sidebar 的 第一个配置项 first 
;; (@:define-config 'at-sidebar:first "我是配置项 at-sidebar:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-sidebar:first) ;; 获取配置顶的值
;; (@:set-config 'at-sidebar:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "@侧边栏" "加载侧边栏" "(at-sidebar:load)" )
(@:add-menu "@侧边栏" "编译侧边栏" "(at-sidebar:compile)" )
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
	(command-s "netload" (findfile netdll))
	(command "@Palette"))
    (progn
      (@::@log "INFO" "编译 at-sidebar")
      (at-sidebar:compile)
      )))
(defun at-sidebar:compile ()
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
      ))
(defun at-sidebar:make-pattern-img ()
  "开发版本"
  (setq pat-files (vl-directory-files (strcat @::*prefix*"pattern/") "*.pat" 1))
  (setq rec
	(entity:make-rectangle '(0 0)'(200 200)))
  (setq box (entity:getbox rec 0))
  (setvar "hpscale" 75)
  (setvar "cmdecho" 0)
  (foreach
   patfile% pat-files
   (setvar "hpname" (vl-filename-base patfile%))

   (if(not (findfile (strcat @::*prefix*"pattern/"(getvar "hpname")".png")))
       (progn
	 (command "-hatch" "s" rec "" "")
	 (vla-regen *DOC* acAllViewports)
	 (command "-plot" "y" "" "PublishToWeb PNG.pc3" "200x200" 
		  "P" "n" "w" (car box)(cadr box) "f" "c" "y" (@:get-config 'base:ctb) "y" "a"
		  (strcat @::*prefix*"pattern/" (getvar "hpname")".png")  "n" "y" )
	 (mapcar 'entdel (pickset:to-list (ssget "x" '((0 . "hatch")))))
	 )))
  (setvar "cmdecho" 1)
  (entdel rec)
  (princ)
  )
