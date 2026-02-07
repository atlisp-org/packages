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
	;; (vla-Load (vla-GetInterfaceObject *ACAD* (findfile new-netdll)))
	;;比较源和目标文件确定是否编译。
	(if (< (@::mktime1900(vl-file-systime netdll))
	       (@::mktime1900(vl-file-systime (strcat (@::package-path  "at-sidebar") "at-sidebar.slnx"))))
	    (at-sidebar:compile))
	;; 复制最新文件
	(vl-file-copy netdll
		      (setq new-netdll (strcat netdll "."(@::timestamp)".dll")))
	(command-s "netload" (findfile new-netdll))
	(command "@Palette"))
    (progn
      (@::@log "INFO" "编译 at-sidebar, 请在编译成功后重新加载.")
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
	(@::@log "WARN" "没有发现 .NET SDK开发环境,请在成功安装 .NET SDK后重试.")
	(command "start"
		 (findfile (strcat (@::package-path  "at-sidebar")
				   "install-dotnet.bat"))
		 )
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
(defun at-sidebar:split-acadpat ()
  (setq patfp (open (strcat @::*prefix* "acadiso.pat") "r"))
  (while (setq line (read-line patfp))
    (cond
     ((= 42 (ascii line))
      ;;关闭前一个
      (if opatfp (close opatfp))
      (setq opatfile (vl-string-trim "*" (car (string:to-list line ","))))
      (setq opatfp (open (strcat @::*prefix* opatfile".pat")"w" "utf8"))
      (write-line line opatfp))
     (t
      (write-line line opatfp))))
  (if opatfp (close opatfp))
  (if patfp (close patfp))
  )
(defun at-sidebar:make-library-img ()
  "开发版本"
  (setq pat-files (vl-directory-files (strcat @::*prefix*"standard/lib/说明/") "*.dwg" 1))
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
(defun at-sidebar:insertlib (blkname / dwgfile content)
  ;; TODO: 下载pat
  (setq dwgfile (strcat @::*prefix* "dw/" blkname".dwg"))
  (@::mkdir (@::path (vl-filename-directory dwgfile)))
  (if (not (findfile dwgfile))
      (@::down-by-base64
       (strcat (@::uri)"/dw/library/"blkname".dwg")
       dwgfile
       (@::timestamp)
       )
    )
  
  (if (findfile dwgfile)
      (progn
	(block:insert (last (@::path blkname))
		      (@::path-os
		       (strcat (vl-filename-directory dwgfile)"/"))
		      (getpoint "请给出插入点:") 0 1)
	)
    (@::@log "WARN" "下载 图库 文件失败")
    ))
(defun at-sidebar:hatch (patname / patfile content)
  ;; TODO: 下载pat
  (setq patfile (strcat @::*prefix* patname".pat"))
  (if (and (not (findfile patfile))
	   (setq content (@::@get (strcat (@::uri)"/dw/pat/"patname".pat"))))
      (progn
	;;处理乱码
	(setq lines (string:to-list content "\n"))
	(setq patfp  (open patfile "w"))
	(write-line (strcat  "*" patname) patfp)
	(foreach line% (cdr lines)
		 (if (not (member (chr (ascii line%))'("*"";")))
		     (write-line
		      (vl-string-trim "\r" line%)
		      patfp)))
	(close patfp))
    (@::@log "WARN" "下载 pat 文件失败")
    )
  
  (if (findfile patfile)
      (progn
	(setvar "hpname" patname)
	(command "-hatch")
	)))
