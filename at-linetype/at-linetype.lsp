;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-linetype:first 用于 应用包 at-linetype 的 第一个配置项 first 
;; (@:define-config 'at-linetype:first "我是配置项 at-linetype:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-linetype:first) ;; 获取配置顶的值
;; (@:set-config 'at-linetype:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "@线型" "重载线型" '(at-linetype:reload))
(@:add-menu "@线型" "编辑线型" '(at-linetype:edit))
(@:add-menu "@线型" "编辑形" '(at-linetype:edit-shp))
(defun at-linetype:reload ()
  (@:help '("重新加载@lisp线型"
	    ))
  (setvar "filedia" 0)
  (at-linetype:load-shx)
  (command "-linetype" "l" "*" (strcat (@:package-path "at-linetype") "@lisp.lin")
	   )
  (while (> (getvar "cmdactive") 0)
    (command ""))
  (setvar "filedia" 1)
  (princ)
  )
(defun at-linetype:compile-shp ()
  (@:help '("编译并加载线形使用的型文件"))
  (@:cmd "compile" (strcat (@:package-path "at-linetype")"@lisp.shp"))
  (@:cmd "load"  (strcat (@:package-path "at-linetype")"@lisp.shx")))
(defun at-linetype:load-shx ()
  (if (null (findfile "@lisp.shx"))
      (progn
	(if (null (findfile (strcat (@:package-path "at-linetype")"@lisp.shx")))
	    (@:cmd "compile" (strcat (@:package-path "at-linetype")"@lisp.shp")))
	(vl-file-copy (strcat (@:package-path "at-linetype")"@lisp.shx")
		      (strcat @:*prefix* "@lisp.shx"))
	))
  (@:cmd "load" "@lisp.shx" ""))
(defun at-linetype:edit()
  (@:help '("编辑 @lisp 线型文件"))
  (@:cmd @:editor  (strcat (@:package-path "at-linetype")"@lisp.lin")))
(defun at-linetype:edit-shp()
  (@:help '("编辑 @lisp 线型相关的形文件"))
  (@:cmd @:editor  (strcat (@:package-path "at-linetype")"@lisp.shp")))
