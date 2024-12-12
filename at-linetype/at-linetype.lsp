;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@:add-menus '((_"linetype")
	       ("加载@lisp线型" (at-linetype:reload "@lisp"))
	       ("加载设备线型" (at-linetype:reload "equip"))
	       ("加载我的线型" (at-linetype:reload "user"))
	       ("编辑我的线型" (at-linetype:edit))
	       ("编辑形文件" (at-linetype:edit-shp))
	       ("编译形文件" '(at-linetype:compile-shp))
	       ("我要定制线型" '(at-linetype:require))))
(defun at-linetype:reload (filename-lt) 
  (@::prompt '("重新加载线型"))
  (setvar "filedia" 0)
  (at-linetype:load-shx)
  (if (findfile (strcat (@:package-path "at-linetype") filename-lt ".lin"))
      (progn
	(setvar "cmdecho" 0)
	(command "-linetype" "l" "*" (strcat (@:package-path "at-linetype") filename-lt ".lin"))
	(while (> (getvar "cmdactive") 0) 
	  (command "")
	  )
	(setvar"cmdecho" 1)
	))
  (setvar "filedia" 1)
  (princ)
)
(defun at-linetype:compile-shp () 
  (@::prompt '("编译并加载线形使用的型文件"))
  (vl-file-delete (strcat @:*prefix* "@lisp.shx"))
  (vl-file-delete (strcat (@:package-path "at-linetype") "@lisp.shx"))
  (@:cmd "compile" (strcat (@:package-path "at-linetype") "@lisp.shp"))
  (vl-file-copy 
    (strcat (@:package-path "at-linetype") "@lisp.shx")
    (strcat @:*prefix* "@lisp.shx")
  )
  (@:cmd "load" "@lisp.shx" "")
)
(defun at-linetype:load-shx () 
  (if (null (findfile "@lisp.shx")) 
    (progn 
      (if (null (findfile (strcat (@:package-path "at-linetype") "@lisp.shx"))) 
        (@:cmd "compile" (strcat (@:package-path "at-linetype") "@lisp.shp"))
      )
      (vl-file-copy 
        (strcat (@:package-path "at-linetype") "@lisp.shx")
        (strcat @:*prefix* "@lisp.shx")
      )
    )
  )
  (@:cmd "load" "@lisp.shx" "")
)
(defun at-linetype:edit () 
  (@::prompt '("编辑 @lisp 线型文件"))
  
  (@:cmd @:editor (strcat (@:package-path "at-linetype") "user.lin"))
)
(defun at-linetype:edit-shp () 
  (@::prompt '("编辑 @lisp 线型相关的形文件"))
  (vl-file-delete (strcat @:*prefix* "@lisp.shx"))
  (vl-file-delete (strcat (@:package-path "at-linetype") "@lisp.shx"))
  (@:cmd @:editor (strcat (@:package-path "at-linetype") "@lisp.shp"))
)
(defun at-linetype:require ()
  (@::prompt '("请求@lisp开发者定制线型"
            "您可以通过 QQ群 , 微信公众号，微信群等联系我们进行定制"))
  (@:browser))
