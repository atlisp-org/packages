(defun sidebar:make-menu (/ *error* fp)
  (defun *error* (msg)
    (if (= 'file (type fp))
	(close fp))
    (@:*error* msg))
  (defun expr2str (expr)
    (cond
     ((listp expr)
      (setq expr (vl-prin1-to-string expr)))
     ((/= 'str (type expr))
      (setq expr (vl-prin1-to-string expr))))
    (setq expr
	  (string:subst-all
	   "&quot;" "\""
	   expr)))
	
  (@:help '("生成 @lisp 屏幕菜单"))
  (if (and (getvar "lispsys")
	   (> (getvar "lispsys") 0))
      (setq fp (open (strcat (@:package-path "sidebar")"Cmd_atlisp.xml")"w" "utf8"))
    (setq fp (open (strcat (@:package-path "sidebar")"Cmd_atlisp-ansi.xml")"w")))
  (write-line "<?xml version=\"1.0\" encoding=\"utf-8\"?>" fp)
  (write-line "<ScreenMenu>" fp)
  (setq i -1)
  (foreach group (append @:*menu* (evaltrans @:*devmenu*) (evaltrans @:menu-man))
	   (write-line
	    (strcat
	     "<Group TextColor=\"\" Title=\"" (car group)"\" indexNum=\"" (itoa (setq i (1+ i))) "\">")
	    fp)
	   (foreach
	    menu (cdr group)
	    (if (= "--" (car menu))
		(write-line
		 (strcat
		  "<Item Shortcut=\"\" "
		  "indexNum=\"" (itoa (setq i (1+ i))) "\">"
		  "-"
		  "</Item>"
		  )
		 fp)		
	    (write-line
	     (strcat
	      "<Item Shortcut=\"\" "
	      "Command=\""(expr2str(cadr menu)) "\" " 
	      "indexNum=\"" (itoa (setq i (1+ i))) "\">"
	      (car menu)
	      "</Item>"
	      )
	     fp)))
	   (write-line "</Group>" fp)
	   )
  (write-line "</ScreenMenu>" fp)
  (close fp)
  (if (and (or (null (getvar "lispsys"))
	       (= 0 (getvar "lispsys")))
	   (findfile (strcat (@:package-path "sidebar")"Cmd_atlisp-ansi.xml"))
	   )
      ;;转码
      (progn
	(if (null (findfile "bin\\iconv.exe"))
	    (@:down-and-unzip "archives/iconv.zip" "bin")
	  (command "shell-bg"
		   (strcat  @:*prefix* "bin\\iconv.exe -f gb2312 -t utf-8 \""
			    (strcat (@:package-path "sidebar")"Cmd_atlisp-ansi.xml") "\" > \"" (strcat (@:package-path "sidebar")"Cmd_atlisp.xml")  "\""
			    ))
	  )))
  )
(defun sidebar:load ()
  (if (null (findfile (strcat  (strcat (@:package-path "sidebar")"Config.xml"))))
      (progn
	(if (/= 'subr (type @:down-file))
	    (@:load-module 'pkgman))
	(@:down-pkg-file (@:uri) (strcat "sidebar/Config.xml") "stable")))
  (if (null (findfile (strcat  (strcat (@:package-path "sidebar")"Cmd_atlisp.xml"))))
      (progn
	(if (/= 'subr (type @:down-file))
	    (@:load-module 'pkgman))
	(@:down-pkg-file (@:uri) (strcat "sidebar/Cmd_atlisp.xml") "stable")))
  
  (setvar "cmdecho" 0)
  (command "netload" (strcat (@:package-path "sidebar")"CAD_ScreenMenu.dll"))
  (setvar "cmdecho" 1)
  )
(if (null (findfile "bin/unzip.exe"))
    (progn
      (if (/= 'subr (type @:down-file))
	  (@:load-module 'pkgman))
      (@:down-file "bin/unzip.exe")
      (sleep 1)))
(if (null (findfile "bin\\iconv.exe"))
    (@:down-and-unzip "archives/iconv.zip" "bin"))

(sidebar:load)
