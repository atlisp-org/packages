;;(@:define-config 'lpsidemenu:first "我是配置项 lpsidemenu:first 的值" "这个配置项的用途说明。")
(@:add-menu "侧边栏" "加载侧栏" "(lpsidemenu:load)" )
(@:add-menu "侧边栏" "生成菜单" "(lpsidemenu:make-menu)" )

(defun lpsidemenu:load()
  (if (findfile (strcat (@::package-path "lpsidemenu") "lpsidemenu2013.dll"))
      (command "netload" (strcat (@::package-path "lpsidemenu") "lpsidemenu2013.dll")))
  )
(defun lpsidemenu:make-menu()
  (setq i 0)
  (setq fp (open (strcat (@::package-path "lpsidemenu") "@lisp.ini") "w"))
  ;; (setq i 0)
  (foreach
   menu @::*menu*
   (write-line (strcat "[" (car menu)"]") fp)
   (foreach
    menuitem (cdr menu)
    (write-line (strcat
		 (car menuitem)"="
		 (cadr menuitem))
		fp)))
  (close fp)
  (setq fp (open (strcat (@::package-path "lpsidemenu") "Setting.ini") "w"))
  (setq cfg '("[mMenu]"  "Isautoload=1 ;是否自动加载,1加载，0不加载"
	      "Location=1  ;1:左，2:上，3:右，4:下，5:浮动"
	      "Isaddlp=0 ;是否加载LP菜单，1加载，0不加载"
	      "Width=120 ;面板宽度" "nWidth=100"
	      "Imgwidth=20"  "Textheight=12 ;文字高度"
	      "Textcolor=White ;菜单组文字颜色"
	      "Textbackcolor=LightSlateGray ;菜单组文字背景颜色"
	      "Textcolor2=White ;内容文字颜色"
	      "Textbackcolor2=LightGreen ;内容文字背景颜色"
	      "Textbackcolor3=#3B4453 ;展开背景颜色"
	      "Title=@lisp侧边栏"
	      "Title1=@lisp ;可增加20个菜单"
	      ))
  (foreach cfg% cfg
	   (write-line cfg% fp))
  ;;加载其它菜单组
  (if (setq menus  (vl-directory-files (vl-string-right-trim "\\/" (@::package-path "lpsidemenu")) ".ini"  1))
      (repeat menu menus
	      (if (not (member menu '("Setting.ini""@lisp.ini")))
		  (write-line (strcat "Title"(itoa (setq i(1+ i)))"="(vl-filename-base menu))fp))))
  (close fp))
