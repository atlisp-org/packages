(@:define-config 'lpsidemenu:width 120 "面板宽度")
(@:define-config 'lpsidemenu:isaddlp 1 "是否加载LP菜单，1加载，0不加载")
(@:define-config 'lpsidemenu:menugroup-color "White" "菜单组文字颜色")
(@:define-config 'lpsidemenu:menuitem-color "White" "菜单项文字颜色")
(@:define-config 'lpsidemenu:menuitem-bgcolor "LightSlateGray" "菜单项背景颜色")
(@:define-config 'lpsidemenu:menuitem-hovercolor "Black" "鼠标悬停时菜单项文字颜色")
(@:define-config 'lpsidemenu:menuitem-hoverbgcolor "LightGreen" "鼠标悬停时菜单项背景颜色")
(@:define-config 'lpsidemenu:title "@lisp侧边栏" "选项板标题")

(@:add-menu "LP侧边栏" "设置侧栏" "(lpsidemenu:setup)" )
(@:add-menu "LP侧边栏" "加载侧栏" "(lpsidemenu:load)" )
(@:add-menu "LP侧边栏" "生成菜单" "(lpsidemenu:make-menu)" )

(defun lpsidemenu:load()
  (if (findfile (strcat (@::package-path "lpsidemenu") "lpsidemenu2013.dll"))
      (command "netload" (strcat (@::package-path "lpsidemenu") "lpsidemenu2013.dll")))
  )
(defun lpsidemenu:about()
  (@::prompt '(""
	     ))
  )
(defun lpsidemenu:setup (/ res)
   (setq @::tmp-search-str "lpsidemenu")
  (@::edit-config-dialog))
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
  (setq cfg (list
	     "[mMenu]"  "Isautoload=1 ;是否自动加载,1加载，0不加载"
	     "Location=1  ;1:左，2:上，3:右，4:下，5:浮动"
	     (strcat "Isaddlp= "(itoa (@::get-config 'lpsidemenu:isaddlp))" ;是否加载LP菜单，1加载，0不加载")
	     (strcat "Width="(itoa (@::get-config 'lpsidemenu:width)) " ;面板宽度")
	     "nWidth=100"
	     "Imgwidth=20"  "Textheight=12 ;文字高度"
	     (strcat "Textcolor=" (@::get-config 'lpsidemenu:menugroup-color) " ;菜单组文字颜色")
	     "Textbackcolor=LightSlateGray ;菜单组文字背景颜色"
	     (strcat "Textcolor2="(@::get-config 'lpsidemenu:menuitem-color)  " ;内容文字颜色")
	     (strcat "Textbackcolor2="(@::get-config 'lpsidemenu:menuitem-hoverbgcolor)" ;内容文字背景颜色")
	     "Textbackcolor3=#3B4453 ;展开背景颜色"
	     (strcat "Title="(@::get-config 'lpsidemenu:title) )
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
