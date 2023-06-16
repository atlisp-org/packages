;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @ base -- @lisp 基础函数库
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基本库 lib-std.lsp
;;; 基本常用函数。

(setq
 ;;常用VLA对象、集合
 *ACAD*  (vlax-get-acad-object)
 *DOC*   (vla-get-ActiveDocument *ACAD*)
 *DOCS*  (vla-get-Documents *ACAD*)
 *MS*    (vla-get-modelSpace *DOC*)
 *PS*    (vla-get-paperSpace *DOC*)
 *BLKS*  (vla-get-Blocks *DOC*)
 *LAYS*  (vla-get-Layers *DOC*)
 *LTS*   (vla-get-Linetypes *DOC*)
 *STS*   (vla-get-TextStyles *DOC*)
 *GRPS*  (vla-get-groups *DOC*)
 *DIMS*  (vla-get-DimStyles *DOC*)
 *LOUTS* (vla-get-Layouts *DOC*)
 *VPS*   (vla-get-Viewports *DOC*)
 *VS*    (vla-get-Views *DOC*)
 *DICS*  (vla-get-Dictionaries *DOC*)
 *Layouts* (vla-get-Layouts *doc*)
 *DISPLAY* (vla-get-display (vla-get-preferences (vla-get-application *acad*)))
 )
;; 路径分析
(defun @:path (path-str)
  "将字符串格式的路径转换为列表格式。"
  (@:string-to-list path-str "\\"))

(defun @:mkdir (dir-list / dir-path dir% )
  "按列表逐级创建目录。"
  (setq dir-path "")
  (foreach dir% dir-list
	   (if (= 'STR (type dir%))
	       (cond
		 ((= 58 (last (vl-string->list dir%))) ;; 盘符
		  (setq dir-path (strcat dir-path dir%)))
		 ((> (strlen dir%) 0)
		  (setq dir-path (strcat dir-path "\\" dir%))
		  (vl-mkdir dir-path))))
	   )
  dir-path
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 文件操作函数
(defun @:put-list-to-file ( lst wfp / i  slen w lf )
  "将表以行方式写入文件"
  (setq slen (length lst))
  (setq  i 1 lst '() )
  (setq lf (open wfp "w"))
  (foreach w lst
	   (write-line w lf)
	   )
  (close lf)
  (princ)
  )

;;; 排序
(defun @:sort-by-order (mylist order / l m b c)
  "对列表进行排序"
  (setq n (length mylist))
  (setq l 0)
  (setq m 1)
  (while (< l n)
    (setq b (nth l mylist))
    (while (< m n)
      (setq c (nth m  mylist))
      (if (< (atoi (last (@:string-to-list
			  (@:string-subst "-" "－" (cdr (assoc order c))) "-")))
	     (atoi (last (@:string-to-list
			  (@:string-subst "-" "－" (cdr (assoc order b)))"-" ))))
          (progn
            (setq mylist (subst 'aa (nth l mylist) mylist))
            (setq mylist (subst 'bb (nth m mylist) mylist))
            (setq mylist (subst c 'aa mylist))
            (setq mylist (subst b 'bb mylist))
            (setq b c)
            )
          )
      (setq m (1+ m))
      )
    (setq l (1+ l))
    (setq m (1+ l))
    )
  mylist
  )

(defun @:check-consistency (contents order / ti% tmplist)
  "检查某键值的唯一性。返回值为整数。"
  (setq tmplist '())
  (foreach ti% contents 
	   (if (= nil (member (cdr (assoc order ti%)) tmplist))
	       (setq tmplist (append tmplist (list (cdr (assoc order ti%)))))))
  (length tmplist)
  )

(defun @:princ (content)
  (princ (@:to-string content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基础函数
;;; 用于定义一些全局变量和其他分类库需要使用的通用基础函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun std:acad-object nil
  "返回CAD对象"
  (eval (list 'defun 'std:acad-object 'nil (vlax-get-acad-object)))
  (std:acad-object)
  )

(defun std:active-document nil
  "返回当前活动文档对象"
  (eval (list 'defun 'std:active-document 'nil (vla-get-activedocument (vlax-get-acad-object))))
  (std:active-document)
  )

(defun std:model-space nil
  "返回模型空间对象"
  (eval (list 'defun 'std:model-space 'nil (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))))
  (std:model-space)
  )

(defun std:layers nil
  "返回图层集合"
  (eval (list 'defun 'std:layers 'nil (vla-get-Layers (vla-get-activedocument (vlax-get-acad-object)))))
  (std:layers)
  )

(defun std:linetypes nil
  "返回线型集合"
  (eval (list 'defun 'std:line-types 'nil (vla-get-Linetypes (vla-get-activedocument (vlax-get-acad-object)))))
  (std:line-types)
  )

(defun std:TextStyles ()
  "返回字体样式集合"
  (eval (list 'defun 'std:TextStyles 'nil (vla-get-TextStyles (vla-get-activedocument (vlax-get-acad-object)))))
  (std:TextStyles)
  )

(defun std:getinput (promptstr inplist default / inp) 
  "获取输入，结合initget和getkword函数"
;;;arg:promptstr:提示字符串
;;;arg:inplist:关键字列表
;;;arg:default:默认返回关键字，如果没有为nil
;;;return:返回字符串
;;;example:(std:getinput "请输入参数" '("Y" "N") "Y")
  (initget (if default 0 1)	(string::from-lst inplist " ")) ;根据默认值确定initget参数
  (if (setq inp 
	    (getkword 
	     (strcat
	      (if promptstr (strcat promptstr " [") "[") ;结合提示字符串和[]
	      (string::from-lst inplist "/") ;处理提示字符串
	      "]"                  
	      (if (and default (member default inplist)) ;处理默认值
		  (strcat " <" default ">: ")
		  ": ")
	      )
	     )
	    ) 
      inp
      default
      )
  );此函数未处理参数 inplist 合法性

(defun std:startundo (doc)
;;;name:std:startundo
;;;desc:开始撤销编组 -- lee mac
;;;arg:doc:当前活动文档-(std:active-document)
;;;return:nil
;;;example:(std:startundo (std:active-document))
  (std:endundo doc)
  (vla-startundomark doc)
  )

(defun std:endundo ( doc )
;;;name:std:endundo
;;;desc:结束编组 -- lee mac
;;;arg:doc:当前活动文档-(std:active-document)
;;;return:nil
;;;example:(std:endundo (std:active-document))
  (while (= 8 (logand 8 (getvar 'undoctl)))
    (vla-endundomark doc)
    )
  )

(defun std:protect-assign (syms)
;;;name:std:protect-assign
;;;desc:符号保护 
;;;arg:syms:变量名列表
;;;return:
;;;example:(std:protect-assign '(aaa bbb))
  (eval	(list 'pragma
	      (list 'quote (list (cons 'protect-assign syms)))
	      )
	)
  )

(defun std:unprotect-assign (syms)
;;;name:std:unprotect-assign
;;;desc:符号解除保护 
;;;arg:syms:变量名列表
;;;return:
;;;example:(std:unprotect-assign '(aaa bbb))
  (eval
   (list 'pragma
	 (list 'quote (list (cons 'unprotect-assign syms)))
	 )
   )
  )

(defun defconstant (name value)
  "定义全局常量。全局常量通常以 + 开始和结尾。"
  "一个符号保护的变量"
  "(defconstant '+aaa+ 2)"
  (setq name (vl-symbol-name name))
  (eval
   (list 'pragma
	 (list 'quote (list (cons 'unprotect-assign name)))
	 )
   )
  ;;(std:unprotect-assign (list (read name)))
  (set (read name) value)
  (eval	(list 'pragma
	      (list 'quote (list (cons 'protect-assign name)))
	      )
	)
  ;;(std:protect-assign (list (read name)))
  )

(defun s:doc-gen (lspfilename / arg description docpath example fbasename ff filepath fpath header lines markdownfile ret subroutine)
  
;;;name:std:doc-gen
;;;desc:文档生成函数
;;;arg:lspfilename:要生成文档的lsp文件名，格式为getfiled返回值的格式
;;;return:生成markdown文件
;;;example:(std:doc-gen "E:\\lisptest.lsp")
  (defun header (filename)
    (write-line  (string:format "# {0}\r\n" filename) markdownfile))
  
  (defun subroutine (str)
    (write-line  (string:format "## {0}\r\n" str) markdownfile))
  
  (defun description (str) 
    (write-line  (string:format "说明：\r\n{0}\r\n\r\n参数：\r\n" str) markdownfile))
  
  (defun arg (str)
    (setq str (string:to-lst str ":"))
    (write-line 
     (if (> (length str) 1) 
	 (string:format "* {0} - {1}\r\n" str)
	 "* No arguments\r\n"
	 )
     markdownfile
     )
    )
  
  (defun ret (str)
    (write-line (string:format "返回值: \r\n{0}\r\n" str) markdownfile)
    )
  
  (defun example (str)
    (write-line (string:format "示例:\r\n```\r\n{0}\r\n ```\r\n" str) markdownfile)
    )
  
  (defun default (str)
    (write-line (string:format " + {0}\r\n" str) markdownfile)
    )
  
  (defun defaultexample (str)
    (write-line (string:format "```\r\n{0}\r\n ```\r\n" str) markdownfile)
    )
  
  (setq filepath (vl-filename-directory lspfilename)
	fbasename (vl-filename-base lspfilename)
	docpath (strcat filepath "\\doc\\")
	)
  (vl-mkdir docpath)
  (setq markdownfile (open (setq fpath (strcat docpath fbasename ".markdown")) "w"))
  (header (strcat (vl-filename-base lspfilename) ".lsp"))
  (setq ff (open lspfilename "r"))
  (while (setq lines (read-line ff))
    (cond 
      ((wcmatch lines ";;;name:*") (subroutine (vl-string-subst "" ";;;name:" lines)))
      ((wcmatch lines ";;;desc:*") (description (vl-string-subst "" ";;;desc:" lines)))
      ((wcmatch lines ";;;arg:*") (arg (vl-string-subst "" ";;;arg:" lines)))
      ((wcmatch lines ";;;return:*") (ret (vl-string-subst "" ";;;return:" lines)))
      ((wcmatch lines ";;;example:*") (example (vl-string-subst "" ";;;example:" lines)))
      ((wcmatch lines ";;;(*") (defaultexample (vl-string-subst "" ";;;" lines)))
      ((wcmatch lines ";;;*") (default (vl-string-subst "" ";;;" lines)))
      )
    )
  
  (close ff)
  (close markdownfile)
  (print (strcat "生成markdown文档完毕，位置：" fpath ))
  (princ)
  )

(defun std:timer-start ()
  "计时器开始函数"
  "计时器全局变量"
  (setq @:*timer-prg* (getvar "TDUSRTIMER"))
  )
(defun std:timer-end ()
  "计时器结束函数"
  (princ "\n    用时")
  (princ (* (- (getvar "TDUSRTIMER") @:*timer-prg*) 86400))
  (princ "秒\n")
  (setq @:*timer-prg* nil)
  (princ)
  )
(defun std:e->vla (ename)
;;;name:std:e->vla
;;;desc:重定义vlax-ename->vla-object函数
;;;arg:ename:图元名
;;;return:vla对象
;;;example:(std:e->vla (car (entsel)))
  (vlax-ename->vla-object ename)
  )
(defun std:vla->e (obj)
;;;name:std:vla->e
;;;desc:重定义vlax-vla-object->ename函数
;;;arg:obj:vla对象名
;;;return:图元名
;;;example:(std:vla->e obj)
  (vlax-vla-object->ename obj)
  )
(defun std:save-system-variable (a)
;;;desc:保存系统变量函数,保存当前的系统变量,为程序在非正常退出时恢复系统变量用
;;;arg:a:系统变量名组成的表(变量名  变量名 ....)
;;;return:全局变量-*user-system-variable*-系统变量及其值组成的表((变量名 . 值) (...  ...))
;;;example:(std:save-system-variable '("cmdecho" "osmode" "dimtxt"))
  (setq *user-system-variable* (mapcar 'cons a (mapcar 'getvar a))) 
  )
(defun std:reset-system-variable ()
;;;name:std:reset-system-variable
;;;desc:恢复系统变量函数，和std:save-system-variable成对使用
;;;arg:
;;;return:nil
;;;example:(std:reset-system-variable)
  (mapcar 'setvar (mapcar 'car *user-system-variable*) (mapcar 'cdr *user-system-variable*))
  (setq *user-system-variable* nil)
  )
(defun std:return (value)
  "返回值函数，用于包装将要返回的值，主要作用还是为了含义更明确。"
  value)
(defun std:AddSupportPath (lst)
;;;name:std:AddSupportPath
;;;desc:添加支持文件搜索路径，将路径添加到最后
;;;arg:lst:要添加的路径列表
;;;return:支持文件搜索路径字符串=ACAD环境变量值
;;;example:(std:AddSupportPath '("C:\\Folder1" "C:\\Folder2" "C:\\Folder3"))
  ((lambda (str lst)
     (if (setq lst
	       (vl-remove-if
		'(lambda ( x )
		  (or (vl-string-search (strcase x) (strcase str))
		   (not (findfile x))
		   )
		  )
		lst
		)
	       )
	 (setenv "ACAD" (strcat str ";" (apply 'strcat (mapcar '(lambda (x) (strcat x ";")) lst))))
	 )
     )
   (vl-string-right-trim ";" (getenv "ACAD"))
   (mapcar '(lambda (x) (vl-string-right-trim "\\" (vl-string-translate "/" "\\" x))) lst)
   )
  )
(defun std:RemoveSupportPath (lst / del str tmp)
;;;name:std:RemoveSupportPath
;;;desc:删除支持文件搜索路径
;;;arg:lst:要删除的路径列表
;;;return:支持文件搜索路径字符串=ACAD环境变量值
;;;example:(std:RemoveSupportPath '("C:\\Folder1" "C:\\Folder2" "C:\\Folder3"))
  
  (defun del (old str / pos)
    (if (setq pos (vl-string-search (strcase old) (strcase str)))
	(strcat (substr str 1 pos) (del old (substr str (+ 1 pos (strlen old)))))
	str
	)
    )   
  (setq str (strcat (vl-string-right-trim ";" (getenv "ACAD")) ";")
	tmp str
	)
  (foreach pth lst
	   (setq str (del (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" pth)) ";") str))
	   )
  (if (/= tmp str) (setenv "ACAD" str))
  )


(defun std:CatchApply (fun args / result)
;;;name:std:CatchApply
;;;desc:重定义 VL-CATCH-ALL-APPLY ，Gu_xl
;;;arg:fun:函数 如 distance or 'distance
;;;arg:args:函数的参数表
;;;return:如函数运行错误返回nil,否则返回函数的返回值
;;;example:(std:CatchApply '+ '(1 2 3 4))
  (if
   (not
    (vl-catch-all-error-p
     (setq result
	   (vl-catch-all-apply
	    (if (= 'SYM (type fun))
		fun
		(function fun)
		)
	    args
	    )
	   )
     )
    )
   result
   )
  )

(defun std:RemoveMenuItem (POPName / menubar menuitem)
;;;name:std:RemoveMenuItem
;;;desc:移除下拉菜单，Gu_xl 
;;;arg:POPName:下拉菜单名称
;;;return:成功返回T，反之nil
;;;example:(std:RemoveMenuItem "CASS工具");移除 “CASS工具” 菜单
  (setq MenuBar (vla-get-menubar (vlax-get-acad-object)))
  ;; 找菜单 Item 
  (setq menuitem (std:CatchApply 'vla-item (list MenuBar POPName)))
  (if menuitem (std:CatchApply 'vla-RemoveFromMenuBar (list menuitem)))
  )

(defun std:AddMenu (MenuGroupName POPName PopItems InsertBeforeItem / i menubar menuitem n popupmenu)
;;;name:std:AddMenu
;;;desc:添加下拉菜单，Gu_xl
;;;arg:MenuGroupName:要插入的菜单组名称
;;;arg:POPName:下拉菜单名称
;;;arg:PopItems:下拉菜单列表，如 '((标签 命令 帮助字串 次级子项)...) 表为下拉菜单列表，注意命令后要有一个空格
;;;arg:InsertBeforeItem:在该菜单条名称之前插入，例如 "工具箱"，若为 nil,则插在最后
;;;return:无
;;;example:(std:AddMenu "ACAD" "CASS工具" items "工具箱")
  ;;卸载原有菜单
  (std:RemoveMenuItem POPName)
  
  (setq MenuBar (vla-get-menubar (vlax-get-acad-object)))
  (if InsertBeforeItem
      (progn
	;; 查找菜单“工具箱”
	(setq n (vla-get-count MenuBar))
	(setq i (1- n))
	(while
	    (and (>= i 0)      ; 没有超过上限
		 (/= InsertBeforeItem
		     (vla-get-name (setq menuitem (vla-item MenuBar i)))
		     )        ; 找到"工具箱"菜单条
		 )
	  (setq i (1- i))
	  )
	(if (< i 0)      ; 如果没有文件菜单, 取最后一条菜单菜单
	    (setq i (vla-get-count MenuBar))
	    )
	)
      (setq i (vla-get-count MenuBar)) ;_  取最后一条菜单菜单
      )
  ;;创建"CASS工具"菜单条
  (if (not
       (setq popupmenu
	     (std:CatchApply
	      'vla-Item
	      (list
	       (vla-get-menus
		(vla-item
		 (vla-get-MenuGroups (vlax-get-acad-object))
		 MenuGroupName ;_ "测量工具集" 菜单组名称
		 )
		)
	       POPName ;_ "CASS工具" 下拉菜单名称
	       )
	      )
	     )
       )
      (setq popupmenu
	    (vla-add
	     (vla-get-menus
	      (vla-item (vla-get-MenuGroups (vlax-get-acad-object))
			MenuGroupName ;_ "测量工具集" 菜单组名称
			)
	      )
	     POPName ;_ "CASS工具" 下拉菜单名称
	     )
	    )
      )
  ;;清除Menu子项
  (vlax-for popupmenuitem popupmenu
	    (vla-delete popupmenuitem)
	    )
  ;;插入"CASS工具"菜单条
  (vla-InsertInMenuBar popupmenu i)
  (std:insertPopMenuItems popupmenu PopItems)
  (princ)
  )
(defun std:insertPopMenuItems (popupmenu PopItems / K TMP)
;;;name:std:insertPopMenuItems
;;;desc:逐项插入菜单条，Gu_xl
;;;arg:popupmenu:菜单条vla对象
;;;arg:PopItems:下拉菜单列表,如 '((标签 命令 帮助字串 次级子项)...) 表为下拉菜单列表，注意命令后要有一个空格
;;;return:菜单项列表
;;;example:(std:insertPopMenuItems popupmenu PopItems)
  (setq k 0)
  ;;插入"CASS工具"菜单子项目
  (mapcar
   (function
    (lambda (x / Label cmdstr hlpstr subItems tmp)
     (setq Label    (car x)
	   cmdstr   (cadr x)
	   hlpstr   (caddr x)
	   subItems (cadddr x)
	   )
     (if (= label "--")
	 ;; 插入分隔符
	 (vla-AddSeparator
	  popupmenu
	  (setq k (1+ k))
	  )
	 (if (and Label cmdstr)
	     ;; 插入菜单条
	     (progn
	       (setq tmp
		     (vla-addmenuitem
		      popupmenu
		      (setq k (1+ k))
		      Label
		      cmdstr
		      )
		     )
	       (vla-put-helpstring tmp hlpstr)
	       )
	     ;; 插入下一级子菜单
	     (progn
	       (setq tmp
		     (vla-addsubmenu
		      popupmenu
		      (setq k (1+ k))
		      Label
		      )
		     )
	       (if subItems ;_ 添加子级菜单
		   (std:insertPopMenuItems tmp subItems)
		   )
	       )
	     )
	 )
     )
    )
   ;;'((标签 命令 帮助字串 次级菜单项)) 表为菜单项，注意命令后要有一个空格
   PopItems
   )
  )

(defun std:AddToolBars (MENUGROUPNAME TOOLBARITEMS / flyout flyoutbutton helpstring idx items largeiconname left macro menugroupobj name smalliconname toolbar toolbaritem toolbarname toolbars top)
;;;name:std:AddToolBars
;;;desc:添加工具条 By Gu_xl 明经通道
;;;arg:MENUGROUPNAME:菜单组名
;;;arg:TOOLBARITEMS:要添加的工具条列表,格式如下:((toolBarName Left Top (Name HelpString Macro SmallIconName [LargeIconName] [FlyoutButton])...)...)
;;;toolBarName ;_ 工具条名称
;;;Left ;_ 工具条在屏幕左边像素坐标
;;;Top ;_ 工具条在屏幕顶部像素坐标
;;;Name ;_ 按钮名称
;;;HelpString ;_ 说明字串
;;;Macro ;_ 命令宏，注意命令后要有一个空格
;;;SmallIconName ;_ 按钮小图标16x16,图像文件要在搜索目录下或在DLL资源文件中
;;;[LargeIconName] ;_ 按钮大图标24x24,图像文件要在搜索目录下或在DLL资源文件中
;;;[FlyoutButton] ;_ 若是浮出按钮，则为 浮出按钮关联的工具条名称字串，否则为nil或不提供
;;;return:无
;;;example:(std:AddToolBars "ACAD" items)
  
  (if (not (setq menugroupobj
		 (std:CatchApply
		  vla-item
		  (list
		   (vla-get-MenuGroups (vlax-get-acad-object))
		   MenuGroupName ;_ "测量工具集" 菜单组名称
		   )
		  )
		 )
	   )
      (progn
	(alert (strcat "菜单组\""
		       MenuGroupName
		       "\"不存在！无法加载菜单条！"
		       )
	       )
	(exit)
	)
      )
  (setq toolBars (vla-get-toolbars menugroupobj)) ;_ 工具条
  (foreach items toolbarItems
	   (setq toolBarName (car items) ;_ 工具条名称
		 Left        (cadr items) ;_ 工具条 屏幕位置
		 Top        (caddr items) ;_ 工具条屏幕位置
		 items        (cdddr items)
		 )
	   (if  (setq toolbar
		      (std:CatchApply
		       vla-item
		       (list toolBars toolBarName)
		       )
		      )
		(vla-delete toolbar)
		)
	   (setq toolbar (vla-add toolBars toolBarName))
	   (vla-put-left toolbar left)
	   (vla-put-top toolbar Top)
	   (setq idx 0)
	   (foreach lst items
		    (setq name (car lst)
			  HelpString (cadr lst)
			  Macro (caddr lst)
			  SmallIconName (cadddr lst)
			  LargeIconName (car (cddddr lst))
			  FlyoutButton (cadr (cddddr lst))
			  )
		    (if (not LargeIconName)
			(setq LargeIconName SmallIconName)
			)
		    (if FlyoutButton
			(setq Flyout :vlax-true)
			(setq Flyout :vlax-false)
			)
		    (setq ToolbarItem
			  (std:CatchApply
			   vla-AddToolbarButton
			   (list toolbar idx name HelpString Macro Flyout)
			   )
			  )
		    (std:CatchApply
		     vla-SetBitmaps
		     (list ToolbarItem SmallIconName LargeIconName)
		     )
		    (if FlyoutButton
			(std:CatchApply
			 vla-AttachToolbarToFlyout
			 (list ToolbarItem MENUGROUPNAME FlyoutButton)
			 )
			)
		    (setq idx (1+ idx))
		    )
	   )
  )

(defun std:rgb (red green blue)
  "计算RGB颜色对应的整数值。Red Green Blue 取值范围为 [0,255]的整数或[0,1)的小数。"
  "RGB颜色值"
  "(std:rgb 255 0 0) or (std:rgb 0.999 0 0);红色"
  (cond
    ((and (<= 0 red)(< red 1)
	  (<= 0 green)(< green 1)
	  (<= 0 blue)(< blue 1))
     (+ (fix (* 256 red)) (* 256 (fix (* 256 green))) (* 65536 (fix (* 256 blue)))))
    ((and (<= 0 red 255)
	  (<= 0 green 255)
	  (<= 0 blue 255))
     (+ (fix red) (* 256 (fix green)) (* 65536 (fix blue))))
    (t 
     (+ 255 (* 256 255)(* 65536 255)))))

(setq @:get-eval-code @:get-exec-permit)
(setq @:run-from-web @:load-remote)

;; Local variables:
;; coding: gb2312
;; End: 
