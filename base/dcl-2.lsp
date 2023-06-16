;;==============================块统计

;;;----------------------------------------------------------------------------------------------
;;;检查输入的原始参数表是否使用了组件的别名，如果使用了，便把别名改成组件全名。无论是否已使用组件的别名，都返回可供后续程序使用的参数表。
(defun listFormatInputList (listInput             /                     listComponentAlias    listFormatedInput     listMemberOfInput
                            boolIsAlias           intDefinedAliasNumber k                     strInputComponentName strAlias
                            )
;;;----------------------------------------------------------------------------------------------
;;;定义组件别名表，形式为  ( (  组件别名 组件原名) )
  (setq listComponentAlias '(("bt" "button")
                             ("edit" "edit_box")
                             ("edit12" "edit12_box")
                             ("edit32" "edit32_box")
                             ("listbox" "list_box")
                             ("ComboBox" "popup_list")
                             ("btRadio" "radio_button")
                             ("tg" "toggle")
                             ("btOK" "ok_only")
                             ("btCancel" "cancel_button")
                             ("btErrer" "errtile")
                             ("btHelp" "help_button")
                             ("btInfo" "info_button")
                             ("btOC" "ok_cancel")
                             ("btOCH" "ok_cancel_help")
                             ("btOCHE" "ok_cancel_help_errtile")
                             ("btOCHI" "ok_cancel_help_info")
                             ("color17" "color_palette_1_7")
                             ("color19" "color_palette_1_9")
                             ("color09" "color_palette_0_9")
                             ("color250255" "color_palette_250_255")
                             ("stdColor" "std_rq_color")
                             )
	)
;;;----------------------------------------------------------------------------------------------
  (setq listFormatedInput nil)
  (foreach listMemberOfInput listInput
	   (setq k 0
		 boolIsAlias "No"
		 )
	   (setq strInputComponentName (strcase (car listMemberOfInput) T))
	   (setq intDefinedAliasNumber (length listComponentAlias))
	   (while (and (< k intDefinedAliasNumber) (= boolIsAlias "No"))
	     (setq strAlias (strcase (car (nth k listComponentAlias)) T))
	     (if (= strInputComponentName strAlias)
		 (progn (setq boolIsAlias "Yes")
			(setq listFormatedInput (append listFormatedInput
							(list (cons (cadr (nth k listComponentAlias))
								    (cdr listMemberOfInput)
								    )
							      )
							)
			      )
			)
		 )
	     (setq k (1+ k))
	     )
	   (if (= boolIsAlias "No")
	       (setq listFormatedInput (append listFormatedInput
					       (list (cons (strcase (car listMemberOfInput) T)
							   (cdr listMemberOfInput)
							   )
						     )
					       )
		     )
	       )
	   )
  listFormatedInput
  )
;;;----------------------------------------------------------------------------------------------
;;;把输入的参数表转换为字符串表
(defun listInputToString (listInput  /  listMemberOfInput   listCdrMemberOfInput
			  listMemberOfComponentParameters   listComponentParameters  listCadrMemberOfComponentParameters
			  listResult   k j   boolDefinementFound strTmp  test1   test2  )
;;;----------------------------------------------------------------------------------------------
;;;组件定义参数表，形式为 ( ( 组件名列表) ( 对应参数名列表) )
;;; 无属性控件，生成dcl文件时，在组件名后添加" ; “即可；如为“end”,在dcl文件里加上"}"字符即可
;;; 容器控件及带属性控件，需要在名前添加” : "，名后加 "{"
  (setq listComponentParameters '((("容器控件" "dialog")
                                   ("label"               "key"                 "value"               "initial_focus"
                                    "height"              "width"               "children_alignment"  "children_fixed_height"
                                    "children_fixed_width"
                                    )
                                   )
                                  (("容器控件"          "boxed_column"      "boxed_row"         "boxed_radio_column"
                                    "boxed_radio_row"   "column"            "row"               "radio_row"         "radio_column"
                                    "concatenation"     "paragraph"
                                    )
                                   ("label"               "key"                 "is_enabled"          "alignment"
                                    "height"              "width"               "fixed_height"        "fixed_width"
                                    "children_alignment"  "children_fixed_height"                     "children_fixed_width"
                                    )
                                   )
                                  (("带属性控件" "button")
                                   ("label"         "key"           "action"        "alignment"     "height"        "width" "horizontal_margin"
                                    "vertical_margin" "fixed_height"  "fixed_width"   "is_cancel"     "is_default"    "is_enabled"    "is_tab_stop"
                                    "mnemonic"
                                    )
                                   )
                                  (("带属性控件" "edit_box" "edit12_box" "edit32_box" "fcf_ebox" "fcf_ebox1")
                                   ("label"          "key"            "value"          "action"         "alignment"      "height"
                                    "width"          "fixed_height"   "fixed_width"    "allow_accept"   "edit_limit"     "edit_width"
                                    "is_enabled"     "is_tab_stop"    "mnemonic"       "password_char"
                                    )
                                   )
                                  (("带属性控件" "image" "image_block" "icon_image")
                                   ("key"           "value"         "action"        "alignment"     "height"        "width"
                                    "fixed_height"  "fixed_width"   "is_enabled"    "is_tab_stop"   "mnemonic"      "aspect_ratio"
                                    "color"
                                    )
                                   )
                                  (("带属性控件" "image_button" "swatch" "fcf_ibut" "fcf_ibut1")
                                   ("key"            "action"         "alignment"      "height"         "width"          "fixed_height"
                                    "fixed_width"    "is_enabled"     "is_tab_stop"    "mnemonic"       "allow_accept"   "aspect_ratio"
                                    "color"
                                    )
                                   )
                                  (("带属性控件" "list_box")
                                   ("label"          "key"            "value"          "action"         "alignment"      "height"
                                    "width"          "fixed_height"   "fixed_width"    "allow_accept"   "fixed_width_font"
                                    "is_enabled"     "is_tab_stop"    "list"           "mnemonic"       "multiple_select"
                                    "tabs"           "tab_truncate"
                                    )
                                   )
                                  (("带属性控件" "popup_list")
                                   ("label"         "key"           "value"         "action"        "alignment"     "height"
                                    "width"         "fixed_height"  "fixed_width"   "edit_width"    "fixed_width_font"
                                    "is_enabled"    "is_tab_stop"   "list"          "mnemonic"      "tabs"          "tab_truncate"
                                    )
                                   )
                                  (("带属性控件" "radio_button")
                                   ("label"         "key"           "value"         "action"        "is_enabled"    "is_tab_stop"
                                    "mnemonic"      "alignment"     "height"        "width"         "fixed_height"  "fixed_width"
                                    )
                                   )
                                  (("带属性控件" "slider")
                                   ("label"         "key"           "value"         "action"        "alignment"     "height"
                                    "width"         "fixed_height"  "fixed_width"   "big_increment" "layout"        "max_value"
                                    "min_value"     "mnemonic"      "small_increment"
                                    )
                                   )
                                  (("带属性控件" "spacer")
                                   ("value" "height" "width" "fixed_height" "fixed_width")
                                   )
                                  (("带属性控件" "text" "text_part" "text_25")
                                   ("label" "key" "value" "alignment" "height" "width" "fixed_height" "fixed_width" "is_bold")
                                   )
                                  (("带属性控件" "toggle")
                                   ("label" "key" "value" "action" "alignment" "height" "width" "fixed_height" "fixed_width" "is_enabled"
                                    "is_tab_stop")
                                   )
                                  (("无属性控件"           "cancel_button"        "errtile"              "help_button"
                                    "info_button"          "ok_cancel"            "ok_cancel_help"       "ok_cancel_help_errtile"
                                    "ok_cancel_help_info"  "ok_only"              "spacer"               "spacer_0"
                                    "spacer_1"             "color_palette_1_7"    "color_palette_1_9"    "color_palette_0_9"
                                    "color_palette_250_255"                       "std_rq_color"
                                    )
                                   )
                                  (("无属性控件" "end")) ;以"end"作为单个组件定义的结束，生成dcl文件时，以“}“代替
                                  )
	)
;;;----------------------------------------------------------------------------------------------        
  (setq listResult nil)
  (setvar "dimzin" 8)
  (foreach listMemberOfInput listInput
	   (setq k 0
		 boolDefinementFound "NotYet"
		 )
	   (while (and (= boolDefinementFound "NotYet")
		       (< k (length listComponentParameters))
		       ) ;未找到组件参数名列表且未搜索完组件预定义列表时循环
	     (setq listMemberOfComponentParameters (nth k listComponentParameters))
	     (if (and (member (car listMemberOfInput)
			      (car listMemberOfComponentParameters)
			      )
		      (if (= "spacer" (car listMemberOfInput))
			  (>= (length listMemberOfInput)
			      (length listMemberOfComponentParameters)
			      )
			  T
			  ) ;因spacer既可为无属性控件也可为带属性控件，故特别处理
		      )
		 (progn (setq boolDefinementFound  "Found"
			      listCdrMemberOfInput (cdr listMemberOfInput)
			      )
			(cond ((or (= "容器控件" (car (car listMemberOfComponentParameters)))
				   (= "带属性控件" (car (car listMemberOfComponentParameters)))
				   )
			       listCdrMemberOfInput ;组件参数值有数据时
			       (setq listResult (append listResult
							(list (strcat ":" (car listMemberOfInput) "{\n"))
							)
				     )
			       (setq j                                   0
				     listCadrMemberOfComponentParameters (cadr listMemberOfComponentParameters)
				     )
			       (while (< j (length listCdrMemberOfInput))
				 (if (not (= "" (nth j listCdrMemberOfInput)))
				     (progn ;参数值非空时
				       (if (numberp (nth j listCdrMemberOfInput))
					   (setq strTMP (strcat (nth j listCadrMemberOfComponentParameters)
								"="
								(rtos (nth j listCdrMemberOfInput) 2 3)
								";\n"
								)
						 ) ;参数为数值时
					   (setq strTMP (strcat (nth j listCadrMemberOfComponentParameters)
								"=\""
								(nth j listCdrMemberOfInput)
								"\";\n"
								)
						 ) ;参数非数值时
					   )
				       (setq listResult (append listResult (list strTMP)))
				       )
				     )
				 (setq j (1+ j))
				 )
			       (if (= "带属性控件" (car (car listMemberOfComponentParameters)))
				   (setq listResult (append listResult (list "}\n")))
				   ) ;带属性控件时，在字符串末尾加上组件结束标志 "}"
			       )
			      ((= (car listMemberOfInput) "end")
			       (setq listResult (append listResult (list "}\n")))
			       )
			      (T ;(= "无属性控件" (car (car listMemberOfComponentParameters))) ,默认为无属性控件
			       (setq listResult (append listResult
							(list (strcat (car listMemberOfInput) ";\n"))
							)
				     )
			       )
			      )
			)
		 (setq k (1+ k))
		 )
	     )
	   )
  listResult
  )
;;;----------------------------------------------------------------------------------------------
;;;生成并显示输入对话框
;;;调用形式 ( listGenerateDCL  DCL文件名(无路径及后缀)
;;;                 表( ( ( "组件名或别名")  ( 参数值表 )  )   ...)
;;;                 表( (  "组件编号"  "组件初始值"  )   ...)    ;组件显示值初始化
;;;                 表( (  "组件编号"  "动作代码"  )   ...)      ;需设置动作的组件及对应的动作
;;;                 表("组件编号"  ...)   )    ;用户点“确定”键时，需获取输入值的组件名
;;;注意，调用参数均为字符串形式
;;;返回值为表,形式为 ( 关闭对话框的整数代码       指定组件返回值列表)
(defun listGenerateDCL (strDCLFileName      listInputDefinements                    listKeysAndValues   listKeysAndActions
                        listKeysToGetValue  /                   listFormatedInput   intDialogCloseType  listKeysValue
                        listResult          fStream             strFileFullName     objectFile          fileStream
                        templist            i                   dclid
			)
;;;----------------------------------------------------------------------------------------------
;;;按 ( ( “组件名”  显示"  ) ) 表，设置各组件的值
  (defun SetDCLValues (listKeysAndValues / listEachKeyAndValue)
    (foreach listEachKeyAndValue listKeysAndValues
	     (set_tile (car listEachKeyAndValue)
		       (cadr listEachKeyAndValue)
		       )
	     )
    )
;;;----------------------------------------------------------------------------------------------
;;;按 ( ( “组件名”  “动作"  ) ) 表，把组件与动作关联
  (defun SetDCLActions (listKeysAndActions / listEachKeyAndAction)
    (foreach listEachKeyAndAction listKeysAndActions
	     (action_tile (car listEachKeyAndAction)
			  (cadr listEachKeyAndAction)
			  )
	     )
    )
;;;----------------------------------------------------------------------------------------------
;;;按 ( “组件名” ) 表，查询各组件值并返回值表
  (defun listGetDCLValues (listKeys / listEachKey listValues)
    (setq listValues nil)
    (foreach listEachKey listKeys
	     (setq listValues (append listValues (list (get_tile listEachKey))))
	     )
    listValues
    )
;;;----------------------------------------------------------------------------------------------                 
  (setq strFileFullName (vl-filename-mktemp (strcat strDCLFileName ".dcl")))
  (setq objectFile (open strFileFullName "w"))
  (setq listFormatedInput (listFormatInputList listInputDefinements))
  (setq fileStream (append (list strDCLFileName)
                           (listInputToString listFormatedInput)
			   )
	)
  (foreach fStream fileStream (princ fStream objectFile))
  (close objectFile)
  ;;以上生成dcl文件,以下调用DCL，设置组件值、关联动作，获取返回值
  (setq listResult nil)
  (setq dclid (load_dialog strFileFullName))
  (if (not (new_dialog strDCLFileName dclid ""))
      (progn (alert "对话框加载失败!") (exit))
      )
  (if listKeysAndValues
      (SetDCLValues listKeysAndValues)
      )
  (if listKeysAndActions
      (SetDCLActions listKeysAndActions)
      )
  (if listKeysToGetValue
      (action_tile "accept"  "(setq listKeysValue (listGetDCLValues listKeysToGetValue)) (done_dialog 1)" )
      )
  (setq intDialogCloseType (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete strFileFullName)
  (setq listResult (append (list intDialogCloseType) listKeysValue))
  listResult
  )
;;;----------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------
;;;----------------------------------------------------------------------------------------------
;;;;funlib.lsp
;;; -------------------------------------------------------------------------
;;;计算以当前设置书写的文本占用长度
;;;调用参数形式 (  字符串 )
(defun strLength (str / sLength x1 x2 lst)
  (setq lst (textbox (list (cons 1 str))))
  (setq x1 (car (nth 0 lst))
	x2 (car (nth 1 lst))
	)
  (setq sLength (abs (- x2 x1)))
  sLength
  )
;;; -------------------------------------------------------------------------
;;;以当前设置初始化文本高、宽
(defun initText (/ pt str eTextN)
  (setq pt (list 0 0))
  (setq str "初始化")
  (command "text" pt #ZiGao# 0 str)
  (setq eTextN (entlast))
  (entdel eTextN)
  )
;;; -------------------------------------------------------------------------
;;; 返回polyline的点表
;;;调用参数形式 (  多义线图元名 )
(defun getplpts (pl / mark pts ver1 i ee pt)
  (if (= "POLYLINE" (cdr (assoc 0 (entget pl))))
      (progn  ; read points of ployline
	(setq mark "VERTEX"
              i    0
              ver1 (entnext pl)
	      )
	(while (= "VERTEX" mark)
	  (setq pts (append pts (list (cdr (assoc 10 (entget ver1))))))
	  (setq ver1 (entnext ver1)
		i    (1+ i)
		)
	  (setq mark (cdr (assoc 0 (entget ver1))))
	  )
	)
      (progn  ; read points of lwployline
	(setq ee (entget pl))
	(foreach pt ee
		 (if (= 10 (car pt))
		     (setq
		      pts (append
			   pts
			   (list (append (cdr pt) (list (cdr (assoc 38 ee)))))
			   )
		      )
		     )
		 )
	)
      )
  pts
  )



;;;-------------------------------------------------------------
;;;在001图层、当前空间画直线
;;;调用形式 (  AddLineone 起点坐标  终点坐标 )，如果成功，返回定义数据的图元表，否则返回 nil。
(defun AddLineone (listStartPoint listEndPoint)
  (setq clay (getvar "clayer"))    ; 原始图层状态赋值给oldlay
  (if (null (tblsearch "LAYER" "001")) ; 如果还没有001图层
      (command "._layer" "m" "001" "c" "240" "" "lt" "continuous" "" "LW"
               "0.13" "" ""
	       )
      (setvar "clayer" "001")           ; 否则直接转换到001图层
      )
  (entmake (list '(0 . "LINE")
                 (cons 10 listStartPoint)
                 (cons 11 listEndPoint)
		 )
	   )
  (while (/= 0 (getvar "cmdactive"))   ; 没有它回不到原来图层
    (command pause)
    )
  (SETVAR "CLAYER" CLAY)
  )

;;;------------------------------------------------------------------------
;;;在当前图层、当前空间、按文本居中方式，写单行文本
;;;调用形式 (  AddText_AlignmentMiddle  插入点坐标 显示 文本旋转角度(rad)  文本内容  宽高比例 字体样式名)，如果成功，返回定义数据的图元表，否则返回 nil。
(defun AddText_AlignmentMiddle  (listInsertPoint floatTextHigh floatRotateAngle strText floatScaleFactor  strStyleName)
  (setq clay (getvar "clayer"))    ; 原始图层状态赋值给oldlay
  (if (null (tblsearch "LAYER" "003")) ; 如果还没有003图层
      (command "._layer" "m" "003" "c" "100" "" "lt" "continuous" "" "LW"
               "0.13" "" ""
	       )
      (setvar "clayer" "003")           ; 否则直接转换到003图层
      )
  (entmake (list '(0 . "TEXT")
                 '(10 0 0 0)
                 (cons 11 listInsertPoint)
                 (cons 40 floatTextHigh)
                 (cons 1 strText)
                 (cons 50 floatRotateAngle)
                 (cons 41 floatScaleFactor)
                 (cons 7 strStyleName)
                 '(72 . 1)
                 '(100 . "AcDbText")
		 )
	   )
  (while (/= 0 (getvar "cmdactive"))   ; 没有它回不到原来图层
    (command pause)
    )
  (SETVAR "CLAYER" CLAY)
  )

;;;------------------------------------------------------------------------
(defun AddText_AlignmentMiddleone  (listInsertPointone floatTextHighone floatRotateAngleone strTextone floatScaleFactorone  strStyleNameone)
  (setq clay (getvar "clayer"))    ; 原始图层状态赋值给oldlay
  (if (null (tblsearch "LAYER" "0")) ; 如果还没有0图层
      (command "._layer" "m" "0" "c" "255" "" "lt" "continuous" "" "LW"
               "0.13" "" ""
	       )
      (setvar "clayer" "0")           ; 否则直接转换到0图层
      )
  (entmake (list '(0 . "TEXT")
                 '(10 0 0 0)
                 (cons 11 listInsertPointone)
                 (cons 40 floatTextHighone)
                 (cons 1 strTextone)
                 (cons 50 floatRotateAngleone)
                 (cons 41 floatScaleFactorone)
                 (cons 7 strStyleNameone)
                 '(72 . 1)
                 '(100 . "AcDbText")
		 )
	   )
  (while (/= 0 (getvar "cmdactive"))   ; 没有它回不到原来图层
    (command pause)
    )
  (SETVAR "CLAYER" CLAY)
  )

;;;------------------------------------------------------------------------
(defun AddText_AlignmentMiddletwo  (listInsertPointtwo floatTextHightwo floatRotateAngletwo strTexttwo floatScaleFactortwo  strStyleNametwo)
  (setq clay (getvar "clayer"))    ; 原始图层状态赋值给oldlay
  (if (null (tblsearch "LAYER" "004")) ; 如果还没有004图层
      (command "._layer" "m" "004" "c" "210" "" "lt" "continuous" "" "LW"
               "0.13" "" ""
	       )
      (setvar "clayer" "004")           ; 否则直接转换到004图层
      )
  (entmake (list '(0 . "TEXT")
                 '(10 0 0 0)
                 (cons 11 listInsertPointtwo)
                 (cons 40 floatTextHightwo)
                 (cons 1 strTexttwo)
                 (cons 50 floatRotateAngletwo)
                 (cons 41 floatScaleFactortwo)
                 (cons 7 strStyleNametwo)
                 '(72 . 1)
                 '(100 . "AcDbText")
		 )
	   )
  (while (/= 0 (getvar "cmdactive"))   ; 没有它回不到原来图层
    (command pause)
    )
  (SETVAR "CLAYER" CLAY)
  )
  ;;;------------------------------------------------------------------------
(defun AddText_AlignmentMiddlethree  (listInsertPointthree floatTextHighthree floatRotateAnglethree strTextthree floatScaleFactorthree  strStyleNamethree)
  (setq clay (getvar "clayer"))    ; 原始图层状态赋值给oldlay
  (if (null (tblsearch "LAYER" "005")) ; 如果还没有005图层
      (command "._layer" "m" "005" "c" "41" "" "lt" "continuous" "" "LW"
               "0.13" "" ""
	       )
      (setvar "clayer" "005")           ; 否则直接转换到005图层
      )
  (entmake (list '(0 . "TEXT")
                 '(10 0 0 0)
                 (cons 11 listInsertPointthree)
                 (cons 40 floatTextHighthree)
                 (cons 1 strTextthree)
                 (cons 50 floatRotateAnglethree)
                 (cons 41 floatScaleFactorthree)
                 (cons 7 strStyleNamethree)
                 '(72 . 1)
                 '(100 . "AcDbText")
		 )
	   )
  (while (/= 0 (getvar "cmdactive"))   ; 没有它回不到原来图层
    (command pause)
    )
  (SETVAR "CLAYER" CLAY)
  )
;;输出表头
;;;------------------------------------------------------------------------
;;;在当前图层、当前空间、按文本左对齐方式，写单行文本
;;;调用形式 (  AddText_AlignmentLeft  插入点坐标 显示 文本旋转角度(rad)  文本内容  宽高比例 字体样式名)，如果成功，返回定义数据的图元表，否则返回 nil。
(defun AddText_AlignmentLeft  (listInsertPoint floatTextHigh floatRotateAngle strText floatScaleFactor  strStyleName)
  (entmake (list '(0 . "TEXT")
                 (cons 10 listInsertPoint)
                 (cons 40 floatTextHigh)
                 (cons 1 strText)
                 (cons 50 floatRotateAngle)
                 (cons 41 floatScaleFactor)
                 (cons 7 strStyleName)
                 '(100 . "AcDbText")
		 )
	   )
  )

;;;------------------------------------------------------------------------
;;;在当前图层、当前空间插入块
;;;调用形式 ( InsertBlock  显示  插入点  旋转角度 )
;;;成功时，返回dxf组码，否则返回nil
(defun InsertBlock ( strBlockName listInsertPoint floatRotateAngle )
  (entmake (list '(0 . "INSERT")
		 '(100 . "AcDbEntity")
		 '(100 . "AcDbBlockReference")
		 (cons 2 strBlockName)
		 (cons 10 listInsertPoint)
		 (cons 50 floatRotateAngle)))
  )
;;;-------------------------------------------------------------
;;;标记undo编组开始点
(defun BeginUndoGroup()
  (command "undo" "be")
  )
;;; -------------------------------------------------------------------------
;;;标记undo编组结束点
(defun EndUndoGroup()
  (command "undo" "e")
  )
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;块统计.LSP


;;;--------------------------------------------------------------------------------
;;;从块选择集中选择指定块名的对象，并返回结果选择集
(defun intCountSingleBlock (ssOriginal strTargetBlockName /
			    strEntityName listEntityDXF strBlockName intSingleBlockCount k)
  (setq intSingleBlockCount 0
	k -1 )
  (repeat (sslength ssOriginal) ; 循环与所选择的对象数量相等的次数
	  (setq strEntityName (ssname ssOriginal (setq k (1+ k)))) ; strEntityName，取得第k个对象名
	  (setq listEntityDXF (entget strEntityName))
	  (setq strBlockName (cdr (assoc 2 listEntityDXF)))
	  (if (= strBlockName strTargetBlockName)
	      (setq intSingleBlockCount (1+ intSingleBlockCount))
	      )
	  )
  intSingleBlockCount
  )
;;;--------------------------------------------------------------------------------
;;;从块选择集中删除指定块名的对象，并返回结果选择集
(defun ssDelEntitysFromBlockSelectionSet (ssOriginal strTargetBlockName
					  / strEntityName listEntityDXF strBlockName ssResult k)
  (setq ssResult (ssadd)
	k -1 )
  (repeat (sslength ssOriginal) ; 循环与所选择的对象数量相等的次数
	  (setq strEntityName (ssname ssOriginal (setq k (1+ k)))) ; strEntityName，取得第k个对象名
	  (setq listEntityDXF (entget strEntityName))
	  (setq strBlockName (cdr (assoc 2 listEntityDXF)))
	  (if (/= strBlockName strTargetBlockName)
	      (setq ssResult (ssadd strEntityName ssResult))
	      )
	  )
  ssResult
  )
;;;--------------------------------------------------------------------------------
;;;插入块缩略图
(defun PrintBlockMiniature (floatBasicPointX                  floatBasicPointY                  strBlockName
                            /                                 floatMaxBlockWidth                floatMaxBlockHigh
                            floatBlockOriginalWidth           floatBlockOriginalHigh            floatBlockWidthScale
                            floatBlockHighScale               floatBlockBoundingBoxTargetMinPointX
                            floatBlockBoundingBoxTargetMinPointY   floatBlockBoundingBoxTargetMaxPointX
                            floatBlockBoundingBoxTargetMaxPointY   listTargetBlockCenterPoint
                            listBlockBoundingBoxMinPoint      listBlockBoundingBoxMaxPoint      objectBlockEntity
                            strEntityName                     listInsertPoint                   floatBlockScale
                            listBlockEntityDXF                listBlockCenterPoint
                            )
  ;; floatBasicPointX floatBasicPointY 缩略图所在表格单元左下角点坐标
  ;;计算图块缩略图在图中允许放置范围的左下及右上角点坐标的X、Y数值
  (setq floatMaxBlockWidth 21
	floatMaxBlockHigh 8
	)
  (setq floatBlockBoundingBoxTargetMinPointX (+ floatBasicPointX 2)
	floatBlockBoundingBoxTargetMinPointY (+ floatBasicPointY 1)
	floatBlockBoundingBoxTargetMaxPointX (+ floatBasicPointX floatMaxBlockWidth 2)
	floatBlockBoundingBoxTargetMaxPointY (+ floatBasicPointY floatMaxBlockHigh 1)
	)
  (setq listTargetBlockCenterPoint (list (/ (+ floatBlockBoundingBoxTargetMinPointX  floatBlockBoundingBoxTargetMaxPointX ) 2)
                                         (/ (+ floatBlockBoundingBoxTargetMinPointY floatBlockBoundingBoxTargetMaxPointY )  2 )
                                         0
					 )
	)
  (setq listInsertPoint (list floatBlockBoundingBoxTargetMinPointX  floatBlockBoundingBoxTargetMinPointY  ) )
  (InsertBlock strBlockName listInsertPoint 0)
  ;;以块缩略图允许放置范围的左下角点为块缩略图的基点插入图块
  (setq strEntityName (entlast))
  (setq objectBlockEntity (vlax-ename->vla-object strEntityName))
  (if  (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox
                                                 (list objectBlockEntity  'listBlockBoundingBoxMinPoint 'listBlockBoundingBoxMaxPoint ))
			     ) ;判断块是否存在边框，若块含无限长直线等时，则不存在边框
       (AddText_AlignmentMiddle listBlockCenterPoint 3 0 "本块无缩略图" 0.8 "standard")
       
       (progn
	 (setq listBlockBoundingBoxMinPoint (vlax-safearray->list listBlockBoundingBoxMinPoint) )
	 (setq listBlockBoundingBoxMaxPoint (vlax-safearray->list listBlockBoundingBoxMaxPoint) )
	 
	 (if (> (car listBlockBoundingBoxMaxPoint)  (car listBlockBoundingBoxMinPoint) )
	     (setq floatBlockWidthScale (/ floatMaxBlockWidth
					   (- (car listBlockBoundingBoxMaxPoint) (car listBlockBoundingBoxMinPoint) )
					   )
		   )
	     (setq  floatBlockWidthScale 0)
	     )

	 (if (> (cadr listBlockBoundingBoxMaxPoint) (cadr listBlockBoundingBoxMinPoint)  )
	     (setq   floatBlockHighScale  (/ floatMaxBlockHigh
					     (- (cadr listBlockBoundingBoxMaxPoint) (cadr listBlockBoundingBoxMinPoint) )
					     )
		     )
	     (setq   floatBlockHighScale 0)
	     )
					;计算块缩略图允许放置范围的边框长宽与块外框长宽的比值
	 
	 (cond
	   ((= (+ floatBlockWidthScale floatBlockHighScale) 0)  (setq floatBlockScale 1)) ;块为单点时,缩放比例取为1
	   ((=  floatBlockWidthScale 0)  (setq floatBlockScale floatBlockHighScale) ) ;块为竖直短线时
	   ((=  floatBlockHighScale 0)  (setq floatBlockScale floatBlockWidthScale) ) ;块为水平短线时
	   ((> floatBlockWidthScale floatBlockHighScale )  (setq floatBlockScale floatBlockHighScale) ) ;数值较小者为块的控制缩放比例
	   (T  (setq floatBlockScale floatBlockWidthScale) )  
	   )
	 (setq listBlockEntityDXF (entget strEntityName))
	 (entmod (subst (cons 41 floatBlockScale) (assoc 41 listBlockEntityDXF) listBlockEntityDXF ) )
	 (entupd strEntityName)
	 (setq listBlockEntityDXF (entget strEntityName))
	 (entmod (subst (cons 42 floatBlockScale) (assoc 42 listBlockEntityDXF) listBlockEntityDXF ) )
	 (entupd strEntityName)
	 (setq listBlockEntityDXF (entget strEntityName))
	 (entmod (subst (cons 43 floatBlockScale) (assoc 43 listBlockEntityDXF) listBlockEntityDXF ) )
	 (entupd strEntityName)
	 ;;缩放块
	 (vla-GetBoundingBox objectBlockEntity 'listBlockBoundingBoxMinPoint 'listBlockBoundingBoxMaxPoint)
	 (setq listBlockBoundingBoxMinPoint (vlax-safearray->list listBlockBoundingBoxMinPoint) )
	 (setq listBlockBoundingBoxMaxPoint (vlax-safearray->list listBlockBoundingBoxMaxPoint) )
	 (setq listBlockCenterPoint (list (* 0.5 (+ (car listBlockBoundingBoxMaxPoint) (car listBlockBoundingBoxMinPoint) ) )
					  (* 0.5 (+ (cadr listBlockBoundingBoxMaxPoint) (cadr listBlockBoundingBoxMinPoint) ) )
					  0
					  )
	       )
	 (vla-move objectBlockEntity (vlax-3d-point listBlockCenterPoint) (vlax-3d-point listTargetBlockCenterPoint))
	 )
       )
  )
;;;--------------------------------------------------------------------------------
;;;显示统计结果表
(defun PrintCountResultList (listResult  /  i  ListLength strBlockName
			     intNumberOfSSSingleBlockName   strNumberOfSSSingleBlockName  pt pt1 pt2  pt3 pt4 pt5 pt6 pt7  x  y  x1  y1 y2
			     x2  x3  floatTextHigh floatTextHighone  blocknumber blocknumbersum)
  (setq pt (getpoint "\n点取要标注块统计结果信息的位置:"))
  (setq x (car pt)
	y (cadr pt)
	i 1
	floatTextHigh 4
        floatTextHighone 7
        floatTextHightwo 4.5
	)
  (setq ListLength (length listResult))
  (setq y1 (- y (* (1+ (+ 1 ListLength)) 10))) ;行高取10
  (while (<= i 2)
    (setq x1 (+ x (* i 25))) ;列宽取25
    (setq pt1 (list x1 y 0)
          pt2 (list x1 y1 0)
	  )
    (AddLineone pt1 pt2)
    (setq i (1+ i))
    )
;;;画竖向表格线
  (setq i 1)
  (setq x1 (+ x (* 3 25)))
  (while (<= i (1+ ListLength))
    (setq y1 (- y (* i 10)))
    (setq pt1 (list x y1 0)
          pt2 (list x1 y1 0)
	  )
    (AddLineone pt1 pt2)
    (setq i (1+ i))
    )
;;;画横向表格线
;;;------------------------------------------------------------------------
  (setq clay (getvar "clayer"))    ; 原始图层状态赋值给oldlay
  (if (null (tblsearch "LAYER" "002")) ; 如果还没有002图层
      (command "._layer" "m" "002" "c" "41" "" "lt" "continuous" "" "LW"
               "0.5" "" ""
	       )
      (setvar "clayer" "002")           ; 否则直接转换到002图层
      )
  (setq x1 (+ x (* 3 25))
	y1 (- y (* 10 (+ 2 ListLength))))
  (setq pt1 (list x y 0)
	pt2 (list x1 y1 0)
        pt3 (list x y1 0)
        pt4 (list x1 y 0)
	)
  (command "pline" pt1 pt4 pt2 pt3 "c")
  (princ)
  (while (/= 0 (getvar "cmdactive"))   ; 没有它回不到原来图层
    (command pause)
    )
  (SETVAR "CLAYER" CLAY)
;;;画外围矩形
;;;------------------------------------------------------------------------
  (setq x1 (+ x (* 0.5 25))
	x2 (+ x (* 1.5 25))
	x3 (+ x (* 2.5 25))
	y1 (- y 7)
        y2 (+ y 5)
	)
  (setq pt1 (list x1 y1 0)
	pt2 (list x2 y1 0)
	pt3 (list x3 y1 0)
        pt8 (list x2 y2 0)
	)
  (AddText_AlignmentMiddlethree pt1 floatTextHigh 0 "块缩略图" 0.8 "standard")
  (AddText_AlignmentMiddlethree pt2 floatTextHigh 0 "块名称" 0.8 "standard")
  (AddText_AlignmentMiddlethree pt3 floatTextHigh 0 "块数量" 0.8 "standard")
  (AddText_AlignmentMiddlethree pt8 floatTextHighone 0 "图块数量统计" 0.8 "standard")
  ;;输出表头
;;;------------------------------------------------------------------------

  (setq i 0
	floatTextHigh 3
        blocknumber 0    )
  (while (< i ListLength)
    (setq y1 (+ y (* -10 (+ i 2))))
    (setq ;pt1 (list x1 y1 0)
     pt2 (list x2 (+ y1 3) 0)
     pt3 (list x3 (+ y1 3) 0)
     )
    (setq strBlockName (car (nth i listResult))
          intNumberOfSSSingleBlockName (cadr (nth i listResult))
	  )
    (setq blocknumber (+ blocknumber intNumberOfSSSingleBlockName))
    (setq strNumberOfSSSingleBlockName (itoa intNumberOfSSSingleBlockName))
    (AddText_AlignmentMiddle pt2 floatTextHigh 0 strBlockName 0.8 "standard")
    (AddText_AlignmentMiddle pt3 floatTextHigh 0 strNumberOfSSSingleBlockName 1.0 "standard")
    
    (if (vl-catch-all-error-p (vl-catch-all-apply 'PrintBlockMiniature (list x y1 strBlockName)))
	(AddText_AlignmentLeft (list (+ x 1) (+ y1 2)) 3 0 "生成块缩略图时出错" 0.8 "standard")
	)
    (setq i (1+ i))
    )
;;;显示表内容
  (setq i 0
	blocknumbersum 0 )
  (while (< i ListLength)
    (setq blocknumbersum (+ blocknumbersum intNumberOfSSSingleBlockName))
    (setq i (1+ i))
    )
  (setq intNumberOfSSSingleBlockName (itoa blocknumbersum))               
  (setq strNumberOfSSSingleBlockName (itoa blocknumber))          
  (setq   y1 (- (- y (* 10 (+ 1 ListLength))) 7 )
	  )
  (setq pt5 (list x1 y1 0)
	pt6 (list x2 y1 0)
	pt7 (list x3 y1 0)
	)
  (AddText_AlignmentMiddletwo pt5 floatTextHightwo 0 intNumberOfSSSingleBlockName  0.8 "standard")
  (AddText_AlignmentMiddle pt5 floatTextHightwo 0 "共    种"  0.8 "standard")
  (AddText_AlignmentMiddlethree pt6 floatTextHightwo 0 "汇  总" 0.8 "standard")
  (AddText_AlignmentMiddletwo pt7 floatTextHightwo 0 strNumberOfSSSingleBlockName 0.8 "standard")
  (AddText_AlignmentMiddle pt7 floatTextHightwo 0 "共    个"  0.8 "standard")
  ;;输出表尾
;;;------------------------------------------------------------------------
  )
;;;--------------------------------------------------------------------------------
(defun GetBlocksSelectionRange (/ strDCLFileName listInputDefinements listKeysAndValues listKeysAndActions listKeysToGetValue
				listDCLReturn intButtonClick strSelectRange)
  (setq strSelectRange  "UserSelection" )
  (setq strDCLFileName "BlocksSelectionRange")
  (setq listInputDefinements '(("dialog" "指定统计范围" "")
                               ("spacer")
                               ("radio_column" "进行块统计的范围:")
                               ("btRadio" "手工选择" "brUserSelection")
                               ("btRadio" "整个图形" "brDrawingFile")
                               ("end")
                               ("text" "注:不统计含无限长直线的块!")
                               ("spacer")
                               ("btOK")
                               ("end")
                               )
	)
  (setq listKeysAndValues '(("brUserSelection" "1")))
  (setq listKeysAndActions '(("brUserSelection" "(setq strSelectRange \"UserSelection\")")
                             ("brDrawingFile" "(setq strSelectRange \"DrawingFile\")")) )
  (setq listKeysToGetValue nil)
  (setq listDCLReturn (listGenerateDCL strDCLFileName listInputDefinements listKeysAndValues listKeysAndActions listKeysToGetValue) )
  (setq intButtonClick (car listDCLReturn )  )
  strSelectRange
  )
;;;--------------------------------------------------------------------------------
;;;块数量统计
(defun tktj (/ ssObjects  strEntityName  listEntityDXF  strBlockName
	     listResult  intSingleBlockCount  listMinPoint listInsertPoint  floatBlockRotateAngle
             ;;listResult 用于记录统计结果，形式为((  显示  块数量  同名块中一个实体的对象名 )...)
             )
					; (initget "D S _DrawingFile UserSelection")
					; (setq strSelectRange (getkword "\n统计块的范围[显示(D)/显示(S)]<S>:"))
  (setq strSelectRange (GetBlocksSelectionRange))
  (if (= strSelectRange "DrawingFile")
      (setq ssObjects  (ssget "X" '((0 . "insert")(100 . "AcDbBlockReference")))) ; 创建选择集 ssObjects
      (progn
	(princ "\n请选择需要统计的块:\n")
	(setq ssObjects (ssget '((0 . "INSERT")(100 . "AcDbBlockReference")))) ; 创建选择集 ssObjects
	)
      )
  (if ssObjects
      (progn
	(setq listResult nil)
	(while (> (sslength ssObjects) 0)
	  (setq strEntityName (ssname ssObjects 0)) ; strEntityName，取得第1个对象名
	  (setq listEntityDXF (entget strEntityName))
	  (setq strBlockName (cdr (assoc 2 listEntityDXF)))
	  (setq intSingleBlockCount (intCountSingleBlock ssObjects strBlockName ) )
	  (setq ssObjects (ssDelEntitysFromBlockSelectionSet ssObjects strBlockName))
	  (setq listResult (append listResult
				   (list (list strBlockName intSingleBlockCount))
				   )
		)
	  )
	(setvar "dimzin" 8)
	(setvar "osmode" 0)
	(if  (tblsearch "style" "standard")
	     ;;判断是否存在"standard"字体，有则设为当前，无则创建。
	     (setvar "textstyle" "standard")
	     (command "_style" "standard" "sceie.shx,sceic.shx" 0 0.8 0 "N" "N" "N")
	     )
	(PrintCountResultList listResult)
	(setvar "osmode" 16383)
	)
      )
  (princ)
  )

(defun c:tjk()(tktj))
;;;--------------------------------------------------------------------------------
