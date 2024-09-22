;阿甘计算器(命令:ag或agg，ag可修改)-2021.2.24
(setq GL-precision 3)  ;小数保留位数，默认3位，自己可根据需要修改
(setq GL-texthight 400)  ;插入文字高度400，自己可根据需要修改

;以下为快捷键（c:后为快捷键，;号后为注释），自己可根据需要修改
(defun c:ag() (vl-cmdf "agg")(princ));启动命令ag,可修改
(defun c:ad() (GL:jiafa)) ;加
(defun c:a2() (GL:jianfa)) ;减
(defun c:a3() (GL:chengfa)) ;乘
(defun c:a4() (GL:chufa)) ;除
(defun c:sq() (c:shiqutxt)) ;拾取
(defun c:css() (command "GC:agcalcss")) ;插算式

;以下为快捷键（计算并插入结果），自己可根据需要修改
(defun c:ys() (if (GL:jiafa)(GL:crjswz GL-result))(princ)) ;加
(defun c:y2() (if (GL:jianfa)(GL:crjswz GL-result))(princ)) ;减
(defun c:y3() (if (GL:chengfa)(GL:crjswz GL-result))(princ)) ;乘
(defun c:y4() (if (GL:chufa)(GL:crjswz GL-result))(princ)) ;除

;以下为快捷键（计算并插入算式），自己可根据需要修改
(defun c:dd() (if (GL:jiafa)(GL:crjswz GL-formula))(princ)) ;加
(defun c:d2() (if (GL:jianfa)(GL:crjswz GL-formula))(princ)) ;减
(defun c:d3() (if (GL:chengfa)(GL:crjswz GL-formula))(princ)) ;乘
(defun c:d4() (if (GL:chufa)(GL:crjswz GL-formula))(princ)) ;除

;↓↓↓以下内容尽量不要修改(有一定lisp编程基础除外)↓↓↓
(vl-load-com)
;; (setq cadpath (vla-get-Path (vlax-get-acad-object))) ;获取CAD安装路径
(setq vers (substr (getvar "acadver") 1 2))
(if (< (atoi vers) 19)
    (command "netload" (strcat (@:package-path "agan-cal") "AganCal18.dll")) ;DLL文件位置，可修改
    (command "netload" (strcat (@:package-path "agan-cal") "AganCal.dll"))) ;DLL文件位置，可修改
(GC:agrecord (strcat (@:package-path "agan-cal") "历史记录.txt")) ;历史记录txt文件位置，可修改

(command "cal")(command)     ;先调用cal，否则后面cal函数无法使用
(setq GL-result "")          ;计算结果，全局变量
(setq GL-formula "")         ;算式

;c#调用lisp命令
(defun c:GL-ad() (GL:jiafa)) ;加
(defun c:GL-a2() (GL:jianfa)) ;减
(defun c:GL-a3() (GL:chengfa)) ;乘
(defun c:GL-a4() (GL:chufa)) ;除

(defun GL:jiafa ()(GL:Galculate (GL:GetNumber "+")))
(defun GL:jianfa ()(GL:Galculate (GL:GetNumber "-")))
(defun GL:chengfa ()(GL:Galculate (GL:GetNumber "*")))
(defun GL:chufa ()(GL:Galculate (GL:GetNumber "/")))

;选择文字
(defun GL:GetNumber (ope / *error* en en-lst lst n num pt ss ss_data)
  (defun *error* (x) ;出错函数
    (UnHighLight en-lst)
    (setvar "ErrNo" 0)
    (princ x)
  )
  (setvar "ErrNo" 0)
  (setq en-lst (list))
  (while (/= (getvar "ErrNo") 52)
    (if (setq ss (ssget ":S" '((0 . "*TEXT,DIMENSION,INSERT,ATTDEF,ACAD_TABLE,TCH_ELEVATION,TCH_SPACE,TCH_DRAWINGNAME")))) ; 拾取文字、尺寸标注、属性字、CAD表格、天正：标高、面积、图名
    (progn
	(if (= (caar (setq ss_data (ssnamex ss 0))) 1)
	  (progn ;点选时
	    (setq pt (trans (cadr (last (car ss_data))) 0 1)
		      en (car (nentselp pt))
		      en-lst (reverse en-lst);表倒序
		      en-lst (cons en en-lst)
		      en-lst (MJ:delsame en-lst)
		      en-lst (reverse en-lst);表倒序
		      n (itoa (length en-lst))
	    )
	    (redraw en 3)
	    (princ (strcat "\n已拾取 " n " 个对象：" (GL:Formula en-lst ope)))
	  )
	  (progn ;框选时
	    (setq lst (ss->lst ss)
		      lst (reverse lst)
		      en-lst (append en-lst lst)
		      en-lst (MJ:delsame en-lst)
		      n (itoa (length en-lst))
	    )
	    (HighLight lst)
	    (princ (strcat "\n已拾取" n "个对象：" (GL:Formula en-lst ope)))
	  )
	)
	))
  )
  (if en-lst
    (progn
      (setq Num (GL:Formula en-lst ope))
      (UnHighLight en-lst)
    )
  )
  (setvar "ErrNo" 0)
  Num
)

;提取算式
(defun GL:Formula (lst ope / ed en ent i num regex text tn)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0)            ;不忽略大小写
  (vlax-put-property regex "Global" 1)                ;全文匹配，而不是只匹配第一处
  (setq i 0 Num "")
  (repeat (length lst)
    (setq en (nth i lst)
          ed (entget en)
          ent (cdr (assoc 0 ed))
    )
    (cond
      ((wcmatch ent "MTEXT") (setq text (cdr (assoc 1 ed))) (setq text (mtext2text text)));多行文字
      ((wcmatch ent "*TEXT,TCH_ELEVATION,TCH_DRAWINGNAME") (setq text (cdr (assoc 1 ed))));文字、天正：标高、图名
      ((wcmatch ent "TCH_SPACE") (setq text (cdr (assoc 41 ed))));天正面积
      ((wcmatch ent "DIMENSION") (setq text (cdr (assoc 42 ed))));尺寸标注(测量值)
      ((wcmatch ent "ATTDEF") (setq text (cdr (assoc 2 ed))));属性字(提取“标记” 组码2)
    )
    (if text
     (progn
      (vlax-put-property regex "Pattern" "[^0-9\\+\\-\\*\\/\\.\\(\\)\\=]") ;匹配数字和运算符
      (setq text (vlax-invoke-method regex "Replace" text ""))
      (if (/= text "")
       (if (= Num "");表达式加括号 
        (if (wcmatch text "*`+*,*`-*,*`**,*`/*")
         (setq Num (strcat"(" text ")") TN nil)
         (setq Num text TN nil)
        )
        (if (wcmatch text "*`+*,*`-*,*`**,*`/*")
          (setq Num (strcat Num ope "(" text ")"))
          (setq Num (strcat Num ope text))
        )
       )
      )
      (setq text "")
      )
     )
    (setq i (1+ i))
  )
  Num
)

;计算
(defun GL:Galculate (Num / regex)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0)            ;不忽略大小写
  (vlax-put-property regex "Global" 1)                ;全文匹配，而不是只匹配第一处
  (if (and num (/= num ""))
   (progn
    (if (setq GL-result (cal (strcat Num "*" "1.0"))) ;*1.0将整数转换为小数，整数只能介于-32768和32767之间
      (progn
	    (setq GL-result (rtos GL-result 2 GL-precision))
	    (if (wcmatch GL-result "*.*") ;去掉小数点后多余0
	      (progn
	        (vlax-put-property regex "Pattern" ".0+?$") ;去掉.后多余的0
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	        (vlax-put-property regex "Pattern" "[.]$") ;如最后一位是.则去掉
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	    ))
	    (setq GL-formula (strcat Num "=" GL-result))
	    (if (= (car (GC:agform T)) "True")
	      (progn
            (GC:agjsjg GL-result Num) ;c#计算器窗口显示结果
            (princ (strcat "\n表达式：" Num "=" GL-result "  >>>>计算结果：" GL-result))
          )
          (princ (strcat "\n表达式：" Num "=" GL-result "  >>>>计算结果：" GL-result  "       !!!!!计算器窗口未启动，无法保存历史记录!!!!!"))
        )
      )
      (progn
        (setq GL-result "")
        (setq GL-formula "")
        (alert (strcat Num " 表达式错误" "\n\n或数据太大超出计算范围" "\n(数据须介于2147483647和-2147483648 之间>"))
    )))
    (progn
      (setq GL-result "")
      (setq GL-formula "")
  ))
  (princ)
)

;c#计算
(defun GCGalculate (str / jg num regex)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0)            ;不忽略大小写
  (vlax-put-property regex "Global" 1)                ;全文匹配，而不是只匹配第一处
  (vlax-put-property regex "Pattern" "[^0-9\\+\\-\\*\\/\\.\\(\\)\\=]")
  (setq str (vlax-invoke-method regex "Replace" str ""))
  (if str
   (progn
     (setq jg (cal (strcat "(" str ")*1.0"))) ;表达式*1.0将整数转换为小数，整数只能介于-32768和32767之间,否则大整数计算回出错
     (setq GL-result (rtos jg 2 GL-precision))
	    (if (wcmatch GL-result "*.*") ;去掉小数点后多余0
	      (progn
	        (vlax-put-property regex "Pattern" ".0+?$") ;去掉.后多余的0
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	        (vlax-put-property regex "Pattern" "[.]$") ;如最后一位是.则去掉
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	    ))
     (GC:agjsjg GL-result str) ;c#计算器窗口显示结果
     (princ (strcat "\n表达式：" str "=" GL-result "  >>>>计算结果：" GL-result))
   )
   (alert (strcat Num " 表达式错误" "\n\n或数据太大超出计算范围" "\n(数据须介于2147483647和-2147483648 之间>"))
  )
  (princ)
)

;拾取数字和运算符到C#窗口
(defun c:shiqutxt (/ num)
  (setq num (GL:GetNumber "+"))
  (GC:agjsxs num) ;c#计算器窗口显示
  (princ (princ (strcat "\n表达式：" num)))
  (princ)
)

;亮显对象
(defun HighLight (en-lst / en i)
  (setq i 0)
  (repeat (length en-lst)
    (setq en (nth i en-lst))
    (redraw en 3)
    (setq i (1+ i))
  )
)

;取消亮显对象
(defun UnHighLight (en-lst / en i)
  (setq i 0)
  (repeat (length en-lst)
    (setq en (nth i en-lst))
    (redraw en 4)
    (setq i (1+ i))
  )
)

;选择集->图元列表 By caiqs --感谢bbs.mjtd.com黄总
(defun ss->lst (ss / retu)
  (setq retu (apply 'append (ssnamex ss)))
  (setq retu (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) retu))
)

;删除表中相同图元--感谢bbs.mjtd.com黄总
(defun MJ:delsame (L)
  (if L
    (cons (car L) (MJ:delsame (vl-remove (car L) (cdr L))))
  )
)

;提取多行文字,去除无用格式符号--来自bbs.mjtd.com
(defun mtext2text (MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;引用正则表达式控件
  (vlax-put-property regex "IgnoreCase" 0) ;不忽略大小写
  (vlax-put-property regex "Global" 1) ;匹配方式，全文字匹配
  (setq s MTextString)
     ;替换\\字符
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
     ;替换\{字符
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
     ;替换\}字符
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
     ;删除段落缩进格式
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除制表符格式
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除堆迭格式
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除字体、颜色、字高、字距、倾斜、字宽、对齐格式
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除下划线、删除线格式
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除不间断空格格式
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除换行符格式
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除换行符格式(针对Shift+Enter格式)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;删除{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     
     ;替换回\\,\{,\}字符
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
     
  (vlax-release-object regex)
  s
)

;插入文字-动态插入
(defun GL:crjswz (GL-result / boolean code en endate motion pt pt2 text-jiaodu TorN)
  (if (/= GL-result "")
    (progn
      (setq pt (cadr (grread 1)));取得当前光标坐标
      (entmake (list
          '(0 . "TEXT")
           (cons 1 GL-result)
           (cons 10 pt)
           (cons 40 GL-texthight) ;文字高度
           )
      )
      (setq en (entlast))
      (if en ;动态文字
        (progn
                (setq TorN t)
                (setq endate (entget en))
				(princ "\n点取位置或[转90度(A)/右键退出]")
				(setq boolean t)
				(setq text-jiaodu 0)
				(while boolean
					 (setq motion (grread T 8));grread 函数返回一个表，其中第一个元素说明输入类型的代码，第二个元素既可能是整数，又可能是点
					 (setq code (car motion)) ;grread表第一个元素输入类型的代码
					 (setq pt2 (cadr motion)) ;grread表第二个元素 拖动模式坐标
					 (cond
					  ((= code 5)   ;鼠标拖动模式
						 (entmod (setq endate (subst (cons 10 (trans pt2 1 0)) (assoc 10 endate) endate)));动态改文字坐标
					  )
					  ((= code 3)   ;鼠标左鍵按下
					  (setq boolean nil)
					  )
					  ((= code 11)
					   (setq boolean nil)
					   (entdel en)
					   (setq TorN nil)
					  )
					  ((= code 25)
					   (setq boolean nil)
					   (entdel en)
					   (setq TorN nil)
					  )
					  ((equal motion '(2 32))
					   (setq boolean nil)
					  )
					  ((equal motion '(2 13))
					   (setq boolean nil)
					  )
					  ((equal motion '(2 27))
					   (setq boolean nil)
					   (entdel en)
					   (setq TorN nil)
					  )
					  ((equal motion '(2 65))
						 (setq text-jiaodu (+ text-jiaodu (/ pi 2)))
						 (entmod (setq endate (subst (cons 50 text-jiaodu) (assoc 50 endate) endate)));动态改文字角度
					  )
					  ((equal motion '(2 97))
						 (setq text-jiaodu (+ text-jiaodu (/ pi 2)))
						 (entmod (setq endate (subst (cons 50 text-jiaodu) (assoc 50 endate) endate)));动态改文字角度
					  )
					 )
                );end while
  ))))
  (princ)
)
