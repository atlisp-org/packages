;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vitaltools -- 唯他工具集
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 文本操作
;;; 排版，批量改对齐方式，属性变文本，
(@:define-config '@text:fonts "tssdeng.shx,tssdchn.shx" "文字样式 standard 的字体文件")
(@:define-config '@text:color 10 "临时绘线的颜色号，每次变化加以区别")
(@:define-config '@text:temp-layer "@temp@" "临时绘线的图层")

(defun @text:setup (/ res)
  "工程管理基本信息"
  (setq res 
	(ui:input "配置信息"
		  (mapcar '(lambda (x) (list (strcase (vl-symbol-name (car x)) T)(cadr x)(cddr x)))
			  (vl-remove-if '(lambda (x) (not (wcmatch (vl-symbol-name (car x)) "`@TEXT:*")))
					(if @:*config.db*
					    @:*config.db* (@:load-config))))))
  (foreach res% res
   	   (@:set-config (read (car res%)) (cdr res%)))
  )
(defun @:set-fonts (/ fonts)
  (setq fonts (@:string-to-list (@:get-config '@text:fonts) ","))
  (if (and (findfile (car fonts)) (findfile (last fonts)))
      (command "-style" "Standard" (@:get-config '@text:fonts)
	       0 0.72 0 "n" "n" "n")))
(defun @text:multi-text-align (/ A ALL B C L M N P VAL X0 XYZ_NEW Y Y0 Z)
  (@:help 
   "对多个乱序排列的单行文本左对齐.并按行距整齐排列。")
  (setq a (ssget (list (cons 0 "text"))))
  (setq n (sslength a))
  (setq all nil)
  (setq m 0)
  (while (< m n)
    (setq all (append all (list (entget (ssname a m)))))
    (setq m (1+ m))
    )
  ;; 排序算法
  (setq l 0)
  (setq m 1)
  (while (< l n)
    (setq b (nth l all))
    (while (< m n)
      (setq c (nth m all))
      (if (> (nth 2 (assoc '10 c)) (nth 2 (assoc '10 b)))
          (progn
            (setq all (subst 'aa (nth l all) all))
            (setq all (subst 'bb (nth m all) all))
            (setq all (subst c 'aa all))
            (setq all (subst b 'bb all))
            (setq b c)
            )
          )
      (setq m (1+ m))
      )
    (setq l (1+ l))
    (setq m (1+ l))
    )

  (setq val (getdist "\n行距："))
  (setq p (getpoint "\n首行的插入点："))
  (setq x0 (car p))
  (setq y0 (cadr p))

  (setq m 0)
  (while (< m n)
    (setq b (nth m all))
    (setq y (- y0 (* m val)))
    (setq z (nth 3 (assoc '10 b)))
    (setq xyz_new (list '10 x0 y z))
    (setq b (subst (cons '72 0) (assoc '72 b) b))
    (setq b (subst (cons '73 0) (assoc '73 b) b))
    (setq b (subst xyz_new (assoc '10 b) b))
    (entmod b)
    (setq m (1+ m))
    )
  )
(defun @text:insert-time( / ctextstyle cl )
  "插入时间戳记"
  (push-var nil)
					;(setvar "textstyle" "vitalhz350")
  (setq pt0 (getpoint "\n请指定插入位置点 :"))
  (setq date0 (menucmd "M=$(edtime,$(getvar,date),YYYY.MO.DD hh:mm:ss)"))
					;(setvar "textsize" 250)
  (setq cl (getvar "clayer"))
  (if (/= (tblsearch "layer" "org" ) nil )
      (setvar "clayer" "org")
      (progn
	(command "-layer" "n" "org" "")
	(command "-layer" "c" "t" "210,180,60" "org" "")
	)
      )
  (entity:make-text date0 pt0 350 0 0.8 0 13)
  (pop-var)
  (princ)
  )

(defun @text:justify( / s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "调整单行文本对齐方式(文本的位置不变)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;构造针对TEXT的选择集
  (command "justifytext" s1 "")
;;;  (setq ti% 0)
;;;   (if (/= s1 nil)
;;;       (progn
;;; 	(while 
;;; 	    (<= 
;;; 	     ti% 
;;; 	     (- (sslength s1) 1)
;;; 	     )
;;; 	  (setq ename(ssname s1 ti%))
;;; 	  (setq p11 nil)			;将p11置空
;;; 	  (setq e (entget ename))		;取实体表e
;;; 	  (setq p11 (assoc  11 e))	;取对齐点
;;; 	  (setq p10 (assoc 10 e))	;取其始点表
;;; 	  (setq f (assoc 10 e))	;如上
;;; 	  (setq en e)
;;; 	  (if (= (cdr f) 0) (setq p11n (cons 11 (cdr p10))))
;;; 
;;; 	  (if (/= (cdr f ) 0)
;;; 	      (progn
;;; 		(setq p10n (cons 10 (cdr p11)))	;将对齐点位构成其始点表赋给p10n
;;; 		(setq en (subst p10n p10 en))	;用新的其始点位更换旧的其始 点表
;;; 		)
;;; 	    )
;;; 	  (setq fn 0)
;;; 	  (setq fn (cons 72 fn))			;(72 . 0) 为左齐
;;; 	  (setq fn1 (assoc 72 en))		;取旧的对齐方式
;;; 	  (setq e (subst fn fn1 e))		;更换旧的对齐方式为左齐
;;; 
;;; 	  (entmod e)
;;; 	  (setq ti%(+ 1 ti%))
;;; 	  )  ;end while
;;; 	) ;endprogn s1/=nil
;;;     )  ;endif  s1 /= nil
;;;   (setq s1 nil)
  (princ)
  )

(defun @text:justifytext-left(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "调整单行文本左对齐(文本的位置不变)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;构造针对TEXT的选择集
  (command "justifytext" s1 "" "L" )
  )
(defun @text:justifytext-right(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "调整单行文本右对齐(文本的位置不变)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;构造针对TEXT的选择集
  (command "justifytext" s1 "" "r" )
  )

(defun @text:justifytext-middle(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "调整单行文本中对齐(文本的位置不变)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;构造针对TEXT的选择集
  (command "justifytext" s1 "" "m" )
  )
(defun @text:a2t(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1 attribtext)
  (@:help "将属性转化为单行文本.")
  (setq  s1  (ssget  '((0 . "ATTDEF"))))		;构造针对TEXT的选择集
  (setq attribtext '((0 . "TEXT") (100 . "AcDbEntity") (67 . 0) (410 . "Model") (8 . "0") (100 . "AcDbText") )  ) 
  (setq ti% 0)
  (if (/= s1 nil)
      (progn
	(while 
	    (<= 
	     ti% 
	     (- (sslength s1) 1)
	     )
	  (setq ename(ssname s1 ti%))
	  (setq p11 nil)			;将p11置空
	  (setq e (entget ename))		;取实体表e
	  (setq en e)
	  ;;	(setq fn (cons 0 "TEXT"))  ; (72 . 0) 为左齐
	  ;;	(setq fn1 (assoc 0 en))	   ; 取旧的对齐方式
	  ;;	(setq e (subst fn fn1 e))  ; 更换旧的对齐方式为左齐
	  (setq e1 attribtext)
	  (setq e1 (append e1 (list (cons 1 (cdr (assoc 2 en))) (assoc 10 en) (assoc 11 en) (assoc 7 en) (assoc 50 en) (assoc 71 en) (assoc 72 en) (assoc 73 en) (assoc 210 en) (assoc 40 en) (assoc 41 en))))
	  ;;;(princ e1)
	  (entmake e1)
	  (entdel ename)
	  (setq ti%(+ 1 ti%))
	  )  ;end while
	) ;endprogn s1/=nil
      )  ;endif  s1 /= nil
  (setq s1 nil)
  (princ)
  )

;;单行文字以左上角为基点转多行文字
;;根据<<动态修改单行文字宽度范围>>修改而来   
;;修改：孤帆
(defun @text:to-mtext( / #height #layer #pnt1 #read #string #text #textwidth Textwidth dl e1 e2 ell ename entext i ss text y #Pnt2)
  (setq ss (ssget '((0 . "TEXT"))) 
        i  0 
        dl nil
	Textwidth 0
	);setq
					;(setq #Pnt1 (getpoint "\输入左上角插入点: "))
  (if ss
      (progn 
        (repeat (sslength ss)
                (setq ename (ssname ss i)
		      box (getbox ename)
                      ell    (entget ename)
                      y      (caddr (assoc 10 ell))
                      text   (cdr (assoc 1 ell))
                      #Height   (cdr (assoc 40 ell))
		      #textwidth (* #Height (cdr (assoc 41 ell)))
                      Textwidth  (max Textwidth (- (caadr box)(cadr (assoc 10 ell))))
		      #textstyle (cdr (assoc 7 ell))
                      #Layer   (cdr (assoc 8 ell))
                      i      (1+ i)
                      );setq
                (setq dl (append dl (list (list y text ename))))
                );repeat
        (setq dl    (vl-sort dl (function (lambda (e1 e2) (> (car e1) (car e2)))))
              i     1
              );setq
        (setq #Pnt2 (getbox (caddar dl)) #Pnt1 (list (caar #Pnt2)(cadadr #Pnt2) 0.0))
	(setq text  (cadr (nth 0 dl)))
        (repeat (- (length dl) 1)
                (setq text (strcat text "\\P" (cadr (nth i dl))))
                (setq i (1+ i))
                );repeat
        (command "erase" ss "")
        );progn
      (princ "\n未选中任何文字！")
      );if
  (setq #String text)
  (setq #Text (@text:MText #Pnt1 #String Textwidth #Layer 1 #Height #textstyle)) 
					;(setq enText (vlax-vla-object->ename #Text))
					;(command "explode" enText)   
  (redraw)
  (princ)
  )
;;;取得实体外矩形框---返回（p1 p2）
(defun getbox (en / obj box-p1 box-p2)
  (setq obj (vlax-ename->vla-object en))
  (vla-getboundingbox obj 'box-p1 'box-p2)
  (setq box-p1 (vlax-safearray->list box-p1))
  (setq box-p2 (vlax-safearray->list box-p2))
  (list box-p1 box-p2)
  )
(defun @text:MText (#InsertionPoint #String #Width #Layer #Justification #Height  #style / #insertion #object #space)
  (or #Width (setq #Width 0))
  (or *AcadDoc*
      (setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object)))
      ) ;_ or
  (setq #Space     (if (or (eq acmodelspace
                               (vla-get-activespace *AcadDoc*)
                               ) ;_ eq
                           (eq :vlax-true (vla-get-mspace *AcadDoc*))
			   ) ;_ or
                       (vla-get-modelspace *AcadDoc*)
                       (vla-get-paperspace *AcadDoc*)
                       ) ;_ if
        #Insertion (cond
                     ((vl-consp #InsertionPoint) (vlax-3d-point #InsertionPoint))
                     ((eq (type #InsertionPoint) 'variant) #InsertionPoint)
                     (T nil)
                     ) ;_ cond
	) ;_ setq
  (setq #Object (vla-addmtext #Space #Insertion #Width #String))
  (if #Height (vla-put-Height #Object #Height) ) 
  (and #Layer
       (tblsearch "layer" #Layer)
       (vla-put-layer #Object #Layer)
       ) ;_ and
  (cond ((member #Justification (list 1 2 3 4 5 6 7 8 9))
         (vla-put-attachmentpoint #Object #Justification)
         (vla-move #Object
                   (vla-get-InsertionPoint #Object)
                   #Insertion
		   ) 
         )
	)
  (vla-put-StyleName #Object #style)
  #Object
  )

(defun @text:menu-add-prefix-or-suffix(/ res)
  (setq res (ui:input "请输入前后缀文字" '(("前缀:" )("后缀:"))))
  (@text:add-prefix-suffix (cdr (assoc "前缀:" res))
			   (cdr (assoc "后缀:" res))
			   (pickset:to-list (ssget '((0 . "TEXT")))))
  (princ))
    
(defun @text:add-prefix-suffix (str-prefix str-suffix lst-ent)
  (mapcar '(lambda (x)
	     (entity:putdxf x 1 
			    (strcat str-prefix (entity:getdxf x 1) str-suffix)))
	  lst-ent))

(defun @text:find-from-line(/ s1 pt-base txt ename ti% p11 p10 f en p11n p10n fn fn1 attribtext)
  (@:help "选择一个文本a，选择查找范围，查找内容相同的文本，并连线。")
  (if (null layer:make)(require 'layer:*))
  (if (= 'subr (type layer:make))
      (layer:make (@:get-config '@text:temp-layer) 1 nil nil))
  (setq en1 (car (entsel "请选择一个单行文本:")))
  (setq pt-base (cdr (assoc 10 (entget en1))))
  (setq txt (cdr (assoc 1 (entget en1))))
  (if (and pt-base txt)
      (progn
	(princ "\n请选择需要查找的区域:")
	(setq s1 (ssget  (list '(0 . "text,tch_text") (cons 1 txt))))
	(mapcar
	 (function
	  (lambda (x)
	    (entity:putdxf
	     (entity:putdxf
	      (entity:make-line pt-base
				(cdr (assoc 10 (entget x))))
	      8 (@:get-config '@text:temp-layer))
	     62 (@:get-config '@text:color))))
	 (pickset:to-list s1))
	(@:set-config '@text:color
		      (* 10
			 (if (< (@:get-config '@text:color) 240)
			     (+ 10 (@:get-config '@text:color))
			   10)))))
  )

(defun @text:menu-format-number(/ int-n int-fraction res)
  (@:help "格式化文本中的数字，")
  (setq res (ui:input "请输入格式化配置参数"
		      '(("整数位数" 2)
			("小数位数" 0)
			("填充字符" "0"))))
  (prompt "请选择要处理的文本")
  (setq txts (pickset:to-list (ssget '((0 . "text")))))
  (mapcar
   '(lambda(x)
      (entity:putdxf
       x 1
       (string:from-list
	(mapcar
	 '(lambda (y)
	    (if (string:numberp y)
		(string:number-format
		 (vl-string-left-trim "0 " y)
		 (cdr (assoc "整数位数" res))
		 (cdr (assoc "小数位数" res))
		 (cdr (assoc "填充字符" res))
		 )
	      y)
	    )
	 (string:auto-split (entity:getdxf x 1)))
	""
	)))
   txts)
  )


;; Local variables:
;; coding: gb2312
;; End: 
