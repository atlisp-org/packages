(setq @:font-dir (@:string-to-list (getenv "ACAD") ";"))

(if (null (member (strcat @:*prefix* "packages\\fonts") @:font-dir))
    (setenv "ACAD" (strcat (vl-string-right-trim ";" (getenv "ACAD"))
			   ";"
			   @:*prefix* "packages\\fonts;")))

(defun fonts:merge (/ date1 date2 font font_obj fontlist fontname n to-shx to-ttf)
  "归并字体样式。"
  (setq date1 (getvar "millisecs"))
  (defun to-shx(shxx shxb / a3)
    (setq a3 (entget (tblobjname "style" font)));取出字体的数据串行
    (setq a3 (subst (cons 3 shxx )(assoc 3 a3) a3));将字体字型改成新字型
    (setq a3 (subst (cons 4 shxb )(assoc 4 a3) a3));将字体字型改成新字型
    (entmod a3);更新字体
    )
  (defun to-ttf(ttf / obj)
    (setq obj (vla-add font_obj font))
    (vla-setFont obj ttf :vlax-false :vlax-false 134 2)
    )
  (setq font_obj (vla-get-TextStyles(vla-get-ActiveDocument(vlax-get-acad-object))))
  (vlax-for sobj font_obj
            (setq fontname (vla-get-name sobj))
            (setq fontlist (vl-remove "" (cons fontname fontlist)))
            )
  (setq n 0)
  (repeat (length fontlist)
          (setq font (nth n fontlist))
          (cond
            ((wcmatch font "*仿宋*")(to-ttf "仿宋"))
            ((wcmatch font "*宋体*")(to-ttf "宋体"))
            ((wcmatch font "*黑体*")(to-ttf "黑体"))
            (t(to-shx "tssdeng.shx" "hztxt.shx"))
            )
          (setq n (+ n 1))
          )
  (repeat 1 (vl-cmdf "regen"))
  (setq date2 (getvar "millisecs"))
  (princ (strcat "，耗时" (rtos(/(- date2 date1)1000.000)2 3) "秒。"))
  )

(defun fonts:merge1(/ a1 a2 date1 date2 to-shx to-ttf)
  (setq date1 (getvar "millisecs"))
  (defun to-shx(shxx shxb / a3)
    (setq a3 (entget (tblobjname "style" a2)));取出字体的数据串行
    (setq a3 (subst (cons 3 shxx )(assoc 3 a3) a3));将字体字型改成新字型
    (setq a3 (subst (cons 4 shxb )(assoc 4 a3) a3));将字体字型改成新字型
    (entmod a3);更新字体
    )
  (defun to-ttf(ttf / font_obj obj)
    (setq font_obj (vla-get-TextStyles(vla-get-ActiveDocument(vlax-get-acad-object))))
    (setq obj (vla-add font_obj a2))
    (vla-setFont obj ttf :vlax-false :vlax-false 134 2)
    )
  (setq a1 (tblnext "style" t));将指针移到第一个字体
  (while a1
    (setq a2 (cdr (assoc 2 a1)));取出字体名称
    (cond
      ((wcmatch a2 "*仿宋*")(to-ttf "仿宋"))
      ((wcmatch a2 "*宋体*")(to-ttf "宋体"))
      ((wcmatch a2 "*黑体*")(to-ttf "黑体"))
      (t(to-shx "tssdeng.shx" "hztxt.shx"))
      )
    (setq a1 (tblnext "style"));找出下一个字体
    )
  (repeat 1 (vl-cmdf "regen"))
  (setq date2 (getvar "millisecs"))
  (princ (strcat "，耗时" (rtos(/(- date2 date1)1000.000)2 3) "秒。"))
  )
(defun fonts:nulltoFonts(shxx shxb ttf / err font_obj)
  (setq font_obj (vla-get-TextStyles(vla-get-ActiveDocument(vlax-get-acad-object))))
  (vlax-for x font_obj ;单独分离函数时需重新定义font_obj
	    (vla-getfont x 'a 'b 'c 'd 'e)
	    (if (= a "")
		(progn
		  (if (and
		       (not (findfile (vla-get-fontfile x)))
		       (not (findfile (strcat (vla-get-fontfile x) ".shx")))
		       )
		      (vla-put-fontfile x shxx)
		      )
		  (if (and
		       (/= (vla-get-bigfontfile x) "")
		       (not (findfile (vla-get-bigfontfile x)))
		       (not (findfile (strcat (vla-get-bigfontfile x) ".shx")))
		       )
		      (vla-put-bigfontfile x shxb)
		      )
		  )
		(progn
		  (setq err (vl-catch-all-apply 'vla-setfont (list x a b c d e)))
		  (if (vl-catch-all-error-p err)
		      (vla-setfont x ttf b c d e)
		      )
		  )
		)
	    )
  (princ(strcat "\n>>>空字体分别替换为" shxx "、" shxb "、" ttf))
  (repeat 1 (vl-cmdf "regen"))
  (princ)
  )

(defun fonts:check (/ lst-missing st)
  "检查是否有字体文件"
  (setq st (tblnext "style" t))
  (setq lst-missing
	(cons (list (cdr (assoc 3 st))(cdr (assoc 4 st)))
	      nil))
  
  (while (setq st (tblnext "style"))
    (setq lst-missing
	  (cons (list (cdr (assoc 3 st))(cdr (assoc 4 st)))
		lst-missing))
    )
  (vl-remove nil
	     (mapcar
	      '(lambda(x)
		(cond
		  ((null(vl-filename-extension x))
		   (if (findfile (strcat x ".shx")) nil x))
		  ((member (strcase (vl-filename-extension x) t) '(".shx"))
		   (if (findfile x) nil x))
		  ((member (strcase (vl-filename-extension x) t) '(".ttf" ".ttc"))
		   (if (findfile (strcat (getenv "windir") "\\Fonts\\" x)) nil x))))
	      (list:remove-duplicates(vl-remove nil  (vl-remove "" (apply 'append lst-missing))))
	      ))
  )
