(@:define-config 'fonts:default "tssdeng.shx,tssdchn.shx" "默认的字体配置。")
(@:define-config 'fonts:autofix 1  "是否自动替换有问题的字体样式。1 是，0 否")

(defun fonts:check_fontfile ( sty ttf wid / cz thisdrawing textstyles textstyle)
  (setq cz (tblsearch "style" sty))
  (or wid
      (>= 100 (setq wid (cdr (assoc 41 cz))) 0.01)
      (setq wid 1)
      )
  (setq ttf (strcase ttf))
  (if (not (vl-catch-all-error-p
            (vl-catch-all-apply
             '(lambda()
		(if (or (vl-string-search "," ttf)
			(vl-string-search ".shx" ttf)
			)
                    (progn
		      (if (= 'subr (type command-s))
			  (command-s "-style" sty ttf)
			(command "-style" sty ttf))
                      (if (zerop (getvar "cmdactive"))
                          (setq ttf nil)
                        (progn
			  (if (= 'subr (type command-s))
			      (command-s 0 wid)
			    (command 0 wid))
			    
			  (while (not (zerop (getvar "cmdactive")))
			    (if (= 'subr (type command-s))
				(command-s "")
			      (command ""))
			    ))
                        )
		      )
                  (progn (setq thisdrawing (vlax-get-property (vlax-get-acad-object) 'activedocument)
                               textstyles (vlax-get-property thisdrawing 'textstyles)
                               textstyle (cond (cz (vla-item textstyles sty))
                                               (T (vla-add textstyles sty))
					       )
			       )
                         (vla-setfont textstyle ttf :vlax-false :vlax-false 1 0)
                         (vlax-put thisdrawing 'activetextstyle textstyle)
			 )
                  )))
	    ))
      ttf
    ))
;;函数：FIND字体文件，如果是TTF文件则至WINDOWS目录下查找，或是SHX文件则至ACAD目录下查找。
(defun fonts:fix-fonts ( tips / old_regenmode winfonts
			      stylen TS_eng TS_chn TS_ttf shx1 shx2 chk1 styn
			      name0 name1 name2 reg  st 
			      m1 m2 m3 m4 fonts ts_ttf2
			      )

  (setq old_regenmode (getvar "regenmode")
        winfonts (strcat (getenv "windir") "\\fonts\\")
	)
  (setvar "regenmode" 0)
  (mapcar 'set '(ts_eng ts_chn ts_ttf) mydefaultfonts)
  (vlax-for st (vlax-get-property (vlax-get-property (vlax-get-acad-object) 'activedocument) 'textstyles)
            (setq styn (strcase (vlax-get-property st 'name))
		  name1 (strcase (vlax-get-property st 'fontfile))
		  name2 (strcase (vlax-get-property st 'BigFontFile))
		  )
            (or (= (substr styn 1 1) "*")
		(= styn "")
		(wcmatch styn "*|*")
					; (= name1 name2 "") ;没设置,无法读取信息
		(setq fonts (cons (list styn name1 name2) fonts))
		)
	    )
  (foreach st fonts
           (mapcar 'set '(styn name1 name2) st)
           (setq name0 (car (string:to-list name1 "."))
		 chk1 nil
		 )
           (if (wcmatch name1 "*.TTF,*.TTC")
               (progn 
                 (cond ((member name1 m4))   ;之前已检查，字库存在
                       ((member name1 m3) (setq chk1 T))    ;之前已检查，字库不存在
                       ((or (findfile name1) ;在acad路径内找到字库
                            (findfile (strcat winfonts name0 ".TTF")) ;在windows字库路径内找到字库
                            (findfile (strcat winfonts name0 ".TTC"))
                            )
                        (setq m4 (if (member name1 m4) m4 (cons name1 m4)))
                        )
                       (T (setq m3 (cons name1 m3) chk1 T)) ;没字库
                       )
                 (if chk1
                     (progn (if ts_ttf2
				(fonts:check-fontfile styn ts_ttf2 nil)
                                (or (setq ts_ttf2 (fonts:check-fontfile styn ts_ttf nil))
                                    (setq ts_ttf2 (fonts:check-fontfile styn "宋体" nil))
                                    (setq ts_ttf2 (fonts:check-fontfile styn "新宋体" nil))
                                    (setq ts_ttf2 (fonts:check-fontfile styn "黑体" nil))
                                    )
				)
                            (setq reg T)
			    )
                     )
		 )
               (progn
		 (setq shx1 "" shx2 "")
		 (if (= "" name1) (setq name1 ts_eng))
		 (if (= "" name2) (setq name2 ts_chn))
		 (or (wcmatch name1 "*.SHX") (setq name1 (strcat name1 ".SHX")))
		 (or (wcmatch name2 "*.SHX") (setq name2 (strcat name2 ".SHX")))
		 (cond ((member name1 m4) (setq shx1 name1)) ;之前已检查，字库存在
                       ((member name1 m1) (setq shx1 ts_eng chk1 T)) ;之前已检查，字库不存在
                       ((= "" name1) (setq chk1 T shx1 ts_eng)) ;没设置
                       ((findfile name1) (setq shx1 name1 m4 (cons name1 m4))) ;检查，字库存在
                       (T (setq shx1 ts_eng m1 (if (member name1 m1) m1 (cons name1 m1)))) ;检查，字库不存在
		       )
		 (cond 
                  ((= name2 "")) ;没设置
                  ((member name2 m4) (setq shx2 name2)) ;之前已检查，字库存在
                  ((member name2 m2) (setq shx2 ts_chn chk1 T)) ;之前已检查，字库不存在
                  ((findfile name2) (setq shx2 name2 m4 (cons name2 m4))) ;检查，字库存在
                  (T (setq chk1 T shx2 ts_chn m2 (cons name2 m2))) ;检查，字库不存在
		  )
		 (setq shx1 (if (= shx2 "") shx1 (strcat shx1 "," shx2)))
		 (if chk1
                     (progn
		       (if (= 'subr(type command-s))
			   (command-s "-style" styn shx1)
			 (command "-style" styn shx1))
                       (while (not (zerop (getvar "cmdactive")))
			 (if (= 'subr (type command-s))
			     (command-s "")
			   (command "")))
		       (setq reg T)
		       )
		   )
		 )
               )
	   )
  (if tips
      (progn
        (foreach a m1 (princ (strcat "\n字库< " a " >已替换为: <" ts_eng ">")))
        (foreach a m2 (princ (strcat "\n字库< " a " >已替换为: <" ts_chn ">")))
        (foreach a m3 (princ (strcat "\n字库< " a " >已替换为: <" ts_ttf ">")))
        (setvar "regenmode" old_regenmode)
        )
    )
  (if reg
      (if (= 'subr (type command-s))
	  (command-s "_.regen")
	(command "_.regen")
	))
  (princ)
  )

(defun fonts:use_myfonts ( / old_echo old_regenmode x ts_eng ts_chn ts_ttf ts_ttf2 styn st styn name1 name2)
  (setq old_echo (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (mapcar 'set '(ts_eng ts_chn ts_ttf) (@:get-config 'fonts:default ))
  (setq ts_eng (strcat ts_eng "," ts_chn)
        old_regenmode (getvar "regenmode")
	)
  (initget "S X")
  (setq ts_chn (getkword (strcat "\n替换字库[全部为Shx字库(S) |" (strcase ts_eng T) "|/全部为Ttf系统字库(X) |" ts_ttf "|]:<回车智能判断>")))
  (setvar "regenmode" 0)
  (princ "\n正在替换字库......")
  (vlax-for st (vlax-get-property (vlax-get-property (vlax-get-acad-object) 'activedocument) 'textstyles)
            (setq styn (strcase (vlax-get-property st 'name))
		  name1 (strcase (vlax-get-property st 'fontfile))
		  name2 (strcase (vlax-get-property st 'BigFontFile))
		  )
            (or (= (substr styn 1 1) "*")
		(= styn "")
		(wcmatch styn "*|*")
		(setq fonts (cons (list styn name1 name2) fonts))
		)
	    )
  (foreach st fonts
           (mapcar 'set '(styn name1 name2) st)
           (cond ((or (= ts_chn "X")
                      (and (not ts_chn)
                           (or (= name1 name2 "") (wcmatch name1 "*.TTF,*.TTC"))
			   )
		      )
		  (if ts_ttf2
                      (fonts:check-fontfile styn ts_ttf2 nil)
                      (or (setq ts_ttf2 (fonts:check-fontfile styn ts_ttf nil))
			  (setq ts_ttf2 (fonts:check-fontfile styn "宋体" nil))
			  (setq ts_ttf2 (fonts:check-fontfile styn "新宋体" nil))
			  (setq ts_ttf2 (fonts:check-fontfile styn "黑体" nil))
			  )
		      )
		  )
		 ((fonts:check-fontfile styn ts_eng nil))
		 )
	   )
  (princ "\n替换字库结束")
  (setvar "regenmode" old_regenmode)
  (if (= 'subr (type command-s))
      (command-s "_.regen")
    (command "_.regen")
    )
  (setvar "cmdecho" old_echo)
  (princ)
  )

