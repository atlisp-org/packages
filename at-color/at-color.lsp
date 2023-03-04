(@:add-menu (_"Color") (_"Modify color") "(at-color:change-color)" )
(@:add-menu (_"Color") (_"Remove TrueColor") "(at-color:del-rgb)" )

(defun at-color:del-rgb ()
  ;;(@:help "框选要去除真彩色的图元,用索引色替换。")
  (prompt "请框选要去除真彩色的图元:")
  (entity:deldxf (ssget '((-4 . ">")(420 . 0))) 420)
  )
(defun at-color:change-color (/ fname fn x dclid lin)
  (setvar "cmdecho" 0)
  (vl-load-com)
  (setq fname (vl-filename-mktemp nil nil ".dcl" ))
  (setq fn (open fname "w" ))
  (foreach x '(
               "buttonGL:button{horizontal_margin=none;vertical_margin=none;} //自定义按钮"
               "  ksgs : dialog{" 
               "  label=\"快速改色-DIY\";" 
               "     :boxed_row{"
               "      label=\"改对象颜色\";"
               "      :image_button{key=\"1\";color=1;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"2\";color=2;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"3\";color=3;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"4\";color=4;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"5\";color=5;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"6\";color=6;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"7\";color=7;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"8\";color=8;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"9\";color=9;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"10\";color=190;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"11\";color=15;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :buttonGL{key=\"12\";label=\"其它\";width=6;}"
               "      :buttonGL{key=\"13\";label=\"对象颜色随层\";width=true;}"
               "     }"
               "     :boxed_row{"
               "      label=\"改图层颜色\";"
               "      :image_button{key=\"51\";color=1;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"52\";color=2;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"53\";color=3;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"54\";color=4;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"55\";color=5;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"56\";color=6;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"57\";color=7;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"58\";color=8;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"59\";color=9;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"60\";color=190;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :image_button{key=\"61\";color=15;width=6;aspect_ratio=1;allow_accept=true;}"
               "      :buttonGL{key=\"62\";label=\"其它\";width=6;}"
               "      :buttonGL{key=\"63\";label=\"图层颜色随层\";width=6;}"
               "     }"
               "     cancel_button;"
               "    }"
               )
           (princ x fn)
           (write-line "" fn)
	   )
  (close fn)
  (setq fn (open fname "r" ))
  (setq dclid (load_dialog fname))
  (while (or (eq (substr (setq lin (vl-string-right-trim "\" fn)" (vl-string-left-trim "(write-line \"" (read-line fn)))) 1 2) "//" ) (eq (substr lin 1 (vl-string-search " " lin)) "" ) (not (eq (substr lin (+ (vl-string-search " " lin) 1) 9) " : dialog" ))))
  (new_dialog (substr lin 1 (vl-string-search " " lin)) dclid)
  (action_tile "1" "(done_dialog 1)")
  (action_tile "2" "(done_dialog 2)")
  (action_tile "3" "(done_dialog 3)")
  (action_tile "4" "(done_dialog 4)")
  (action_tile "5" "(done_dialog 5)")
  (action_tile "6" "(done_dialog 6)")
  (action_tile "7" "(done_dialog 7)")
  (action_tile "8" "(done_dialog 8)")
  (action_tile "9" "(done_dialog 9)")	
  (action_tile "10" "(done_dialog 10)")
  (action_tile "11" "(done_dialog 11)")
  (action_tile "12" "(done_dialog 12)")
  (action_tile "13" "(done_dialog 13)")			 
  (action_tile "51" "(done_dialog 51)")
  (action_tile "52" "(done_dialog 52)")
  (action_tile "53" "(done_dialog 53)")
  (action_tile "54" "(done_dialog 54)")
  (action_tile "55" "(done_dialog 55)")
  (action_tile "56" "(done_dialog 56)")
  (action_tile "57" "(done_dialog 57)")
  (action_tile "58" "(done_dialog 58)")
  (action_tile "59" "(done_dialog 59)")
  (action_tile "60" "(done_dialog 60)")
  (action_tile "61" "(done_dialog 61)")
  (action_tile "62" "(done_dialog 62)")
  (action_tile "63" "(done_dialog 63)")
  (action_tile "cancel" "(done_dialog 0)" )
  (setq re (start_dialog))
  (cond
    ((= re 1) (GL:gs1))
    ((= re 2) (GL:gs2))
    ((= re 3) (GL:gs3))
    ((= re 4) (GL:gs4))
    ((= re 5) (GL:gs5))
    ((= re 6) (GL:gs6))
    ((= re 7) (GL:gs7))
    ((= re 8) (GL:gs8))	
    ((= re 9) (GL:gs9))	
    ((= re 10) (GL:gs10))
    ((= re 11) (GL:gs11))
    ((= re 12) (GL:gsqt))
    ((= re 13) (GL:gssc))
    ((= re 51) (GL:gcs1))
    ((= re 52) (GL:gcs2))
    ((= re 53) (GL:gcs3))
    ((= re 54) (GL:gcs4))
    ((= re 55) (GL:gcs5))
    ((= re 56) (GL:gcs6))
    ((= re 57) (GL:gcs7))
    ((= re 58) (GL:gcs8))	
    ((= re 59) (GL:gcs9))	
    ((= re 60) (GL:gcs10))
    ((= re 61) (GL:gcs11))
    ((= re 62) (GL:gcsqt))
    ((= re 63) (c:shf))
    )
  (unload_dialog dclid)
  (close fn)
  (vl-file-delete fname)
  (princ)
  )

;;-----------------------------------------------------------------
					;改对象颜色
					;1红.2黄.3绿.4青.5蓝.6品红.7白.8灰.9浅灰.10紫色.11咖色		
(defun GL:gs1()(GL:gdxys 1));红色
(defun GL:gs2()(GL:gdxys 2))
(defun GL:gs3()(GL:gdxys 3))
(defun GL:gs4()(GL:gdxys 4))
(defun GL:gs5()(GL:gdxys 5))
(defun GL:gs6()(GL:gdxys 6))
(defun GL:gs7()(GL:gdxys 7))
(defun GL:gs8()(GL:gdxys 8))
(defun GL:gs9()(GL:gdxys 9))
(defun GL:gs10()(GL:gdxys 190))
(defun GL:gs11()(GL:gdxys 15))
(defun GL:gsqt(/ yanse)
  (if (setq yanse (acad_colordlg 1))
      (GL:gdxys yanse)
      )
  )

;;改颜色随层子程序
(defun GL:gssc(/ ss)
  (princ "颜色改为随层")
  (while (setq ss (ssget))
    (princ (strcat (itoa (sslength ss)) "个对象的颜色改为随层"))
    (command "change" ss "" "P" "C" "bylayer" "")
    );while
  )

;;改对象颜色子程序
(defun GL:gdxys (dxys / en i obj ss)
  (while (setq ss (ssget ":s"))
    (princ (strcat "共改变了<" (itoa (sslength ss)) ">个对象的颜色"))
    (command "change" ss "" "p" "c" dxys "")
    );while
  (princ)
  )

;;-----------------------------------------------------------------	
;;改图层颜色
(defun GL:gcs1()(GL:gtcys 1))
(defun GL:gcs2()(GL:gtcys 2))
(defun GL:gcs3()(GL:gtcys 3))
(defun GL:gcs4()(GL:gtcys 4))
(defun GL:gcs5()(GL:gtcys 5))
(defun GL:gcs6()(GL:gtcys 6))
(defun GL:gcs7()(GL:gtcys 7))
(defun GL:gcs8()(GL:gtcys 8))
(defun GL:gcs9()(GL:gtcys 9))
(defun GL:gcs10()(GL:gtcys 190))
(defun GL:gcs11()(GL:gtcys 15))
(defun GL:gcsqt(/ yanse)
  (if (setq yanse (acad_colordlg 1))
      (GL:gtcys yanse)
      )
  )
					;改图层颜色子程序
(defun GL:gtcys (yanse / acaddoc acadobj en i lay layobj obj ss vlay vlay1)
  (setq AcadObj (vlax-get-Acad-object)
	AcadDoc (vla-get-ActiveDocument AcadObj)
	LayObj (vla-get-layers AcadDoc)
	)
  (if (setq ss (ssget))
      (progn
	(setq i 0)
	(repeat (sslength ss)
		(setq en (ssname ss i);取图元名
		      obj (vlax-ename->vla-object en);转换图元
		      lay (vla-get-layer obj);图层名
		      vlay (vla-item LayObj lay);转为VL图层名
		      )
		(if (= (equal vlay vlay1) nil)
		    (progn
		      (vla-put-color vlay yanse);改图层颜色
		      (setq vlay1 vlay)
		      ))
		(setq i (1+ i))
		)
	))
  (princ)
  )
(defun C:shf () 
  (princ "\n图层颜色已恢复默认！cecolor")  
  (setvar "cecolor" "bylayer")
  (princ) 
  );defun
