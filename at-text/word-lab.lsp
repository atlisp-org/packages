;;阿甘词库ck
					;第一次运行请根据自己需要修改以下内容：
(setq ckml (strcat @:*prefix*  "packages/at-text/" )) ;引号内为词库目录 注意路径为反斜杠“/”
(setq texth 300) ;300为文字高度
(setq textst "-宋体") ;字体
(setq textlay "WIRE-照明") ;WIRE-照明 为文字图层
(setq textwh 0.7);0.7为文字宽高比
(setq textsty "0");默认插入文字类型 0为单行文字 1为多行文字


;;以下不用修改
(setq suoyin "0")
(setq suoyin2 "0")
(defun c:ck ()
  (setvar "cmdecho" 0)
  (defun xsckdhk();显示词库对话框
					;(setq tzbl (getvar "HPSCALE" ));天正比例
    (setq en nill)
    (setq mulu (list "" ))
    (setq mulu (vl-directory-files ckml "*.txt" ))
    (setq ml mulu)
    (setq mulu (list mulu))
    (setq fname (vl-filename-mktemp nil nil ".dcl" ))
    (setq filen (open fname "w" ))
    (foreach x '(
                 "  dcl_settinsx : default_dcl_settings { audit_level = 3; }" 
                 "  ck : dialog{" 
                 "   label=\"@云词库 V2.0\";" 
                 " :row {" 
                 "  :column {" 
                 "   :list_box {" 
                 "    key = \"2\" ;" 
                 "    label = \"词组目录:\" ;" 
                 "				width = 20 ;" 
                 "   }" 
                 "  }" 
                 "  :column {" 
                 "   :list_box {" 
                 "    height = 25 ;" 
                 "    key = \"1\" ;" 
                 "    label = \"词组内容:\" ;" 
                 "    width = 45 ;" 
                 "   }" 
                 "  }" 
                 " }" 
                 "  :row {"
                 "  :edit_box"
                 "  {"
                 "    label=\"添加到词库\";"
                 "    key=\"bjk\";"
                 "    width = 45 ;"
                 "    height = 1 ;"
                 "    allow_accept=true;"
                 "  }"
                 "  :button{key=\"sq\";label=\"拾取\";}"
                 "  :button{key=\"tj\";label=\"添加\";}"
                 "  }" 
                 "	 spacer;"
		 " :row {"
		 " :toggle {"
		 "    label = \"多行文字\" ;" 
		 "    key = \"3\" ;"
		 "}"
		 "  :button{key=\"gz\";label=\"改图中字\";}"
		 "  :button{key=\"op\";label=\"打开文件\";}"
                 "   cancel_button;" 
		 "  }" 
                 "  }" 
		 );end ;endlist
             (princ x filen)
             (write-line "" filen)
	     );end foreach
    (close filen)
    (setq filen (open fname "r" ))
    (setq dclid (load_dialog fname))
    (while (or (eq (substr (setq lin (vl-string-right-trim "\" filen)" (vl-string-left-trim "(write-line \"" (read-line filen)))) 1 2) "//" ) (eq (substr lin 1 (vl-string-search " " lin)) "" ) (not (eq (substr lin (+ (vl-string-search " " lin) 1) 9) " : dialog" ))))
    (new_dialog (substr lin 1 (vl-string-search " " lin)) dclid)
    (start_list "1" )
    (if (= lst nil);第一次读取 第一个txt文件内容
        (progn
          (setq text_2 (nth 0 ml))
          (setq file (open (strcat ckml text_2) "r" ))
          (setq txt_t (read-line file) lst (list "" ))
          (while (/= txt_t nil)
            (setq lst (append lst (list txt_t)))
            (setq txt_t (read-line file))
            );end while
          (close file)
          (setq lsti lst)
          (setq lst (list (cdr lst)))
       	  ));end if
    (mapcar 'add_list (car lst))
    (end_list)
    (load_text suoyin 1)
    (start_list "2" )
    (mapcar 'add_list (car mulu))
    (end_list)
    (set_tile "3" textsty)
    (if bjk-txt
	(set_tile "bjk" bjk-txt)
	)
    (action_tile "1" "(new_text $value $reason)" )
    (action_tile "2" "(load_text $value $reason)	(setq suoyin2 (itoa 0))")
    (action_tile "3" "(setq textsty $value)") 
    (action_tile "sq" "(done_dialog 1)")
    (action_tile "op" "(done_dialog 2)")
    (action_tile "gz" "(done_dialog 3)")
    (action_tile "bjk" "(setq bjk-txt $value)")
    (action_tile "tj" "(tjwz)")
    (set_tile "2" suoyin);获取焦点
    (set_tile "1" suoyin2)
    (action_tile "cancel" "(done_dialog 0)" )
    (setq re (start_dialog))
    (cond
      ((= re 1) (shiqu))
      ((= re 2) (dkwj))
      ((= re 3) (gtzwz))
      )
    (start_dialog)
    (unload_dialog dclid)
    (close filen)
    (vl-file-delete fname)
    (if (/= en nill) ;动态文字
        (progn
	  (princ "\n点取位置或[转90度(A)/右键退出]")
	  (setq boolean t)
	  (setq text-jiaodu 0)
	  (while boolean
	    (setq motion (grread T 8));grread 函数返回一个表，其中第一个元素说明输入类型的代码，第二个元素既可能是整数，又可能是点
	    (setq code (car motion)) ;grread表第一个元素输入类型的代码
	    (setq pt2 (cadr motion)) ;grread表第二个元素 拖动模式坐标
	    (cond
	      ((= code 5)   ;鼠标拖动模式
	       (entmod (setq endate (subst (cons 10 pt2) (assoc 10 endate) endate)));动态改文字坐标
	       )
	      ((= code 3)   ;鼠标左鍵按下
	       (setq boolean nil)
	       )
	      ((= code 11)
	       (setq boolean nil)
	       (entdel en)
	       )
	      ((= code 25)
	       (setq boolean nil)
	       (entdel en)
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
	  ));end if
    (princ)
    ) ;end xsckdhk

  (defun load_text (value reason);子函数 提取txt内容
    (if (= reason 1)
        (progn
          (setq suoyin value)
          (setq text_2 (nth (atoi value) ml))
          (setq file (open (strcat ckml text_2) "r" ))
          (setq txt_t (read-line file) lst (list "" ))
          (while (/= txt_t nil)
            (setq lst (append lst (list txt_t)))
            (setq txt_t (read-line file))
            );end while
          (close file)
          (setq lsti lst)
          (setq lst (list (cdr lst)))
	  ));end if
    (start_list "1" )
    (mapcar 'add_list (car lst))
    (end_list)
    (setq wjm (nth (atoi value) ml))
    (setq filename (strcat ckml wjm))
    );end load_text

  (defun new_text (value reason / ttlen twid)
    (setq text (nth (1+ (atoi value)) lsti))
    (if (= reason 4)
        (progn
          (done_dialog 0);关闭对话框
          (setq pt (cadr (grread 1)));取得光标坐标
          (if pt
	      (if (= textsty "0")
                  (progn
                    (entmake (list
                              '(0 . "TEXT" );单行文字
                              (cons 1 text)
                              (cons 7 textst)
                              (cons 8 textlay)
                              (cons 10 pt)
					;(cons 40 (/ (* tzbl texth) 100))
			      (cons 40 texth)	 
                              (cons 41 textwh)																									 
                              );end list
                             );end entmake
                    (setq en (entlast))
                    (setq endate (entget (entlast)))
		    )
                  (progn
		    (setq ttlen (strlen text))   ;取得文本长度
                    (setq twid (* (* (* texth 0.7) ttlen) (/ tzbl 100)))  ;计算文本宽度
                    (entmake (list
                              '(0 . "MTEXT" );多行文字
                              (cons 100 "AcDbEntity")	
                              (cons 100 "AcDbMText")	
                              (cons 1 text)
                              (cons 7 textst)
                              (cons 8 textlay)
                              (cons 10 pt)
					;(cons 40 (/ (* tzbl texth) 100))
			      (cons 40 texth)
                              (cons 41 twid)																								 
                              );end list
                             );end entmake
                    (setq en (entlast))
                    (setq endate (entget (entlast)))
                    )
                  )												
              );end if
          );end progn
	);end if
    (if (= reason 1)
	(setq suoyin2 value)
	)
    );end new_text

					;拾取文字
  (defun shiqu (/ ent1)
    (if (setq ent1 (entsel ))
	(progn
	  (setq bjk-txt (cdr (assoc 1 (entget (car ent1)))));文字内容
	  (xsckdhk)
	  ))
    );end shiqu

					;打开文件
  (defun dkwj()
    (startapp "notepad" filename)
    ) 
  
					;改图中文字
  (defun gtzwz (/ sel i ent ob)
    (if (setq sel (ssget '((0 . "TEXT,MTEXT"))))
	(progn
	  (setq i 0)
	  (repeat (sslength sel)
		  (setq ent (ssname sel i))
		  (setq ob (vlax-ename->vla-object ent)) ;转换
		  (vlax-put-property ob 'TextString text) ;改变text特性
		  (setq i (1+ i))
		  )
	  ))
    (princ) 
    )
  
					;添加文字到词库
  (defun tjwz(/ file)
    (if (/= bjk-txt "")
	(progn
	  (setq file (open filename "a"))
	  (write-line bjk-txt file)
	  (close file)
	  (load_text suoyin 1) ;刷新文字内容
	  ))
    ) 

  (xsckdhk)
  (setvar "cmdecho" 1)
  (princ)
  );end defun
