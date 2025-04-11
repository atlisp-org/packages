(@:add-menus
 '("sd1t"
   ("输出PDF" (sd1t:export-pdf))
   ("选择测试" (sd1t:select-frames))
   ))
;;依赖的函数
(defun sd1t:frame-p (blkname / ents box height width mapsheet)
  "测试一个块定义是否为图框，即块内多段线的四角点形成的矩形，
是否符合图幅规定,如果是则返回图幅名及4角点和基点，否则返回nil"
  (if (and (p:enamep blkname)
	   (wcmatch (entity:getdxf blkname 0) "INSERT,BLOCK"))
      (setq blkname (entity:getdxf blkname 2)))
  (if (and (p:stringp blkname)
	   ;;图块中含有公司名
	   (setq co-name
		 (vl-remove-if-not
		  '(lambda(x)
		    (and (= "TEXT"(entity:getdxf x 0))
		     (wcmatch  (entity:getdxf x 1)
		      "佛山市*有限公司")))
		  (block:ent-list blkname)))
	   (setq ents (vl-remove-if-not
		       '(lambda(x)
			 (and (wcmatch (entity:getdxf x 0) "LINE,LWPOLYLINE")
			  (= (vla-get-visible (e2o x)):vlax-true)
			  ;;直线水平或竖直 (= 4 (entity:getdxf x 90))
			  (cond
			    ((eq"LINE"(entity:getdxf x 0))
			     (setq ang
				   (angle (entity:getdxf x 10)
					  (entity:getdxf x 11)))
			     (or (equal ang 0 1e-8)
				 (equal ang (* 0.5 pi) 1e-8)
				 (equal ang (* 1.0 pi) 1e-8)
				 (equal ang (* 1.5 pi) 1e-8)
				 (equal ang (* 2.0 pi) 1e-8))
			     )
			    ((eq"LWPOLYLINE"(entity:getdxf x 0))
			     (< (entity:getdxf x 90) 8)
			     )
			    )
			  ))
		       (block:ent-list blkname)))
	   (setq box (pickset:getbox ents 0))
	   (setq height (- (cadr (cadr box))
			   (cadr (car box))))
	   (setq width (- (car (cadr box))
			  (car (car box))))
	   (setq ents (vl-remove-if-not
		       '(lambda(x)
			 (or (="LWPOLYLINE"(entity:getdxf x 0))
			  (and (="LINE"(entity:getdxf x 0))
			   (or (<= height
				   (curve:length x)
				   width)
			       (>= height
				   (curve:length x)
				   width)
			   ))))
		       ents))
	   (setq box (pickset:getbox ents 0))
	   (setq height (- (cadr (cadr box))
			   (cadr (car box))))
	   (setq width (- (car (cadr box))
			  (car (car box))))
	   )
      (progn
	(if (setq mapsheet (@pm:calc-mapsheet height width))
	    (list mapsheet
		  (apply 'point:rec-2pt->4pt box)
		  (entity:getdxf (tblobjname "block" blkname) 10)
		  ))
	)))
(defun sd1t:frame-recognition-by-block ()
  (setq blknames 
	(vl-remove-if-not  'sd1t:frame-p (block:list)))
  ;; (setq frameblkdata
  ;; 	(vl-remove-if-not
  ;; 	 '(lambda(x)
  ;; 	   (cdr x))
  ;; 	 (mapcar 
  ;; 	  '(lambda(x)
  ;; 	    (cons x
  ;; 	     (sd1t:frame-p x)))
  ;; 	  blknames)))
  ;; 块及图幅对照表
  (setq frameblkrefs
	(ssget "x" (list '(0 . "insert")
			 (cons 2 (string:subst-all "`*" "*" (string:from-list blknames ","))))))
  ;;去除包含的
  ;; (setq boxs
  ;; 	(mapcar '(lambda(x)
  ;; 		  (setq tkdata (member (entity:getdxf x 2) frameblkdata))
  ;; 		  (setq ptc (point:centroid (setq box (caddr tkdata))))
  ;; 		  (setq box (mapcar '(lambda(x)
  ;; 				      (polar x
  ;; 				       (angle x ptc)
  ;; 				       2))
  ;; 			     box))
  ;; 		  (mapcar '(lambda(x)(block:bcs2wcs x (last tkdata)
  ;; 				      (entity:getdxf x 10)
  ;; 				      (entity:getdxf x 50)
  ;; 				      (entity:getdxf x 41)))
  ;; 		   box))
  ;; 		frameblkrefs))
  ;;去除小框
  ;; (sssetfirst nil frameblkrefs)
  (pickset:to-list frameblkrefs)
  )
(defun sd1t:auto-select-frames()
  ;; 识别图框块
  (setq tks (sd1t:frame-recognition-by-block))
  ;; 去有X线的图框
  (setq tks
	(vl-remove-if
	 '(lambda(tk / box)
	   (setq box (entity:getbox tk 0))
	   (setq box (apply 'point:rec-2pt->4pt
		      (entity:getbox tk (* -0.0001 (apply 'distance box)))))
	   (setq x-lines (ssget "F" box '((0 . "LINE,LWPOLYINE"))))
	   (setq x-linex
	    (vl-remove-if
	     '(lambda(x)
	       (cond
		 ((eq"LINE"(entity:getdxf x 0))
		  (setq ang
			(angle (entity:getdxf x 10)
			       (entity:getdxf x 11)))
		  (or (equal ang 0 1e-8)
		      (equal ang (* 0.5 pi) 1e-8)
		      (equal ang (* 1.0 pi) 1e-8)
		      (equal ang (* 1.5 pi) 1e-8)
		      (equal ang (* 2.0 pi) 1e-8))
		  )
		 ;; ((eq"LWPOLYLINE"(entity:getdxf x 0))
		 ;;  (< (entity:getdxf x 90) 8)
		 ;;  )
		)
	       )
	     (pickset:to-list x-lines)))
	   )
	 tks)))
(defun sd1t:exportpdf (frames / *error* content% s1 minpoint maxpoint tufu ti% boxInsp boxScale boxRotate corner p1 p2 tuming old-ucs-name)
  (defun *error* (msg)
    (pop-var)
    (princ msg)(princ))
  (push-var '("snapmode""gridmode""osmode"))
  (setq old-ucs-name (getvar "ucsname"))
  (if (= 42 (ascii old-ucs-name))
      (setq old-ucs-name ""))
  (setvar "snapmode" 0)
  (setvar "gridmode" 0)
  (if (< (getvar"osmode") 16384)
      (setvar "osmode" (+ (getvar"osmode") 16384)))
  ;;(if (not is-zwcad) (setvar "3dosmode" 1))
  (if (p:picksetp frames)
      (setq frames(pickset:to-list frames)))
  ;;排序
  (setq frames(pickset:sort frames "Yx" 1))
  (setq s1 (@pm:frames2contents frames))
  ;;清空临时文件夹
  (if (findfile (strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))))
      (foreach pdf
	       (vl-directory-files
		(strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))) "*.pdf" 1)
	       (vl-file-delete 
		(strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))
			"\\"pdf)
		)))
  (setq ti% 0)
  (setq sum-plot 0)
  (if (/= s1 nil)
      (progn
	(while (< ti% (length s1) )
	  (setq content% (nth
			  ti% s1))
	  (setq tufu (@pm:get-map-sheet content%))
	  (setvar "cmdecho" 0)
	  (setvar "ctab" (@pm:get-layout content%))

	  (setq boxScale (float (cdr (assoc "boxScale" content%))))
	  (setq boxRotate (float (cdr (assoc "boxRotate" content%))))
	  ;; 图框角度不为0
	  (if (and (/= (- boxRotate (geometry:ucs-angle)) 0)
		   (= "Model" (getvar "ctab")))
	      (progn
		(command "ucs" "ob" (cdr (assoc "ename" content%)))
		(command "plan" "c")))
	  
	  (setq boxInsp (cdr (assoc "boxInsp" content%)))
	  (if (and (setq tufu-number (cdr (assoc tufu  @:paper-size)))
		   ;; (cdr (assoc (@:get-config '@pm:drawing-number) content%))
		   )
	      (progn
		(setq corner (@pm:get-corner content%))
		(if (> (abs (- (car (car corner))(car (cadr corner))))
		       (abs (- (cadr (car corner))(cadr (cadr corner)))))
		    (setq zongheng "L")
		    (setq zongheng "P"))
		(setq dir-path (@:mkdir (append (@::path(getvar "dwgprefix"))
						(list (vl-filename-base(getvar "dwgname"))))))
		;; (if (>= (@:get-config '@pm:print-tuming) 1)
		;;     (progn
		;;       (setq tuming (@:string-subst "" " "
		;; 				   (@pm:get-tuming content%)))
		;;       ;; 替换特殊字符
		;;       (setq tuming (filename:replace-special tuming))
		;;       (if (> (strlen tuming) 0)
		;; 	  (setq tuming (strcat "_" tuming)))
		;;       )
		;;   (setq tuming "")
		;;   )
		;; (setq tuming "")
		;; (setq tuhao (cdr (assoc (@:get-config '@pm:drawing-number) content%)))
		;; (setq tuhao (filename:replace-special tuhao))
		;;建立工程目录 建设单位 工程名称 子项名称 施工图 专业 
		;; 打印需要的数据 p1 p2 两个点，保存的文件名(含目录)
		(if  (and tufu-number
			  (/= tufu "非标图框")
			  ;;(cdr (assoc (@:get-config '@pm:drawing-number) content%))
			  )
		    (progn
		      (princ (strcat (rtos (nth 1 tufu-number)) "x" (rtos (nth 0 tufu-number))
				     ": "(rtos boxScale)  "\n"))
		      (setq tufu (@pm:handle-tufu tufu))
		      ;; (if (>= (@:get-config '@pm:print-tuming) 2)
		      ;;  ;;图幅
		      ;;  (setq tuming (strcat tuming "_" (filename:replace-special tufu))))
		      ;; (setq file-name (strcat  dir-path "\\"
		      ;; 			   (@:get-config '@pm:pdf-prefix)
		      ;; 			   (if (and (= 1 (@:get-config '@pm:prefix-profession))
		      ;; 				    (@pm:have-tubie-attribute content%))
		      ;; 			       (strcat (@pm:get-tubie content%) "-" tuhao)
		      ;; 			       tuhao)
		      ;; 			   tuming
		      ;; 			   ".pdf"))
		      (setq file-name (strcat dir-path "\\tmp-" (@::timestamp)".pdf"))
		      (princ "test1...")
		      (plot:to-pdf tufu zongheng (car corner) (cadr corner) file-name)
		      (setvar "cmdecho" 1)
		      )
		  (if (cdr (assoc (@:get-config '@pm:drawing-number) content%))
		      (@:log "WARR" (strcat (cdr (assoc(@:get-config '@pm:drawing-number)  content%)) tuming " 为非标准图框，不能打印。"))
		      (@:log "WARR" "非标准图框，不能打印。"))
		  
		  ))
	      (@:log "WARR" (strcat (if (cdr (assoc (@:get-config '@pm:drawing-number) content%)) (cdr (assoc (@:get-config '@pm:drawing-number) content%))) tufu  " 本程序不支持"))
	    )
	  ;;end plot
	  (setq ti% (+ 1 ti%)))
	))
  ;;打印处理
  (if (= "Model" (getvar "ctab"))
      (if (or (= "" old-ucs-name)(null old-ucs-name))
	  (progn (command "ucs" "w")(command "plan" "C"))
	(progn (command "ucs" "NA" "R" old-ucs-name)(command "plan" "C"))))
  ;; 打开最后一张图的目标文件夹
  (pop-var)
  (setq folder (vl-filename-directory file-name))
  (sd1t:pdftk:merge folder)
  ;; (system:explorer folder)
  (if (findfile (strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))))
      (progn
	(foreach pdf
		 (vl-directory-files
		  (strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))) "*.pdf" 1)
		 (vl-file-delete 
		  (strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))
			  "\\"pdf)
		  ))
	(vl-file-delete (strcat (getvar"dwgprefix")(vl-filename-base(getvar"dwgname"))))
	))
  (princ)
  )
(defun sd1t:pdftk:merge (folder / app files filetmp filename-bk fp-bookmark file-bk file-bk-utf8)
  "将参数folder 文件夹下的 pdf 文件合并成一个文件。合并后的文件名为 `文件夹.pdf' "
  ""
  "(sd1t:pdftk:merge \"D:\\Output\\A项目\")"
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  (if (and (findfile app)
	   folder
	   (vl-directory-files folder "*.pdf" 1))
      (progn
	;;(@:patch-pgp-shell)
	;; 删除旧pdf
	(if (findfile (strcat folder ".pdf"))
	    (vl-file-delete (strcat folder ".pdf")))
	(setq files "*.pdf")
	(setq filetmp "merge-tmp.pdf")
	;;(setq filename-bk "merge-all.pdf")
	(setvar "cmdecho" 0)
	;;创建标签文件。
	(setq fp-bookmark (open (strcat folder "\\bk.txt")"w"))
	(setq i 0)
	(foreach bk% (vl-directory-files folder "*.pdf" 1)
		 (write-line "BookmarkBegin" fp-bookmark)
		 (write-line (strcat "BookmarkTitle: "
				     (vl-filename-base bk%))
			     fp-bookmark)
		 (write-line "BookmarkLevel: 1" fp-bookmark)
		 (write-line (strcat "BookmarkPageNumber: "
				     (itoa (setq i (1+ i))))
			     fp-bookmark))
	(close fp-bookmark)
	;; 转码
	(setq file-bk (strcat folder "\\bk.txt"))
	(setq file-bk-utf8 (strcat folder "\\bk-utf8.txt"))
	(command "shell-bg"
		 (strcat  @:*prefix* "bin\\iconv.exe -f gb2312 -t utf-8 \""
			  file-bk "\" >> \"" file-bk-utf8  "\""
			  ))
	;;(sleep 3)
	(print folder)
	
	(command "start-bg"
		 (strcat " /D \"" folder "\""
			 " /MIN /B  "
			 @:*prefix* app  " "
			 files " "
			 "output " filetmp
			 ))
	(while (null (findfile (strcat folder "\\" filetmp)))
	  (sleep 3))
	(command "start-bg"
		 (strcat  " /D \"" folder "\""
			  " /MIN /B  "
			  @:*prefix* app  " "
			  filetmp " "
			  "update_info_utf8 "
			  " bk-utf8.txt "
			  " output \"..\\"
			  (vl-filename-base folder) ".pdf" "\" "
			  ))
	(while (null (findfile (strcat folder "\\..\\"
				       (vl-filename-base folder)
				       ".pdf")))
	  (sleep 3))
	(vl-file-delete file-bk)
	(vl-file-delete file-bk-utf8)
	(vl-file-delete (strcat folder "\\" filetmp))
	
	(setvar "cmdecho" 1)
	;; (system:explorer (strcat folder "\\..\\" ))
	))
  (princ)
  )

;; 功能函数
(defun sd1t:select-frames ()
  ;; 亮显要打印的图
  (if(setq tks (sd1t:auto-select-frames))
     (sssetfirst nil (pickset:from-list tks)))
  )
(defun c:sd1t_tks ()
  (sd1t:select-frames))
(defun sd1t:export-pdf ()
  (if (setq tks (sd1t:auto-select-frames))
      ;; plot
      (sd1t:exportpdf tks)
  ))
(defun c:sd1t_exportpdf ()
  (sd1t:export-pdf))
