(defun at-dim:cutting-symbol (/ *error* bu code data dcl_re dclname dlg 
			      stream tempname tex loopit
			      l0
			      cutline1 cuttext1
			      cutline2 cuttext2
			      cutdetail-text
			      cutdetail-l1
			      cutdetail-l2
			      cutdrection-l
			      cutdrection-text
			      )
  (defun *error* (msg)
    (mapcar 'entdel
	    (vl-remove nil
		       (list l0
			     cutline1 cuttext1
			     cutline2 cuttext2
			     cutdetail-text
			     cutdetail-l1
			     cutdetail-l2
			     cutdrection-l
			     cutdrection-text)))
    (princ msg)
    )
  (setq cutmode 1 ;;符号模式
	loopit t)
  (setvar "CELTYPE" "BYLAYER")
  (if (null (tblsearch "ltype" "DASHED"))
      (command "-linetype" "L" "DASHED" "" "")
      )
  (if (setq ss (ssget "X" '((0 . "TEXT") (1 . "[A-Z]")
			    (-3 ("CUTNUMBER"))
			    )
		      )
	    )
      (progn
	(setq lst '())
	(repeat (setq i (sslength ss))
		(setq lst (cons (cdr (assoc 1 (entget (ssname ss (setq i (1- i)))))) lst))
		)
	(setq tex (chr (1+ (ascii (car (vl-sort lst '>))))))
	)
      (setq tex "A")
      )
  (if (null bi)
      (setq bi (getvar "DIMSCALE"))
      )
  (while (progn
	   (initget "S")
	   (if (= (setq s (getpoint (strcat "\n指定剖切线起始点,或捕捉对齐点,或[设置(S)]:")))
		  "S"
		  )
	       (progn
		 (setq dclname (cond
				 ((setq tempname (vl-filename-mktemp "re-dcl-tmp.dcl")
					filen (open tempname "w")
					)
				  (foreach stream '("\n" "RENAME:dialog {\n"
						    "    label = \"设置\" ;\n" "        :edit_box {  label = \" 文字高度:\";    key = \"e03\" ;  }\n"
						    "        :edit_box {  label = \" 箭头大小:\";    key = \"e04\" ;  }\n" "    :row {\n"
						    "        :button {is_default = true ; key = \"e02\" ; label = \"确认\" ; }\n"
						    "        :button { is_cancel = true ; key = \"btn_cancle\" ; label = \"取消\" ; }\n"
						    "         }\n" "}\n"
						    )
					   (princ stream filen)
					   )
				  (close filen)
				  tempname
				  )
				 )
		       )
		 (setq dcl_re (load_dialog dclname))
		 (new_dialog "RENAME" dcl_re)
		 (set_tile "e03" (rtos (* bi 4)))
		 (set_tile "e04" "同字高")
		 (action_tile "e02" "(setq bi ( * 0.25 (atof (get_tile \"e03\"))))(done_dialog )")
		 (setq dlg (start_dialog))
		 (unload_dialog dcl_re)
		 (vl-file-delete dclname)
		 )
	       (setq pt0 s)
	       )
	   (= s "S")
	   )
    )
  (if (ssget "c" pt0 pt0)
      (setq pt0 (getpoint pt0 "指定起点："))
      )
  (princ (strcat "\n指定箭头方向,或符号:<" tex ">,右键向视"))
  
  (setq l0
	(entity:putdxf
	 (entity:putdxf 
	  (entity:make-line pt0 pt0)
	  62 8)
	 6 "DASHED"))
  (setq ent0 l0)

  (setq cutline1
	(entity:putdxf
	 (entity:make-lwpolyline
	  (list (polar pt0 (* 0.5 pi) (* bi 4))
		pt0
		(polar pt0 0 (* bi 2))
		(polar pt0 0 (* bi 4))
		)
	  nil
	  (list
	   (* bi 0.3)
	   0
	   (list (* bi 0.3) 0))
	  0 0)
	 62 4)
	)
  
  (setq cuttext1
	(entity:make-text tex  pt0 (* bi 4)  0 0.8 0  "MM"))
  (xdata:put cuttext1 "CUTNUMBER" tex)
  
  
  (entmake (cdr (entget cutline1)))
  (setq cutline2 (entlast))
  (entmake (cdr (entget cuttext1)))
  (setq cuttext2 (entlast))
  (while (progn
	   (setq gr (grread t 15 0)
		 code (car gr)
		 data (cadr gr)
		 )
	   (cond
	     ((= code 2)	       ; 键盘区域
	      (redraw)
	      (if (= data 15)
		  (progn
		    (if (= (getvar "ORTHOMODE") 0)
			(progn
			  (prompt "\n<正交 开>")
			  (setvar "orthomode" 1)
			  )
			(progn
			  (prompt "\n<正交 关>")
			  (setvar "orthomode" 0)
			  )
			)
		    )
		  )
	      (setq s (strcase (chr data)))
	      (if (wcmatch s "[A-Z]")
		  (progn
		    (setq tex s)
		    (entity:putdxf curttext1 1 tex)
		    (entity:putdxf curttext2 1 tex)
		    ;; (entity:putdxf curttext3 1 tex)
		    )
		  )
	      (if (= cutmode 1)
		  (princ (strcat "\n指定箭头方向,或符号:<" tex ">,右键向视"))
		  )
	      (if (= cutmode 3)
		  (princ (strcat "\n指定箭头方向,或符号:<" tex ">,右键剖视"))
		  )
	      )
	     ((= code 3)	       ; 鼠标左击,标索引详图号
	      (redraw)
	      (cond
		((= cutmode 1)
		 (setq cutmode 2)
		 (entdel l0)
		 (setq cutdetail-text
		       (entity:make-text (strcat tex"-"tex) data (* bi 4)  0 0.8 0  "MM"))

		 (setq box (text:box cutdetail-text))
		 (setq cutdetail-l1
		       (entity:make-lwpolyline
			(mapcar '(lambda(x)
				  (polar
				   x
				   (* 1.5 pi)
				   (* bi 0.5)))
				(list (car box)(cadr box)))
			nil
			(* bi 0.2)
			0 0))
		 (setq cutdetail-l2
		       (entity:make-lwpolyline
			(mapcar '(lambda(x)
				  (polar
				   x
				   (* 1.5 pi)
				   (* bi 1)))
				(list (car box)(cadr box)))
			nil
			0 
			0 0))
		 
		 )
		((= cutmode 2)
		 (setq loopit nil)
		 )
		((= cutmode 3)
		 (setq cutmode 2)
		 (entdel l0)
		 (setq cutdetail-text
		       (entity:make-text (strcat tex"-"tex) data (* bi 4)  0 0.8 0  "MM"))
		 
		 (setq box (text:box cutdetail-text))
		 (setq cutdetail-l1
		       (entity:make-lwpolyline
			(mapcar '(lambda(x)
				  (polar
				   x
				   (* 1.5 pi)
				   (* bi 0.5)))
				(list (car box)(cadr box)))
			nil
			(* bi 0.2)
			0 0))
		 (setq cutdetail-l2
		       (entity:make-lwpolyline
			(mapcar '(lambda(x)
				  (polar
				   x
				   (* 1.5 pi)
				   (* bi 1.5)))
				(list (car box)(cadr box)))
			nil
			0 
			0 0))
		 )
		)
	      (princ "\n指定插入点:")
	      )
	     ((= code 5)   ;; 鼠标移动
	      (if (= (getvar "ORTHOMODE") 1)
		  (progn
		    (setq x0 (car pt0)
			  y0 (cadr pt0)
			  x1 (car data)
			  y1 (cadr data)
			  )
		    (if (> (abs (- x0 x1)) (abs (- y0 y1)))
			(setq pt (list x1 y0))
			(setq pt (list x0 y1))
			)
		    )
		  (setq pt data)
		  )
	      (setq r (angle pt0 pt)
		    rr (* r (/ 180 pi))
		    )
	      (if (= cutmode 1)
		  (progn
		    (entity:putdxf ent0 11 pt)
		    ;;
		    (curve:put-points cutline1
				      (list (polar pt0 r (* bi 4))
					    nil
					    (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* bi 2))
					    (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* bi 4)))

				      )
		    ;; 文字位置
		    (entity:putdxf cuttext1  11
				   ;; (polar  ;; 剖向线上
				   ;;  (point:mid pt0 (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* bi 4)))
				   ;;  (m:fix-angle(+ pi r))
				   ;;  (* 1.2(entity:getdxf cuttext1 40)))
				   (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* bi 6)))
		    
		    (entity:putdxf cuttext1 50 (m:fix-angle (- r (* 0.5 pi))))
		    (curve:put-points cutline2
				      (list (polar pt(m:fix-angle(+ r pi)) (* bi 4))
					    pt
					    (polar pt (m:fix-angle (- r (* 0.5 pi))) (* bi 2))
					    (polar pt (m:fix-angle (- r (* 0.5 pi))) (* bi 4)))
				      )
		    (entity:putdxf cuttext2 11
				   (polar pt (m:fix-angle (- r (* 0.5 pi))) (* bi 6)))
		    (entity:putdxf cuttext2 50(m:fix-angle (- r (* 0.5 pi))))
		    ))
	      (if (= cutmode 2)
		  (progn ;; 移动detail
		    (entity:putdxf cutdetail-text 11 data)
		    (setq box (text:box cutdetail-text))
		    (curve:put-points cutdetail-l1
				      (mapcar '(lambda(x)
						(polar
						 x
						 (* 1.5 pi)
						 (* bi 0.5)))
					      (list (car box)(cadr box))))
		    (curve:put-points cutdetail-l2
				      (mapcar '(lambda(x)
						(polar
						 x
						 (* 1.5 pi)
						 (* bi 1.5)))
					      (list (car box)(cadr box))))
		    ))
	      (if (= cutmode 3);;向视
		  (progn
		    (entity:putdxf l0 11 pt)
		    (if cutdrection-l
			(curve:put-points
			 cutdrection-l
			 (list pt0
			       (polar
				pt0
				(m:fix-angle(+ pi r))
				(* bi 8))
			       (polar
				pt0
				(m:fix-angle(+ pi r))
				(* bi 12)))))
		    (if cutdrection-text
			(progn
			  (entity:putdxf
			   cutdrection-text
			   11
			   (polar
			    (point:mid pt0
				       (polar
					pt0
					(m:fix-angle(+ pi r))
					(* bi 5)))
			    (+ r (* 0.5 pi))
			    (* bi 5)
			    ))
			  (entity:putdxf cutdrection-text 50
					 (m:fix-angle r)))
			)
		    ))
	      )
	     ((or(= code 11) (= code 25));; 鼠标右击
	      (if (= cutmode 1)
		  (progn
		    (princ (strcat "\n指定箭头方向,或符号:<" tex ">,右键剖视"))
		    (setq cutmode 3)
		    ;;删除剖视
		    (mapcar '(lambda(x)
			      (if (e2o x)
				  (vla-put-visible
				   (e2o x)
				   :vlax-false)))
			    (list cutline1 cuttext1
				  cutline2 cuttext2))
		    ;;绘制向视
		    (if (and  cutdrection-l cutdrection-text)
			(mapcar '(lambda(x)
				  (if (e2o x)
				      (vla-put-visible
				       (e2o x)
				       :vlax-true)))
				(list cutdrection-l
				      cutdrection-text
				      ))
			(progn
			  (setq cutdrection-l
				(entity:make-lwpolyline
				 (list pt0
				       (polar pt0 pi (* bi 8))
				       (polar pt0 pi (* bi 12)))
				 nil
				 (list (* bi 0.3)
				       (list (* bi 1) 0))
				 0 0)
				)
			  (setq cutdrection-text
				(entity:make-text (strcat tex "向")
						  (polar
						   (point:mid
						    pt0
						    (polar pt0 pi (* bi 8)))
						   (- (angle
						       pt0
						       (polar pt0 pi (* bi 6)))
						      (* 0.5 pi))
						   (* bi 3)
						   )
						  (* bi 4)  0 0.8 0  "MM"))
			  )))
		  (if (= cutmode 3)
		      (progn
			(princ (strcat "\n指定箭头方向,或符号:<" tex ">,右键向视"))
			(setq cutmode 1)
			(mapcar '(lambda(x)
				  (if (e2o x)
				      (vla-put-visible
				       (e2o x)
				       :vlax-false)))
				(list cutdrection-l
				      cutdrection-text
				      ))
			(mapcar '(lambda(x)
				  (if (e2o  x)
				      (vla-put-visible
				       (e2o x)
				       :vlax-true)))
				(list cutline1 cuttext1
				      cutline2 cuttext2))
			))
		  )
	      (if (= cutmode 2)
		  (progn
		    (setq loopit nil)
		    )
		  )
	      (redraw)
	      )
	     )
	   loopit
	   )
    )
  (mapcar
   '(lambda (x)
     (if (and x
	      (e2o x)
	      (= :vlax-false(vla-get-visible (e2o x))))
	 (entdel x)))
   (list l0
	 cutline1 cuttext1
	 cutline2 cuttext2
	 cutdetail-text
	 cutdetail-l1
	 cutdetail-l2
	 cutdrection-l
	 cutdrection-text))
  (princ)
  )
