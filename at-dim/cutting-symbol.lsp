(defun at-dim:cutting-symbol (/ *error* code data cscale
				cutn loopit  
				l0
				cutline1 cuttext1
				cutline2 cuttext2
				cutdetail-text
				cutdetail-l1
				cutdetail-l2
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
			     )))
    (princ msg)
    )
  (setq cutmode 1 ;;符号模式
	loopit t)
  (setvar "CELTYPE" "BYLAYER")
  (if (null (tblsearch "ltype" "DASHED"))
      (setq err (vl-catch-all-apply 'vla-Load (list (vla-get-Linetypes *doc*) "DASHED" *linefile*)))
      )
  (if (setq ss (ssget "X" '((0 . "TEXT") (1 . "[A-Z]")
			    (-3 ("CUTNUMBER"))
			    )))
      (progn
	(setq lst '())
	(repeat (setq i (sslength ss))
		(setq lst (cons (cdr (assoc 1 (entget (ssname ss (setq i (1- i)))))) lst))
		)
	(setq cutn   (chr (1+ (ascii (car (vl-sort lst '>))))))
	)
    (setq cutn  "A")
    )
  (if (null cscale)
      (setq cscale (getvar "DIMSCALE"))
    )
  (while (progn
	   (initget "S")
	   (if (= (setq s (getpoint (@:speak "指定剖切线起始点,或捕捉对齐点,或[设置(S)]:")))
		  "S"
		  )
	       (progn
		 (dcl:dialog "cuttingSetting")
		 (dcl:input "txtHeight" "文字高度" "3.5" "")
		 (dcl:input "arrowSize" "箭头大小" "3.5" "")
		 (dcl:dialog-end-ok-cancel)
		 (dcl:new "cuttingSetting")
		 (set_tile "txtHeight" (rtos (* cscale 4)))
		 (set_tile "arrowSize" (rtos (* cscale 4)))
		 (action_tile "accept"
			      "(setq cscale ( * 0.25 (atof (get_tile \"txtHeight\"))))(done_dialog )")
		 (dcl:show)
		 )
	     (setq pt0 s)
	     )
	   (= s "S")
	   )
    )
  (if (ssget "c" pt0 pt0)
      (setq pt0 (getpoint pt0 (@:speak"指定起点：")))
    )
  (@:prompt (strcat "\n指定箭头方向,或符号:<" cutn  ">,右键向视"))
  
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
	  (list (polar pt0 (* 0.5 pi) (* cscale 4))
		pt0
		(polar pt0 0 (* cscale 2))
		(polar pt0 0 (* cscale 4))
		)
	  nil
	  (list
	   (* cscale 0.3)
	   0
	   (list (* cscale 0.5) 0))
	  0 0)
	 62 4)
	)
  
  (setq cuttext1
	(entity:make-text cutn   pt0 (* cscale 4)  0 0.8 0  "MM"))
  (xdata:put cuttext1 "CUTNUMBER" cutn)
  
  
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
			 (@:prompt "<正交 开>")
			 (setvar "orthomode" 1)
			 )
		     (progn
		       (@:prompt "<正交 关>")
		       (setvar "orthomode" 0)
		       )
		     )
		   )
		 )
	     (setq s (strcase (chr data)))
	     (if (wcmatch s "[A-Z]")
		 (progn
		   (setq cutn  s)
		   (entity:putdxf curttext1 1 cutn)
		   (entity:putdxf curttext2 1 cutn)
		   ;; (entity:putdxf curttext3 1 tex)
		   )
	       )
	     (if (= cutmode 1)
		 (@:prompt(strcat "\n指定箭头方向,或符号:<" cutn  ">,右键向视"))
	       )
	     (if (= cutmode 3)
		 (@:prompt(strcat "\n指定箭头方向,或符号:<" cutn  ">,右键剖视"))
	       )
	     )
	    ((= code 3)	    ; 鼠标左击,标索引详图号
	     (redraw)
	     (cond
	      ((= cutmode 1)
	       (setq cutmode 2)
	       (entdel l0)
	       (setq cutdetail-text
		     (entity:make-text (strcat cutn "-"cutn ) data (* cscale 4)  0 0.8 0  "MM"))
	       
	       (setq box (text:box cutdetail-text))
	       (setq cutdetail-l1
		     (entity:make-lwpolyline
		      (mapcar '(lambda(x)
				 (polar
				  x
				  (* 1.5 pi)
				  (* cscale 0.5)))
			      (list (car box)(cadr box)))
		      nil
		      (* cscale 0.2)
		      0 0))
	       (setq cutdetail-l2
		     (entity:make-lwpolyline
		      (mapcar '(lambda(x)
				 (polar
				  x
				  (* 1.5 pi)
				  (* cscale 1)))
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
		     (entity:make-text (strcat cutn"-"cutn) data (* cscale 4)  0 0.8 0  "MM"))
	       
	       (setq box (text:box cutdetail-text))
	       (setq cutdetail-l1
		     (entity:make-lwpolyline
		      (mapcar '(lambda(x)
				 (polar
				  x
				  (* 1.5 pi)
				  (* cscale 0.5)))
			      (list (car box)(cadr box)))
		      nil
		      (* cscale 0.2)
		      0 0))
	       (setq cutdetail-l2
		     (entity:make-lwpolyline
		      (mapcar '(lambda(x)
				 (polar
				  x
				  (* 1.5 pi)
				  (* cscale 1.5)))
			      (list (car box)(cadr box)))
		      nil
		      0 
		      0 0))
	       (@:prompt "\n指定插入点:")
	       )
	      )
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
				     (list (polar pt0 r (* cscale 4))
					   pt0
					   (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* cscale 2))
					   (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* cscale 4)))

				     )
		   ;; 文字位置
		   (entity:putdxf cuttext1  11
				  (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* cscale 6)))
		   
		   (entity:putdxf cuttext1 50 (m:fix-angle (- r (* 0.5 pi))))
		   (curve:put-points cutline2
				     (list (polar pt(m:fix-angle(+ r pi)) (* cscale 4))
					   pt
					   (polar pt (m:fix-angle (- r (* 0.5 pi))) (* cscale 2))
					   (polar pt (m:fix-angle (- r (* 0.5 pi))) (* cscale 4)))
				     )
		   (entity:putdxf cuttext2 11
				  (polar pt (m:fix-angle (- r (* 0.5 pi))) (* cscale 6)))
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
						 (* cscale 0.5)))
					     (list (car box)(cadr box))))
		   (curve:put-points cutdetail-l2
				     (mapcar '(lambda(x)
						(polar
						 x
						 (* 1.5 pi)
						 (* cscale 1.5)))
					     (list (car box)(cadr box))))
		   ))
	     (if (= cutmode 3);;向视
		 (progn
		   (entity:putdxf l0 11 pt)
		   (curve:put-points
		    cutline1
		    (list pt0
			  (polar
			   pt0
			   (m:fix-angle(+ pi r))
			   (* cscale 8))
			  (polar
			   pt0
			   (m:fix-angle(+ pi r))
			   (* cscale 10))
			  (polar
			   pt0
			   (m:fix-angle(+ pi r))
			   (* cscale 12))))
		   (progn
		     (entity:putdxf
		      cuttext1
		      11
		      (polar
		       (point:mid pt0
				  (polar
				   pt0
				   (m:fix-angle(+ pi r))
				   (* cscale 5)))
		       (+ r (* 0.5 pi))
		       (* cscale 5)
		       ))
		     (entity:putdxf cuttext1 50
				    (m:fix-angle r)))
		   )
	       ))
	    
	    ((or(= code 11) (= code 25));; 鼠标右击
	     (if (= cutmode 1)
		 (progn
		   (@:prompt (strcat "\n指定箭头方向,或符号:<" cutn  ">,右键剖视"))
		   (setq cutmode 3)
		   ;;删除剖视
		   (mapcar '(lambda(x)
			      (if (e2o x)
				  (vla-put-visible
				   (e2o x)
				   :vlax-false)))
			   (list cutline2 cuttext2))
		   ;;绘制向视(改line1 text1)
		   (curve:put-points
		    curline1
		    (list pt0
			  (polar pt0 pi (* cscale 8))
			  (polar pt0 pi (* cscale 10))
			  (polar pt0 pi (* cscale 12))
			  ))
		   (entity:putdxf cuttext11 1 (strcat cutn  "向"))
		   (entity:putdxf cuttext11 11
				  (polar
				   (point:mid
				    pt0
				    (polar pt0 pi (* cscale 8)))
				   (- (angle
				       pt0
				       (polar pt0 pi (* cscale 6)))
				      (* 0.5 pi))
				   (* cscale 3)
				   ))
		   )
	       (if (= cutmode 3)
		   (progn
		     (@:prompt (strcat "\n指定箭头方向,或符号:<" cutn ">,右键向视"))
		     (setq cutmode 1)
		     ;; line1 text1 改剖视形态
		     (or r (setq r (* 0.5 pi)))
		     (curve:put-points cutline1
				       (list (polar pt0 r (* cscale 4))
					     pt0
					     (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* cscale 2))
					     (polar pt0(m:fix-angle (- r (* 0.5 pi))) (* cscale 4)))
				       
				       )
		     (entity:putdxf cuttext11 1 (strcat cutn ))
		     (mapcar '(lambda(x)
				(if (e2o  x)
				    (vla-put-visible
				     (e2o x)
				     :vlax-true)))
			     (list cutline2 cuttext2))
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
	 ))
  (princ)
  )
