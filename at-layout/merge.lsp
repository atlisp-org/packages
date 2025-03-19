(defun @layout:ssgetx (layout)
  "选择布局中除主视口个的所有图元"
  (ssget "x" (list (cons 410 layout)
		   '(-4 . "<NOT")'(-4 . "<AND")
		   '(0 . "viewport")'(69 . 1)
		   '(-4 . "AND>")'(-4 . "NOT>"))))
(defun @layout:inner-merge-layout (x / ss bref box return)
  (if(and (setq ss (@layout:ssgetx x))
	  (setq box (pickset:getbox ss 0))
	  (setq bname (strcat x "-" (@:timestamp)))
	  (entity:block ss bname (car box)))
     (if
      (setq bref (vla-InsertBlock (vla-get-block layout1)
				  (point:to-ax pt-lb)
				  bname
				  1 1 1 0))
      (progn
	(vla-explode bref)
	(setq return (vl-catch-all-apply 
		      '(lambda () 
			(vla-delete bref))))
	(setq pt-lb (polar pt-lb 0(* 1.1 (- (caadr box) (caar box)))))
	))))
(defun @layout:merge-selelected (/ merge-layout)
  "合并选中的布局内容到选中的第一个布局中。"
  (setq layouts (layout:vla-list))
  (setq layoutnames (mapcar 'vla-get-name layouts))
  (setq layouts-sel (reverse(ui:select-multi"请选择要合并的布局"layoutnames)))
  
  (setq layoutname1 (car layouts-sel))
  
  (setq layout1 (car (vl-remove-if-not '(lambda(x)
				    (= (vla-get-name x)
				     layoutname1))
				  layouts)))
  (if(and (setq ss(@layout:ssgetx layoutname1))
	  (setq box (pickset:getbox ss 0)))
     (progn
       (setq pt-lb (polar (car box) 0 (* 1.1 (- (caadr box) (caar box))))))
     (setq pt-lb (list 0.0 0.0 0.0)))
  (foreach
   layout% (cdr layouts-sel)
   (@layout:inner-merge-layout layout%))
  (vla-purgeall *DOC*)
  
  ;; 打开所有视口
  (setq ss (ssget "x"
		  (list '(0 . "viewport")
			'(-4 . "<NOT")'(69 . 1)'(-4 . "NOT>"))))
  (mapcar '(lambda(x)
	    (vla-put-viewporton x :vlax-true))
	  (pickset:to-vlalist ss))
  (setvar "ctab" layoutname1)
  (princ))
(defun @layout:merge (/ merge-layout)
  "合并布局内容到第一个布局中。"
  (setq layouts (layout:vla-list))
  (setq layout1 (car layouts))
  (setq layoutnames (mapcar 'vla-get-name layouts))
  (setq layoutname1 (car layoutnames))
  (if(and (setq ss(@layout:ssgetx layoutname1))
	  (setq box (pickset:getbox ss 0)))
     (progn
       (setq pt-lb (polar (car box) 0 (* 1.1 (- (caadr box) (caar box))))))
     (setq pt-lb (list 0.0 0.0 0.0)))
  (foreach
   layout% (cdr layoutnames)
   (@layout:inner-merge-layout layout%))
  (vla-purgeall *DOC*)
  
  ;; 打开所有视口
  (setq ss (ssget "x"
		  (list '(0 . "viewport")
			'(-4 . "<NOT")'(69 . 1)'(-4 . "NOT>"))))
  (mapcar '(lambda(x)
	    (vla-put-viewporton x :vlax-true))
	  (pickset:to-vlalist ss))
  (setvar "ctab" layoutname1)
  (princ))

(defun @layout:merge-next (/ merge-layout)
  (@::prompt '("将当前布局后面相邻的第一个布局内容移到当前布局中。"))
  (setq layouts (layout:vla-list))
  (setq layoutnames (member (getvar "ctab") (mapcar 'vla-get-name layouts)))
    
  (setq layoutname1 (car layoutnames))
  
  (if (setq layoutname2 (cadr layoutnames))
      (progn
	(setq layout1 (car (vl-remove-if-not '(lambda(x)
					       (= (vla-get-name x)
						layoutname1))
					     layouts)))
	
	(if(and (setq ss(@layout:ssgetx layoutname1))
		(setq box (pickset:getbox ss 0)))
	   (progn
	     (setq pt-lb (polar (car box) 0 (* 1.1 (- (caadr box) (caar box))))))
	   (setq pt-lb (list 0.0 0.0 0.0)))
	(@layout:inner-merge-layout layoutname2)
	(vl-catch-all-apply 
	 '(lambda () 
	   (vla-purgeall *DOC*)))
	
	;; 打开所有视口
	(setq ss (ssget "x"
			(list '(0 . "viewport")
			      '(-4 . "<NOT")'(69 . 1)'(-4 . "NOT>"))))
	(princ "flag 2\n")
	(mapcar '(lambda(x)
		  (vl-catch-all-apply 
		   '(lambda ()
		     (vla-put-viewporton x :vlax-true))))
		(pickset:to-vlalist ss))
	(setvar "ctab" layoutname1)))
  (princ))
