(defun @layout:ssgetx (layout)
  "选择布局中除主视口个的所有图元"
  (ssget "x" (list (cons 410 layout)
		   '(-4 . "<NOT")'(-4 . "<AND")
		   '(0 . "viewport")'(69 . 1)
		   '(-4 . "AND>")'(-4 . "NOT>"))))
(defun @layout:merge (/ merge-layout)
  "合并布局内容到第一个布局中。"
  (defun merge-layout (x / ss bref box)
    (if(and (setq ss (@layout:ssgetx x))
	    (setq box (pickset:getbox ss 0))
	    (setq bname (strcat x "-" (@:timestamp))))
	(progn
	  (entity:block ss bname (car box))
	  (setq bref (vla-InsertBlock (vla-get-block layout1)
				      (point:to-ax pt-lb)
				      bname
				      1 1 1 0))
	  (vla-explode bref)
	  (vla-delete bref)
	  (setq pt-lb (polar pt-lb 0(* 1.1 (- (caadr box) (caar box)))))
	  )))
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
   (merge-layout layout%))
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
