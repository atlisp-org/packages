;;;布局1和纬地视口两种,重新运行命令需要pu一下
(defun @layout:merge (/ merge-layout)
  "合并布局内容到第一个布局中。"
  (defun merge-layout (x / ss bref box)
    (if(and (setq ss (ssget "x"
		    (list '(0 . "viewport")
			  (cons 410 x)
			  '(-4 . "<NOT")
			  '(69 . 1)
			  '(-4 . "NOT>")
			  )))
	    (setq box (pickset:getbox ss 0))
	    (setq bname (strcat x "-" (@:timestamp))))
	(progn
	  (entity:block ss bname (car box))
	  (setq bref
		(vla-InsertBlock (vla-get-block layout1)
				 (point:to-ax pt-lb)
				 bname
				 1 1 1 0))
	  (vla-explode bref)
	  (vla-delete bref)
	  (setq pt-lb (polar pt-lb 0 (distance (car box)(cadr box))))
	  )
      ))
  (setq layouts (layout:vla-list))
  (setq layout1 (car layouts))
  (setq layoutnames (mapcar 'vla-get-name layouts))
  (setq layoutname1 (car layoutnames))
  ;;(setvar "ctab" layoutname1)
  (if(and (setq ss (ssget "x"
			  (list '(0 . "viewport")
				(cons 410 layoutname1)
				'(-4 . "<NOT")
				'(69 . 1)
				'(-4 . "NOT>")
				)))
	  (setq box (pickset:getbox ss 0)))
      (progn
	(setq pt-lb (polar (car box) 0 (distance (car box)(cadr box)))))
    (setq pt-lb (list 0.0 0.0 0.0)))
  (foreach
   layout% (cdr layoutnames)
   (merge-layout layout%)
   )
  (vla-purgeall *DOC*)

  ;; 打开所有视口
  (setq ss (ssget "x"
		  (list '(0 . "viewport")
			'(-4 . "<NOT")
			'(69 . 1)
			'(-4 . "NOT>")
			)))
  (mapcar '(lambda(x)
	     (vla-put-viewporton x :vlax-true))
	  (pickset:to-vlalist ss))
  (setvar "ctab" layoutname1)
  (princ)
  )
