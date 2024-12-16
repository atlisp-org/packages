(@:define-config '@text:target-lang "en" "翻译文本的目标语言，支持en,zh,zht,jp,kor等")
(defun @text:translate (/ txts boxs res maxdis)
  (@::help '("翻译选中的单行或多行文本"))
  (setq txts (pickset:to-list (ssget '((0 . "*text")))))
  (setq boxs(mapcar '(lambda(x)(entity:getbox x 0)) txts))
  (setq res (mapcar '(lambda(x / lst-res)
		      (setq lst-res
		       (string:to-list
			(text:remove-fmt (string:subst-all "%0A" "\\P" (text:get-mtext x)))
			"%0A"))
		      (string:from-lst
		       (mapcar '(lambda(x)
				 (@:machine-translate
				  x
				  "zh"(strcase (@:get-config '@text:target-lang) t)))
			lst-res)
		       "\\P"
		       ))
		    txts))

  (setq maxdis (-
		(apply 'max (mapcar '(lambda(x)(car (cadr x))) boxs))
		(apply 'min (mapcar '(lambda(x)(car (car x))) boxs))))
  ;; 英文字高减半
  (mapcar
   '(lambda(x y / box)
     (setq box (entity:getbox y 0))
     (if y
	 (entity:make-mtext
	  x
	  (polar (list (caar box)
		       (cadadr box)
		       0)
		 0 (* maxdis 1.2))
	  (if (string-equal "en" (@:get-config '@text:target-lang))
	      (* 0.6 (entity:getdxf y 40))
	      (entity:getdxf y 40))
	  (- (car (cadr box))
	     (car (car box)))
	  (- (cadr (cadr box))
	     (cadr (car box))))))
   res txts))
(defun @text:translate-from-en (/ txts boxs res maxdis)
  (@::help '("翻译选中纯英文的单行或多行文本至当前系统语言"))
  (setq txts (pickset:to-list (ssget '((0 . "*text")))))
  ;;去除非英文
  (setq txts (vl-remove-if
	      '(lambda(x)
		(> 
		 (apply 'max
		  (string:s2l-ansi
		   (text:remove-fmt (string:subst-all "%0A" "\\P" (text:get-mtext x)))))
		 128))
	      txts))
  (setq boxs(mapcar '(lambda(x)(entity:getbox x 0)) txts))
  (setq res (mapcar '(lambda(x / lst-res)
		      (setq lst-res
		       (string:to-list
			(text:remove-fmt (string:subst-all "%0A" "\\P" (text:get-mtext x)))
			"%0A"))
		      (string:from-lst
		       (mapcar '(lambda(x)
				 (@:machine-translate
				  x
				  "en" ;; (strcase (@:get-config '@text:target-lang) t)
				  "zh"
				  ))
			lst-res)
		       "\\P"
		       ))
		    txts))

  (setq maxdis (-
		(apply 'max (mapcar '(lambda(x)(car (cadr x))) boxs))
		(apply 'min (mapcar '(lambda(x)(car (car x))) boxs))))
  (mapcar
   '(lambda(x y / box)
     (setq box (entity:getbox y 0))
     (if y
	 (entity:make-mtext
	  x
	  (polar (list (caar box)
		       (cadadr box)
		       0)
		 0 (* maxdis 1.2))

	  (entity:getdxf y 40)
	  (- (car (cadr box))
	     (car (car box)))
	  (- (cadr (cadr box))
	     (cadr (car box))))))
   res txts))
