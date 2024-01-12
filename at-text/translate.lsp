(@:define-config '@text:target-lang "en" "翻译文本的目标语言，支持en,zh,zht,jp,kor等")
(defun @text:translate (/ txts boxs res maxdis)
  (@:help '("翻译选中的单行或多行文本"))
  (setq txts (pickset:to-list (ssget '((0 . "*text")))))
  (setq boxs(mapcar '(lambda(x)(entity:getbox x 0)) txts))
  (setq res (mapcar '(lambda(x)
		      (@:machine-translate
		       (text:remove-fmt (text:get-mtext x))
		       "zh" (@:get-config '@text:target-lang)))
		    txts))

  (setq maxdis (-
		(apply 'max (mapcar '(lambda(x)(car (cadr x))) boxs))
		(apply 'min (mapcar '(lambda(x)(car (car x))) boxs))))
  (mapcar
   '(lambda(x y)
     (entity:make-mtext x
      (polar (entity:getdxf y 10) 0 (* maxdis 1.2))
      (entity:getdxf y 40)
      (* maxdis 2)
      (if (entity:getdxf y 50)(entity:getdxf y 50) (entity:getdxf y 40))))
   res txts))