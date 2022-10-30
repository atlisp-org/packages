(@:add-menu "@试验室" "文字改z" '(at-lab:set-z))
(@:define-config '@lab:zblkname "A$C*" "桩体名，支持通配符")
(defun at-lab:set-z ()
  ;; 1、选中所有 “承台面标高*" 文字，取标高值。
  (@:prompt "请框选承台面标高文字")
  (setq lst-txtz(pickset:to-list (ssget '((0 . "text")(1 . "承台面标高*")))))
  (foreach
   txtz lst-txtz
   ;; 取标高值
   (setq value-z (last (string:auto-split (entity:getdxf txtz 1))))
   ;; 2、求文字图元下部一定范围的 A$C* 名称的块。可求文字框下部的左右字向下 一定的高度范围。
   (setq pt-lb (car (entity:getbox txtz 0)))
   (setq zhuang
	 (pickset:to-list
	  (ssget "c" pt-lb
		 (mapcar '+ pt-lb '(3300 -2600 0))
		 (list '(0 . "insert")
		       (cons 2 (@:get-config '@lab:zblkname))))))
   ;; 3、求块范围内的 D开头的文字，并赋Z值为 标高值即可。（块图元包围盒)
   (if zhuang
       (foreach
	zhuang% zhuang
	(setq zbox
	      (entity:getbox zhuang% 0))
	(setq txt-d
	      (pickset:to-list
	       (ssget "w"
		      (car zbox)
		      (cadr zbox)
		      '((0 . "text")(1 . "D*")))))
	(foreach
	  txt-d%
	  txt-d
	  (entity:putdxf
	   txt-d%
	   10
	   (list 
	    (car (entity:getdxf txt-d% 10))
	    (cadr (entity:getdxf txt-d% 10))
	    (atof value-z)))
	  (@:log "INFO" (strcat (entity:getdxf txt-d% 1)
				" z =>"
				value-z))
	  
	  ))))
  (princ)
  )

       
  
