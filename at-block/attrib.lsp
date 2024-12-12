(defun @block:match-att-style ()
  (@::prompt "将修改的属性样式刷到其它同名块引用的同名属性上。")
  (setq srcatt (car (nentsel (@::prompt "请选择源属性:"))))
  (if (eq "ATTRIB" (entity:getdxf srcatt 0))
      (progn
	;;上级块名
	(@::prompt "请选择目标块:")
	(setq bname (block:get-effectivename
		     (setq srcblk (entity:getdxf srcatt 330))))
	;;选择目标块
	(setq blks (block:ssget nil (list bname) nil))
	(mapcar '(lambda(blk)
		  (mapcar
		   '(lambda(targetatt)
		     ;;(foreach dxf '(7 40 41 50 72 74)
		     (entity:putdxf targetatt 7 (entity:getdxf srcatt 7))
		     (entity:putdxf targetatt 41 (entity:getdxf srcatt 41))
		     (entity:putdxf targetatt 72 (entity:getdxf srcatt 72))
		     (entity:putdxf targetatt 74 (entity:getdxf srcatt 74))
		     
		     (entity:putdxf targetatt 40
		      (* (/ (entity:getdxf srcatt 40)
			    (entity:getdxf srcblk 41))
		       (entity:getdxf blk 41)))
		     (entity:putdxf targetatt 50
		      (+ (- (entity:getdxf srcatt 50)
			    (entity:getdxf srcblk 50))
		       (entity:getdxf blk 50)))
		     )
		   (vl-remove-if-not
		    '(lambda(y)
		      (eq (entity:getdxf srcatt 2)
		       (entity:getdxf y 2)))
		    (block:get-attrib-ents blk))))
		(pickset:to-list blks))))
  (princ))
	
