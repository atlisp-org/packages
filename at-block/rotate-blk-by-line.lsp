(defun @block:rotate-blk-by-line()
  (@:prompt '("选择块内一条直线，按该直线水平旋转块."
	      "输入Y旋转所有同名块。"
	      ))
  (if (and 
       (setq lineblk (nentsel "请选择块内一条直线:"))
       (setq line (car lineblk))
       (setq blkref (car (last lineblk)))
       (= "INSERT" (entity:getdxf blkref 0))
       (= "LINE" (entity:getdxf line 0 )))
      (progn
	;; 选择同名块
	(entity:putdxf
	 (pickset:to-list (ssget "x" (list '(0 . "insert")
					   (cons 2 (entity:getdxf blkref 2)))))
	 50
	 (* -1 (angle(entity:getdxf line 10)
		     (entity:getdxf line 11))))
	)))
