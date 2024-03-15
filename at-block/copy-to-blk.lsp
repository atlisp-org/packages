(defun @block:copy-by-blk ()
  (@:help '("只支持xyz轴缩放值相同的块,如果不同仅考虑X轴的缩放。"
	    "操作步骤："
	    "1、选中要复制的图形，"
	    "2、选择源块"
	    "3、选择一个或多个目标块"
	    ))
  (@:prompt "请选择要复制的图形:")
  (setq ents(pickset:to-list (ssget)))
  (@:prompt "请选择源块:")
  (setq blk-src(car (pickset:to-list(ssget ":E:S" '((0 . "insert"))))))
  (@:prompt "请选择目标块:")
  (if (eq (vla-get-isdynamicblock (e2o blk-src)) :vlax-true)
      (progn 
	(setq blk-targets
	      (vl-remove-if-not
	       (function(lambda(x)
			  (eq
			   (vla-get-effectivename (e2o x))
			   (vla-get-effectivename (e2o blk-src)))))
	       (pickset:to-list
		(ssget (list '(0 . "insert")
			     (cons 2 (strcat (entity:getdxf blk-src 2)
					     ",`**"))
			     ))))))
    (setq blk-targets
	  (pickset:to-list
	   (ssget (list '(0 . "insert")
			(cons 2 (entity:getdxf blk-src 2)))))))
   
  (setq pt-base (entity:getdxf blk-src 10))
  (setq rotate-base (entity:getdxf blk-src 50))
  (setq scale-base (entity:getdxf blk-src 41));; '(41 42 43)))
  ;; 计算
  ;; 生成新图形
  (foreach
   blk-target blk-targets
   (setq pt-target (entity:getdxf blk-target 10))
   (setq rotate-target (entity:getdxf blk-target 50))
   (setq scale-target (entity:getdxf blk-target 41));; '(41 42 43)))
   (mapcar
    '(lambda(x)
       (vla-move x (point:to-ax pt-base)
		 (point:to-ax pt-target))
       (vla-scaleentity x (point:to-ax pt-target)
		  (/ scale-target scale-base))
       (vla-rotate x (point:to-ax pt-target)
		   (- rotate-target rotate-base)))
    (mapcar 'vla-copy (mapcar 'e2o ents)))
   ))
