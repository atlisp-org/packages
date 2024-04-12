;; 取块剪裁范围点，两点为矩形

(defun @block:explode-cliped (blk ) ;; pts ent0 ents-x objs-x )
  (push-var '("osmode" "snapmode" "gridmode"))
  (setvar "osmode" 0)
  (setvar "gridmode" 0)
  (setvar "snapmode" 0)
  (if (setq pts(entity:getdxf(entity:getdxf (entity:getdxf (entity:getdxf blk 360) 360)360) 10))
      (progn
	;; (setq ents-x nil);;炸开的块图元
	(setq objs-x (vlax-safearray->list (vlax-variant-value (vla-Explode (e2o blk)))))
	(mapcar 'vla-update objs-x)
	(setq ents-x (mapcar 'o2e objs-x))
	;; 删除原块引用
	(vla-delete (e2o blk))
	;;绘制剪裁范围框，用于人工核查
	(if (= 2 (length pts))
	    (setq pts (apply 'point:rec-2pt->4pt pts)))
	(setq border
	      (entity:putdxf 
	       (entity:make-lwpolyline
		pts nil 0 1 0)
	       62 1))
	(if (null(curve:clockwisep border))
	    (setq obj-trim (car (vlax-safearray->list (vlax-variant-value (vla-offset (e2o  border) 10)))))
	    (setq obj-trim (car (vlax-safearray->list (vlax-variant-value (vla-offset (e2o  border) -10)))))
	    )
	(setq pts-trim (curve:get-points (o2e obj-trim)))
	(setq pts-trim (append pts-trim (list (car pts-trim))))
	(vla-delete obj-trim)
	;; 取剪裁范围的图元
	(setq ents-in (pickset:to-list (ssget "cp" pts)))
	;; 去掉不在范围内的图元
	(if (pickset:intersect  ents-in ents-x)
	    (mapcar '(lambda(x)
		      (if (not(member x ents-in))
			  (entdel x)))
		    ents-x))
	;; trim 相交以外的图形
	(command "trim" "O" "S" (pickset:from-entlist ents-in) "" "t" (ssadd border) "" "f")
	(while (car pts-trim)
	  (command (string:from-list (mapcar 'rtos (vl-remove 0 (car pts-trim)))","))
	  (setq pts-trim (cdr pts-trim)))
	(command "" "")
	(entdel border)
	))
  (pop-var)
  )
 
(defun @block:menu-explode-cliped ()
  (@::help "炸开剪裁块")
  (@::alert "注意: 当前程序只能处理原始剪裁的块，还无法处理复制的剪裁块")
  (mapcar '@block:explode-cliped
	  (pickset:to-list (ssget '((0 . "insert")))))

  )
