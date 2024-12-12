 
(defun @block:explode-cliped (blkref / *error*) ;; pts ent0 ents-x objs-x )
  (defun *error*(msg)
    (pop-var)
    (princ msg))
  (push-var '("osmode" "snapmode" "gridmode"))
  (setvar "osmode" 0)
  (setvar "gridmode" 0)
  (setvar "snapmode" 0)
  (if (setq pts (block:get-clip-boundary blkref))
      (progn
	(if (= 1
	       (entity:getdxf 
		(setq ent-sf (entity:getdxf (entity:getdxf (entity:getdxf blkref 360) 360)360)) 290))
	    (setq flag-out t)
	    (setq flag-out nil))
	;; 炸开后的块图元 ents-x
	(setq objs-x (list:flatten (@block:explode-chain blkref)))
	(mapcar 'vla-update objs-x)
	(setq ents-x (mapcar 'o2e objs-x))
	;;(princ "abc")
	;; 炸到底
	;; 删除原块引用
	;; (vla-delete (e2o blkref))
	;;绘制剪裁范围框，用于人工核查
	(setq border
	      (entity:putdxf 
	       (entity:make-lwpolyline
		pts nil 0 1 0)
	       62 1))

	(setq offset-value
	      (if (null(curve:clockwisep border))
		  10
		  -10))
	(if flag-out
	    (setq offset-value (* -1 offset-value)))
	(setq obj-trim (car (vlax-safearray->list (vlax-variant-value (vla-offset (e2o  border) offset-value)))))
	
	(setq pts-trim (curve:get-points (o2e obj-trim)))
	(setq pts-trim (append pts-trim (list (car pts-trim))))
	(vla-delete obj-trim)
	;; 取剪裁范围的图元
	(if flag-out
	    (progn
	      (setq ents-in (pickset:to-list (ssget "wp" pts)))
	      ;; 去掉在范围内的图元
	      (mapcar '(lambda(x)
			(if (member x ents-in)
			    (entdel x)))
		      ents-x))
	    (progn
	      (setq ents-in (pickset:to-list (ssget "cp" pts)))
	      ;; 去掉不在范围内的图元
	      (mapcar '(lambda(x)
			(if (not(member x ents-in))
			    (entdel x)))
		      ents-x)))
	(setvar "cmdecho" 0)
	(command "trim" "O" "S" (pickset:from-entlist ents-in) "" "t" (ssadd border) "" "f")
	(while (car pts-trim)
	  (command (string:from-list (mapcar 'rtos (vl-remove 0 (car pts-trim)))","))
	  (setq pts-trim (cdr pts-trim)))
	(command "" "")
	(setvar "cmdecho" 1)
	(entdel border)
	))
  (pop-var)
  )
 
(defun @block:menu-explode-cliped ()
  (@::prompt "炸开剪裁块")
  (mapcar '@block:explode-cliped
	  (pickset:to-list (ssget '((0 . "insert")))))

  )
