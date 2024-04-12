(defun @block:clip-to-blk ()
  (@::help "剪裁图形生成块")
  (setq border (entity:make-rectangle (setq pt1 (getpoint))(setq pt2 (getcorner pt1))))
  
  (setq ents (pickset:to-list(ssget "c" pt1 pt2)))
  (setq ents-new nil)
  (foreach ent% ents
	   (if (= "INSERT" (entity:getdxf ent% 0))
	       (setq ents-new
		     (append ents-new
			     (mapcar 'o2e
				     (vlax-safearray->list (vlax-variant-value (vla-explode (e2o ent%)))))))
	       (setq ents-new
		     (append ents-new
			     (list (o2e (vla-copy (e2o ent%))))))))
  (if (null(curve:clockwisep border))
      (setq obj-trim (car (vlax-safearray->list (vlax-variant-value (vla-offset (e2o  border) 10)))))
      (setq obj-trim (car (vlax-safearray->list (vlax-variant-value (vla-offset (e2o  border) -10)))))
      )
  (setq pts-trim (curve:get-points (o2e obj-trim)))
  (setq pts-trim (append pts-trim (list (car pts-trim))))
  ;; 取剪裁范围的图元
  (setq ents-in (pickset:to-list (ssget "c" pt1 pt2)))
  ;; 去掉不在范围内的图元
  (if (pickset:intersect  ents-in ents-new)
      (mapcar '(lambda(x)
		(if (not(member x ents-in))
		    (entdel x)))
	      ents-new))
  (group:make ents-new (strcat "g"(@::timestamp)))
  ;; trim 相交以外的图形
  (command "trim" "O" "S" (pickset:from-entlist ents-new) "" "t" (ssadd border) "" "f")
  (while (car pts-trim)
    (command (string:from-list (mapcar 'rtos (vl-remove 0 (car pts-trim)))","))
    (setq pts-trim (cdr pts-trim)))
  (command "" "")
  ;; (entdel border)
  )
