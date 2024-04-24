(defun @hvac:dim-pipe ()
  (@:help'("标地暖管"))
  (@:prompt "点选地暖管")
  (if (and (setq pipe-d (car (ssnamex (ssget ":E:S" '((0 . "lwpolyline")(8 . "*地暖*"))))))
	   (p:enamep (nth 1 pipe-d)))
      (progn
	(setq len (* 0.001 (curve:length (nth 1 pipe-d))))
	(if (< (- len (fix len)) 0.5)
	    (setq len (+ (fix len) 1.0))
	  (setq len (+ (fix len) 1.5)))
	(setq ml
	      (entity:make-multileader
	       (list (setq pt-b (cadr (nth 3 pipe-d)))
		     (setq pt-c (getpoint pt-b "标注位置")))
	       (strcat "D=300mm\\PL="
		       (string:number-format
			(rtos len 2 1)
			2 1 " 0")
		       "m")))
	(vla-put-ScaleFactor (e2o ml)
			     (* 10 (@:get-config '@::draw-scale)))
	;;(entity:putdxf ml 12 (polar pt-c pi 1000))
	)
    ))
(defun @hvac:move-pt-base (ent pts / dxfent)
  "更改曲线控制点及端点列表。pts为新的点位置,nil为不替换。"
  "ent"
  "(curve:put-points (car (entsel))
    '((0 0 0)))"
  (if (p:vlap ent)
    (setq ent (o2e ent)))
  (if (= (quote ename)
      (type ent))
      (cond
       ((wcmatch (entity:getdxf ent 0)
		 "MULTILEADER")
        (setq dxfent (entget ent))
        (mapcar (quote (lambda (dxf1011 pt)
			 (if pt (setq dxfent (subst (cons (car dxf1011)
							  pt)
						    dxf1011 dxfent)))))
		(vl-remove-if-not (quote (lambda (x)
					   (or (= 10 (car x))
					       (= 12 (car x))
					       )))
				  (entget ent))
		pts)
        (entmod dxfent)
        (entupd ent)))))
(defun @hvac:batch-dim-pipe ()
  (@:help'("批量标地暖管的间距和分支长度"))
  (@:prompt "请框选地暖管:")
  (if (setq pipe-s (pickset:to-list (ssget  '((0 . "lwpolyline")(8 . "*地暖*")))))
      (progn
	(setq ents
	      (mapcar
	       '(lambda(pipe-d)
		  (setq len (* 0.001 (curve:length pipe-d)))
		  (if (< (- len (fix len)) 0.4)
		      (setq len (+ (fix len) 1.0))
		    (setq len (+ (fix len) 1.5)))
		  ;; 找水平线的中点
		  (setq pt-s (curve:midpoint pipe-d))
		  (while (or (="arc"(curve:subsegment-picked-type pipe-d pt-s))
			     (not (or (equal 0 (apply 'angle (curve:subsegment-picked-points pipe-d pt-s))
					     0.01)
				      (equal pi (apply 'angle (curve:subsegment-picked-points pipe-d pt-s))
					     0.01)))
			     )
		    (print (curve:subsegment-picked-type pipe-d pt-s))
		    (print (apply 'angle (curve:subsegment-picked-points pipe-d pt-s)))
		    (print pt-s)
		    (setq pt-s (cadr (curve:subsegment-picked-points pipe-d pt-s))))
		  (setq pt-s (apply 'point:mid (curve:subsegment-picked-points pipe-d pt-s)))
		  (setq pt-d (polar pt-s (* 1.5 pi) 6000))
		  (setq line-t (entity:make-line pt-s pt-d))
		  (setq pt-inters (vl-sort (curve:inters (e2o pipe-d)(e2o line-t) acExtendNone)
					   '(lambda(x y)
					      (> (cadr x)(cadr y)))))
		  (entdel line-t)
		  
		  (setq d (car (stat:mode (stat:stat (mapcar 'fix (mapcar 'distance pt-inters (cdr pt-inters)))))))
		  (setq ml
			(entity:make-multileader
			 (list pt-s pt-d)
			 (strcat "D="(itoa d)"mm\\PL="
				 (string:number-format
				  (rtos len 2 1)
				  2 1 " 0")
				 "m")))
		  (vla-put-ScaleFactor (e2o ml)
				       (* 10 (@:get-config '@::draw-scale)))
		  ml
		  )
	       pipe-s))
	(setq flag t)
	(if (p:ename-listp ents)
	    (progn
	      (while flag
		(setq gr (grread t 16))
		(cond
		 ((= 3 (car gr))
		  "按下鼠标左键"
		  ;;绘制并退出
		  (setq flag nil)
		  )
		 ((or (= 25 (car gr))
		      (= 11 (car gr)))
		  "按下鼠标右键"
		  ;; 删除并退出
		  (mapcar 'entdel ents)
		  (setq ents  nil)
		  (setq flag nil)
		  )
		 ((= 5 (car gr))
		  "移动鼠标"
		  (mapcar (function(lambda(x)
				     (setq pt-ml (entity:getdxf x 10))
				     (@hvac:move-pt-base
				      x
				      (list
				       (list (car (nth 0 pt-ml))
					     (cadr (cadr gr))
					     0)
				       (list (car (nth 0 pt-ml))
					     (+ 450 (cadr (cadr gr)))
					     0)
				       (list (car (nth 1 pt-ml))
					     (cadr (cadr gr))
					     0)
				       (nth 2 pt-ml)
				       (nth 3 pt-ml)))
				       ))
		     ents)
		  ;; (setq pt-base (cadr gr))
		  )
		 (t "其它情况"
		    (princ gr)))
		)
	      ents
	      )
	  ))
    )
  )
