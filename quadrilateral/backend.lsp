(defun quadrilateral:dim-backend ( / segments pts-ent i% )
  "查找四边形"
  ;; 检查权限
  (if (= (@:get-config 'quadrilateral:evalcode) "DEMO")
      (alert "当前使用的是测试版本，执行数量会有限制!\n")
      (if (/= (@:get-config 'quadrilateral:evalcode) (@:get-eval-code "quadrilateral"))
	  (progn (princ "ERROR:invalid code.")
		 (exit))))
  (setq scale1 (@:get-config 'quadrilateral:scale))
  (setq lst-lines (pickset:to-list
		   (ssget "x"
			  '((0 . "LINE")))));,LWPOLYLINE,POLYLINE")))))
  (@:debug "INFO" (strcat (itoa (length lst-lines))))
  (setq result '()) ;; 4个图元一组的四边形
  ;;(setq pts (curve:pline-3dpoints (car lst-lines))) ;;第一个元素的端点
  (setq pts '())
  (defun ssget-by-point (pt1 / lst-ss1)
    (command "zoom" "C" pt1 "1000")
    (setq lst-ss1 (pickset:to-list
		   (ssget "C" (polar pt1 (* 1.25 pi) 1)
			  (polar pt1 (* 0.25 pi) 1) 
			  '((0 . "LINE"))))));; ,LWPOLYLINE,POLYLINE"))))))
  ;; 向前找;
  (setq i% 0)
  ;; lst-ss-pre 根据一点得到的线集
  (while (and (> (length lst-lines) 3)
	      (< i% 10))
    (setq quad (list (car lst-lines)))
    (setq lst-lines (cdr lst-lines))
    (setq pts (curve:pline-3dpoints (car quad)))
    ;; (print pts)
    ;; 寻找连续线
    (setq i% 0)
    (while (and (> (length (setq lst-ss-pre (ssget-by-point (car pts)))) 1)
		(< (length pts) 6)
		(< 100 (distance (last pts) (car pts)));;
		(< i% 10)
		;; DEMO 版
		(if (= "DEMO" (@:get-config 'quadrilateral:evalcode))
		    (< (length result) 6)
		    T)
		)
      ;; 注意三边形死环
      (foreach ent% lst-ss-pre
	       (if (null (member ent% quad))
		   (progn
		     (setq quad (cons ent% quad));;加入选择集
		     (setq lst-lines (vl-remove ent% lst-lines))
		     (setq pts-ent (curve:pline-3dpoints ent%))
		     (if (< (distance (car pts-ent) (car pts)) 0.001)
			 (setq pts (append (reverse pts-ent) (cdr pts)))
			 (if (< (distance (last pts-ent) (car pts)) 0.001)
			     (setq pts (append pts-ent (cdr pts)))))
		     )))
      (setq i% (1+ i%))
      )
    ;;(print pts)
    ;; (entity:make-arrow (car pts)(caddr pts) 50)
    (if (and (= (length quad) 4)
	     (= (length pts) 5))
	;; 测试四边形
	;; 从总线集中去除
	(progn
	  ;;
	  (setq result (append result (list quad)))
	  (setq quad '())
	  ;; 显示结果
	  ;; (print pts)
	  (setq pt-mid (polar (car pts) (angle (car pts)(cadr pts)) (* 0.5 (distance (car pts) (cadr pts)))))
	  (entity:dimaligned (car pts) (cadr pts) (polar pt-mid (+ (angle (car pts)(cadr  pts)) (* 0.5 pi)) (* scale1 750)))
	  (setq pt-mid (polar (cadr pts) (angle (cadr pts)(caddr pts)) (* 0.5 (distance (cadr pts) (caddr pts)))))
	  (entity:dimaligned (cadr pts) (caddr pts) (polar pt-mid (+ (angle (cadr pts)(caddr  pts)) (* 0.5 pi)) (* scale1 750)))
	  
	  (setq pt-mid (polar (nth 2 pts) (angle (nth 2 pts)(nth 3 pts)) (* 0.5 (distance (nth 2 pts) (nth 3 pts)))))
	  (entity:dimaligned (nth 2 pts) (nth 3 pts) (polar pt-mid (+ (angle (nth 2 pts)(nth 3 pts)) (* 0.5 pi)) (* scale1 750)))
	  
	  (setq pt-mid (polar (nth 3 pts) (angle (nth 3 pts)(nth 4 pts)) (* 0.5 (distance (nth 3 pts) (nth 4 pts)))))
	  (entity:dimaligned (nth 3 pts) (nth 4 pts) (polar pt-mid (+ (angle (nth 3 pts)(nth 4 pts)) (* 0.5 pi)) (* scale1 750)))
	  (setq pt-mid (polar (nth 0 pts) (angle (nth 0 pts)(nth 2 pts)) (* 0.5 (distance (nth 0 pts) (nth 2 pts)))))
	  (entity:dimaligned (nth 0 pts) (nth 2 pts) (polar pt-mid (+ (angle (nth 0 pts)(nth 2 pts)) (* 0.5 pi)) (* scale1 750)))
	  (setq pt-mid (polar (nth 1 pts) (angle (nth 1 pts)(nth 3 pts)) (* 0.5 (distance (nth 1 pts) (nth 3 pts)))))
	  (entity:dimaligned (nth 1 pts) (nth 3 pts) (polar pt-mid (+ (angle (nth 1 pts)(nth 3 pts)) (* 0.5 pi)) (* scale1 750)))
	  
	  (setq i% 0) ; 重新计数。
	  ))
    )
  
  ;;(print lst-ss1)
  ;;(print pts)
  (command "zoom" "a")
  (princ)
  )
