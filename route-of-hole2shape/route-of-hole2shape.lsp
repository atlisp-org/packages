(@:define-config 'route-of-hole2shape:offset 3.0 "刀路相对原多段线的偏移量，正为外偏，负数为内偏。")
(@:define-config 'ROUTE-OF-HOLE2SHAPE:layer  "BBB" "刀路线所在图层")
(@:define-config 'ROUTE-OF-HOLE2SHAPE:color  200 "新建的刀路线颜色")
(@:define-config 'route-of-hole2shape:c2pl-vertex  4 "圆转多段线的顶点数。该值不得小于2")
(setq route-of-hole2shape:*show-clockwise* nil)
(layer:make (@:get-config  'ROUTE-OF-HOLE2SHAPE:layer)
	    (@:get-config  'ROUTE-OF-HOLE2SHAPE:color)
	    nil nil)
(@:add-menus
 '(("孔到边刀路"
    ("设置" "(route-of-hole2shape:config)")
    ("手动刀路" "(route-of-hole2shape:menu-route)")
    ("自动刀路" "(route-of-hole2shape:auto)")
    ("圆变PL线" "(route-of-hole2shape:c2pl)")
    ;;("显隐方向" "(route-of-hole2shape:show-clockwise)")
    ("删除刀路" "(route-of-hole2shape:remove-route)")
    ("PL圆分色" "(route-of-hole2shape:bianbie)")
    )))

(defun route-of-hole2shape:config (/ res)
  "工程管理基本信息"
  (setq res 
	(ui:input "配置信息"
		  (mapcar '(lambda (x) (list (strcase (vl-symbol-name (car x)) T)(cadr x)(cddr x)))
			  (vl-remove-if '(lambda (x) (not (wcmatch (vl-symbol-name (car x)) "ROUTE-OF-HOLE2SHAPE:*")))
					(if @:*config.db*
					    @:*config.db* (@:load-config))))))
  (foreach res% res
   	   (@:set-config (read (car res%)) (cdr res%)))
  )
(defun route-of-hole2shape:input-offset (/ res)
  "工程管理基本信息"
  (setq res 
	(ui:input "配置信息"
		  (mapcar '(lambda (x) (list (strcase (vl-symbol-name (car x)) T)(cadr x)(cddr x)))
			  (vl-remove-if '(lambda (x) (not (wcmatch (vl-symbol-name (car x)) "ROUTE-OF-HOLE2SHAPE:OFFSET")))
					(if @:*config.db*
					    @:*config.db* (@:load-config))))))
  (foreach res% res
   	   (@:set-config (read (car res%)) (cdr res%)))
  )

(defun route-of-hole2shape:menu-route (/ pt-c pt-pl clockwise offset i new-pts ent-shape)
  (push-var)
  (setq pt-c (entity:getdxf (car (entsel "选择圆：")) 10))
  (setvar "osmode" 16383)
  (setq pt-pl (getpoint pt-c "选择多段线:"))
  (pop-var)
  (setq ent-shape (car(pickset:to-list(ssget pt-pl '((0 . "lwpolyline"))))))
  (setq clockwise (ui:confirm "顺时针方向点确定，逆时针点取消."))
  (route-of-hole2shape:input-offset)
  (setq offset (@:get-config 'route-of-hole2shape:offset))
  (route-of-hole2shape:route pt-c ent-shape clockwise offset nil))

(defun route-of-hole2shape:route (pt-c ent-pl clockwise offset closeto?
				       / ent-shape ent-pts pt-closeto
				       convexity closed?)
  ;; closeto? T 为曲线上的任意点， nil 为曲线上的顶点
  ;;选择多段线
  ;; 方向不同 正负不同
  
  (setq ent-shape (route-of-hole2shape:offset-shape ent-pl))
  (setq closed? (entity:getdxf ent-shape 70))
  ;; 逆时针点序不对
  (if (and (/= (curve:clockwisep ent-shape)
	       clockwise)
	   (= 1 closed?))
      (progn
	(setq ent-shape1 (curve:lwpl-turn-clockwise ent-shape))
	(entdel ent-shape)
	(setq ent-shape ent-shape1
	      ent-shape1 nil)
 	))

  (setq ent-pts (curve:pline-3dpoints ent-shape))
  (setq convexity (curve:pline-convexity ent-shape))
  (if (> (length ent-pts) (entity:getdxf ent-shape 90))
      (if closeto?
	  (setq ent-pts (reverse(cdr(reverse ent-pts))))
	(setq ent-pts (cdr ent-pts))))
  (if (> (length convexity) (entity:getdxf ent-shape 90))
      (setq convexity (reverse(cdr(reverse convexity)))))
  (if (= 1 closed?)
      (if closeto?
	  (setq pt-closeto (route-of-hole2shape:pt-closeto-curve pt-c ent-shape))
	(setq pt-closeto (route-of-hole2shape:pt-closeto-vertex pt-c ent-shape)))
    (if (= 0 closed?)
	(if (< (distance pt-c (car ent-pts))
	       (distance pt-c (last ent-pts)))
	    (setq pt-closeto (car ent-pts))
	  (progn
	    (setq pt-closeto (last ent-pts))
	    (setq ent-pts (reverse ent-pts)))
	  )))
			  
   ;; 最近点是否与顶点重合
  (setq i -3)
  (while (and
	  (> (distance (car ent-pts) pt-closeto) 0.00001)
	  (not (curve:pt-in-arc-p
		    pt-closeto
		    (car ent-pts)
		    (cadr ent-pts)
		    (car convexity)))
	  (<= i (length ent-pts)))
    (setq ent-pts (append (cdr ent-pts) (list (car ent-pts))))
    (setq convexity (append (cdr convexity) (list (car convexity))))
    (setq i (1+ i))
    )
  (setq ent-pts (append (cdr ent-pts) (list (car ent-pts))))
  (setq convexity (append (cdr convexity) (list (car convexity))))
  ;; 修正最后两点相同
  ;; (if (= (car (reverse ent-pts))
  ;; 	 (cadr (reverse ent-pts)))
  ;;     (setq ent-pts (reverse (cdr (reverse ent-pts)))))
  
  ;; 修正 凸度
  (if (equal (apply 'min (mapcar '(lambda(x)(distance x pt-closeto)) ent-pts))
	     0.0 0.0000001)
      (progn
	(princ "最近点为顶点\n")
	(if (= 1 closed?)
	    (progn
	      (if (equal 0.0 (distance pt-closeto (car ent-pts)) 0.0000001)
		  (progn 
		    (setq ent-pts (append (cdr ent-pts)(list (car ent-pts))))
		    (setq convexity (append (cdr convexity)(list (car convexity)))))
		    )
	      (setq new-convexity (append (list 0) convexity (list 0)))
	      (setq new-pts (append (list pt-c pt-closeto) ent-pts )))
	  (progn 
	    (setq new-convexity (append (list 0) convexity))
	    (setq new-pts (append (list pt-c pt-closeto) (reverse (cdr (reverse ent-pts))))))))
      (progn
	(setq new-convexity
	      (if (= 0 (last convexity))
		  (setq convexity (append (list 0 0) convexity (list 0)))
		(progn 
		  (setq O (curve:bulge2O (last ent-pts)(car ent-pts)(last convexity)))
		  (setq convexity
			(append (list 0)
				(list (* (if (> (last convexity)0) 1 -1)(curve:O2bulge pt-closeto
									   (car ent-pts)
									   O)))
				(reverse(cdr (reverse convexity)))
				(list  (* (if (> (last convexity)0) 1 -1) ;;(if clockwise 1 -1)
					  (curve:O2bulge pt-closeto
							 (last ent-pts)
							 O)))
				(list 0)))
		  )))
	(setq new-pts (append (list pt-c pt-closeto) ent-pts (list pt-closeto)))
	;;(format t "vertexs ~a | ~a" (length new-pts)(length ent-pts))

	))
  ;; (princ "\n")
  ;; (princ convexity)(princ "\n")
  ;; (princ ent-pts)(princ "\n")
  ;; (princ "\n")
  ;; (princ new-convexity)(princ "\n")
  ;; (princ new-pts)(princ "\n")
  ;;(curve:ptoncurve pt ;; 顺时针为 红，逆时针为
  (push-var)
  (setvar "osmode" 0)
  (entity:putdxf
   (entity:putdxf 
    (entity:make-lwpline-bold
     new-pts new-convexity nil closed? 0)
    62 222);; (if clockwise 1 2))
   8 (@:get-config 'route-of-hole2shape:layer))
  (entdel ent-shape)
  (pop-var)
  (princ)
  )

;; 曲线偏移
(defun route-of-hole2shape:offset-shape (ent / offset )
  (if (= 1 (entity:getdxf ent 70))
      (progn
	(if (curve:clockwisep ent)
	    (setq offset (- (@:get-config 'route-of-hole2shape:offset)))
	  (setq offset (@:get-config 'route-of-hole2shape:offset)))
	(o2e(car (vlax-safearray->list(vlax-variant-value (vla-Offset (e2o ent) offset))))))
    (o2e(vla-copy (e2o ent)))))
  

;; 点到曲线的最小距离点
(defun route-of-hole2shape:pt-closeto-curve (pt ent)
  (vlax-curve-getclosestpointto (e2o ent) pt))
;; 点到曲线的最小距离点
(defun route-of-hole2shape:pt-closeto-vertex (pt ent / pts )
  (setq pts (curve:pline-3dpoints ent))
  (car (vl-sort pts '(lambda (x y)
		       (< (distance pt x)(distance pt y))))))
;; 过程
;; 1. 根据 :offset 生成shape; 2. shape 内有 hole 的，连接 hole (最近点)
(defun route-of-hole2shape:auto (/ clockwise shapes hole-ent offset vertexs)
  (@:help (strcat "自动刀路:\n"
		  " 1. 选择刀路方向，顺时针 or 逆时针\n"
		  " 2. 设置偏移量\n"
		  " 3. 选择 shape "))
  (setq clockwise (ui:confirm "顺时针方向点确定，逆时针点取消."))
  (route-of-hole2shape:input-offset)
  (setq offset (@:get-config 'route-of-hole2shape:offset))
  (setq shapes (pickset:to-list (ssget '((0 . "LWPOLYLINE")(70 . 1)))))
  
  (foreach shape% shapes
	   (if (curve:pline-3dpoints shape%)
	       (progn
		 (setq hole-ent (car (pickset:to-list (ssget "WP" (curve:pline-3dpoints shape%) '((0 . "circle"))))))
		 (if hole-ent
		     (progn
		       (print "发现内部孔,准备生成刀路")
		       (route-of-hole2shape:route (entity:getdxf hole-ent 10) shape% clockwise offset T))
		   (progn
		     (if (curve:lwpl-is-circle-p shape%)
			 (progn
			   (setq vertexs (curve:pline-3dpoints shape%))
			   (route-of-hole2shape:route (curve:bulge2O (car vertexs)(cadr vertexs)(car (curve:pline-convexity shape%)))
						      shape% clockwise offset T))
		       (princ "没有发现内部孔，跳过。")))
		   )))))


(defun route-of-hole2shape:remove-route ()
  (@:help (strcat "框选要删除的刀路线。"))
  (mapcar 'entdel
	  (pickset:to-list
	   (ssget (list '(0 . "lwpolyline")
			(cons 8 (@:get-config 'ROUTE-OF-HOLE2SHAPE:layer))))))
  (princ))
	   
(defun route-of-hole2shape:c2pl (/ circles)
  (@:help (strcat "把圆转换为多段线。\n"))
  (prompt "请选择要进行转换的圆:")
  (setq circles (pickset:to-list(ssget '((0 . "circle")))))
  (if (< (@:get-config 'route-of-hole2shape:c2pl-vertex) 2)
      (@:set-config 'route-of-hole2shape:c2pl-vertex 4))
  (mapcar '(lambda (x) (curve:circle2lwpl x (fix (@:get-config 'route-of-hole2shape:c2pl-vertex))))  circles)
  (mapcar 'entdel circles)
  (princ)
  )
(defun route-of-hole2shape:show-clockwise ()
  (foreach x  (pickset:to-list
	       (ssget "x" (list '(0 . "lwpolyline")
				(cons 8 (@:get-config 'ROUTE-OF-HOLE2SHAPE:layer)))))
	   
	     ;;(if route-of-hole2shape:*show-clockwise*
	   (progn
	     
	     (if (curve:clockwisep x)
		 (entity:putdxf x 62 1)
	       (entity:putdxf x 62 2)))
	   ;;(setq route-of-hole2shape:*show-clockwise* nil)
		   
	       ;; (progn
	       ;; 	 (entity:putdxf x 62 256)
	       ;; 	 (setq route-of-hole2shape:*show-clockwise* T)
	       ;; 	 )
	       )
  (princ))
(defun ROUTE-OF-HOLE2SHAPE:bianbie ()
  (foreach ent% (pickset:to-list (ssget '((0 . "lwpolyline,circle"))))
	   (cond
	    ((= "CIRCLE" (entity:getdxf ent% 0))
	     (entity:putdxf ent% 62 1))
	    ((= "LWPOLYLINE" (entity:getdxf ent% 0))
	     (entity:putdxf ent% 62 2)))))
