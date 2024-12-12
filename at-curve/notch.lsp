;; 直线开缺口

;; 选初始直线L，直线端点的垂直线 L2.
;; L 端点退至pt-l , L2 拆分为两个，pta-l2~pt-a, pt-b~ptb-l2.
;; 绘制 pt-l~pt-a,pt-l~pt-b
;; TODO: 颜色 图层 线型，宽度 同原线
(@:define-config '@curve:notch-type 0 "生成缺口的默认类型，0 楔形，1 圆弧")
(@:define-config '@curve:notch-width 80 "生成缺口的默认宽度")
(@:define-config '@curve:notch-height 50 "生成缺口的默认深度")


(defun at-curve:notch (/ args)
  (@::prompt '("在丁字线交点处生成缺口。"))
  (if (setq args (ui:input
		  "缺口参数"
		  (list
		   (list "缺口宽度" (@:get-config '@curve:notch-width))
		   (list "缺口深度" (@:get-config '@curve:notch-height)))))
      (progn
	(setq width (cdr (assoc "缺口宽度" args)))
	(setq height (cdr (assoc "缺口深度" args)))
	(@:set-config '@curve:notch-width width)
	(@:set-config '@curve:notch-height height)
	(setq lst-line (pickset:to-list (ssget '((0 . "line")))))
	(foreach
	 line% lst-line
	 (setq pts (curve:get-points line%))
	 ;; pt-a
	 (setq pt-a (car pts))
	 (setq pt-b (cadr pts))
	 (if (> (sslength (setq ss (ssget "c" pt-a pt-a '((0 . "line"))))) 1)
	     (progn
	       (setq ss (ssdel line% ss))
	       (foreach l2 (pickset:to-list ss)
			(setq ang(abs (- (angle (entity:getdxf l2 10)
						(entity:getdxf l2 11))
					 (angle (entity:getdxf line% 10)
						(entity:getdxf line% 11)))))
			(while (> ang pi)(setq ang (- ang pi)))
			
			(if (equal ang (* 0.5 pi) 0.07)
			    (progn
			      (entity:putdxf line% 10
					     (polar pt-a
						    (angle pt-a pt-b)
						    height))
			      (if (> (distance (entity:getdxf l2 10) pt-a)
				     (* 0.5 width))
				  (progn
				    (entity:make-line
				     (entity:getdxf l2 10)
				     (polar pt-a
					    (angle pt-a (entity:getdxf l2 10))
					    (* 0.5 width)))
				    (entity:make-line
				     (polar pt-a
					    (angle pt-a (entity:getdxf l2 10))
					    (* 0.5 width))
				     (polar pt-a
					    (angle pt-a pt-b)
					    height))
				    ))
			      (if (> (distance (entity:getdxf l2 11) pt-a)
				     (* 0.5 width))
				  (progn
				    (entity:make-line
				     (entity:getdxf l2 11)
				     (polar pt-a
					    (angle pt-a (entity:getdxf l2 11))
					    (* 0.5 width)))
				    (entity:make-line
				     (polar pt-a
					    (angle pt-a (entity:getdxf l2 11))
					    (* 0.5 width))
				     (polar pt-a
					    (angle pt-a pt-b)
					    height))))
			      (entdel l2)
			      )))))
	 ;; pt-b
	 (if (> (sslength (setq ss (ssget "c" pt-b pt-b '((0 . "line"))))) 1)
	     (progn
	       (setq ss (ssdel line% ss))
	       (foreach l2 (pickset:to-list ss)
			(setq ang (abs (- (angle (entity:getdxf l2 10)
						(entity:getdxf l2 11))
					 (angle (entity:getdxf line% 10)
						(entity:getdxf line% 11)))))
			(while (> ang  pi)(setq ang (- ang pi)))
			
			(if (equal ang (* 0.5 pi) 0.07)
			    (progn
			      (if (> (distance (entity:getdxf l2 10) pt-b)
				     (* 0.5 width))
				  (progn
				    (entity:putdxf line% 11
						   (polar pt-b
							  (angle pt-b pt-a)
							  height))
				    (entity:make-line
				     (entity:getdxf l2 10)
				     (polar pt-b
					    (angle pt-b (entity:getdxf l2 10))
					    (* 0.5 width)))
				    (entity:make-line
				     (polar pt-b
					    (angle pt-b (entity:getdxf l2 10))
					    (* 0.5 width))
				     (polar pt-b
					    (angle pt-b pt-a)
					    height))
				    ))
			      (if (> (distance (entity:getdxf l2 11) pt-b)
				     (* 0.5 width))
				  (progn
				    (entity:make-line
				     (entity:getdxf l2 11)
				     (polar pt-b
					    (angle pt-b (entity:getdxf l2 11))
					    (* 0.5 width)))
				    (entity:make-line
				     (polar pt-b
					    (angle pt-b (entity:getdxf l2 11))
					    (* 0.5 width))
				     (polar pt-b
					    (angle pt-b pt-a)
					    height))))
			      (entdel l2)
			      )))))
	 
	 )))
  )


