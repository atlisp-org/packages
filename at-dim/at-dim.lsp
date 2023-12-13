;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'at-dim:first for package at-dim 's configitem first 

(@:define-config '@dim:layer "dim-coor" "标注图层，为空时为当前图层")
(@:define-config '@dim:arrow "" "箭头样式")
(@:define-config '@dim:units 3 "标注精度，小数位")
(@:define-config '@dim:text-units "m" "标注单位，m米，mm毫米")
(@:define-config '@dim:draw-units "mm" "绘图单位，m米，mm毫米")
(@:define-config '@dim:scale 100 "标注比例")
(@:define-config '@dim:textheight 2.5 "标注文字高度")
(@:define-config '@dim:coordinate-position 0 "坐标标注位置，0 自动，1 手动")
(@:define-config '@dim:prefix "XY" "标注方式，可选 XY,AB,NE")
(@:define-config '@dim:elevation 1 "标注标高，0不标注标高，1标注标高")
(@:define-config '@dim:switch-xy 0 "XY坐标互换。0 不交换，1 交换")
;; (@:get-config 'at-dim:first) 
;; (@:set-config 'at-dim:first  "New Value")
;; Add menu in @lisp panel
(@:add-menu "标注" "标注设置" "(@dim:setup)" )
(@:add-menu "标注" "选线标斜率" "(at-dim:menu-dim-slope)" )
(@:add-menu "标注" "坐标标注" "(at-dim:menu-zbbz)" )
(@:add-menu "标注" "设置比例" "(at-dim:set-scale)" )

(defun @dim:setup (/ res)
  (setq res 
	(ui:input "配置信息"
		  (mapcar '(lambda (x) (list (strcase (vl-symbol-name (car x)) T)(cadr x)(cddr x)))
			  (vl-remove-if '(lambda (x) (not (wcmatch (vl-symbol-name (car x)) "`@DIM:*")))
					(if @:*config.db*
					    @:*config.db* (@:load-config))))))
  (foreach res% res
   	   (@:set-config (read (car res%)) (cdr res%)))
  )
(defun at-dim:set-env (/ obj-st obj-dimst ent-dimst)
  (if (null (tblsearch "layer" "坡度标注"))
      (layer:make "坡度标注" 1 nil nil))
  (setvar "clayer"  "坡度标注" )
  (if (null  (tblsearch "style" "坡度标注"))
      (progn
	(setq obj-st (vla-add *STS* "坡度标注"))
	(vla-put-fontfile obj-st "complex")))
  ;;(setvar "textstyle"  "断面标注" )
  (if (null  (tblsearch "dimstyle" "坡度标注"))
      (entmakex '((0 . "DIMSTYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbDimStyleTableRecord") (2 . "坡度标注") (70 . 0) (3 . "") (41 . 5.0) (42 . 0.625) (43 . 3.75) (44 . 1.25) (73 . 0) (74 . 0) (77 . 1) (78 . 8) (140 . 5.0) (141 . 2.5) (143 . 0.0393701) (147 . 0.625) (171 . 3) (172 . 1) (271 . 0) (272 . 0) (274 . 3) (278 . 44) (283 . 0) (284 . 8))))
  ;;(setvar "cdimstyle"  "断面标注" )
  ;; (setq ent-dimst (std:vla->e (setq obj-dimst (vla-add *DIMS* "断面标注"))))
  ;; (entity:putdxf ent-dimst 3 "m")
  
  )
(defun at-dim:menu-dim-slope ()
  (at-dim:set-env)
  (prompt "请选择要标注斜率的直线或多段线:")
  (setq ss1 (pickset:to-list (ssget '((0 . "*line")))))
  (foreach ent% ss1
	   (at-dim:dim-slope ent%)))

(defun at-dim:dim-slope (ent1 / pts pt-mid len1 ent-text angle-arrow  angle-text scale1)
  ;; (setq ent1 (car (entsel "请选择一个多段线：")))
  (setq scale1 (@:get-config '@dim:scale))
  (setq pts (curve:pline-2dpoints ent1))
  (setq i% 0)
  (while (< i% (1- (length pts) ))
    (setq angle-text (angle (nth i% pts) (nth (1+ i%) pts)))
    (cond
      ((and (> angle-text  (* 0.5 pi)) (< angle-text pi))
       (setq angle-text (+ angle-text pi)))
      ((and (>= angle-text  pi) (< angle-text (* 1.5 pi)))
       (setq angle-text (- angle-text pi))))

    (setq ent-text
	  (entity:make-text (strcat "1:" (rtos (abs (/ (- (car (nth i% pts))
							  (car (nth (1+ i%) pts)))
						       (- (cadr (nth i% pts))
							  (cadr (nth (1+ i%) pts)))))
					       2 2))
			    (setq pt-mid (polar  (nth i% pts) (angle (nth i% pts) (nth (1+ i%) pts))
						 (* 0.5 (distance (nth i% pts) (nth (1+ i%) pts)))))
			    (* scale1 2.5)
			    angle-text
			    0.8 0 23))
    (setq angle-arrow (angle (nth i% pts) (nth (1+ i%) pts)))
    (if (< angle-arrow pi) (setq angle-arrow (+ pi angle-arrow)))
    (entity:putdxf (entity:make-leader (polar pt-mid  angle-arrow
					      (* 0.2 (distance (nth i% pts) (nth (1+ i%) pts))))
				       (polar pt-mid  (- angle-arrow pi)
					      (* 0.2  (distance (nth i% pts) (nth (1+ i%) pts))))
				       )
		   62 1)
    (setq i% (1+ i%))
    )
  )
(defun at-dim:set-scale (/ scale1 )
  (setq scale1  (getint (strcat "请输入标注比例 <" (itoa (@:get-config '@dim:scale)) ">: ")))
  (if scale1
      (progn
	(@:set-config '@dim:scale scale1)))
  )
(defun at-dim:menu-zbbz (/ pt-b  pt-c  txtx txty txth line th unit1)
  (@:help "标注选定点的坐标及标高")
  (if (and (/= "" (@:get-config '@dim:layer))
	   (null(tblsearch "layer"  (@:get-config '@dim:layer))))
      (layer:make  (@:get-config '@dim:layer) 2 nil nil))
      
  (setq pt-b (getpoint "选定坐标点:"))
  (setq th (* (@:get-config '@dim:textheight)(@:get-config '@dim:scale)))
  (cond
    ((= (@:get-config '@dim:draw-units)(@:get-config '@dim:text-units))
     (setq unit1 1.0))
    ((and (= (@:get-config '@dim:draw-units) "mm")
	  (= (@:get-config '@dim:text-units) "m"))
     (setq unit1 0.001))
    ((and (= (@:get-config '@dim:draw-units) "m")
	  (= (@:get-config '@dim:text-units) "mm"))
     (setq unit1 1000))
    (t (setq unit1 1.0))
    )

    
  (setq ss (ssget pt-b))
  ;;relative position
  (setq ang-rel (angle (point:centroid (pickset:getbox ss 0)) pt-b))
  ;;绘制
  (if(= 0 (@:get-config '@dim:coordinate-position))
     (cond
       ((< ang-rel (* 0.5 pi))
	(setq txt-ang 0)
	(setq pt-c (polar pt-b (* 0.25 pi) (* th 4))))
       ((< (* 0.5 pi) ang-rel pi)
	(setq txt-ang pi)
	(setq pt-c (polar pt-b (* 0.75 pi) (* th 4))))
       ((< pi ang-rel (* 1.5 pi))
	(setq txt-ang pi)	
	(setq pt-c (polar pt-b (* 1.25 pi) (* th 4))))
       ((< (* 1.5 pi) ang-rel (* 2 pi))
	(setq txt-ang 0)
	(setq pt-c (polar pt-b (* 1.75 pi) (* th 4)))))
     (progn
       (setq pt-c (getpoint pt-b "文字位置:"))
       (setq txt-ang (getangle pt-c "文字方向:"))
       ))
  (setq txt-ang1 (if (> txt-ang (* 0.5 pi))
		     (- txt-ang pi)
		     txt-ang))
  (if (< txt-ang  (* 0.5 pi))
      (setq  txtdq "LB")
      (setq txtdq "RB"))
  (if(> (strlen (@:get-config '@dim:prefix))1)
     (progn
       (setq prefix-x (substr (@:get-config '@dim:prefix) 1 1))
       (setq prefix-y (substr (@:get-config '@dim:prefix) 2 1)))
     (progn
       (setq prefix-x "X")
       (setq prefix-y "Y"))
     )
  (setq txtx
	(entity:make-text (strcat prefix-x "=" (rtos(* unit1 (car pt-b)) 2 (@:get-config '@dim:units))) pt-c
			  th
			  txt-ang1 0.8 0 txtdq))
  (setq txty
	(entity:make-text (strcat prefix-y "=" (rtos (* unit1(cadr pt-b)) 2 (@:get-config '@dim:units)))
			  (polar pt-c  (+ txt-ang1 (* 1.5 pi)) (* 1.2 th))
			  th
			  txt-ang1 0.8 0 txtdq))
  (setq ents (list txtx txty))
  (if (= 1 (@:get-config '@dim:elevation))
      (progn
	(setq txth
	      (entity:make-text (strcat "H=" (rtos (* unit1(caddr pt-b)) 2 (@:get-config '@dim:units))) (polar pt-c (+ txt-ang1 (* 1.5 pi)) (* 2.4 th)) th txt-ang1 0.8 0 txtdq))
	(setq ents (cons txth ents))))
  (apply 'max
	 (mapcar '(lambda(x / box)
		   (setq box (text:box x))
		   (distance (car box)(cadr box)))
		 ents))
  (setq line
	(entity:make-lwpolyline
	 (list pt-b
	       pt-c
	       (polar pt-c txt-ang (apply 'max
					  (mapcar '(lambda(x / box)
						    (setq box (text:box x))
						    (distance (car box)(cadr box)))
						  (list txtx txty txth)))))
	 nil 0 0 0))
  (entity:putdxf line 38 (caddr pt-b))
  (setq ents (cons line ents))
  (if (/= "" (@:get-config '@dim:layer))
      (mapcar '(lambda (x)
		(entity:putdxf x 8 (@:get-config '@dim:layer)))
	      ents))
       
  ;; 编组
  (group:make ents (strcat "XY" (@:timestamp)))
  )

	 
