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
(@:define-config '@dim:coordinate-type  0 "坐标标注图元类型，0 多重引线，1 文本和多段线")
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
(@:add-menu "标注" "标等分弧长" "(at-dim:menu-dimarc-div)" )

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
