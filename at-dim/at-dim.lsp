;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'at-dim:first for package at-dim 's configitem first 
(@:define-config 'at-dim:first "I'm th default value for at-dim:first" "This Item 's Explain.")
(@:define-config 'at-dim:scale 100 "标注比例")
;; (@:get-config 'at-dim:first) 
;; (@:set-config 'at-dim:first  "New Value")
;; Add menu in @lisp panel
(@:add-menu "标注" "选线标斜率" "(at-dim:menu-dim-slope)" )
(defun at-dim:hello ()
  (@:help (strcat "The content can show in user interface .\n"
  	  		  ))
  (alert (strcat "标注 's first function.\n"
		 "Created a config item at-dim:first .\n"
		 "THe config ietm th family is this item: " (@:get-config 'at-dim:first)
		 ))
  (princ)
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
  (setq scale1 (@:get-config 'at-dim:scale))
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
(@:add-menu "标注" "设置比例" "(at-dim:set-scale)" )
(defun at-dim:set-scale (/ scale1 )
  (setq scale1  (getint (strcat "请输入标注比例 <" (itoa (@:get-config 'at-dim:scale)) ">: ")))
  (if scale1
      (progn
	(@:set-config 'at-dim:scale scale1)))
  )

;; (defun @dim:dim-box (/ pts box ent-lw)
;;   "包围盒周边标注"
;;   (setq pts (line:get-lwpoints (setq ent-lw (car (entsel "请选择多段线:")))))
;;   (setq box (entity:getbox ent-lw))

;;   ;; x 向
;;   (setq pts-x pts)
;;   (while (> (length pts-x) 1)
;;     (
  

