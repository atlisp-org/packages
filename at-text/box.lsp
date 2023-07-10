(@:define-config '@text:box-type 1 "有效值: 1 矩形，2 圆 ")
(@:define-config '@text:box-linewidth 0.5 "框线宽度")
(@:define-config '@text:box-offset 1.0 "框线偏移值")
(@:define-config '@text:box-color  1 "框线颜色号")
(@:define-config '@text:box-layer  "textbox" "框线图层")

(defun @text:draw-box (ent / box newent)
  ;; 文本加框
  (setq box (entity:getbox ent (@:get-config '@text:box-offset)))
  (cond
   ((= (@:get-config '@text:box-type) 1)
    (setq newent
	  (apply 'entity:make-rectangle
		 box)))
   ((= (@:get-config '@text:box-type) 2)
    (setq newent
	  (entity:make-circle
	   (apply 'point:mid box)
	   (* 0.5 (apply 'distance box)))))
   (t (setq newent (apply 'entity:make-rectangle
			  box))))
  (if newent
      (progn
	(if (= "LWPOLYLINE"(entity:getdxf newent 0))
	    (setq newent (entity:putdxf newent 43 (@:get-config '@text:box-linewidth))))
	(setq newent (entity:putdxf newent 62 (@:get-config '@text:box-color)))
	(if (/= "" (@:get-config '@text:box-layer))
	    ;; 创建图层
	    (entity:putdxf newent 8 (@:get-config '@text:box-layer)))
	)
    )
  )
(defun @text:menu-draw-box (/ s1)
  (@:help "选择或输入文本，选择查找范围，查找内容相同的文本并加框。")
  (if (null layer:make)(require 'layer:*))
  (if (= 'subr (type layer:make))
      (layer:make (@:get-config '@text:box-layer) 1 nil nil))
  ;; (setq en1 (car (entsel "请选择一个单行文本:")))
  ;; (setq pt-base (cdr (assoc 10 (entget en1))))
  (setq txt (ui:getstring "输入要匹配的文字或点选文字:"))
  ;; (sleep 0.1)
  (if (and pt-base txt)
      (progn
	(@:prompt "\n请选择需要查找的区域:")
	(setq s1 (ssget (list '(0 . "text") (cons 1 txt))))
	(mapcar
	 '@text:draw-box
	 (pickset:to-list s1))
	)))
(defun @text:menu-remove-box ()
  (@:help '("删除文本框"))
  (setq boxs (pickset:to-list
	      (ssget "x" (list '(0 . "lwpolyline,circle")
			       (cons 8 (@:get-config '@text:box-layer))))))
  (mapcar 'entdel boxs))
