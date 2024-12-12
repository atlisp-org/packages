(defun @lab:rec-tube (/ res w h t1 ang)
  (@::prompt "绘制矩形管")
  (while (setq res (ui:input "请输入参数"
			     '(("Width" 200 "矩形管宽度")
                               ("Height" 300 "矩形管高度")
                               ("t" 10 "矩形管厚度"))))
    (while (setq pt (getpoint "\n 请输入绘制位置:"))
      (setq w (* 0.5 (cdr (assoc "Width" res))))
      (setq h (* 0.5 (cdr (assoc "Height" res))))
      (setq t1  (cdr (assoc "t" res)))
      (entity:make-rectangle
       (polar (polar pt pi w)
              (* 1.5 pi) h)
     (polar (polar pt 0 w)
            (* 0.5 pi) h))
      
    (entity:make-rectangle
     (polar (polar pt pi (- w t1))
            (* 1.5 pi)  (-  h t1))
     (polar (polar pt 0 (- w t1))
            (* 0.5 pi) (-  h t1 )))))
  (princ))
