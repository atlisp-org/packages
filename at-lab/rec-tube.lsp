(defun @lab:rec-tube (/ res w h t1 ang)
  (@::prompt "���ƾ��ι�")
  (while (setq res (ui:input "���������"
			     '(("Width" 200 "���ιܿ��")
                               ("Height" 300 "���ιܸ߶�")
                               ("t" 10 "���ιܺ��"))))
    (while (setq pt (getpoint "\n ���������λ��:"))
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
