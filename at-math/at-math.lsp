(defun at-math:help()
  (princ))
 
(if (and (= "TEXT" (dxf s1 0))
      (= 1 (dxf s1 62)))
  (setq w1 (ssadd s1 w1))			
)
