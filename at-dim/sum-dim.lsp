(defun @dim:sum-dim(/ total dims)
  "选择要求和的尺寸"
  (setq dims (pickset:to-list(ssget '(( 0 . "dimension")))))
  ;; Measurement

  (setq total (apply
	 '+
	 (mapcar (function (lambda(x)
			   (vla-get-measurement (e2o x))))
		 dims)))
  (princ (strcat "所选尺寸值的和:"
		 (rtos total 2 3)))
  (princ))
  
