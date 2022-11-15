(@:define-config '@curve:optimize-angle 0.0001 "可优化多段线的夹角差。")
(defun at-curve:optimize-lwpl (/ optimize-lwpl)
  (@:help '("优化多段线顶点。当连续多点共线或共圆时，减少顶点。"))
  (setq lwpls (pickset:to-list(ssget '((0 . "lwpolyline")))))
  (mapcar 'curve:optimize-lwpl lwpls))

