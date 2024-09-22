(@:add-menu "阿甘工具" "阿甘计算器" '(c:ag))
(defun agan-cal:init(/ fp)
  (if (not (findfile (strcat @::*prefix* "packages/agan-cal/历史记录.txt")))
      (progn
	(setq fp (open (strcat @::*prefix* "packages/agan-cal/历史记录.txt")"w"))
	(close fp))))
(agan-cal:init)
