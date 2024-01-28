(if (= "AutoCAD" @:cad-platform)
    (progn
      (@:load "base/condition")
      (@:load "base/format")))


(defun @:check-consistency (contents order / ti% tmplist)
  "检查某键值的唯一性。返回值为整数。"
  (setq tmplist '())
  (foreach ti% contents 
	   (if (= nil (member (cdr (assoc order ti%)) tmplist))
	       (setq tmplist (append tmplist (list (cdr (assoc order ti%)))))))
  (length tmplist)
  )

(setq @:get-eval-code @:get-exec-permit)
(setq @:run-from-web @:load-remote)
