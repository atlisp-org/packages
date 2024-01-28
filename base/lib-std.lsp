;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @ base -- @lisp 基础函数库
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 基本库 lib-std.lsp
;;; 基本常用函数。

;;; 文件操作函数

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
