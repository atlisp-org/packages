(defun @m:cal (str / arxs)
  (setq arxs (arx))
  ;; 替换大括号
  (setq lq (mapcar '(lambda(x)(string:l2s-ansi (list x))) (string:s2l-ansi "（｛{［["))
	rq (mapcar '(lambda(x)(string:l2s-ansi (list x))) (string:s2l-ansi "）｝］}]")))
  (mapcar '(lambda(x)
	    (setq str
	     (string:subst-all "(" x str)))
	  lq)
  (mapcar '(lambda(x)
	    (setq str
	     (string:subst-all ")" x str)))
	    rq)
  (if (or (member "geomcal.arx" arxs)
	  (member "geomcal.crx" arxs)
	  (/= "F" (arxload "geomcal" "F")))
      (cal str)))

      
(defun @m:cal-text ()
  (@::prompt "选择一个公式文字，并进行计算。")
  (setq @m:*result*
	(@m:cal
	 (text:remove-fmt
	  (text:get-mtext
	   (car
	    (pickset:to-list(ssget ":S:E" '((0 . "*text"))))))))))

(defun @m:cal-text-post ()
  (@::prompt '("选择公式文字（单行文本），并进行计算。并将=结果附后"
	       "当公式没有运算符时，为变量符号赋值。即将=后面的值赋给变量"
	       ))
  ;;取初值
  (setq  formula-txt (pickset:to-list(ssget '((0 . "*text")))))
  (mapcar '(lambda(x / formula res)
	     (setq formula (string:to-list (text:remove-fmt (text:get-mtext x)) "="))
	     (setq expr (formula->lisp (car formula)))
	     (if (and (not (listp expr))
		      (cadr formula))
		 (set expr (read (cadr formula)))))
	  formula-txt)
  (mapcar '(lambda(x / formula res)
	     (setq formula (string:to-list (text:remove-fmt (text:get-mtext x)) "="))
	     (setq expr (formula->lisp (car formula)))
	     (if (listp expr)
		 (entity:putdxf x
				1
				(strcat (car formula)
					"="
					(@::to-string
					 (@m:cal (car formula)))))))
	  formula-txt))

