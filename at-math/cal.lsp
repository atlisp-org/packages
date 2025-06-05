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

