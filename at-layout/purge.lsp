(defun @layout:purge ()
  (@:help "清理空布局。")
  (mapcar '(lambda(x)
	     (if (null
		  (ssget "x"
			 (list
			  (cons 410 (vla-get-name x))
			  '(-4 . "<NOT")
			  '(69 . 1)
			  '(-4 . "NOT>")
			  )))
		 (vla-delete x)))
	  (layout:vla-list)))
