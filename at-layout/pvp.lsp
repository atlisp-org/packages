(@:add-menus
 '("布局"
   ("视口正交坐标"(@layout:pvp-v))
   ("视口世界坐标"(@layout:pvp-w))))
   
(defun @layout:pvp-v (/ clayout)
  (@:help "视口坐标随视图")
  (setq clayout (getvar "ctab"))
  (foreach layout (layout:list)
	   (setvar "ctab" layout)
	   (setq n (sslength (ssget"x"
				   (list '(0 . "viewport")
					 (cons 410 layout)
					 '(-4 . "<NOT")
					 '(69 . 1)
					 '(-4 . "NOT>")))))
	   (@:cmd "zoom" "a")
	   (setq i 1)(repeat n(@:cmd"mspace")(setvar "cvport" (setq i (1+ i)))(@:cmd"ucs""v")))
  (setvar"ctab" clayout)
  (@:cmd"pspace")
  )

(defun @layout:pvp-w (/ clayout)
  (@:help "视口坐标随视图")
  (setq clayout (getvar "ctab"))
  (foreach layout (layout:list)
	   (setvar "ctab" layout)
	   (setq n (sslength (ssget"x"
				   (list '(0 . "viewport")
					 (cons 410 layout)
					 '(-4 . "<NOT")
					 '(69 . 1)
					 '(-4 . "NOT>")))))
	   (@:cmd "zoom" "a")
	   (setq i 1)(repeat n(@:cmd"mspace")(setvar "cvport" (setq i (1+ i)))(@:cmd"ucs""w")))
  (setvar"ctab" clayout)
  (@:cmd"pspace")
  )

