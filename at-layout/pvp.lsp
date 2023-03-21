(@:add-menus
 '("布局"
   ("视口正交坐标"(@layout:pvp-v))
   ("视口世界坐标"(@layout:pvp-w))
   ("视口冻结刷"(@layout:pvp-ma)))
 )
   
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
(defun @layout:pvp-ma ()
  (@:help "将选中的视口的冻结图层刷到其它视口")
  (@:prompt "请选择源视口:")
  (if (setq src-layout (ssget "_:S:E" '((0 . "VIEWPORT"))))
      (progn
	(setq obj-vflayers
	      (vla-getxdata (e2o (ssname src-layout 0))
			    "" 'xtypeOut 'xdataOut))
	(@:prompt "请选择目标视口")
	
	(if (setq dist-layouts (ssget '((0 . "VIEWPORT"))))
	    ;;清除原来的冻结状态
	    (progn
	      (@:cmd "vplayer" "t" "*" "S" dist-layouts "" "")
	      (foreach dst (pickset:to-list dist-layouts)
		       
		       (vla-setXdata (e2o dst)
				     xtypeOut xdataOut)
		       (vla-display (e2o dst) :vlax-false)
		       (vla-display (e2o dst) :vlax-true)
		       ;;(vla-syncmodelview (e2o dst))
		       (vla-update (e2o dst))
		       ))))))

