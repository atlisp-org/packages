(@:add-menus
 '("@电气"
   ("电气说明" (@elec:draw-readme))
   ("电气样例" (@elec:draw-plan-example))
   ))
(defun @elec:draw-readme ()
  (@:help '("插入暖通说明。" ))
  (if (findfile (strcat @::*prefix* "packages/at-elec/readme-elec.dwg"))
      (progn
	(setq readme-elec
	      (block:insert
	       "readme-elec"
	       (strcat @::*prefix* "packages/at-elec/")
	       (getpoint "请点击插入位置:")
	       0 1))
	(if (string-equal "insert" (entity:getdxf readme-elec 0))
	    (progn
	      (vla-explode (e2o readme-elec))
	      (vla-delete (e2o readme-elec))))))
	
  )
(defun @elec:draw-plan-example ()
  (@:help '("插入暖通平面图样例。"
	    ))
  (if (findfile (strcat @::*prefix* "packages/at-elec/example-elec.dwg"))
      (progn
	(setq example-plan-elec
	      (block:insert
	       "example-plan-elec"
	       (strcat @::*prefix* "packages/at-elec/")
	       (getpoint "请点击插入位置:")
	       0 1))
	)))
