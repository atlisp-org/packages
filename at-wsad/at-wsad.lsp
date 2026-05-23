(@:add-menus
 '("@给排水"
   ("给排水说明" (@wsad:draw-readme))
   ("给排水图例" (@wsad:draw-legend-example))
   ("给排水平面样例" (@wsad:draw-plan-example))
   ("给排水系统样例" (@wsad:draw-system-example))
   ("给排水详图样例" (@wsad:draw-detail-example))
   ))
(defun @wsad:draw-readme ()
  (@:help '("插入给排水说明。" ))
  (if (findfile (strcat @::*prefix* "packages/at-wsad/readme-wsad.dwg"))
      (progn
	(setq readme-wsad
	      (block:insert
	       "readme-wsad"
	       (strcat @::*prefix* "packages/at-wsad/")
	       (getpoint "请点击插入位置:")
	       0 1))
	(if (string-equal "insert" (entity:getdxf readme-wsad 0))
	    (progn
	      (vla-explode (e2o readme-wsad))
	      (vla-delete (e2o readme-wsad))))))
	
  )
(defun @wsad:draw-plan-example ()
  (@:help '("插入给排水平面图样例。"
	    ))
  (if (findfile (strcat @::*prefix* "packages/at-wsad/example-wsad-plan.dwg"))
      (progn
	(setq example-plan-wsad
	      (block:insert
	       "example-wsad-plan"
	       (strcat @::*prefix* "packages/at-wsad/")
	       (getpoint "请点击插入位置:")
	       0 1))
	)))
(defun @wsad:draw-system-example ()
  (@:help '("插入给排水系统图样例。"
	    ))
  (if (findfile (strcat @::*prefix* "packages/at-wsad/example-wsad-system.dwg"))
      (progn
	(block:insert
	 "example-wsad-system"
	 (strcat @::*prefix* "packages/at-wsad/")
	 (getpoint "请点击插入位置:")
	 0 1))
      ))
(defun @wsad:draw-legend-example ()
  (@:help '("插入给排水图例样例。"
	    ))
  (if (findfile (strcat @::*prefix* "packages/at-wsad/example-wsad-legend.dwg"))
      (progn
	(block:insert
	 "example-wsad-legend"
	 (strcat @::*prefix* "packages/at-wsad/")
	 (getpoint "请点击插入位置:")
	 0 1))
      ))
(defun @wsad:draw-detail-example ()
  (@:help '("插入给排水图例样例。"
	    ))
  (if (findfile (strcat @::*prefix* "packages/at-wsad/example-wsad-detail.dwg"))
      (progn
	(block:insert
	 "example-wsad-detail"
	 (strcat @::*prefix* "packages/at-wsad/")
	 (getpoint "请点击插入位置:")
	 0 1))
      ))
