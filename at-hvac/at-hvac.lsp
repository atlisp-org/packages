(@:add-menus
 '("@暖通"
   ("插入说明" (@hvac:draw-readme))
   ("平面图样例" (@hvac:draw-plan-example))
   ("标地暖管"(@hvac:dim-pipe))
   ("批量标地暖管"(@hvac:batch-dim-pipe))
   ("分集水器平衡分析"(@hvac:equip-balance))
   ("绘制设备表"(@hvac:make-equip-bom))
   ))
(defun @hvac:draw-readme ()
  (@:help '("插入暖通说明。" ))
  (@:load-module 'pkgman)
  (if @::require-down
      (@::require-down "at-hvac/readme-hvac.dwg"))
    
  (if (findfile (strcat @::*prefix* "packages/at-hvac/readme-hvac.dwg"))
      (progn
	(setq readme-hvac
	      (block:insert
	       "readme-hvac"
	       (strcat @::*prefix* "packages/at-hvac/")
	       '(0 0 0)
	       0 1))
	(if (ui:dyndraw readme-hvac '(0 0 0))
	    (if (string-equal "insert" (entity:getdxf readme-hvac 0))
		(progn
		  (vla-explode (e2o readme-hvac))
		  (vla-delete (e2o readme-hvac))))))
      ))
(defun @hvac:draw-plan-example ()
  (@:help '("插入暖通平面图样例。"
	    ))
  (@:load-module 'pkgman)
  (if @::require-down
      (@::require-down "at-hvac/example-hvac.dwg"))

  (if (findfile (strcat @::*prefix* "packages/at-hvac/example-hvac.dwg"))
      (progn
	(setq example-plan-hvac
	      (block:insert
	       "example-plan-hvac"
	       (strcat @::*prefix* "packages/at-hvac/")
	       '(0 0 0)
	       0 1))
	(ui:dyndraw readme-hvac '(0 0 0))
	)))
