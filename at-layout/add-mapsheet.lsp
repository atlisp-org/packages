(defun @layout:add-mapsheet-a4 ()
  (@::prompt "视口加A4图框")
  (setq vps (pickset:to-list (ssget '((0 . "viewport")))))
  (mapcar
   '(lambda(x)
     (vla-update
      (e2o
       (block:insert
	"A4"
	(strcat (if (@::get-config '@pm:tuku)
		    (@::get-config '@pm:tuku)
		    "D:/Design/standard")
		"/")
      (polar (polar (entity:getdxf x 10) 0 (* 0.5 (@::scale 210)))
	     (* 1.5 pi) (* 0.5 (@::scale 297)))
      0 1))))
   vps)
  )
	 
(defun @layout:add-mapsheet-a3 ()
  (@::prompt "视口加A3图框")
  (setq vps (pickset:to-list (ssget '((0 . "viewport")))))
  (mapcar
   '(lambda(x)
     (vla-update
      (e2o
       (block:insert
	"A3"
	(strcat (if (@::get-config '@pm:tuku)
		    (@::get-config '@pm:tuku)
		    "D:/Design/standard")
		"/")
      (polar (polar (entity:getdxf x 10) 0 (* 0.5  (@::scale 420)))
	     (* 1.5 pi) (* 0.5  (@::scale 297)))
      0 1))))
   vps)
  )
	 
