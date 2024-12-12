(defun @block:minsert2insert ()
  (@::prompt "将多重插入块改为普通块")
  (setq mblkref (pickset:to-list (ssget '((0 . "insert")(44 . 0.0)(45 . 0.0)(-4 . ">")(70 . 0)(-4 . ">")(71 . 0)))))
  (mapcar
   '(lambda(x)
     (block:insert
      (entity:getdxf x 2)
      ""
      (entity:getdxf x 10)
      (entity:getdxf x 50)
      (entity:getdxf x 41))
     (entdel x))
   mblkref)
  (princ))
