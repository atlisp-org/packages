;;表格实体居中------
(defun at-lab:center-content (/ wt)
  (if (setq content (ssget '((0 . "*TEXT,CIRCLE,ARC,ELLIPSE,DIMENSION,LEADER,INSERT,ATTDEF,TCH_ARROW,TCH_TEXT,TCH_DRAWINGNAME,TCH_MULTILEADER,TCH_ELEVATION,SPLINE"))))
      (at-lab:center-ss-in-rectangle content)
    )
)
(defun at-lab:center-ss-in-rectangle (s1 / elis tp n d en p0 p1 p11 p13 pts pls ss nam obj)
  (setq elis (pickset:to-list s1)) ;选择集转为图元表
  (while (setq en (car elis))
    (setq tp (cdr (assoc 0 (entget en))))
    (cond 
     ((= tp "CIRCLE")
      (setq  pts
	     (curve:circle2pts en 32))
      )
     ((= tp "ELLIPSE")
      (earc->pline en);;转拟合多段线
      (setq pts (entity:getbox (entlast) 0))
      )
     (t
      (setq pts (entity:getbox en 0))
      (entdel en)
      (if (setq nam
		(bpoly (point:centroid pts)))  
          (progn 
            (setq pts (curve:get-points nam))
            (entdel nam)
            )
        )
      (entdel en)
      )
     )
    (if pts 
	(progn 
          (setq p0 (mapcar '(lambda (x y) (* (+ x y) 0.5)) (apply 'mapcar (cons 'min pts)) (apply 'mapcar (cons 'max pts))))
        (setq ss (ssget "WP" pts))
        (if (and ss (> (sslength ss) 0))
          (progn
            (setq n -1)
            (while (setq nam (ssname ss (setq n (1+ n)))) (if (not (ssmemb nam s1)) (ssdel nam ss)))
            (setq pls (pickset:getbox ss 0)
		  p1 (point:centroid pls))
	    (if (> (distance p1 p0) 0.01)
		(mapcar '(lambda(x)
			   (vla-move
			    (e2o x)
			    (point:to-ax p1)
			    (point:to-ax p0))
			   )
			   (pickset:to-list ss))
	      )
          
            (setq n -1)
            (while (setq nam (ssname ss (setq n (1+ n)))) 
              (if (member (cdr (assoc 0 (entget nam))) '("ELLIPSE" "CIRCLE"))
                  (setq elis (append elis (list nam)))
                (vl-remove nam elis)
              )
            )
          )
        )
      )
    )
    (setq elis (cdr elis))
  )
)
