;;悬空线检查
(defun at-curve:noclosed-endpt (/ expert i pts-end n n100 nn pp pt1 pt2 pts ss-curves sslast)
  (if(null (tblsearch "layer" "@temp"))
     (progn
       (layer:make "@temp" nil nil nil)
       (layer:plotable "@temp" nil)))
  (setq ss-curves(ssget'((0 . "arc,*line,ELLIPSE")(-4 . "<not")(-4 . "&")(70 . 1)(-4 . "not>"))))
  (setq pts-end
	(apply 'append
	       (mapcar '(lambda(x)(list
				   (vlax-curve-getStartPoint x)
				   (vlax-curve-getEndPoint x)))
		       (pickset:to-vlalist ss-curves))))
  ;; 捕捉影响，需要缩放
  (foreach pt pts-end 
	   (setq pp pts-end
		 n100 (if (> n 0)(rem (setq i(1+ i)) n)1))
	   (vla-ZoomWindow *ACAD*
			   (point:to-ax (mapcar '- pt '(0.001 0.001)))
			   (point:to-ax (mapcar '+ pt '(0.001 0.001))))
	   (setq curves (ssget "C" pt pt))
	   (if (= 1 (sslength curves))(setq pts(cons pt pts)))
           )
  (setq flags (entity:make-circle pts (* 0.05  (cadr (getvar "screensize")))))
  (mapcar '(lambda(x)
	    (entity:putdxf x 62 240)
	    (entity:putdxf x 8 "@temp")
	    )
	  flags)
  (pickset:zoom (pickset:from-list flags))
  (sssetfirst nil (pickset:from-list flags))
  (princ (strcat "\n找到孤点" (itoa (length pts))"个。"))
  (princ)
  )
(defun at-curve:rm-flagpts ()
  (mapcar 'entdel (pickset:to-list(ssget '((0 . "circle")(8 . "@temp")))))
  )
