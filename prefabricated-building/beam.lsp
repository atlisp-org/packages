(defun prefabricated-building:stat-beam ()
  "统计梁长"
  (setq beams (pickset:to-list(ssget '((0 . "LWPOLYLINE")(8 . "GJ")))))

  (setq statdata (stat:stat
		  (mapcar '(lambda(x)
			    (* 10
			     (round
			      (/ 
			       (+ 160 (curve:length x))
			       10))))
			  beams)))

  (setq table-data
	(mapcar '(lambda(x)
		  (list
		   (car x)
		   (cdr x)
		   (- (car x) 320)
		   (cond
		     ((< (car x) 3000)
		      "2D14")
		     ((< 3000 (car x) 3900)
		      "2D18")
		     ((< 4000 (car x) 4900)
		      "2D20")
		     ((< 5000 (car x) 5900)
		      "4D20 2/2"))
		   ""
		   ))
		statdata))
  (setq ent-tbl
	(table:make '(0 0 0)
		    "梁统计表"
		    '("梁长La""个数""净跨L" "底筋""备注")
		    table-data))
  (ui:dyndraw ent-tbl '(0 0 0)))
  
(defun prefabricated-building:dim-beam ()
  "注梁净跨"
  (setq beams (pickset:to-list(ssget '((0 . "LWPOLYLINE")(8 . "GJ")))))
  (mapcar '(lambda(x)
	    (setq pts (curve:get-points x))
	    (setq ang
	     (if (or (equal (angle (car pts)(last pts)) 0 0.1)
		     (equal (angle (car pts)(last pts)) pi 0.1)
		     )
		 0
		 (* 0.5 pi)))
	    (entity:putdxf 
	    (entity:make-text
	     (itoa (fix (* 10
			   (round
			    (/ 
			     (- (curve:length x) 160)
			     10)))))
	     (polar (point:mid (car pts)(last pts))
	      (+ (* 0.5 pi)ang)
	      100)
	     250
	     ang
	     0.8 0 "MB")
	     62 1))
	  beams))


  
