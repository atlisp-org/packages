(defun split-dwg ()
  (setq frames
	(vl-remove-if-not
	 '(lambda(x)
	    (equal (vla-get-length (e2o x))
		   287000
		   10))
	 (pickset:to-list (ssget "x" '((0 . "lwpolyline")(8 . "C-SHET-SHET")(90 . 4)(70 . 1))))))
  ;; (sssetfirst nil(pickset:from-list frames))
  (setq i 0)
  (princ (strcat  "found " (itoa (length frames)) " frames"))
  (princ)
  (foreach
   frame frames
   (if (and frame
	    (entget frame))
       (progn
	 (setq box (entity:getbox frame 1))
	 (vla-zoomwindow *ACAD* (point:to-ax(car box))(point:to-ax (cadr box)))
	 (setq filename (strcat "D:/OUT/" (itoa (setq i (1+ i)))".dwg"))
	 (if (findfile filename)
	     (vl-file-delete filename))
	 (setq ssetObj (vla-Add (vla-get-SelectionSets *doc*) "SSET"))
	 (setq mode acSelectionSetWindow
               corner1 (point:to-ax (car box))
               corner2 (point:to-ax (cadr box))
	       )
	 
	 (vla-Select ssetObj mode corner1 corner2)
	 (if (> (vla-get-Count ssetObj) 3)
	     (progn
	       (vla-wblock *DOC* filename  ssetObj)
	       (vla-Clear ssetObj)
 	       (vla-Delete ssetObj)
	       ))
	 (entity:make-line (car  box)(cadr box))
	 ))))
(split-dwg)
