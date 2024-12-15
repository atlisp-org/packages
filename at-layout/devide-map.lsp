(defun @layout:devide-map ()
  (@::help "分图到布局")
  (setq ents (pickset:to-list (ssget '((0 . "lwpolyline")))))
  (setq pt-base '(0 0 0))
  (mapcar
   (function(lambda(x / box)
	      (setq box (entity:getbox x 10))
	      (layout:make-viewport
	       "abc"
	       pt-base
	       (- (caadr box)(caar box))
	       (- (cadadr box)(cadar box))
	       (point:centroid box))
	      (setq pt-base
		    (polar pt-base
			   0
			   (* 1.5 (- (caadr box)(caar box)))))))
   ents))
			   
