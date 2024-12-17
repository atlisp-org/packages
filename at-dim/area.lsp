(defun at-dim:area ()
  (@::help "标注区域的面积值")
  (setq lwpls (pickset:to-list (ssget '((0 . "lwpolyline")(70 . 1)))))
  (mapcar
   (function
    (lambda(x)
     (entity:make-text
      (strcat 
       (rtos (* 1e-6 (vla-get-area (e2o x))) 2 3)
       "m\U+00B2")
      (point:centroid (curve:get-points x))
      (* (@::get-config '@::draw-scale) 2.5)
      0 0.8 0 "MM")))
   lwpls)
  )
     
