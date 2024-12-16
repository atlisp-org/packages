(defun @layout:divide-map (/ margin gap box width height ents pt-base)
  (@::prompt "单个图形分图到布局")
  (setq margin (@::get-config '@layout:divide-margin))
  (setq gap (@::get-config '@layout:divide-gap))
  (setq ents (pickset:to-list (ssget)))
  
  (or (member (@::get-config '@layout:divide-layout) (layout:list))
      (vla-add *layouts* (@::get-config '@layout:divide-layout)))
  (setvar "ctab" (@::get-config '@layout:divide-layout))
  ;;去捕捉
  (std:osmode-off)
  (if(and (setq ss(@layout:ssgetx (@::get-config '@layout:divide-layout)))
	  (setq box (pickset:getbox ss 0)))
     (progn
       (setq pt-base (polar (car box) 0 (* 1.1 (- (caadr box) (caar box))))))
     (setq pt-base (list 0.0 0.0 0.0)))
  (foreach x ents
	   (setq box (entity:getbox x margin))
	   (setq width (- (caadr box)(caar box)))
	   (setq height (- (cadadr box)(cadar box)))
	   (setq pt-base
		 (polar pt-base
			0 (* 0.5 width)))
	   (layout:make-viewport
	    (@::get-config '@layout:divide-layout)
	    pt-base
	    width
	    height
	    (point:centroid box))
	   (setq pt-base 
		 (polar pt-base
			0
			(+ gap
			   (* 0.5 width))
			)))
  (std:osmode-on)
  )
			   
(defun @layout:divide-cluster (/ clusters margin gap box width height ents pt-base)
  (@::prompt "图形分堆然后分图到布局")
  (setq margin (@::get-config '@layout:divide-margin))
  (setq gap (@::get-config '@layout:divide-gap))

  (setq clusters (pickset:cluster (ssget) margin))
  
  (or (member (@::get-config '@layout:divide-layout) (layout:list))
      (vla-add *layouts* (@::get-config '@layout:divide-layout)))
  (setvar "ctab" (@::get-config '@layout:divide-layout))
  (std:osmode-off)
  (if(and (setq ss(@layout:ssgetx (@::get-config '@layout:divide-layout)))
	  (setq box (pickset:getbox ss 0)))
     (progn
       (setq pt-base (polar (car box) 0 (* 1.1 (- (caadr box) (caar box))))))
     (setq pt-base (list 0.0 0.0 0.0)))
  (foreach x clusters
	   (setq box x)
	   (setq width (- (caadr box)(caar box)))
	   (setq height (- (cadadr box)(cadar box)))
	   (setq pt-base
		 (polar pt-base
			0 (* 0.5 width)))
	   (layout:make-viewport
	    (@::get-config '@layout:divide-layout)
	    pt-base
	    width
	    height
	    (point:centroid box))
	   (setq pt-base 
		 (polar pt-base
			0
			(+ gap
			   (* 0.5 width))
			)))
  (std:osmode-on)
  )
			   
