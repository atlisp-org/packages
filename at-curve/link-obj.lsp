(@:define-config '@curve:gap 100 "排线间距")
(@:define-config '@curve:pin-length 200 "引脚长度，物体外伸的直线长度")
(@:define-config '@curve:pin-width 50 "引线宽度，多段线的宽度")
(defun @curve:link-obj (/ gap)
  (@:help  '("用水平及垂直路径线连接原物体到目标物体。当前只支持1对多的关系"))
  (@:prompt "选择源物体:")
  (setq ss-src (ssget))
  (setq box-src (pickset:getbox ss-src 0))
  (setq obj-src (pickset:sort-by-box
		 ss-src
		 (if (< (apply 'angle box-src) (* 0.25 pi)) "xy" "yx")
		 0))
  (@:prompt "选择目标物体:")
  (setq ss-target (ssget))
  (setq box-target (pickset:getbox ss-target 0))
  (setq obj-target (pickset:sort-by-box
		    ss-target
		    (if (< (apply 'angle box-target) (* 0.25 pi)) "xy" "yx")
		    0))

  ;; entbox ssbox d-out n/2-gap (n/2-gap + turn-center-line)
  (setq gap (@:get-config '@curve:gap))
  (defun obj-to-centerline (obj inbox outbox centerline / pts startpt pt-box sign order all)
    (setq pts nil)
    ;; start
    (setq startpt (point:centroid (entity:getbox obj 0)))
    (setq pts (cons startpt pts))
    ;; box
    (setq pt-box
	  (if (< (apply 'angle inbox) (* 0.25 pi))
	      ;; to y
	      (polar startpt
		     (if (< (cadr (point:centroid inbox))
			    (cadr (point:centroid outbox)))
			 (* 0.5 pi)
		       (* 1.5 pi))
		     (+ (@:get-config '@curve:pin-length)
		     (if (< (cadr (point:centroid inbox))
			    (cadr (point:centroid outbox)))
			 (-(cadr (cadr inbox))
			   (cadr startpt))
		       (-(cadr startpt)
			 (cadr (car inbox))))))
	    (polar startpt
		   (if (< (car (point:centroid inbox))
			  (car (point:centroid outbox)))
		       0 
		     pi)
		   (+ (@:get-config '@curve:pin-length)
		      (if (< (car (point:centroid inbox))
			  (car (point:centroid outbox)))
		       (-(car (cadr inbox))
			 (car startpt))
		     (-(car startpt)
		       (car (car inbox))))))))
    (setq pts (cons pt-box pts))
    ;; 小为真
    (setq sign
	  (if (< (apply 'angle inbox) (* 0.25 pi))
	      (if (< (car startpt) (car centerline))
		  t)
	    (if (< (cadr startpt) (cadr centerline))
		t)))
    (setq all
	  (if (member obj obj-src)
	      (length obj-src)
	    (if (member obj obj-target)
		(length obj-target))))
    (setq n-less
	  (length
	   (vl-remove nil
		      (mapcar
		       '(lambda(x)
			  (if (< (apply 'angle inbox) (* 0.25 pi))
			      (if (< (car(point:centroid (entity:getbox x 0))) (car centerline))
				  t)
			    (if (< (cadr (point:centroid (entity:getbox x 0))) (cadr centerline))
				t)))
		       (if (member obj obj-src)
			   obj-src
			 obj-target)))))
    (setq n-more (- all n-less))
	   
    (setq order
	  (if (member obj obj-src)
	      (vl-position obj obj-src)
	    (if (member obj obj-target)
		(vl-position obj obj-target))))
    (setq all
	  (if (member obj obj-src)
	      (length obj-src)
	    (if (member obj obj-target)
		(length obj-target))))
    (setq pts (cons
	       (polar
		pt-box
		(angle startpt pt-box)
		(* gap
		   (if sign
		       (- n-less order)
		     (- order n-less))
		   ))
	       pts))
    ;; 到集束
    (setq pts (cons
	       (if (< (apply 'angle inbox) (* 0.25 pi))
		   (list
		    (if sign
			(- (car centerline)
			   (* gap (- n-less order)))
		      (+ (car centerline)
			 (* gap (- order n-less)))
		      )
		    (cadr (car pts))
		    0) 
		   (list
		    (car (car pts))
		    (if sign
			(- (cadr centerline)
			   (* gap (- n-less order)))
		      (+ (cadr centerline)
			 (* gap (- order n-less)))
		      )
		    0))
	       pts))
    (setq pts (cons
	       (if (< (apply 'angle inbox) (* 0.25 pi))
		   (list
		    (car (car pts))
		    (cadr centerline)
		    0)
		 (list
		  (car centerline)
		  (cadr (car pts))
		  0)
		 )
	       pts))
    (reverse pts)
    )
  ;; 1 to n
  (cond
    ((= (length obj-src) 1)
     (mapcar '(lambda(x)
	       (entity:make-lwpolyline
		(append
		 ;; (obj-to-centerline
		 ;;  (car obj-src)
		 ;;  box-src
		 ;;  box-target
		 ;;  (point:centroid box-src))
		 (reverse
		  (obj-to-centerline
		   x
		   box-target
		   box-src
		   (point:centroid box-src)))
		 )
		nil (@:get-config '@curve:pin-width) 0 0))
	     obj-target))
    ((= (length obj-target) 1)
     (mapcar '(lambda(x)
		(entity:make-lwpolyline
		 (append
		  (obj-to-centerline
		   x
		   box-src
		   box-target
		   (point:centroid box-target))
		  ;; (reverse
		  ;;  (obj-to-centerline
		  ;;   (car obj-target)
		  ;;   box-target
		  ;;   box-src
		  ;;   (point:centroid box-target)))
		  )
		 nil (@:get-config '@curve:pin-width) 0 0))
	     obj-src))
    ((= (length obj-target)(length obj-src))
     (mapcar '(lambda(x y)
		(entity:make-lwpolyline
		 (append
		  (obj-to-centerline
		   x
		   box-src
		   box-target
		   (point:centroid box-target))
		  (reverse
		   (obj-to-centerline
		    y
		    box-target
		    box-src
		    (point:centroid box-target))))
		 nil (@:get-config '@curve:pin-width) 0 0))
	     obj-src
	     obj-target))))
		 
  
