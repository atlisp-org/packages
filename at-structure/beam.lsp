(@:define-config 'at-structure:layer-beam "S_BEAMDASH,砼梁" "梁图层")
;; (@:add-menu "结构" "*梁编组" "(at-structure:find-beam)")
(defun at-structure:find-beam (/ ss-beam lst-beam en-line1 en-line2)
  "找出梁边(成对的平行线)"
  (setq ss-beam (pickset:to-list
		 (ssget 
			(list '(0 . "line")
			      (cons 8 (@:get-config 'at-structure:layer-beam))))))
  ;; (setq ss-beam
  ;; 	(vl-sort ss-beam
  ;; 		 '(lambda (x y) ;;按角度排序
  ;; 		   (< (angle (entity:getdxf x 10)(entity:getdxf x 11))
  ;; 		    (angle (entity:getdxf y 10)(entity:getdxf y 11))))))
  (setq lst-beam nil)
  (while (> (length ss-beam) 1)
    (setq en-line1 (car ss-beam))
    (setq ss-beam (vl-sort (cdr ss-beam)
			   '(lambda (x y)
			     (and ((geometry:angle en-line1)
			     (<
			      (geometry:dist-pt-line
				 (geometry:segment-mid (geometry:segment-by-line en-line1))
				 (geometry:segment-by-line x))
			      (geometry:dist-pt-line
			       (geometry:segment-mid (geometry:segment-by-line en-line1))
			       (geometry:segment-by-line y))))))))
    (setq lst-beam (append lst-beam (list (list en-line1 (car ss-beam)))))
    (entity:make-line (geometry:segment-mid (geometry:segment-by-line en-line1))
		      (geometry:segment-mid (geometry:segment-by-line (car ss-beam))))
    (setq ss-beam (cdr ss-beam)))
  lst-beam)

	     
  
  
  
