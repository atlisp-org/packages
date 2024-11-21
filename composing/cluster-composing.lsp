(defun composing:cluster ()
  (@::help "分堆排版，将选择的图形分堆后，按直线排版。")
  (setq clusters (pickset:cluster (ssget) 1))
  ;;显示分堆结果。如果不正确，重新设置间隙重排。
  
  (setq pt-s (getpoint))
  ;; (setq aim-boxs (mapcar '(lambda(x)
  ;; 			   (entity:make-rectangle
  ;; 			    (car  x)(cadr x)))
  ;; 			 clusters))
  (setq pt-e  (getpoint pt-s "END:"))
  (setq i 0)
  (setq step (/ (distance pt-s pt-e)(1- (length clusters))))
  (foreach
   cluster% clusters
   (setq ss% (ssget  "w" (car cluster%)(cadr cluster%)))
   (setq pt0 (point:centroid (pickset:getbox ss% 0.1)))
   (mapcar
    '(lambda(x)
      (vla-move (e2o x)
       (point:to-ax pt0)
       (point:to-ax (polar pt-s (angle pt-s pt-e) (* i step)))))
    (pickset:to-list ss%))
   (setq i (1+ i)))
  
  (princ))
