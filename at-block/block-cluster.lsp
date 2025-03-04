(defun @block:block-cluster ()
  (@::prompt "分堆建块")
  (setq clusters (pickset:cluster (ssget) 1))
  ;;显示分堆结果。如果不正确，重新设置间隙重排。
  ;;(setq pt-s (getpoint))
  ;; (setq aim-boxs (mapcar '(lambda(x)
  ;; 			   (entity:make-rectangle
  ;; 			    (car  x)(cadr x)))
  ;; 			 clusters))
  ;;(setq pt-e  (getpoint pt-s "END:"))
  (setq i 0)
  ;;  (setq step (/ (distance pt-s pt-e)(1- (length clusters))))
  (push-var 'osmode)
  (if (<(getvar "osmode") 16384)
      (setvar "osmode" (+(getvar "osmode")16384)))
  (foreach
   cluster% clusters
   (setq ss% (ssget  "w" (car cluster%)(cadr cluster%)))
   (setq pt0 (point:centroid (pickset:getbox ss% 0.1)))
   
   (command "-block"
	    (setq blkname (strcat "Autoblock"(itoa i)"-"(@::timestamp)))
	    pt0
	    ss% "")
   (block:insert blkname "" pt0  0 1)
   (setq i (1+ i))
   )
  (pop-var)
  (princ))
