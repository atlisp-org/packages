(defun composing:cluster ()
  (@::prompt "分堆排版，将选择的图形分堆后，按直线排版。")
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
(defun composing:cluster-gap ()
  (@::prompt "请选择要排版的图形:")
  (setq clusterboxs (pickset:cluster (ssget) 1))
  (if (and (setq clusters(mapcar '(lambda(cluster%)(ssget  "w" (car cluster%)(cadr cluster%))) clusterboxs))
	   (setq pt-s (getpoint (@::prompt"请点击起始点:"))))
      (progn
	(setq gap (getdist (@::prompt "\n请输入间隙值<10>:")))
	(if (null gap)(setq gap 10))
	(setq i 0)
	(foreach
	 cluster% clusters
	 (setq box% (pickset:getbox cluster% 0))
	 (setq box%  (point:rec-2pt->4pt (car box%)(cadr box%)))
	 (setq pt0 (car box%))
	 (mapcar
	  '(lambda(x)
	    (vla-move (e2o x)
	     (point:to-ax pt0)
	     (point:to-ax pt-s)))
	  (pickset:to-list cluster%))
	 ;;确认下一点
	 (setq pt-s (polar pt-s 0 (distance (car box%)(cadr box%))))
	 (setq pt-s (polar pt-s 0 gap))
	 )))
  (princ))

(defun composing:entity ()
  (@::prompt "单一图形排版，将选择的图形，按直线排版。")
  (setq ents (pickset:to-list (ssget)))
  (setq pt-s (getpoint))
  (setq pt-e  (getpoint pt-s "END:"))
  (setq i 0)
  (setq step (/ (distance pt-s pt-e)(1- (length ents))))
  (foreach
   ent% ents 
   (setq pt0 (point:centroid (entity:getbox ent% 0.1)))
   (vla-move (e2o ent%)
	     (point:to-ax pt0)
	     (point:to-ax (polar pt-s (angle pt-s pt-e) (* i step))))
   (setq i (1+ i)))
  
  (princ))

(defun composing:entity-gap ()
  (@::prompt "单一图形排版，将选择的图形，按直线排版，图形间隙固定。")
  
  (setq ents (pickset:to-list (ssget) ))
  (if (setq pt-s (getpoint (@::prompt"请点击起始点:")))
      (progn
	(setq gap (getdist (@::prompt "\n请输入间隙值<10>:")))
	(if (null gap)(setq gap 10))
	(setq i 0)
	(foreach
	 ent% ents
	 (setq box% (entity:getbox ent% 0))
	 (setq box%  (point:rec-2pt->4pt (car box%)(cadr box%)))
	 (setq pt0 (car box%))
	 (vla-move (e2o ent%)
		   (point:to-ax pt0)
		   (point:to-ax pt-s))
	 (setq pt-s (polar pt-s 0 (distance (car box%)(cadr box%))))
	 (setq pt-s (polar pt-s 0 gap))
	 )))
  (princ))
(defun composing:group ()
  (@::prompt "分堆排版，将选择的图形分堆后，按直线排版。")
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
(defun composing:group-gap ()
  (@::prompt "请选择要排版的图形:")
  (setq clusterboxs (pickset:cluster (ssget) 1))
  (if (and (setq clusters(mapcar '(lambda(cluster%)(ssget  "w" (car cluster%)(cadr cluster%))) clusterboxs))
	   (setq pt-s (getpoint (@::prompt"请点击起始点:"))))
      (progn
	(setq gap (getdist (@::prompt "\n请输入间隙值<10>:")))
	(if (null gap)(setq gap 10))
	(setq i 0)
	(foreach
	 cluster% clusters
	 (setq box% (pickset:getbox cluster% 0))
	 (setq box%  (point:rec-2pt->4pt (car box%)(cadr box%)))
	 (setq pt0 (car box%))
	 (mapcar
	  '(lambda(x)
	    (vla-move (e2o x)
	     (point:to-ax pt0)
	     (point:to-ax pt-s)))
	  (pickset:to-list cluster%))
	 ;;确认下一点
	 (setq pt-s (polar pt-s 0 (distance (car box%)(cadr box%))))
	 (setq pt-s (polar pt-s 0 gap))
	 )))
  (princ))
