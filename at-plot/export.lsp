
(@:add-menu "通用打印" "批打输出" "(@plot:plot-all)")
(defun @plot:plot-all (/ tf tufu zongheng)
  (@:help "打印标记的图框")
  
  (if (setq tf (pickset:to-list
		(ssget "x" '((0 . "LWPOLYLINE")
			     (90 . 4)(70 . 1)
			     (8 . "temp-frames")))))
      ;; 打印
      (progn 
	(@:log "INFO" "打印标记的图框")
	(setvar "cmdecho" 0)

	(setq frames (mapcar '(lambda (x)
				(@:get-rec-points x))
			     tf))
	(setq i% 0)
	(foreach frame frames
		 (setq tuming (car (@pm:pickout-maptitle-first (list (car frame) (last frame)))))
		 ;; 识别图幅，横竖
		 (if (null tuming)
		     (setq tuming "")
		   (setq tuming (strcat "-" tuming))
		   )
		 (setq tufu "A2")
		 (setq zongheng "L")
		 (plot:to-pdf tufu zongheng (car frame) (last frame)
			      (strcat (system:dir (@::get-config '@plot:export-path)) (itoa (setq i% (1+ i%))) tuming ".pdf"))

		 )
	(setvar "cmdecho" 0)
	(princ)
	)))
