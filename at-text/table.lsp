(defun text:get-matrix (/ ss-txt result lst-tmp row)
  "´ÓdwgÍ¼ÖĞ¿òÈ¡µ¥ĞĞÎÄ±¾£¬ĞÎ³É¶şÎ¬ÁĞ±íÊı¾İ"
  "ÓÉ×Ö·û´®×é³ÉµÄ¶şÎ¬ÁĞ±í"
  ""
  (setq ss-txt (pickset:to-list (ssget '((0 . "text")))))
  ;; ×ÔÉÏ¶øÏÂ °´ĞĞ·Ö×é
  (setq ss-txt
	(vl-sort ss-txt
		 '(lambda (x y)
		    (or (and (equal (cadr (entity:getdxf x 10))(cadr (entity:getdxf y 10))
				    (* 1.1 (entity:getdxf x 40)))
			     (<  (car (entity:getdxf x 10))(car (entity:getdxf y 10))))
			(>  (cadr (entity:getdxf x 10))(+ (cadr (entity:getdxf y 10))(* 0.5 (entity:getdxf y 40))))))))
  ;;(print ss-txt)
  (setq result '())
  (setq lst-tmp (list (car ss-txt))) ;; Í¬Ò»ĞĞµÄÎÄ±¾ÁĞ±í
  (while (setq ss-txt (cdr ss-txt))
    (if (equal (cadr (entity:getdxf (last lst-tmp) 10))(cadr (entity:getdxf (car ss-txt) 10))
	       (* 0.3 (entity:getdxf (last lst-tmp) 40)))
	;; Í¬Ò»ĞĞ£¬
	(setq lst-tmp (append lst-tmp (list (car ss-txt))))
	(progn ;;ĞÂĞĞ
	  (setq result (append result (list lst-tmp)))
	  (setq lst-tmp (list (car ss-txt))))
	))
  (setq result (append result (list lst-tmp)))
  ;; ÁĞ¶ÔÆë
  (setq lst-tmp (cdr result))

  (setq result (list (car result)))
  (foreach row lst-tmp
	   (if (= (length row)(length (car result)))
	       (setq result (append result (list row)))
	     (progn
	       (setq i 0) ;;first row 
	       (setq j 0) ;; current row
	       (setq row-tmp '())
	       (repeat (length (car result))
		       (if (equal (car (entity:getdxf (nth j row) 10))
				  (car (entity:getdxf (nth i (car result)) 10))
				  (* 1.1 (entity:getdxf (nth i (car result)) 40)))
			   (progn
			     (setq row-tmp (append row-tmp (list (nth j row))))
			     (setq i (1+ i))(setq j (1+ j)))
			 (progn
			   (setq row-tmp (append row-tmp (list "")))
			   (setq i (1+ i)))))
	       (setq result (append result (list row-tmp))))))
  (mapcar '(lambda (x) (mapcar '(lambda (y) (if (= 'ename (type y)) (entity:getdxf y 1) y)) x)) result))

(defun @text:text2table (/ ss-txt result lst-tmp row)
  (setq ss-txt (pickset:to-list (ssget '((0 . "text")))))
  ;; è‡ªä¸Šè€Œä¸‹ æŒ‰è¡Œåˆ†ç»„
  (setq ss-txt
	(vl-sort ss-txt
		 '(lambda (x y)
		    (or (and (equal (cadr (entity:getdxf x 10))(cadr (entity:getdxf y 10))
				    (* 1.1 (entity:getdxf x 40)))
			     (<  (car (entity:getdxf x 10))(car (entity:getdxf y 10))))
			(>  (cadr (entity:getdxf x 10))(+ (cadr (entity:getdxf y 10))(* 0.5 (entity:getdxf y 40))))))))
  ;;(print ss-txt)
  (setq result '())
  (setq lst-tmp (list (car ss-txt))) ;; åŒä¸€è¡Œçš„æ–‡æœ¬åˆ—è¡¨
  (while (setq ss-txt (cdr ss-txt))
    (if (equal (cadr (entity:getdxf (last lst-tmp) 10))(cadr (entity:getdxf (car ss-txt) 10))
	       (* 0.3 (entity:getdxf (last lst-tmp) 40)))
	;; åŒä¸€è¡Œï¼Œ
	(setq lst-tmp (append lst-tmp (list (car ss-txt))))
	(progn ;;æ–°è¡Œ
	  (setq result (append result (list lst-tmp)))
	  (setq lst-tmp (list (car ss-txt))))
	))
  (setq result (append result (list lst-tmp)))
  ;; åˆ—å¯¹é½
  (setq lst-tmp (cdr result))
  (setq result (list (car result)))
  (foreach row lst-tmp
	   (if (= (length row)(length (car result)))
	       (setq result (append result (list row)))
	     (progn
	       (setq i 0) ;;first row 
	       (setq j 0) ;; current row
	       (setq row-tmp '())
	       (repeat (length (car result))
		       (if (equal (car (entity:getdxf (nth j row) 10))
				  (car (entity:getdxf (nth i (car result)) 10))
				  (* 1.1 (entity:getdxf (nth i (car result)) 40)))
			   (progn
			     (setq row-tmp (append row-tmp (list (nth j row))))
			     (setq i (1+ i))(setq j (1+ j)))
			 (progn
			   (setq row-tmp (append row-tmp (list "NOP")))
			   (setq i (1+ i)))))
	       (setq result (append result (list row-tmp))))))
  (table:make (getpoint "è¯·è¾“å…¥è¡¨æ ¼æ’å…¥ç‚¹:")
   	      "TITLE" nil
	      (mapcar '(lambda (x) (mapcar '(lambda (y) (if (= 'ename (type y)) (entity:getdxf y 1) y)) x)) result))
  ;; (princ (mapcar '(lambda (x) (mapcar '(lambda (y) (if (= 'ename (type y)) (entity:getdxf y 1) y)) x)) result))
	
  )

	       
	   

			  
