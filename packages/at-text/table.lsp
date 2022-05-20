(defun text:get-matrix (/ ss-txt result lst-tmp row)
  "从dwg图中框取单行文本，形成二维列表数据"
  "由字符串组成的二维列表"
  ""
  (setq ss-txt (pickset:to-list (ssget '((0 . "text")))))
  ;; 自上而下 按行分组
  (setq ss-txt
	(vl-sort ss-txt
		 '(lambda (x y)
		    (or (and (equal (cadr (entity:getdxf x 10))(cadr (entity:getdxf y 10))
				    (* 1.1 (entity:getdxf x 40)))
			     (<  (car (entity:getdxf x 10))(car (entity:getdxf y 10))))
			(>  (cadr (entity:getdxf x 10))(+ (cadr (entity:getdxf y 10))(* 0.5 (entity:getdxf y 40))))))))
  ;;(print ss-txt)
  (setq result '())
  (setq lst-tmp (list (car ss-txt))) ;; 同一行的文本列表
  (while (setq ss-txt (cdr ss-txt))
    (if (equal (cadr (entity:getdxf (last lst-tmp) 10))(cadr (entity:getdxf (car ss-txt) 10))
	       (* 0.3 (entity:getdxf (last lst-tmp) 40)))
	;; 同一行，
	(setq lst-tmp (append lst-tmp (list (car ss-txt))))
	(progn ;;新行
	  (setq result (append result (list lst-tmp)))
	  (setq lst-tmp (list (car ss-txt))))
	))
  (setq result (append result (list lst-tmp)))
  ;; 列对齐
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
  ;; 自上而下 按行分组
  (setq ss-txt
	(vl-sort ss-txt
		 '(lambda (x y)
		    (or (and (equal (cadr (entity:getdxf x 10))(cadr (entity:getdxf y 10))
				    (* 1.1 (entity:getdxf x 40)))
			     (<  (car (entity:getdxf x 10))(car (entity:getdxf y 10))))
			(>  (cadr (entity:getdxf x 10))(+ (cadr (entity:getdxf y 10))(* 0.5 (entity:getdxf y 40))))))))
  ;;(print ss-txt)
  (setq result '())
  (setq lst-tmp (list (car ss-txt))) ;; 同一行的文本列表
  (while (setq ss-txt (cdr ss-txt))
    (if (equal (cadr (entity:getdxf (last lst-tmp) 10))(cadr (entity:getdxf (car ss-txt) 10))
	       (* 0.3 (entity:getdxf (last lst-tmp) 40)))
	;; 同一行，
	(setq lst-tmp (append lst-tmp (list (car ss-txt))))
	(progn ;;新行
	  (setq result (append result (list lst-tmp)))
	  (setq lst-tmp (list (car ss-txt))))
	))
  (setq result (append result (list lst-tmp)))
  ;; 列对齐
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
  (table:make (getpoint "请输入表格插入点:")
   	      "TITLE" nil
	      (mapcar '(lambda (x) (mapcar '(lambda (y) (if (= 'ename (type y)) (entity:getdxf y 1) y)) x)) result))
  ;; (princ (mapcar '(lambda (x) (mapcar '(lambda (y) (if (= 'ename (type y)) (entity:getdxf y 1) y)) x)) result))
	
  )

	       
	   

			  
