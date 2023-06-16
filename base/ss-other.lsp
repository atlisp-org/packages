(defun c:km (/ ss ss0 s1 ss1 n x ptn)
  (setq ss1 (SsgetCP (list(car (entsel))'(0 . "*") )) )
  (sssetfirst nil ss1)
  (princ)
  )
(defun makepl (argments)
  ;;argments==>(list pts 闭合标志 全局宽度 线宽 图层 颜色 厚度 线型)pts以后可省略
  (entmakex
   (append (mapcar 'cons
		   '(0 100 100 43 370 8 62 39 6)
		   (append '("LWPOLYLINE" "AcDbEntity" "AcDbPolyline")
			   (cddr argments)
			   )
		   )
	   (cons (cons 90 (length (car argments)))
		 (cons	(cons 70
			      (if (cadr argments)
				  (cadr argments)
				  0
				  )
			      )
			(mapcar '(lambda (x) (cons 10 x)) (car argments))
			)
		 )
	   )
   )
  )
(defun poinpl (p pt)
  ;;:点是否在指定点表?
  (equal
   (abs
    (apply '+
	   (mapcar '(lambda (x y) (rem (- (angle x p) (angle y p)) pi))
		   pt
		   (cons (last pt) pt)
		   )
	   )
    )
   pi
   1e-8
   )
  )
(defun plinexy (e)
  (mapcar 'cdr
	  (vl-remove-if '(lambda (x) (/= (car x) 10)) (entget e))
	  )
  )
(defun SsgetW (arg / a)
  ;;选择指定矩形区域?(不限屏幕范围)
  (ssget "X"
	 (apply	'append
		(list '((-4 . "<and") (-4 . ">=,>="))
		      (setq a (list (car arg) (cadr arg))
			    a (mapcar '(lambda (x) (mapcar x a)) '(car cadr))
			    a (mapcar '(lambda (y)
					(cons 10 (mapcar '(lambda (x) (apply y x)) a))
					)
				      '(min max)
				      )
			    a (list (car a) '(-4 . "<=,<=") (cadr a))
			    )
		      (cddr arg)
		      '((-4 . "and>"))
		      )
		)
	 )
  )
(defun SsgetCP (arg / a i pt s b)
  ;;根据多线段图元名或者其坐标点表进行(ssget"CP"...)但不限屏幕范围
  (if (listp (setq a (car arg)))
      (setq pt a
	    a  (vlax-ename->vla-object (makepl (list pt)))
	    )
      (setq pt (plinexy a)
	    a  (vlax-ename->vla-object a)
	    )
      )
  (if (setq i -1
	    s (SsgetW
	       (append
		(mapcar
		 '(lambda (x)
		   (mapcar '(lambda (y) (apply x y))
		    (mapcar '(lambda (x) (mapcar x pt)) '(car cadr))
		    )
		   )
		 '(min max)
		 )
		(cdr arg)
		)
	       )
	    s (if (SSMEMB (vlax-vla-object->ename a) s)
		  (ssdel (vlax-vla-object->ename a) s)
		  s
		  )
	    )
      (repeat (sslength s)
	      (setq i (1+ i)
		    e (ssname s i)
		    )
	      (if
	       (not
		(or (> (vlax-safearray-get-u-bound
			(vlax-variant-value
			 (vla-intersectwith (vlax-ename->vla-object e) a 0)
			 )
			1
			)
		       1
		       )
		    (poinpl (cdr (assoc 10 (entget e))) pt)
		    )
		)
	       (setq b (cons e b))
	       )
	      )
      )
  (if (listp (car arg))
      (vla-delete a)
      )
  (foreach a b (setq s (ssdel a s)))
  s
  )
(defun SsgetWP (arg / a i pt s b)
  ;;根据多线段图元名或者其坐标点表进行(ssget"WP"...)但不限屏幕范围
  (if (listp (setq a (car arg)))
      (setq pt a
	    a  (vlax-ename->vla-object (makepl (list pt)))
	    )
      (setq pt (plinexy a)
	    a  (vlax-ename->vla-object a)
	    )
      )
  (if (setq i -1
	    s (SsgetW
	       (append
		(mapcar
		 '(lambda (x)
		   (mapcar '(lambda (y) (apply x y))
		    (mapcar '(lambda (x) (mapcar x pt)) '(car cadr))
		    )
		   )
		 '(min max)
		 )
		(cdr arg)
		)
	       )
	    s (if (SSMEMB (vlax-vla-object->ename a) s)
		  (ssdel (vlax-vla-object->ename a) s)
		  s
		  )
	    )
      (repeat (sslength s)
	      (setq i (1+ i)
		    e (ssname s i)
		    )
	      (if (or (> (vlax-safearray-get-u-bound
			  (vlax-variant-value
			   (vla-intersectwith (vlax-ename->vla-object e) a 0)
			   )
			  1
			  )
			 1
			 )
		      (not (poinpl (cdr (assoc 10 (entget e))) pt))
		      )
		  (setq b (cons e b))
		  )
	      )
      )
  (if (listp (car arg))
      (vla-delete a)
      )
  (foreach a b (setq s (ssdel a s)))
  s
  )
