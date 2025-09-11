

;;; 直线相互连接形成之拓朴结构的相关操作 2018-4-1

;; 对二维点索引数据集进行排序，支持三维点但忽略第三维数据
;; (p-sort-points '(((0 0) "PT1")((1 1) "PT2")((2 3) "PT3")((10 -2) "PT4")((2 3) "PT5")) 1.)
;; (((0 0) "PT1") ((1 1) "PT2") ((2 3) "PT5" "PT3") ((10 -2) "PT4"))
(defun p-sort-points (ps tolerance / e lst p r)
  (setq
    ;; 按Y排序
    ps (vl-sort ps '(lambda (e1 e2) (< (cadar e1) (cadar e2))))

    ;; 按X排序
    ps (vl-sort	ps
		'(lambda (e1 e2)
		   (and	(not (equal (caar e1) (caar e2) tolerance))
			(< (caar e1) (caar e2))
		   )
		 )
       )
  )
  ;;  合并相同索引点数据项
  ;;  eg. ((34567.0 181785.0 0.0) <图元名: -21bd38> <图元名: -21bd40>)
  (while ps
    (setq e (car ps)
	  p (car e)
    )
    (while (equal (car e) p tolerance)
      (setq lst	(cons (cadr e) lst) ;_ 将相同项合并
	    ps	(cdr ps)
	    e	(car ps)
      )
    )
    (setq r   (cons (cons p lst) r)
	  lst nil
    )
  )
  (reverse r)
)
;;

;; 构建已排序的一个列表的平衡二叉树(BST)，加速元素检索速度
;; (p-bst-build '(1 2 3 4 5 6 7))
;; (4 (2 (1) (3)) (6 (5) (7)))
(defun p-bst-build (lst / i left n)
  (if (> (length lst) 1)
    (progn
      (setq n (/ (length lst) 2)
	    i 0
      )
      (while (< i n)
	(setq left (cons (car lst) left)
	      lst  (cdr lst)
	      i	   (1+ i)
	)
      )
      (list (car lst)
	    (p-bst-build (reverse left))
	    (p-bst-build (cdr lst))
      )
    )
    lst
  )
)
;;

;; 对排序二维点索引数据集进行二分查找，支持容差
;;;_$ (p-bst-find '(2 2) (p-bst-build '(((0 0) "PT1")((1 1) "PT2")((2 3) "PT3")((10 -2) "PT4"))) 1)
;;;nil
;;;_$ (p-bst-find '(2 2) (p-bst-build '(((0 0) "PT1")((1 1) "PT2")((2 3) "PT3")((10 -2) "PT4"))) 2)
;;;("PT3")
(defun p-bst-find (point bst tolerance /)
  (if (null bst)
    nil
    (if	(equal point (caar bst) tolerance)
      (cdar bst)
      (if (or
	    (and (not (equal (car point) (caaar bst) tolerance))
		 (< (car point) (caaar bst))
	    )
	    (and (equal (car point) (caaar bst) tolerance)
		 (< (cadr point) (cadaar bst))
	    )
	  )
	(p-bst-find point (cadr bst) tolerance)
	(p-bst-find point (caddr bst) tolerance)
      )
    )
  )
)

;; 为直线实体构建二维点索引
;; (p-bst-buildforlines (ssget) 0.1)
;; 
;;;(((7035.45 7376.95 0.0) <图元名: -2569d0>)
;;;  (((5249.75 6739.56 0.0) <图元名: -2569e8> <图元名: -2569e0>)
;;;    (((2749.76 6739.56 0.0) <图元名: -2569e8>))
;;;    (((5249.75 8413.79 0.0) <图元名: -2569e0> <图元名: -2569d8>)
;;;    )
;;;  )
;;;  (((7724.22 8413.79 0.0) <图元名: -2569c8>)
;;;    (((7035.45 8413.79 0.0) <图元名:	-2569d8> <图元名: -2569d0> <图元名: -2569c8>)
;;;    )
;;;    nil
;;;  )
;;;)
(defun p-bst-buildforlines (ss tolerance / ad dxf en n)
  (if ss
    (progn
      (repeat (setq n (sslength ss))
	(setq en  (ssname ss (setq n (1- n)))
	      dxf (entget en)
	)
	(setq ad (cons (list (cdr (assoc 10 dxf)) en) ad)
	      ad (cons (list (cdr (assoc 11 dxf)) en) ad)
	)
      )
      (p-bst-build (p-sort-points ad tolerance))
    )
  )
)

;; (p-bst-find (getpoint) (p-bst-buildforlines (ssget "X") 0.1) 0.1)
;; (p-bst-find (getpoint) (p-bst-buildforlines (ssget "X" '((8 . "1"))) 0.1) 0.1)

