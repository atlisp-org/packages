;;; 统计结果均为点对表。car 为 系数，cdr 为值

(defun-q stat:stat (lst / atom% res)
  "统计列表 lst 中的元素个数。"
  "元素和个数组成的点对表"
  "(stat:stat '(3 a a 2 2)) => ((3 . 1) (A . 2) (2 . 2))"
  (setq res '())
  (foreach atom% lst
	   (if (assoc atom% res)
	       (setq res (subst (cons atom% (+ 1 (cdr (assoc atom% res))))
				  (assoc atom% res)
				  res))
	       (setq res (append res (list (cons atom% 1))))
	       )
	   )
  res)

(defun stat:mode (stat-res)
  "众数"
  (car (vl-sort stat-res (function (lambda (e1 e2)
			   (> (cdr e1)(cdr e2))))))
  )

(defun stat:print ()
  "打印最后一次统计的结果"
  (foreach n  @:tmp-stat-result
	   (princ  (car n ))(princ (cdr n)) (princ "\n")))
(defun stat:draw ( / n pt )
  "绘制最后一次统计的结果"
  (setq pt (getpoint "请输入要绘制的位置:"))
  (setq n 0)
  (table:make pt "统计结果" '("项目" "个数")
	      (mapcar '(lambda (x) (list (car x)(cdr x)))  @:tmp-stat-result))
  ;; (entity:make-text " 项" (m:coordinate pt (list 0 (* n -350))) 250 0 0.8 0 13)
  ;; (entity:make-text (format nil "个数 | ") (m:coordinate pt (list 0 (* n -350))) 250 0 0.8 0 33)
  ;; (setq n 1)
  ;; (foreach x  @:tmp-stat-result
  ;; 	   (entity:make-text (format nil "~a" (car x)) (m:coordinate pt (list 0 (* n -350))) 250 0 0.8 0 13)
  ;; 	   (entity:make-text (format nil "~10d | " (cdr x)) (m:coordinate pt (list 0 (* n -350))) 250 0 0.8 0 33)
  ;; 	   (setq n (1+ n))
  ;; 	   )
  )

;; Local variables:
;; coding: gb2312
;; End: 
