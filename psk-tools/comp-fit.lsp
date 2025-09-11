;; 实体LINE代表的管口实现，因为性能太差，已废弃2021-2-9
;;;  (if (= 'ename (type fit))
;;;    (setq fit (psk-comp-load fit))
;;;  )
;;;  
;;;  (if (= "FIT" (psk-comp-getname fit))
;;;    (progn
;;;      (setq ename (p-get fit -1)
;;;	    geom  (p-refgeom ename)
;;;      )
;;;
;;;      (foreach e (p-block-items (p-dxf ename 2))
;;;	(if (setq props (p-xprop-getall e "PSK-FIT"))
;;;	  (progn
;;;	    (setq ep (p-dxf e '(-1 10))
;;;		  r  (cons (cons (car ep) (p-block-trans (cadr ep) geom))
;;;			   r
;;;		     )
;;;	    )
;;;	  )
;;;	)
;;;      )
;;;      r
;;;    )
;;;  )
;;;_$ (p-benchmark '(psk-fit-getports en) 1000)
;;;"Benchmark loops = 1000, in 1938 ms, 516 invoke / s"
;;;_$ (p-benchmark '(psk-fit-getports en) 1000)
;;;"Benchmark loops = 1000, in 94 ms, 10638 invoke / s"

;; 返回管口数据（相较于块插入点的偏移量 管口方向 特征尺寸）
;; (psk-fit-getports (car (entsel)))
;; ((1 (43178.8 13384.1 0.0) (1.0 0.0 0.0) 10.0) (2 (42778.8 13784.1 0.0) (0.0 1.0 0.0) 10.0))
(defun psk-fit-getports	(fit / p)
  (if (/= 'ename (type fit))
    (setq fit (p-get fit -1))
  )
  (setq p (p-dxf fit 10))

  (setq ports (psk-ports-fetch fit))
  ;; (("CD" (0 0 0) (1 0 0) 20) ("REF" (0 0 0) (1 0 0) 20))

  ;; 计算管口位置
  (mapcar (function (lambda (e) (append (list (car e) (mapcar '+ p (cadr e))) (cddr e)))) ports)
)


;; (psk-fit-moveport (psk-comp-load (car (entsel))) (getpoint )(getpoint ))
(defun psk-fit-moveport	(fit p newp / insert ports offset)
  (setq	insert (p-get fit -1)
	ports  (psk-ports-sort (psk-fit-getports insert) p)
	offset (mapcar '- newp (psk-port-pos (car ports)))
  )

  (if (not (equal '(0. 0. 0.) offset 0.1))
    (progn
      (setq newp (mapcar '+ offset (p-dxf insert 10)))
      (p-entmod insert (cons 10 newp))
      
      (cons offset (cdr (mapcar 'psk-port-pos ports)))
    )
  )
)


;;;(defun psk-make-port (p1 p2 prop)
;;;  (entmake
;;;    (list
;;;      '(0 . "LINE")
;;;      (cons 10 p1)
;;;      (cons 11 p2)
;;;      (list -3
;;;	    (cons "PSK-FIT" (p-xprop->xdata prop nil))
;;;      )
;;;    )
;;;  )
;;;)
(defun psk-fit-create (p a func param ports prop / en name)
  (psk-comp-create-inner p a func param ports prop "PSK-PART")
)

(defun psk-comp-create-inner (p a func param ports prop appid / en name)

  (if (wcmatch (psk-comp-gettype prop) "DUCT*")
    (psk-set-customlayerbyid
      (strcat (psk-path-getservice prop) "-CL")
    )
    (psk-set-customlayerbyid
      (strcat (psk-path-getservice prop) "-PIPE")
    )
  )

  (setq $addnew-block-base p
        name               (p-make-block "*U" func param)
        $addnew-block-base '(0 0 0)
  )

  (psk-comp-save
    prop
    (setq en (p-make-insert name p 1. 1. 1. a))
    appid
  )

  (psk-ports-store en ports)
  en
)


;; 弯头类管件 (psk-fit-create-elbow 路径交点 ..)
(defun psk-create-elbow	(p a1 a2 d erf prop / a3 a4 a5 d1 p1 p2 p3 r)
  (if (null erf)
    (setq erf  $psk-erf-create
	  prop (p-set prop (cons "ERF" erf))
    )
  )
  (setq	a3 (p-angle-regular (/ (- a2 a1) 2.0))
	r  (* erf
	      (if (vl-consp d)
		(car d)
		d
	      )
	   )
	d1 (abs (/ r (p-tan a3)))
	p1 (polar p a1 d1)
	p2 (polar p a2 d1)
  )

  (setq	p3 (polar p
		  (+ a1 a3)
		  (abs (/ r (sin a3)))
	   )
	a4 (angle p3 p1)
	a5 (angle p3 p2)
  )

  (list	(psk-fit-create
	  p
	  0.
	  (function p-make-sharparc)
	  (list p3 r a4 a5)
	  (list	(list "0"
		      (polar '(0 0) a1 d1)
		      (polar '(0 0) a1 1.)
		      d
		)
		(list "1"
		      (polar '(0 0) a2 d1)
		      (polar '(0 0) a2 1.)
		      d
		)
	  )
	  prop
	)
	p1
	p2
  )
)
;;

(defun psk-create-reducer (p len a d1 d2 al prop / p1 p2 v1 v2)
  (setq
    p1 (polar p a (/ len -2.))	
    p2 (polar p a (/ len 2.))
    v1 (polar '(0 0) a -1.)
    v2 (polar '(0 0) a 1.)
  )
  (cond	((= "L" al)
	 ;; (setq p2 (polar p2 (+ a $pi/2) (/ (abs (- (car d1) (car d2)) 2.))))
	 ;; 取消abs后 对齐就第1点击看向第2点击靠哪边对齐
	 (setq p2 (polar p2 (+ a $pi/2) (/ (- (car d1) (car d2)) 2.)))
	)
	((= "R" al)
	 (setq p2 (polar p2 (- a $pi/2) (/ (- (car d1) (car d2)) 2.)))
	)
  )


  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p1 p2)
	   )
	  nil
	  (list	(list "0"
		      (mapcar '- p1 p)
		      v1
		      d1
		)
		(list "1"
		      (mapcar '- p2 p)
		      v2
		      d2
		)
	  )
	  prop
	)
	p1
	p2
  )
)
;;


(defun psk-create-tee (p a1 a2 a3 d1 d2 d3 erf prop / p1 p2 v1 v2)
  (if (null erf)
    (setq erf  $psk-erf-create
	  prop (p-set prop (cons "ERF" erf))
    )
  )
  (setq
    p1 (polar p a1 (* erf (max (car d2) (car d3))))
    p2 (polar p a2 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d2))))
    p3 (polar p a3 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d3))))
    v1 (polar '(0 0) a1 1.)
    v2 (polar '(0 0) a2 1.)
    v3 (polar '(0 0) a3 1.)
  )

  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p p1)
	     (p-make-line p p2)
	     (p-make-line p p3)
	   )
	  nil
	  (list	(list "0"
		      (mapcar '- p1 p)
		      v1
		      d1
		)
		(list "1"
		      (mapcar '- p2 p)
		      v2
		      d2
		)
		(list "2"
		      (mapcar '- p3 p)
		      v3
		      d3
		)
	  )
	  prop
	)
	p1
	p2
	p3
  )
)
;;


(defun psk-create-tee2
       (p a1 a3 d1 d2 d3 erf al prop / a2 p1 p2 p3 v1 v2 v3)
  (if (null erf)
    (setq erf  $psk-erf-create
          prop (p-set prop (cons "ERF" erf))
    )
  )
  (setq
    a2 (p-angle-reverse a1)
    p1 (polar p a1 (* erf (car d3)))
    p2 (polar p a2 (+ 100. (* 0.5 (car d3))))
    p3 (polar p a3 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d3))))
    v1 (polar '(0 0) a1 1.)
    v2 (polar '(0 0) a2 1.)
    v3 (polar '(0 0) a3 1.)
  )
  (cond ((= "L" al)
         (setq
           p2 (polar p2 (+ a2 $pi/2) (/ (- (car d1) (car d2)) 2.))
         )
        )
        ((= "R" al)
         (setq
           p2 (polar p2 (- a2 $pi/2) (/ (- (car d1) (car d2)) 2.))
         )
        )
  )
  (list (psk-fit-create
          p
          0.
          '(lambda ()
             (p-make-line p p1)
             (p-make-line p p2)
             (p-make-line p p3)
           )
          nil
          (list (list "0"
                      (mapcar '- p1 p)
                      v1
                      d1
                )
                (list "1"
                      (mapcar '- p2 p)
                      v2
                      d2
                )
                (list "2"
                      (mapcar '- p3 p)
                      v3
                      d3
                )
          )
          prop
        )
        p1
        p2
        p3
  )
)
;;
(defun psk-create-branch (p a1 a2 d1 d2 erf prop / p2 v1 v2)
  ;; 风管样式
  (if (null erf)
    (setq erf  $psk-erf-create
          prop (p-set prop (cons "ERF" erf))
    )
  )
  (setq p2 (polar p a2 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d2))))
        v1 (polar '(0 0) a1 1.)
        v2 (polar '(0 0) a2 1.)
  )

  (list (psk-fit-create
          p
          0.
          '(lambda ()
             (p-make-line p p2)
           )
          nil
          (list (list "0"
                      '(0 0)
                      v1
                      d1
                )
                (list "1"
                      (mapcar '- p2 p)
                      v2
                      d2
                )
          )
          prop
        )
        p2
  )
)
(defun psk-create-pipetee (p a1 a2 d1 d2 prop / p1 p2 p3)
  ;; 水管样式
  (setq p1 (polar p a1 (/ d2 2.))
        p2 (polar p a1 (/ d2 -2.))
        p3 (polar p a2 (/ d1 2.))
  )

  (list (psk-fit-create
          p
          0.
          '(lambda ()
             (p-make-line p p1)
             (p-make-line p p2)
             (p-make-line p p3)
           )
          nil
          (list (list "0"
                      (mapcar '- p1 p)
                      (polar '(0 0) a1 1.)
                      d1
                )
                (list "1"
                      (mapcar '- p2 p)
                      (polar '(0 0) a1 -1.)
                      d1
                )
                (list "2"
                      (mapcar '- p3 p)
                      (polar '(0 0) a2 1.)
                      d2
                )
          )
          prop
        )
        p1
        p2
        p3
  )
)
;;

(defun psk-create-cross	(p a1 a2 a3 a4 d1 d2 d3 d4 erf prop / p1 p2 p3 p4 v1 v2 v3 v4)
  (if (null erf)
    (setq erf  $psk-erf-create
	  prop (p-set prop (cons "ERF" erf))
    )
  )
  (setq
    p1 (polar p a1 (* erf (max (car d3) (car d4))))
    p2 (polar p a2 (+ 100. (* 0.5 (max (car d3) (car d4)))))
    p3 (polar p a3 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d3))))
    p4 (polar p a4 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d4))))
    v1 (polar '(0 0) a1 1.)
    v2 (polar '(0 0) a2 1.)
    v3 (polar '(0 0) a3 1.)
    v4 (polar '(0 0) a4 1.)
  )

  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p p1)
	     (p-make-line p p2)
	     (p-make-line p p3)
	     (p-make-line p p4)
	   )
	  nil
	  (list	(list "0"
		      (mapcar '- p1 p)
		      v1
		      d1
		)
		(list "1"
		      (mapcar '- p2 p)
		      v2
		      d2
		)
		(list "3"
		      (mapcar '- p3 p)
		      v3
		      d3
		)
		(list "4"
		      (mapcar '- p4 p)
		      v4
		      d4
		)
	  )
	  prop
	)
	p1
	p2
	p3
	p4
  )
)
;;

;;;(defun c:en () (setq en (car (entsel))))