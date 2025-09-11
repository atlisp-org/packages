;;; 绘图与统计材料模块分开 求解器生成不同数据适应各自工作 18-4-18
;;; 2021-2-8 更新以适应有实体管件的版本
(defun psk-get-customlayername (id / name)
  (setq name (cadr (assoc id $psk-layer-config)))

  (if (null name)
    (setq name id) ;_ 自定义图层未在layer-configration中定义时，采用图层索引号做为名称
  )
  name
)

;;;
(defun psk-set-customlayerbyid	(id / conf layer name regen)
  (setq	name (psk-get-customlayername id)
	conf (assoc id $psk-layer-config)
  )

  ;; 图层不存在时根据配置创建
  (p-layer-get name (cddr conf))

  (setq $addnew-layer name)
)


;;;;;; TODO: 当前层锁定时 添加失败


;;; BOOKMARK - 管件绘制

;;; 绘制双线风管
(defun psk-draw-duct (p1 p2 d / a)
  (setq $addnew-layer $psk-layer-duct)
  
  (setq a (angle p1 p2))
  (p-make-line
    (polar p1 (- a $pi/2) (* 0.5 d))
    (polar p2 (- a $pi/2) (* 0.5 d))
  )
  (p-make-line
    (polar p1 (+ a $pi/2) (* 0.5 d))
    (polar p2 (+ a $pi/2) (* 0.5 d))
  )
)
;;;  绘制双线弯头 p弯头两端管道中心线交点, a1,2 两端管道方向, d 管径, r 弯头半径, fl 法兰伸出长度
;;;  (psk-draw-elbow (getpoint) 0 $pi/2 500. 400.)
(defun psk-draw-elbow (p a1 a2 d r fl / a3 a4 a5 d1 p1 p2 p3 l)
  (setq a3 (p-angle-regular (/ (- a2 a1) 2.0))
        d1 (abs (/ r (p-tan a3)))
        p1 (polar p a1 d1) ;_ p1 p2 弯头两管端
        p2 (polar p a2 d1)
        p3 (polar p (+ a1 a3) (abs (/ r (sin a3)))) ;_ 两管端垂线交点
        a4 (angle p3 p1)
        a5 (angle p3 p2)
        l  (* 0.5 d)
  )
  ;; 两端直线
  (setq $addnew-layer $psk-layer-cpnt)
  (p-make-line
    (polar p1 (- a1 $pi/2) (+ fl l))
    (polar p1 (+ a1 $pi/2) (+ fl l))
  )
  (p-make-line
    (polar p2 (- a2 $pi/2) (+ fl l))
    (polar p2 (+ a2 $pi/2) (+ fl l))
  )
  ;; 内外弧线
  (setq $addnew-layer $psk-layer-duct)
  (p-make-sharparc p3 (- r (* 0.5 d)) a4 a5)
  (p-make-sharparc p3 (+ r (* 0.5 d)) a4 a5)
)
;;; TODO: 图层锁定处理
;;;
;;;
;;;  绘制异径管, p1 p2两端点, a 两端管道方向, w1 w2 管径
;;;  (psk-draw-reducer  (getpoint) (getpoint) 0. 500. 250.)
(defun psk-draw-reducer	(p1 p2 a w1 w2 / p11 p12 p21 p22)
  (setq	p11 (polar p1 (- a $pi/2) (* 0.5 w1))
	p12 (polar p1 (+ a $pi/2) (* 0.5 w1))
	p21 (polar p2 (- a $pi/2) (* 0.5 w2))
	p22 (polar p2 (+ a $pi/2) (* 0.5 w2))
  )
  (setq $addnew-layer $psk-layer-cpnt)
  (p-make-line (polar p11 (- a $pi/2) $PSK-duct-flextend) (polar p12 (+ a $pi/2) $PSK-duct-flextend))
  (p-make-line (polar p21 (- a $pi/2) $PSK-duct-flextend) (polar p22 (+ a $pi/2) $PSK-duct-flextend))

  (setq $addnew-layer $psk-layer-duct)
  (p-make-line p11 p21)
  (p-make-line p12 p22)
)
;;; 垂直燕尾三通 （两分支均与主管垂直）
;;; (psk-draw-tee (getpoint)(getpoint)(getpoint) $pi/2 0 pi 1000 500 500 400 400)
(defun psk-draw-tee (p1 p2 p3 a1 a2 a3 d1 d2 d3 erf / a4 a8 c l1 l2 l3 p4 p5 p6 x y)
  (setq	l1 (* erf d2)
	l2 (+ (* (- erf 0.5) d2) (* 0.5 d1))
	l3 (+ (* (- erf 0.5) d3) (* 0.5 d1))
  )

  (setq	p4 (polar p2 a1 l1)
	p5 (polar p3 a1 (* erf d3))

	a4 (p-angle-reverse a1)
	a8 (angle p4 p5)

	c  (distance p4 p5)
	x  (/ (- (* (* (+ erf 0.5) d2) (* (+ erf 0.5) d2))
		 (* (* (+ erf 0.5) d3) (* (+ erf 0.5) d3))
		 (* c (- c))
	      )
	      c
	      2.
	   )
	y  (- (* (* (+ erf 0.5) d2) (* (+ erf 0.5) d2)) (* x x))
  )

  (if (> y 0)
    (if	(or (< (- pi) (- a1 a8) 0) (< pi (- a1 a8) $2pi))
      (setq p6 (polar (polar p4 a8 x) (+ $pi/2 a8) (sqrt y)))
      (setq p6 (polar (polar p4 a8 x) (- a8 $pi/2) (sqrt y)))
    )
    (setq p6 p1)
  )

  ;; 两端直线
  (setq $addnew-layer $psk-layer-cpnt)
  (p-make-line
    (polar p1 a2 (+ (* 0.5 d1) $PSK-duct-flextend))
    (polar p1 a3 (+ (* 0.5 d1) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p2 a1 (+ (* 0.5 d2) $PSK-duct-flextend))
    (polar p2 a1 (- (+ (* 0.5 d2) $PSK-duct-flextend)))
  )
  (p-make-line
    (polar p3 a1 (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 a1 (- (+ (* 0.5 d3) $PSK-duct-flextend)))
  )

  (setq $addnew-layer $psk-layer-duct)
  (if (not (equal d3 d2 1.))
    (p-make-line
      (polar p1 a3 (* 0.5 d1))
      (polar p5 a2 (* (- erf 0.5) d3))
    )
  )

  (p-make-sharparc p4 (* (- erf 0.5) d2) a3 a4)
  (p-make-sharparc p4 (* (+ erf 0.5) d2) (angle p4 p6) a4)
  (p-make-sharparc p5 (* (- erf 0.5) d3) a4 a2)
  (p-make-sharparc p5 (* (+ erf 0.5) d3) a4 (angle p5 p6))
)
;;;
;;;
;;; 三通 1 2在一直线上 3与其垂直
;; (psk-draw-tee-s '(0 0) '(0 360) '(310 160) (- $pi/2) $pi/2 0 500 400 200 0.8 "C")
(defun psk-draw-tee-s (p1 p2 p3 a1 a2 a3 d1 d2 d3 erf al / a3r l p11 p12 p21 p22 p4 p5 x)
  (setq	a3r (p-angle-reverse a3)
	p11 (polar p1 a3r (* 0.5 d1))
	p12 (polar p1 a3 (* 0.5 d1))

	p21 (polar p2 a3r (* 0.5 d2))
	p22 (polar p2 a3 (* 0.5 d2))

	l   (+ (* (- erf 0.5) d3) (* 0.5 d1))

	p4  (polar p1 a3 l)
  )


  (cond	((or (and (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
		  (= "R" al)
	     )
	     (and (equal 0. (p-angle-include (+ a1 $pi/2) a3) 1e-3)
		  (= "L" al)
	     )
	 )
	 (setq x  (+ (- d1 d2) (* (- erf 0.5) d3))
	       x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	       x  (+ 100. (* (+ erf 0.5) d3) (- x))
	       p5 (polar p22 a1 x)
	 )
	)
	((or (and (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
		  (= "L" al)
	     )
	     (and (equal 0. (p-angle-include (+ a1 $pi/2) a3) 1e-3)
		  (= "R" al)
	     )
	 )
	 (setq x  (* (- erf 0.5) d3)
	       x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	       x  (+ 100. (* (+ erf 0.5) d3) (- x))
	       p5 (polar p22 a1 x)
	 )
	)
	(t
	 (setq x  (+ (* (- erf 0.5) d3) (/ d1 2.) (/ d2 -2.))
	       x  (sqrt (abs (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x))))
	       x  (+ 100. (* (+ erf 0.5) d3) (- x))
	       p5 (polar p22 a1 x)
	 )
	)
  )
(setq $addnew-layer $psk-layer-cpnt)
  ;; 接管口
  (p-make-line
    (polar p1 (- a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
    (polar p1 (+ a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p2 (+ a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
    (polar p2 (- a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p3 (- a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 (+ a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
  )

  (setq $addnew-layer $psk-layer-duct)
  ;; 直通段边线
;;;  (if (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;    (progn (p-make-line p21 p5)
;;;	   (p-make-line p22 p12)
;;;    )
;;;    (progn (p-make-line p21 p11)
;;;	   (p-make-line p22 p5)
;;;    )
;;;  )
  (p-make-line p11 p21)
  (p-make-line p22 p5)

  ;; 垂直段弧线
  (p-make-sharparc p4 (* (- erf 0.5) d3) a2 (p-angle-reverse a3))
  (p-make-sharparc p4 (* (+ erf 0.5) d3) a2 (angle p4 p5))
)

;; 水管分支
(defun psk-draw-tee-p (p1 p2 p3 a1 a2 a3 d1 d2 d3 /)
  (setq p11 (polar p1 a3 (* -0.5 d1))
        p21 (polar p2 a3 (* -0.5 d1))
  )

  (setq x   (sqrt (- (p-sqr d1) (p-sqr d3)))
        x   (- d1 x)
        x   (* x -0.5)
        p31 (polar p3 a3 x)
        p12 (polar p1 a3 (* 0.5 d1))
        p22 (polar p2 a3 (* 0.5 d1))
  )

  (p-make-line p11 p21)

  (p-make-line p12 p31)
  (p-make-line p22 p31)
)
;;; 绘制直管上垂直分支处的外观
(defun psk-draw-branch (p3 a1 a3 d1 d3 erf / a2 p4 p5 x)
  (setq	p4 (polar p3 a1 (* erf d3))
	x  (* (- erf 0.5) d3)
	x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	x  (- x (* erf d3))
	a2 (p-angle-reverse a1)
	p5 (polar (polar p3 (p-angle-reverse a3) (* (- erf 0.5) d3)) a2 x)
  )

  ;; 接管口
  (setq $addnew-layer $psk-layer-cpnt)
  (p-make-line
    (polar p3 (- a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 (+ a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
  )

  ;; 垂直段弧线
  (setq $addnew-layer $psk-layer-duct)
  (p-make-sharparc p4 (* (- erf 0.5) d3) a2 (p-angle-reverse a3))
  (p-make-sharparc p4 (* (+ erf 0.5) d3) a2 (angle p4 p5))
)
;;; 任意角度三通
;;;(defun psk-draw-tee-s3 (a1 a2 a3 d1 d2 d3 erf)
;;;  (setq
;;;    a4	(abs (- pi (abs (- a1 a2))))
;;;    a5	(abs (- pi (abs (- a1 a3))))
;;;
;;;    l1	(/ (- (* erf d2) (* (cos a4) (+ (* (- erf 0.5) d2) (* 0.5 d1)))) (sin a4))
;;;    l2	(if (equal a4 $pi/2 1e-6)
;;;	  (+ (* (- erf 0.5) d2) (* 0.5 d1))
;;;	  (- (* (p-tan a4) erf d2) (/ l1 (cos a4)))
;;;	)
;;;
;;;    l1a	(/ (- (* erf d3) (* (cos a5) (+ (* (- erf 0.5) d3) (* 0.5 d1)))) (sin a5))
;;;    l3	(if (equal a5 $pi/2 1e-6)
;;;	  (+ (* (- erf 0.5) d3) (* 0.5 d1))
;;;	  (- (* (p-tan a5) erf d3) (/ l1a (cos a5)))
;;;	)
;;;
;;;    p1	(polar p a1 l1)
;;;    p2	(polar p a2 l2)
;;;    p3	(polar p a3 l3)
;;;  )
;;;)
(defun psk-draw-cross
       (p1 p2 p3 p4 a1 a2 a3 a4 d1 d2 d3 d4 erf / p11 p12 p21 p22 p5 p6 p7 p8 x pad l)
  (setq	p11 (polar p1 a3 (* 0.5 d1))
	p12 (polar p1 a4 (* 0.5 d1))

	p21 (polar p2 a3 (* 0.5 d2))
	p22 (polar p2 a4 (* 0.5 d2))

	p5  (polar p1 a3 (+ (* (- erf 0.5) d3) (* 0.5 d1)))
	p7  (polar p1 a4 (+ (* (- erf 0.5) d4) (* 0.5 d1)))

	pad (* erf (abs (- d4 d3)))
	l   (+ 100. (* (+ erf 0.5) (max d3 d4)))
  )

  (if (> d4 d3)
    (setq p5 (polar p5 a2 pad))
    (setq p7 (polar p7 a2 pad))
  )

  (setq	x  (+ (* (- erf 0.5) d3) (/ d1 2.) (/ d2 -2.))
	x  (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x))
	x  (sqrt (abs x))
	x  (- l
	      x
	      (if (> d4 d3)
		pad
		0.
	      )
	   )
;;;	p6 (if (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;	     (polar p21 a1 x)
;;;	     (polar p22 a1 x)
;;;	   )
	p6 (polar p21 a1 x)

	x  (+ (* (- erf 0.5) d4) (/ d1 2.) (/ d2 -2.))
	x  (- (p-sqr (* (+ erf 0.5) d4)) (p-sqr x))
	x  (sqrt (abs x))
	x  (- l
	      x
	      (if (> d3 d4)
		pad
		0.
	      )
	   )
;;;	p8 (if (equal 0. (p-angle-include (- a1 $pi/2) a4) 1e-3)
;;;	     (polar p21 a1 x)
;;;	     (polar p22 a1 x)
;;;	   )
	p8 (polar p22 a1 x)
  )

  ;; 接管口
  (setq $addnew-layer $psk-layer-cpnt)
  (p-make-line
    (polar p1 (- a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
    (polar p1 (+ a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p2 (+ a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
    (polar p2 (- a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p3 (- a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 (+ a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p4 (- a4 $pi/2) (+ (* 0.5 d4) $PSK-duct-flextend))
    (polar p4 (+ a4 $pi/2) (+ (* 0.5 d4) $PSK-duct-flextend))
  )

  (setq $addnew-layer $psk-layer-duct)
  ;; 直通段边线
;;;  (if (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;    (progn
;;;      (p-make-line p21 p6)
;;;      (p-make-line p22 p8)
;;;    )
;;;    (progn
;;;      (p-make-line p21 p8)
;;;      (p-make-line p22 p6)
;;;    )
;;;  )
  (p-make-line p21 p6)
  (p-make-line p22 p8)

  (if (> pad 0.)
    (if	(> d4 d3)
      (p-make-line p11 (polar p11 a2 pad))
      (p-make-line p12 (polar p12 a2 pad))
    )
  )
  
;;;  (if (> d3 d4)
;;;    (if	(equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;      (p-make-line p12 (polar p12 a2 pad))
;;;      (p-make-line p11 (polar p11 a2 pad))
;;;    )
;;;  )

  ;; 垂直段弧线
  (p-make-sharparc p5 (* (- erf 0.5) d3) a2 (p-angle-reverse a3))
  (p-make-sharparc p5 (* (+ erf 0.5) d3) a2 (angle p5 p6))

  (p-make-sharparc p7 (* (- erf 0.5) d4) a2 (p-angle-reverse a4))
  (p-make-sharparc p7 (* (+ erf 0.5) d4) a2 (angle p7 p8))
)
;;;;;;(defun c:tt (/ en p ss view)
;;;;;;  (d-solve-selection (ssget '((-3 ("MYPROPS_ROUTER,MYPROPS_PARTSRC")))))
;;;;;;  (setq view (p-get-viewdir))
;;;;;;  (setq p (trans (getpoint "指定基点:")  1 0) 
;;;;;;	p (p-wcs->view p view))
;;;;;;
;;;;;;  (setq	en (entlast)
;;;;;;	ss (ssadd)
;;;;;;  )
;;;;;;
;;;;;;  (foreach e $PSK-solve-result
;;;;;;    (d-part-draw e view)
;;;;;;  )
;;;;;;
;;;;;;  (while (setq en (entnext en))
;;;;;;    (ssadd en ss)
;;;;;;  )
;;;;;;
;;;;;;  (command "_.MOVE" ss "" p)
;;;;;;;;;  (d-solve-selection (ssget '((-3 ("MYPROPS_ROUTER,MYPROPS_PART")))))
;;;;;;  (princ)
;;;;;;)
(defun psk-comp-draw (comp / d d1 en elast p ports pts r uid)
  (setq tp (psk-comp-gettype comp))

  (if (psk-path-getservice comp)
    (progn
      (if (wcmatch tp "DUCT*")
        (progn
          (setq $psk-layer-duct (psk-set-customlayerbyid
                                  (strcat (psk-path-getservice comp)
                                          "-DUCT"
                                  )
                                )
                $psk-layer-cpnt
                                (psk-set-customlayerbyid
                                  (strcat (psk-path-getservice comp) "-CPNT")
                                )
          )
        )
        (psk-set-customlayerbyid
          (strcat (psk-path-getservice comp) "-PIPE")
        )
      )
    )
  )

  (setq en            (p-get comp -1)
        uid           (cons 1071 (p-uid))
        $addnew-xdata (list "PSK-DRAFT" uid)
        elast         (entlast)
  )

  (cond	((= "PATH" (psk-comp-getname comp))
          (cond ((wcmatch tp "DUCT*")
                 (setq pts (p-dxf en '(10 11))
                       r   (psk-path-getportsize comp)
                 )

                 (if (or (null r)
                         (and (listp r) (null (car r)))
                     )
                   (princ "\n未指定管道规格")
                   (psk-draw-duct
                     (car pts)
                     (cadr pts)
                     (if (vl-consp r)
                       ;; 风管宽
                       (car r)
                       r
                     )
                   )
                 )
                )
          )
        )
	((= "PARTSET" (psk-comp-getname comp))
	  (psk-keypack-draw comp)
	)
	((= "PART" (psk-comp-getname comp))
	 (cond ((wcmatch tp "DUCT*-EL")
                 (setq p     (p-dxf en 10)
                       ports (psk-comp-getports comp)
                       pts   (mapcar 'psk-port-angle ports)
                       d     (psk-port-size (car ports))
                       d1    (if (vl-consp d)
                               (car d)
                               d
                             )
                       ;;TODO
                       r     (* (p-get comp "ERF") d1)
                 )
                 (if (vl-consp d)
                   (psk-draw-elbow
                     p
                     (car pts)
                     (cadr pts)
                     d1
                     r
                     $psk-duct-flextend
                   )
                   (psk-draw-elbow
                     p
                     (car pts)
                     (cadr pts)
                     d1
                     r
                     0.
                   )
                 )
               )
	       ((wcmatch tp "*-RED")
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-reducer
		   (psk-port-pos (car ports))
		   (psk-port-pos (last ports))
		   (psk-port-angle (last ports))
		   (car (psk-port-size (car ports)))
		   (car (psk-port-size (last ports)))
		 )
	       )
	       ((wcmatch tp "*-TEE")
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-tee
		   (psk-port-pos (car ports))
		   (psk-port-pos (cadr ports))
		   (psk-port-pos (last ports))
		   (psk-port-angle (car ports))
		   (psk-port-angle (cadr ports))
		   (psk-port-angle (last ports))
		   (car (psk-port-size (car ports)))
		   (car (psk-port-size (cadr ports)))
		   (car (psk-port-size (last ports)))
		   (p-get comp "ERF")
		 )
	       )
	       ((wcmatch tp "*-TEES")
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-tee-s
		   (psk-port-pos (car ports))
		   (psk-port-pos (cadr ports))
		   (psk-port-pos (last ports))
		   (psk-port-angle (car ports))
		   (psk-port-angle (cadr ports))
		   (psk-port-angle (last ports))
		   (car (psk-port-size (car ports)))
		   (car (psk-port-size (cadr ports)))
		   (car (psk-port-size (last ports)))
		   (p-get comp "ERF")
		   (p-get comp "AL")
		 )
	       )
               ((wcmatch tp "*-TEEP")
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-tee-p
                   (psk-port-pos (car ports))
                   (psk-port-pos (cadr ports))
                   (psk-port-pos (last ports))
                   (psk-port-angle (car ports))
                   (psk-port-angle (cadr ports))
                   (psk-port-angle (last ports))
                   (psk-port-size (car ports))
                   (psk-port-size (cadr ports))
                   (psk-port-size (last ports))
                 )
	       )
	       ((wcmatch tp "*-BR")
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-branch
		   (psk-port-pos (cadr ports))
		   (psk-port-angle (car ports))
		   (psk-port-angle (cadr ports))
		   (car (psk-port-size (car ports)))
		   (car (psk-port-size (cadr ports)))
		   (p-get comp "ERF")
		 )
	       )
	       ((wcmatch tp "*-CR")
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-cross
		   (psk-port-pos (car ports))
		   (psk-port-pos (cadr ports))
		   (psk-port-pos (caddr ports))
		   (psk-port-pos (last ports))
		   (psk-port-angle (car ports))
		   (psk-port-angle (cadr ports))
		   (psk-port-angle (caddr ports))
		   (psk-port-angle (last ports))
		   (car (psk-port-size (car ports)))
		   (car (psk-port-size (cadr ports)))
		   (car (psk-port-size (caddr ports)))
		   (car (psk-port-size (last ports)))
		   (p-get comp "ERF")
		 )
	       )
	 )
	)
  )
  (p-xdata-set en "PSK-DRAWID" (list uid))
  (setq $addnew-xdata nil)
  (p-ss->enames (p-enames-after elast nil))
)


;; (psk-comps-draw (psk-comps-ssget))
(defun psk-comps-draw (comps /)
  (foreach comp	comps
    (psk-comp-draw comp)
  )
)
;;





;;; (psk-comp-getdrafts (psk-comp-load (car (entsel))) nil)
(defun psk-comp-getdrafts (comp drafts / r uid)
  (setq uid (cdar (p-xdata-get (p-get comp -1) "PSK-DRAWID")))
  (foreach draft drafts
    (if	(= uid (cdar (p-xdata-get draft "PSK-DRAFT")))
      (setq r (cons draft r))
    )
  )
  r
)


(defun psk-drafts-fromviewport (/ ss)
  ;;(p-ss->enames
  (setq	ss
;;;	 (ssget	"W"
;;;		(trans (getvar "EXTMIN") 0 1)
;;;		(trans (getvar "EXTMAX") 0 1)
;;;		'((-3 ("PSK-DRAFT")))
;;;	 )
	 ;; 在UCS下 PLAN修改过的情况下上面的代码无法选到正确的内容2022-3-18
	 (ssget	"_W"
		(getvar "VSMIN")
		(getvar "VSMAX")
		'((-3 ("PSK-DRAFT")))
	 )
  )
  (if (null ss)
    (ssadd)
    ss
  )
)


;;;(psk-comp-redraw (psk-comp-load (car (entsel))))
(defun psk-comp-redraw (comp)
  (if comp
    (progn
;;;      (foreach e (psk-comp-getdrafts comp)
;;;	(entdel e)
;;;      )
      (psk-comp-draw comp)
    )
  )
)
(defun psk-comp-redraw1 (comp)
  (if (null $psk-drafts)
    (setq $psk-drafts (ssadd))
  )
  
  (if comp
    (progn
      (foreach e (psk-comp-getdrafts comp (p-ss->enames $psk-drafts))
        (entdel e)
        (setq $psk-drafts (ssdel e $psk-drafts))
      )

      (foreach e (psk-comp-draw comp)
        (setq $psk-drafts (ssadd e $psk-drafts))
      )
    )
  )
)
;;
(defun psk-cmd-redraw (/ comps ens)
  (setq ens (p-ss->enames (ssget '((-3 ("PSK-PATH,PSK-PART,PSK-PARTSET,PSK-DRAFT"))))))
  (p-timer-start)
  (p-startundomark)
  (foreach en ens
    (if	(p-xdata-get-inner en "PSK-DRAFT")
      (entdel en)
      (setq comps (cons (psk-comp-load en) comps))
    )
  )
  (psk-comps-draw comps)
  (p-endundomark)
;;;    (princ
;;;      (strcat "\r生成进度 " (itoa (1+ n)) " / " (itoa count))
;;;    )
;;;  )

  (princ (strcat
	   "\n绘制 "
	   (itoa (length comps))
	   " 个管件, 耗时 "
	   (itoa (p-timer-stop))
	   ;;(rtos (/ (p-timer-stop) 1000.) 2 0)
	   " ms"
	 )
  )
)


;;;(defun LM:arc->bulge ( c a1 a2 r )
;;;    (list
;;;        (polar c a1 r)
;;;        (   (lambda ( a ) (/ (sin a) (cos a)))
;;;            (/ (rem (+ pi pi (- a2 a1)) (+ pi pi)) 4.0)
;;;        )
;;;        (polar c a2 r)
;;;    )
;;;)
;;;
;;;(defun c:tt ( / e )
;;;    (if (setq e (ssget "_+.:E:S" '((0 . "ARC"))))
;;;        (progn
;;;            (setq e (entget (ssname e 0)))
;;;            (entmake
;;;                (append
;;;                   '(
;;;                        (000 . "LWPOLYLINE")
;;;                        (100 . "AcDbEntity")
;;;                        (100 . "AcDbPolyline")
;;;                        (090 . 2)
;;;                        (070 . 0)
;;;                    )
;;;                    (mapcar 'cons '(10 42 10)
;;;                        (LM:arc->bulge
;;;                            (cdr (assoc 10 e))
;;;                            (cdr (assoc 50 e))
;;;                            (cdr (assoc 51 e))
;;;                            (cdr (assoc 40 e))
;;;                        )
;;;                    )
;;;                )
;;;            )
;;;        )
;;;    )
;;;    (princ)
;;;)

;;
