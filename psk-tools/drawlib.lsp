(setq $psk-block-base  '(0 0)
      $psk-block-angle 0.
)

;; P1 P3
;; P2 P4
(defun psk-draw-rectangle-inner	(p a l h / p1 p2 p3 p4)
  (setq	p1 (polar p (+ a $pi/2) (* h 0.5))
	p2 (polar p
		  (+ a $pi/2)
		  (* h -0.5)
	   )
	p3 (polar p1 a l)
	p4 (polar p2 a l)
  )
  (p-make-line p1 p2)
  (p-make-line p3 p4)
  (p-make-line p1 p3)
  (p-make-line p2 p4)
  (list p1 p2 p3 p4)
)
;; 创建宽l 高h 原点在左边中点上的简单矩形
;; P1 P3
;; P2 P4
;; (psk-draw-rectangle 200 100)
(defun psk-draw-rectangle (l h / p1 p2 p3 p4)
  (psk-draw-rectangle-inner $psk-block-base $psk-block-angle l h)
)
;; P1 P3
;; P2 P4
(defun psk-draw-twoside (l h / p1 p2 p3 p4)
  (setq	p1 (polar $psk-block-base (+ $psk-block-angle $pi/2) (* h 0.5))
	p2 (polar $psk-block-base
		  (+ $psk-block-angle $pi/2)
		  (* h -0.5)
	   )
	p3 (polar p1 $psk-block-angle l)
	p4 (polar p2 $psk-block-angle l)
  )
  (p-make-line p1 p3)
  (p-make-line p2 p4)
)
;; 创建宽l 高h 原点矩形中心的矩形
;; P1 P3
;; P0
;; P2 P4
;; (psk-draw-rectangle-c 200 100)
(defun psk-draw-rectangle-c (l h / a p0 p1 p2 p3 p4)
  (setq	a  $psk-block-angle
	p0 (polar $psk-block-base a (* l -0.5))
	p1 (polar p0 (+ a $pi/2) (* h 0.5))
	p2 (polar p0 (+ a $pi/2) (* h -0.5))
	p3 (polar p1 a l)
	p4 (polar p2 a l)
  )
  (p-make-line p1 p2)
  (p-make-line p3 p4)
  (p-make-line p1 p3)
  (p-make-line p2 p4)
  (list p1 p2 p3 p4)
)
;; 创建宽a 高b 矩形中心在(x, y) 的简单矩形
;;;(defun psk-draw-rectangle-at (x y a b /)
;;;  (p-make-polyline
;;;    (list (list (- x (/ a 2.)) (+ y (/ b 2.)))
;;;	  (list (- x (/ a 2.)) (- y (/ b 2.)))
;;;	  (list (+ x (/ a 2.)) (- y (/ b 2.)))
;;;	  (list (+ x (/ a 2.)) (+ y (/ b 2.)))
;;;    )
;;;    1
;;;    0.
;;;  )
;;;)


;; 一般阀门 l = 阀体直径 d = 阀体长度
;; (psk-draw-valvebody 200 100)
(defun psk-draw-valvebody (l d / p1 p2 p3 p4)
  (setq	p1 (polar $psk-block-base (+ $psk-block-angle $pi/2) (* d 0.5))
	p2 (polar $psk-block-base
		  (+ $psk-block-angle $pi/2)
		  (* d -0.5)
	   )
	p3 (polar p1 $psk-block-angle l)
	p4 (polar p2 $psk-block-angle l)
  )
  (p-make-line p1 p2)
  (p-make-line p3 p4)
  (p-make-line p1 p4)
  (p-make-line p2 p3)
)


;; 截止阀门顶视 d = 阀体直径 l = 阀体长度 c = 手轮直径
;; (psk-draw-global-valve-top 200 100 50)
(defun psk-draw-global-valve-top (l d c /)
  (psk-draw-valvebody l d)
  (p-make-circle
    (polar $psk-block-base $psk-block-angle (* l 0.5))
    (* c 0.5)
  )
)
;;



;; 闸阀门前视 d = 阀体直径 l = 阀体长度 c = 手轮直径 hw = 阀杆高度
;; (psk-draw-gate-valve-fr 200 100 50 100)
(defun psk-draw-gate-valve-fr (l d c hw / p1 p2)
  (psk-draw-valvebody l d)

  (setq	p1 (polar $psk-block-base $psk-block-angle (* l 0.5))
	p2 (polar p1 (+ $psk-block-angle $pi/2) hw)
  )

  (p-make-line p1 p2)
  (p-make-line
    (polar p2 $psk-block-angle (* c 0.5))
    (polar p2 $psk-block-angle (* c -0.5))
  )
)
;;

;; 截止阀门前视 d = 阀体直径 l = 阀体长度 c = 手轮直径 hw = 阀杆高度
;; (psk-draw-global-valve-fr 200 100 50 100)
(defun psk-draw-global-valve-fr	(l d c hw / p1 p3 p5)
  (psk-draw-gate-valve-fr l d c hw)

  (setq	p1 (polar $psk-block-base (+ $psk-block-angle $pi/2) (* d 0.5))
	p3 (polar p1 $psk-block-angle l)
	p5 (polar (polar $psk-block-base $psk-block-angle (* l 0.5))
		  (+ $psk-block-angle $pi/2)
		  hw
	   )
  )

  (p-make-line p5 p1)
  (p-make-line p5 p3)
)
;;




;;; HVAC


;; (psk-draw-hvacflange '(0 0) 0 500 50)
(defun psk-draw-hvacflange (p a w fl / a1 len)
  (setq	a1  (+ $pi/2 a)
	len (+ (* w 0.5) fl)
  )
  (p-make-line
    (polar p a1 (- len))
    (polar p a1 len)
  )
)
(defun psk-draw-hvacinline (p a l w fl / a1 len)
  (setq $addnew-linetype "BYBLOCK")

  (psk-draw-hvacflange $psk-block-base $psk-block-angle w fl)
  (psk-draw-twoside l w)
  (psk-draw-hvacflange
    (polar $psk-block-base $psk-block-angle l)
    $psk-block-angle
    w
    fl
  )
  (setq $addnew-linetype "BYLAYER")
)
;; 常开防火阀 w = 宽 l = 厚 fl = 法兰伸出长度
;; (psk-draw-firedamper-o 320 500 50)
(defun psk-draw-firedamper-o (l w fl /)
  (setq $addnew-linetype "BYBLOCK")

  (psk-draw-hvacinline $psk-block-base $psk-block-angle l w fl)
  (p-make-circle
    (polar $psk-block-base $psk-block-angle (* l 0.5))
    75.
  )
  (p-make-line
    $psk-block-base
    (polar $psk-block-base $psk-block-angle l)
  )
  (setq $addnew-linetype "BYLAYER")
)
;; 常闭防火阀
;; (psk-draw-firedamper-c 320 500 50)
(defun psk-draw-firedamper-c (l w fl / p1)
  (setq	$addnew-linetype "BYBLOCK"
	p1		 (polar $psk-block-base $psk-block-angle (* l 0.5))
  )

  (psk-draw-hvacinline $psk-block-base $psk-block-angle l w fl)
  (p-make-circle
    (polar $psk-block-base $psk-block-angle (* l 0.5))
    75.
  )
  (p-make-line
    (polar p1 (+ $pi/2 $psk-block-angle) (* l 0.5))
    (polar p1 (+ $pi/2 $psk-block-angle) (* l -0.5))
  )
  (setq $addnew-linetype "BYLAYER")
)
;; 止回阀
;; (psk-draw-checkvalve 320 500 50)
(defun psk-draw-checkvalve (l w fl / p1)
  (psk-draw-hvacinline
    $psk-block-base $psk-block-angle l w fl)
  (p-make-circle
    (polar $psk-block-base $psk-block-angle (* l 0.5))
    50.
  )
  (setq p1 (polar $psk-block-base $psk-block-angle (* l 0.5)))
  (p-make-line
    (polar p1 (+ $pi/4 $psk-block-angle) 50)
    (polar p1 (+ $pi/4 $psk-block-angle) 150)
  )
  (p-make-line
    (polar p1 (+ (- $pi/4) $psk-block-angle) 50)
    (polar p1 (+ (- $pi/4) $psk-block-angle) 150)
  )
)
(defun psk-draw-blade (p a)
  (p-make-circle p 50.)
  (p-make-line
    (polar p (+ $pi/4 a) 150)
    (polar p (+ $pi/4 a) -150)
  )
)
;; 调节阀
;; (psk-draw-dvalve 320 500 50)
(defun psk-draw-dvalve	(l w fl / p1)
  (psk-draw-hvacinline
    $psk-block-base $psk-block-angle l w fl)

  (setq p1 (polar $psk-block-base $psk-block-angle (* l 0.5)))
  (psk-draw-blade p1 $psk-block-angle)
)
;; 多叶调节阀
;; (psk-draw-ddvalve 320 500 50)
(defun psk-draw-ddvalve (l w fl / p1)
  (psk-draw-hvacinline
    $psk-block-base $psk-block-angle l w fl)

  (setq p1 (polar $psk-block-base $psk-block-angle (* l 0.5)))
  (psk-draw-blade (polar p1 (+ $pi/2 $psk-block-angle) (* w 0.25)) $psk-block-angle)
  (psk-draw-blade (polar p1 (+ $pi/2 $psk-block-angle) (* w -0.25)) (+ $pi/2 $psk-block-angle))
)
;;;(psk-draw-dvalve2 320 500 50)
(defun psk-draw-dvalve2	(l w fl / p1 p2)
  (psk-draw-hvacinline
    $psk-block-base $psk-block-angle l w fl)

  (setq p1 (polar $psk-block-base $psk-block-angle (* l 0.5)))

  (p-make-line
    (setq p2 (polar p1 (+ $pi/2 $psk-block-angle) (+ 150 (* w 0.5))))
    (polar p1 (+ $pi/2 $psk-block-angle) (* w -0.5))
  )

  (p-make-line p2 (polar p2 $psk-block-angle 150))
)
;; 消音器
;; (psk-draw-noisereducer 320 500 50)
(defun psk-draw-noisereducer (l w fl / len p1)
  (psk-draw-rectangle l (+ (* 2 fl) w))
  (setq	p1  (polar $psk-block-base $psk-block-angle (* l 0.3))
	len (+ (* 0.5 w) fl)
  )
  (p-make-line
    p1
    (polar p1 (+ $pi/2 $psk-block-angle) len)
  )
  (setq p1 (polar $psk-block-base $psk-block-angle (* l 0.7)))
  (p-make-line
    p1
    (polar p1 (+ $pi/2 $psk-block-angle) (- len))
  )
)
;; 双层百叶风口
;; (psk-draw-doubleshutter 300 200)
(defun psk-draw-doubleshutter (a b / r)
  (setq $addnew-linetype "BYBLOCK")
  (setq r (psk-draw-rectangle-c a b))

  (p-make-line (car r) (last r))
  (p-make-line (cadr r) (caddr r))
  (setq $addnew-linetype "BYLAYER")
)
;; 单层百叶风口
;; (psk-draw-singleshutter 300 200)
(defun psk-draw-singleshutter (a b / r)
  (setq $addnew-linetype "BYBLOCK")
  (setq r (psk-draw-rectangle-c a b))

  (p-make-line (car r) (last r))
  (setq $addnew-linetype "BYLAYER")
)

;; 侧送风口 w = 风管宽度 a1 = 风口方向
;; P1    P2
;; P3 P1 P4
;;          W/2
;;    P -> a
;; (psk-draw-sideout 800 150 500 $pi/2)
(defun psk-draw-sideout	(a b w a1)
  (setq $addnew-linetype "BYBLOCK")
  (setq p1 (polar $psk-block-base a1 (* w 0.5)))
  (psk-draw-rectangle-inner p1 a1 b a)
  
  (setq $addnew-linetype "BYLAYER")
)
;; 天圆地方风管
;; (psk-draw-stor 800 500 150 50)
(defun psk-draw-stor (w d l fl / p1 p2 p3 p4 p5)
  (setq	p1 (polar $psk-block-base (+ $psk-block-angle $pi/2) (* w 0.5))
	p2 (polar $psk-block-base
		  (+ $psk-block-angle $pi/2)
		  (* w -0.5)
	   )
	p5 (polar $psk-block-base $psk-block-angle l)
	p3 (polar p5 (+ $psk-block-angle $pi/2) (* d 0.5))
	p4 (polar p5 (+ $psk-block-angle $pi/2) (* d -0.5))
  )
  (psk-draw-hvacflange $psk-block-base $psk-block-angle w fl)
  (psk-draw-hvacflange p5 $psk-block-angle d fl)
  (p-make-line p1 p3)
  (p-make-line p1 p5)
  (p-make-line p2 p4)
  (p-make-line p2 p5)
)
;; 轴流风机
;; (psk-draw-fan 800 500)
(defun psk-draw-fan (l d / p1 p2 p3 p4)
  (psk-draw-rectangle l d)
  (setq	p1 (polar $psk-block-base $psk-block-angle (* 0.2 l))
	p2 (polar $psk-block-base $psk-block-angle (* 0.8 l))
;;;	bd (* 0.2 d)
	p3 (polar $psk-block-base $psk-block-angle (* 0.3 l))
  )
  (p-make-line p1 p2)
  (p-make-line
    p2
    (polar p2 (+ (- $pi/4) $psk-block-angle) -150)
  )
  (p-make-line
    p2
    (polar p2 (+ $pi/4 $psk-block-angle) -150)
  )
  
  (setq p4 (polar p3 (+ $pi/2 $psk-block-angle) (* 0.2 d)))
  (p-make-ellipse p4 (mapcar '- p3 p4) 0.4)
  (setq p4 (polar p3 (+ $pi/2 $psk-block-angle) (* -0.2 d)))
  (p-make-ellipse p4 (mapcar '- p3 p4) 0.4)
)
;;
(defun psk-draw-ductend	(w fl /)
  (setq $addnew-linetype "BYBLOCK")
  (psk-draw-hvacflange $psk-block-base $psk-block-angle w fl)
  (setq $addnew-linetype "BYLAYER")
)
;; 风管上弯显示剖面 a - 宽 b - 厚
(defun psk-draw-duct-up	(a b r f / c x y)
  (setq	x (* b (- r 0.5))
	y (+ x b)
  )
  (p-make-line (list 0 (* a -0.5)) (list 0 (* a 0.5)))
  (p-make-line (list 0 (* a -0.5)) (list (- x f) (* a -0.5)))
  (p-make-line (list 0 (* a 0.5)) (list (- x f) (* a 0.5)))

  (p-make-polyline
    (list (list x (* a -0.5))
	  (list y (* a -0.5))
	  (list y (* a 0.5))
	  (list x (* a 0.5))
    )
    1
    0.
  )

  (p-make-polyline
    (list (list (- x f) (- (* a -0.5) f))
	  (list (+ y f) (- (* a -0.5) f))
	  (list (+ y f) (+ (* a 0.5) f))
	  (list (- x f) (+ (* a 0.5) f))
    )
    1
    0.
  )

  (setq c (* 0.3 (min a b)))
  (p-make-polyline
    (list (list x (* a -0.5))
	  (list (+ x c) (- (* a 0.5) c))
	  (list y (* a 0.5))
    )
    0
    0.
  )
)


;; 风管下弯 a - 宽 b - 厚 r - 弯头半径(0.8b) f 法兰厚
(defun psk-draw-duct-down (a b r f / x y)
  (setq	x (* b (- r 0.5))
	y (+ x b)
  )
  (p-make-line (list 0 (* a -0.5)) (list 0 (* a 0.5)))
  (p-make-line (list y (* a -0.5)) (list y (* a 0.5)))

  (p-make-line (list 0 (* a -0.5)) (list y (* a -0.5)))
  (p-make-line (list 0 (* a 0.5)) (list y (* a 0.5)))

  ;; 虚线
  (setq $addnew-linetype "HIDDEN")
  (p-make-line (list x (* a -0.5)) (list x (* a 0.5)))
  (setq $addnew-linetype "BYLAYER")

  (if (> f 0.)
    (progn
      (p-make-polyline
	(list (list (- x f) (* a -0.5))
	      (list (- x f) (- (* a -0.5) f))
	      (list (+ y f) (- (* a -0.5) f))
	      (list (+ y f) (+ (* a 0.5) f))
	      (list (- x f) (+ (* a 0.5) f))
	      (list (- x f) (* a 0.5))
	)
	0
	0.
      )
    )
  )
)