;; 墙起终点p1 p2 在墙线在哪一侧画 a 绘制高度(一半的绘制区高) h 盘管距墙d1 盘管间距d2
(defun psk-draw-dnpg (p1 p2 a h d1 d2 / a1 h1 p11 p12)
  (setq a1  (angle p1 p2)
        p11 (polar (polar p1 a1 d1) a d1)
        p12 (polar (polar p2 a1 (- d1)) a d1)
        h1  d1
  )
  (while (and
           (< h1 (- h (/ d2 2.)))
           (> (distance p11 p12) d2)
         )
    (p-make-line p11 p12)

    (setq p11 (polar (polar p11 a1 d2) a d2)
          p12 (polar (polar p12 a1 (- d2)) a d2)
          h1  (+ h1 d2)
    )
  )
)
(defun c:tt (/ p1 p2 p3 p4 size)
  (if (and (setq p1 (getpoint "\n指定起点:"))
           (setq p2 (getcorner p1 "\n指定对角点:"))
      )
    (progn
      (setq p3   (list (car p2) (cadr p1) 0)
            p4   (list (car p1) (cadr p2) 0)
            d1   100 ;_ 距墙
            d2   300 ;_ 管间距
;;;            a    100 ;_ 倒角
            size (mapcar '- p2 p1)
            a1   (angle p1 p4)
      )
      (psk-draw-dnpg
        (polar p1 a1 (+ d2 d2))
        (polar (polar p3 a1 (+ d2 d2)) (angle p3 p1) (+ d2 d2))
        a1
        (- (abs (/ (cadr size) 2.)) d2 d2)
        d1
        d2
      )
      (psk-draw-dnpg p2 p4 (angle p2 p3) (abs (/ (cadr size) 2.)) d1 d2)
      (psk-draw-dnpg (polar p1 a1 (+ d2 d2)) p4 (angle p1 p3) (abs (/ (car size) 2.)) d1 d2)
      (psk-draw-dnpg p2 p3 (angle p2 p4) (abs (/ (car size) 2.)) d1 d2)
    )
  )
)