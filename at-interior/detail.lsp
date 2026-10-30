;; ============================================================
;; at-interior detail — 焊缝/软管/管线/开孔
;; 焊缝、软管、管线、背板开孔
;; ============================================================

;; ==================== 焊缝 ====================

(defun at-interior:ai-weld (p1 p2 typ leg / w n i a b ents cx)
  "焊缝：沿 p1→p2 画焊缝符号+长度标注。
  p1 p2 焊缝线起终点；typ fillet=角焊缝(三角，自x=50起间距100，
    base 8，高 leg 缺省6) / butt=对接焊缝(宽3折线+V符号)；
  leg 角焊缝高度(缺省6)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-weld '(0 0) '(2000 0) \"fillet\" 6)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or typ (setq typ "fillet"))
      (or leg (setq leg 6.0))
      (mi-ensure-layer "mi-weld" 8 nil)
      (setvar "CLAYER" "mi-weld")
      (setq w (mi-len (mi-vec p1 p2))
        n (mi-perp (mi-vec p1 p2) 0))
      (setq ents (list (mi-line p1 p2)))
      (if (eq typ "fillet")
        (progn
          (setq i 50.0)
          (while (< i (- w 50.0))
            (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i w)))
              b (mi-off a (mi-scale (mi-vec p1 p2) (/ 8.0 w))))
            (setq ents (append ents (list
                  (mi-pline (list a (mi-off a (mi-scale n leg)) b) 0))))
            (setq i (+ i 100.0))))
        (progn
          (setq ents (append ents (list
                (mi-lwp (list p1 p2) 3.0 0))))
          (setq cx (mi-mid p1 p2))
          (setq ents (append ents (list
                (mi-pline (list (mi-off cx (mi-scale (mi-vec p1 p2) (- 15.0))
                      (mi-scale n 20.0))
                    cx
                    (mi-off cx (mi-scale (mi-vec p1 p2) 15.0)
                      (mi-scale n 20.0))) 0))))))
      ;; 长度标注
      (setq ents (append ents (list
            (mi-text (strcat "L=" (rtos w 2 0))
              (mi-off (mi-mid p1 p2) (mi-scale n 40.0))
              20 0))))
      ents))
)

(defun at-interior:weld ()
  "焊缝：指定焊缝线、类型、角焊缝高度"
  (startundo)
  (at-interior:ai-weld
    (getpoint "\n指定焊缝线起点: ")
    (getpoint "\n指定焊缝线终点: ")
    (getkword "\n类型 [角焊缝/对接焊缝] <角焊缝>: ")
    (getreal (strcat "角焊缝高度 <6>: ")))
  (endundo)
  (princ)
)

;; ==================== 软管 ====================

(defun at-interior:ai-flex (p1 p2 radius / w v n hw a b a0 b0 ents)
  "软管：两条 DASHED 偏移线(±radius)+两端管件(长40，宽2r+16)。
  p1 p2 软管中心线起终点；radius 软管半径(缺省25)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-flex '(0 0) '(1000 0) 25)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or radius (setq radius 25.0))
      (mi-ensure-layer "mi-tube" 8 "DASHED")
      (setvar "CLAYER" "mi-tube")
      (setq w (mi-len (mi-vec p1 p2))
            v (mi-scale (mi-vec p1 p2) (/ 1.0 w))
            n (mi-perp (mi-vec p1 p2) 0))
      (setq ents (list
          (mi-line (mi-off p1 (mi-scale n radius)) (mi-off p2 (mi-scale n radius)))
          (mi-line (mi-off p1 (mi-scale n (- radius)))
            (mi-off p2 (mi-scale n (- radius))))))
      ;; 两端管件
      (foreach p (list p1 p2)
        (setq a (mi-off p (mi-scale v (- 20.0)))
              b  (mi-off p (mi-scale v 20.0))
              hw (+ radius 8.0)
              a0 (mi-off a (mi-scale n (- hw)))
              b0 (mi-off b (mi-scale n (- hw))))
        (setq ents (append ents (list (mi-rect a0 b0 (* 2 hw) 0)))))
      ents))
)

(defun at-interior:flex ()
  "软管：指定中心线、半径"
  (startundo)
  (at-interior:ai-flex
    (getpoint "\n指定中心线起点: ")
    (getpoint "\n指定中心线终点: ")
    (getreal (strcat "软管半径 <25>: ")))
  (endundo)
  (princ)
)

;; ==================== 管线 ====================

(defun at-interior:ai-tube (pts radius dn branch
    / i len mid ents p1 p2 d1 d2 s)
  "管线：多点折线双线墙(斜接)+最长段中点 DN 标注+可选支管。
  pts 折线点表(≥2点)；radius 管半径(缺省50)；dn 管径标注文本
  (缺省按 radius*2 生成，如 \"DN100\")；branch 支管长度(缺省0=不画，
    在内顶点沿角平分线外方向画宽 2r 短管)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-tube '((0 0) (600 0) (600 600)) 50 \"DN100\" 0)"
  (if (or (null pts) (< (length pts) 2))
    (progn (princ "\n管线点不足两个") nil)
    (progn
      (or radius (setq radius 50.0))
      (if (null dn)
        (setq dn (strcat "DN" (rtos (* 2 radius) 2 0))))
      (or branch (setq branch 0))
      (mi-ensure-layer "mi-tube" 8 nil)
      (setvar "CLAYER" "mi-tube")
      ;; 两侧墙
      (setq ents (list
          (mi-pline (mi-pline-offset pts radius 0) 0)
          (mi-pline (mi-pline-offset pts radius 1) 0)))
      ;; DN 标注（最长段中点）
      (setq len 0.0
            mid (mi-mid (car pts) (cadr pts))
            i   0)
      (while (< i (1- (length pts)))
        (setq p1 (nth i pts)
          p2 (nth (1+ i) pts))
        (if (> (mi-len (mi-vec p1 p2)) len)
          (setq len (mi-len (mi-vec p1 p2))
            mid (mi-mid p1 p2)))
        (setq i (1+ i)))
      (setq ents (append ents (list (mi-text dn mid 20 0))))
      ;; 支管（内顶点，角平分线外方向）
      (if (> branch 0.0)
        (progn
          (setq i 1)
          (while (< i (1- (length pts)))
            (setq p1 (nth (1- i) pts)
                  p2 (nth i pts)
                  d1 (mi-perp (mi-vec p1 p2) 0)
                  d2 (mi-perp (mi-vec p2 (nth (1+ i) pts)) 0)
                  s  (mi-add d1 d2))
            (if (< (mi-len s) 0.0001)
              (setq s d1))
            (setq ents (append ents (list
                  (mi-lwp (list p2
                      (mi-off p2 (mi-scale (mi-norm s) branch)))
                    (* 2 radius) 0))))
            (setq i (1+ i)))))
      ents))
)

(defun at-interior:tube (/ pts p)
  "管线：依次拾取折线点，再输入半径、管径、支管长"
  (startundo)
  (progn
    (setq pts nil
      p (getpoint "\n指定管线起点: "))
    (while p
      (setq pts (append pts (list (list (car p) (cadr p))))
        p (getpoint p "\n指定下一点(回车结束): ")))
    (if (> (length pts) 1)
      (at-interior:ai-tube
        pts
        (getreal (strcat "管半径 <50>: "))
        (getstring "\n管径标注(回车=按半径): ")
        (getreal (strcat "支管长度 <0=不画>: ")))))
  (endundo)
  (princ)
)

;; ==================== 背板开孔 ====================

(defun at-interior:ai-back-holes (p1 p2 gap / w ents i a)
  "背板开孔：底边线+均布圆孔(r5，圆心 r1.5 同心圆)，自 x=50 起。
  p1 p2 底边起终点；gap 孔间距(缺省300)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-back-holes '(0 0) '(1200 0) 300)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or gap (setq gap 300.0))
      (mi-ensure-layer "mi-hole" 8 nil)
      (setvar "CLAYER" "mi-hole")
      (setq w (mi-len (mi-vec p1 p2)))
      (setq ents (list (mi-line p1 p2)))
      (setq i 50.0)
      (while (< i (- w 50.0))
        (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i w))))
        (setq ents (append ents (list
              (mi-circle a 5.0)
              (mi-circle a 1.5))))
        (setq i (+ i gap)))
      ents))
)

(defun at-interior:back-holes ()
  "背板开孔：指定底边、孔间距"
  (startundo)
  (at-interior:ai-back-holes
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "孔间距 <300>: ")))
  (endundo)
  (princ)
)
