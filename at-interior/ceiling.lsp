;; ============================================================
;; at-interior ceiling — 天花/灯具/电线
;; 天花轮廓框、天花平面线、天花线立面、石膏板剖面、
;; 电线平面、灯具平面
;; ============================================================

;; ==================== 天花轮廓框 ====================

(defun at-interior:ai-elev-frame (pline-ent height side thk / pts c i a b s out ents v)
  "天花轮廓框：闭合折线每条边向外(默认)偏出 height 的立面矩形+角部方柱+标高文本。
  pline-ent 闭合折线 ename；height 偏出高度(缺省3600)；side 0=外扩(默认，
    自动按质心判定) / 1=内收；thk 角部方柱边长(缺省200)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-elev-frame (car (entsel \"选折线: \")) 3600 0 200)"
  (if (null pline-ent)
    (progn (princ "\n缺少折线") nil)
    (progn
      (or height (setq height 3600.0))
      (or side (setq side 0))
      (or thk (setq thk 200.0))
      (setq pts (mi-pline-pts pline-ent))
      ;; 闭合折线末顶点重复首顶点，去掉
      (if (and (> (length pts) 1)
          (equal (car pts) (car (reverse pts))))
        (setq pts (cdr pts)))
      (if (< (length pts) 3)
        (progn (princ "\n折线顶点不足") nil)
        (progn
          (mi-ensure-layer "mi-ceil" 8 nil)
          (setvar "CLAYER" "mi-ceil")
          (setq c (mi-centroid pts)
                ents '()
                i    0)
          (while (< i (length pts))
            (setq a (nth i pts)
                  b (nth (if (= i (1- (length pts))) 0 (1+ i)) pts)
                  s 0)
            ;; 质心在边左侧时外侧为右；side=1 内收取反侧
            (setq s (if (> (mi-dot (mi-perp (mi-vec a b) 0)
                    (mi-vec (mi-mid a b) c))
                  0.0) 1 0))
            (if (= 1 side) (setq s (1- s)))
            (setq ents (append ents (list (mi-rect a b height s))))
            (setq i (1+ i)))
          (foreach v (mapcar (function (lambda (p) (list (car p) (cadr p)))) pts)
            (setq ents (append ents (list
                  (mi-axis-rect (list (- (car v) (/ thk 2.0))
                      (- (cadr v) (/ thk 2.0)))
                    (list (+ (car v) (/ thk 2.0))
                      (+ (cadr v) (/ thk 2.0))))))))
          (setq ents (append ents (list
                (mi-text (strcat "H=" (rtos height 2 0)) c 25 0))))
          ents)))))

(defun at-interior:elev-frame ()
  "天花轮廓框：选择闭合折线，输入偏出高度、方柱边长"
  (startundo)
  (progn
    (setq sel (entsel "\n选择闭合天花轮廓折线: "))
    (if sel
      (at-interior:ai-elev-frame
        (car sel)
        (getreal (strcat "偏出高度 <3600>: "))
        (getint (strcat "方向 0=外扩/1=内收 <0>: "))
        (getreal (strcat "角部方柱边长 <200>: ")))
      (princ "\n未选择对象")))
  (endundo)
  (princ)
)

;; ==================== 天花平面线 ====================

(defun at-interior:ai-jianxian-plan (p1 p2 offset elev / x1 y1 x2 y2 o o2 c ents)
  "天花平面线：对角点轴对齐外框+内缩线+灯槽环带+中心标高符号。
  p1 p2 对角点；offset 内缩距离(缺省500)；灯槽宽固定 50；
  elev 标高值(缺省3000，文本显示 3 位小数)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-jianxian-plan '(0 0) '(6000 4000) 500 3000)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少对角点") nil)
    (progn
      (or offset (setq offset 500.0))
      (or elev (setq elev 3000.0))
      (mi-ensure-layer "mi-ceil" 8 nil)
      (setvar "CLAYER" "mi-ceil")
      (setq x1 (min (car p1) (car p2))
            x2 (max (car p1) (car p2))
            y1 (min (cadr p1) (cadr p2))
            y2 (max (cadr p1) (cadr p2)))
      (if (and (> (- x2 x1) (* 2 offset 2.0))
          (> (- y2 y1) (* 2 offset 2.0)))
        (progn
          (setq o (+ offset 50.0)
                c    (list (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))
                ents (list
                 (mi-axis-rect (list x1 y1) (list x2 y2))
                 (mi-axis-rect (list (+ x1 offset) (+ y1 offset))
                               (list (- x2 offset) (- y2 offset)))
                 (mi-axis-rect (list (+ x1 o) (+ y1 o))
                               (list (- x2 o) (- y2 o))))
                ents (append ents (mi-elev-symbol c 10 (rtos elev 2 3))))
          ents)
        (progn (princ "\n轮廓太小，内缩距离过大") nil))))
)

(defun at-interior:jianxian-plan ()
  "天花平面线：指定对角点、内缩距离、标高"
  (startundo)
  (at-interior:ai-jianxian-plan
    (getpoint "\n指定第一对角点: ")
    (getpoint "\n指定第二对角点: ")
    (getreal (strcat "内缩距离 <500>: "))
    (getreal (strcat "标高值 <3000>: ")))
  (endundo)
  (princ)
)

;; ==================== 天花线立面 ====================

(defun at-interior:ai-jianxian-elev (p1 p2 h1 dist drop thk / ents u a b)
  "天花线立面：地线+墙体线+主顶面线+灯槽竖线+副顶面线+板厚线+灯具符号。
  p1 p2 地线起终点；h1 主顶面标高(缺省3600)；dist 灯槽距墙体水平距离
  (缺省300)；drop 下吊高度(缺省400)；thk 石膏板厚(缺省12)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-jianxian-elev '(0 0) '(5000 0) 3600 300 400 12)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or h1 (setq h1 3600.0))
      (or dist (setq dist 300.0))
      (or drop (setq drop 400.0))
      (or thk (setq thk 12.0))
      (mi-ensure-layer "mi-ceil" 8 nil)
      (setvar "CLAYER" "mi-ceil")
      (setq u (mi-perp (mi-vec p1 p2) 0))
      (setq ents (list
          ;; 地线
          (mi-line p1 p2)
          ;; 墙体线
          (mi-line p1 (mi-off p1 (mi-scale u (+ h1 drop))))
          ;; 主顶面线
          (mi-line (mi-off p1 (mi-scale u h1)) (mi-off p2 (mi-scale u h1)))
          ;; 灯槽竖线
          (mi-line (mi-off (mi-off p1 (mi-scale (mi-vec p1 p2) dist))
              (mi-scale u h1))
            (mi-off (mi-off p1 (mi-scale (mi-vec p1 p2) dist))
              (mi-scale u (- h1 drop))))
          ;; 副顶面线
          (mi-line (mi-off (mi-off p1 (mi-scale (mi-vec p1 p2) dist))
              (mi-scale u (- h1 drop)))
            (mi-off p2 (mi-scale u (- h1 drop))))
          ;; 副顶板厚线（两端）
          (mi-line (mi-off p2 (mi-scale u (- h1 drop)))
            (mi-off p2 (mi-scale u (- h1 drop thk))))
          (mi-line (mi-off (mi-off p1 (mi-scale (mi-vec p1 p2) dist))
              (mi-scale u (- h1 drop)))
            (mi-off (mi-off p1 (mi-scale (mi-vec p1 p2) dist))
              (mi-scale u (- h1 drop thk))))))
      (setq a (mi-mid (mi-off p1 (mi-scale (mi-vec p1 p2) dist)) p2))
      ;; 副顶面下吊灯具（短线+圆）
      (setq ents (append ents (list
            (mi-line (mi-off a (mi-scale u (- h1 drop thk)))
              (mi-off a (mi-scale u (- h1 drop thk -50.0))))
            (mi-circle (mi-off a (mi-scale u (- h1 drop thk -25.0))) 15))))
      (setq ents (append ents (mi-elev-symbol
            (mi-off p1 (mi-scale u h1)) 10 (rtos h1 2 3))))
      ents)))

(defun at-interior:jianxian-elev ()
  "天花线立面：指定地线、主顶标高、槽距、下吊高度"
  (startundo)
  (at-interior:ai-jianxian-elev
    (getpoint "\n指定地线起点: ")
    (getpoint "\n指定地线终点: ")
    (getreal (strcat "主顶面标高 <3600>: "))
    (getreal (strcat "灯槽距墙距离 <300>: "))
    (getreal (strcat "下吊高度 <400>: "))
    (getreal (strcat "石膏板厚 <12>: ")))
  (endundo)
  (princ)
)

;; ==================== 石膏板剖面 ====================

(defun at-interior:ai-gypsum-sec (p1 p2 drop rod-gap / ents w i a b)
  "石膏板剖面：板底(梁底)线+吊杆+主龙骨线+石膏板上下线。
  p1 p2 板底线起终点(0标高)；drop 下吊高度(缺省150)；
  rod-gap 吊杆间距(缺省1000，自 p1 起 200 首吊)。主龙骨在 -drop+50，
  板顶在 -drop，板底在 -drop-12。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-gypsum-sec '(0 0) '(6000 0) 150 1000)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or drop (setq drop 150.0))
      (or rod-gap (setq rod-gap 1000.0))
      (mi-ensure-layer "mi-ceil" 8 nil)
      (setvar "CLAYER" "mi-ceil")
      (setq w (mi-len (mi-vec p1 p2)))
      (setq ents (list
          ;; 板底(梁底)线
          (mi-line p1 p2)
          ;; 主龙骨线
          (mi-line (mi-off p1 (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop -50.0)))
            (mi-off p2 (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop -50.0))))
          ;; 板顶线
          (mi-line (mi-off p1 (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop 0.0)))
            (mi-off p2 (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop 0.0))))
          ;; 板底线
          (mi-line (mi-off p1 (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop -12.0)))
            (mi-off p2 (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop -12.0))))))
      ;; 吊杆
      (setq i 200.0)
      (while (< i (- w 100.0))
        (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i w)))
          b (mi-off a (mi-scale (mi-perp (mi-vec p1 p2) 0) (- drop -50.0))))
        (setq ents (append ents (list (mi-line a b)))
          i (+ i rod-gap)))
      ents)))

(defun at-interior:gypsum-sec ()
  "石膏板剖面：指定板底线、下吊高度、吊杆间距"
  (startundo)
  (at-interior:ai-gypsum-sec
    (getpoint "\n指定板底线起点: ")
    (getpoint "\n指定板底线终点: ")
    (getreal (strcat "下吊高度 <150>: "))
    (getreal (strcat "吊杆间距 <1000>: ")))
  (endundo)
  (princ)
)

;; ==================== 电线平面 ====================

(defun at-interior:ai-elec-line (lamp-pts switch / ents n)
  "电线平面：多点折线(端点重合自动闭合)+可选开关符号(圆r15+引线+刀杆+S1)。
  lamp-pts 折线点表(≥2点)；switch 开关位置点(nil=不画开关)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-elec-line '((100 100) (500 100) (500 400)) '(600 500))"
  (if (or (null lamp-pts) (< (length lamp-pts) 2))
    (progn (princ "\n电线点不足两个") nil)
    (progn
      (mi-ensure-layer "mi-dengju" 1 nil)
      (setvar "CLAYER" "mi-dengju")
      (setq n (length lamp-pts))
      (setq ents (list
          (mi-pline lamp-pts
            (if (= (car lamp-pts) (nth (1- n) lamp-pts)) 1 0))))
      (if (and switch (not (null switch)))
        (progn
          (setq ents (append ents (list
                (mi-circle switch 15)
                (mi-line switch (list (+ (car switch) 120.0) (cadr switch)))
                (mi-line (list (+ (car switch) 120.0) (cadr switch))
                  (list (+ (car switch) 162.4) (+ (cadr switch) 42.4)))
                (mi-text-l "S1"
                  (list (+ (car switch) 125.0) (+ (cadr switch) 25.0))
                  20 0)))))
        ents)))
)

(defun at-interior:elec-line (/ pts p)
  "电线平面：依次拾取电线点，再选择是否画开关"
  (startundo)
  (progn
    (setq pts nil
      p (getpoint "\n指定电线起点: "))
    (while p
      (setq pts (append pts (list (list (car p) (cadr p))))
        p (getpoint p "\n指定下一点(回车结束): ")))
    (if (> (length pts) 1)
      (at-interior:ai-elec-line
        pts
        (if (eq "Y" (getkword "\n画开关? [Y/N] <N>: "))
          (getpoint "\n指定开关位置: ")
          nil))))
  (endundo)
  (princ)
)

;; ==================== 灯具平面 ====================

(defun at-interior:ai-light-plan (pline-ent dist blk / pts i ents)
  "灯具平面：沿闭合曲线按等距布置灯具符号(圆r50+斜十字)，或 INSERT 块。
  pline-ent 闭合曲线 ename；dist 间距(缺省1000)；blk 灯具块名(可空)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-light-plan (car (entsel \"选曲线: \")) 1000 nil)"
  (if (null pline-ent)
    (progn (princ "\n缺少曲线") nil)
    (progn
      (or dist (setq dist 1000.0))
      (mi-ensure-layer "mi-dengju" 1 nil)
      (setvar "CLAYER" "mi-dengju")
      (setq pts (mi-curve-pts pline-ent dist))
      (if (null pts)
        (progn (princ "\n非曲线实体或长度不足") nil)
        (progn
          (setq ents '()
            i 0)
          (while (< i (length pts))
            (setq ents (append ents (mi-light-symbol (nth i pts) blk)))
            (setq i (1+ i)))
          ents))))
)

(defun at-interior:light-plan ()
  "灯具平面：选择闭合曲线，输入间距和块名"
  (startundo)
  (progn
    (setq sel (entsel "\n选择闭合曲线: "))
    (if sel
      (at-interior:ai-light-plan
        (car sel)
        (getreal (strcat "灯具间距 <1000>: "))
        (getstring "\n灯具块名(回车=默认符号): "))
      (princ "\n未选择对象")))
  (endundo)
  (princ)
)
