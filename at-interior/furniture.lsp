;; ============================================================
;; at-interior furniture — 柜类/窗帘盒/玻璃隔断
;; 衣柜平面、柜体平面/立面/剖面、窗帘盒、玻璃隔断
;; ============================================================

;; ==================== 衣柜平面 ====================

(defun at-interior:ai-closet (p1 p2 depth doors side / ents i a b n)
  "衣柜平面：背边矩形+门分格线+挂衣杆线。
  p1 p2 背边(墙线)起终点；depth 深度(缺省600)；doors 门数(缺省2)；
  side 0左/1右(缺省0)。挂衣杆线位于 0.55*depth 处。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-closet '(0 0) '(2400 0) 600 3 0)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or depth (setq depth 600.0))
      (or doors (setq doors 2))
      (or side (setq side 0))
      (mi-ensure-layer "mi-gj" 8 nil)
      (setvar "CLAYER" "mi-gj")
      (setq ents (list (mi-rect p1 p2 depth side)))
      (if (> doors 1)
        (progn
          (setq i 1)
          (repeat (1- doors)
            (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i doors))))
            (setq b (mi-off a (mi-scale (mi-perp (mi-vec p1 p2) side) depth)))
            (setq ents (append ents (list (mi-line a b))))
            (setq i (1+ i)))))
      (setq n (mi-scale (mi-perp (mi-vec p1 p2) side) (* 0.55 depth)))
      (setq ents (append ents (list (mi-line (mi-off p1 n) (mi-off p2 n)))))
      ents)))

(defun at-interior:closet ()
  "衣柜平面：指定背边起终点、深度、门数"
  (startundo)
  (at-interior:ai-closet
    (getpoint "\n指定背边起点: ")
    (getpoint "\n指定背边终点: ")
    (getreal (strcat "衣柜深度 <600>: "))
    (getint (strcat "门扇数量 <2>: "))
    (mi-get-side))
  (endundo)
  (princ)
)

;; ==================== 柜体平面 ====================

(defun at-interior:ai-cabinet (p1 p2 depth drawers doors side
    / ents u dz i a b m dx0 nb)
  "柜体平面：外轮廓+抽屉区(p1端0.4W，抽屉盒0.6*depth)+门区。
  p1 p2 底边起终点；depth 深度(缺省550)；drawers 抽屉数(缺省0)；
  doors 门数(缺省0，0=门区均分3个开放格)；side 0左/1右(缺省0)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-cabinet '(0 0) '(3000 0) 550 2 2 0)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or depth (setq depth 550.0))
      (or drawers (setq drawers 0))
      (or doors (setq doors 0))
      (or side (setq side 0))
      (mi-ensure-layer "mi-gj" 8 nil)
      (setvar "CLAYER" "mi-gj")
      (setq u (mi-perp (mi-vec p1 p2) side)
        ents (list (mi-rect p1 p2 depth side)))
      (if (> drawers 0)
        (progn
          (setq dz (* 0.4 (mi-len (mi-vec p1 p2)))
                a  (mi-off p1 (mi-scale (mi-vec p1 p2) 0.4))
                b  (mi-off a (mi-scale u depth)))
          (setq ents (append ents (list (mi-line a b))))
          (setq i 1)
          (repeat (1- drawers)
            (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (* 0.4 (/ i drawers)))))
            (setq b (mi-off a (mi-scale u depth)))
            (setq ents (append ents (list (mi-line a b))))
            (setq i (1+ i)))
          (setq i 0)
          (repeat drawers
            (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (* 0.4 (/ i drawers)))))
            (setq b (mi-off p1 (mi-scale (mi-vec p1 p2) (* 0.4 (/ (1+ i) drawers)))))
            (setq m (mi-mid a b))
            (setq ents (append ents (list
                  (mi-rect a b (* 0.6 depth) side)
                  (mi-line (mi-off m (mi-scale u (* 0.85 depth)))
                    (mi-off m (mi-scale u (* 0.97 depth)))))))
            (setq i (1+ i)))))
      (setq dx0 (if (> drawers 0) 0.4 0.0)
            nb  (if (> doors 0) doors 3)
            i   0)
      (if (> nb 1)
        (setq ents (append ents (mi-div-lines
              (mi-off p1 (mi-scale (mi-vec p1 p2) dx0))
              p2 nb depth side))))
      (repeat nb
        (setq a (mi-off p1 (mi-scale (mi-vec p1 p2)
                                     (+ dx0 (* (- 1.0 dx0) (/ i nb)))))
              b (mi-off p1 (mi-scale (mi-vec p1 p2)
                                     (+ dx0 (* (- 1.0 dx0) (/ (1+ i) nb)))))
              m (mi-mid a b))
        (setq ents (append ents (list
              (mi-line (mi-off m (mi-scale u (* 0.85 depth)))
                (mi-off m (mi-scale u (* 0.97 depth)))))))
        (setq i (1+ i)))
      ents)))

(defun at-interior:cabinet ()
  "柜体平面：指定底边、深度、抽屉数、门数"
  (startundo)
  (at-interior:ai-cabinet
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "柜体深度 <550>: "))
    (getint (strcat "抽屉数量 <0>: "))
    (getint (strcat "门扇数量 <0=开放格>: "))
    (mi-get-side))
  (endundo)
  (princ)
)

;; ==================== 柜体立面 ====================

(defun at-interior:ai-cabinet-elev (p1 p2 height doors / ents i a b m)
  "柜体立面：轮廓+门分格线+顶部拉手短线。
  p1 p2 底边起终点；height 柜高(缺省800)；doors 门数(缺省2，1=不分格)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-cabinet-elev '(0 0) '(1200 0) 800 2)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or height (setq height 800.0))
      (or doors (setq doors 2))
      (mi-ensure-layer "mi-gj" 8 nil)
      (setvar "CLAYER" "mi-gj")
      (setq ents (list (mi-rect p1 p2 height 0)))
      (if (> doors 1)
        (setq ents (append ents (mi-div-lines p1 p2 doors height 0))))
      (setq i 0)
      (repeat doors
        (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i doors)))
              b (mi-off p1 (mi-scale (mi-vec p1 p2) (/ (1+ i) doors)))
              m (mi-mid a b))
        (setq ents (append ents (list
              (mi-line (mi-off m (mi-scale (mi-perp (mi-vec p1 p2) 0) (* 0.85 height)))
                (mi-off m (mi-scale (mi-perp (mi-vec p1 p2) 0) (* 0.95 height)))))))
        (setq i (1+ i)))
      ents)))

(defun at-interior:cabinet-elev ()
  "柜体立面：指定底边、柜高、门数"
  (startundo)
  (at-interior:ai-cabinet-elev
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "柜体高度 <800>: "))
    (getint (strcat "门扇数量 <2>: ")))
  (endundo)
  (princ)
)

;; ==================== 柜体剖面 ====================

(defun at-interior:ai-cabinet-sec (p1 p2 height shelves back-thk / ents u i yi s1 s2)
  "柜体剖面：轮廓+搁板(厚18均布)+背板(默认9，靠p2端内侧)。
  p1 p2 底边起终点；height 柜高(缺省800)；shelves 搁板数(缺省3)；
  back-thk 背板厚(缺省9，0=不画)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-cabinet-sec '(0 0) '(500 0) 800 3 9)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or height (setq height 800.0))
      (or shelves (setq shelves 3))
      (or back-thk (setq back-thk 9.0))
      (mi-ensure-layer "mi-gj" 8 nil)
      (setvar "CLAYER" "mi-gj")
      (setq u (mi-perp (mi-vec p1 p2) 0))
      (setq ents (list (mi-rect p1 p2 height 0)))
      (setq i 1)
      (repeat shelves
        (setq yi (* (/ i (1+ shelves)) height))
        (setq s1 (mi-off p1 (mi-scale u (- yi 9.0)))
          s2 (mi-off p2 (mi-scale u (- yi 9.0))))
        (setq ents (append ents (list (mi-rect s1 s2 18.0 0))))
        (setq i (1+ i)))
      (if (> back-thk 0.0)
        (setq ents (append ents (list
              (mi-rect p2 (mi-off p2 (mi-scale u height)) back-thk 0)))))
      ents)))

(defun at-interior:cabinet-sec ()
  "柜体剖面：指定底边、柜高、搁板数、背板厚"
  (startundo)
  (at-interior:ai-cabinet-sec
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "柜体高度 <800>: "))
    (getint (strcat "搁板数量 <3>: "))
    (getreal (strcat "背板厚度 <9> (0=不画): ")))
  (endundo)
  (princ)
)

;; ==================== 窗帘盒 ====================

(defun at-interior:ai-curtain (p1 p2 box-depth gap cord / ents u a b)
  "窗帘盒：窗线侧的盒体矩形+窗帘线(+拉绳)。
  p1 p2 窗/墙线起终点；box-depth 盒深(缺省200)；gap 窗帘与盒前沿间隙
  (缺省100)；cord 1=画拉绳(0.8W处垂线+拉环圆)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-curtain '(0 0) '(1800 0) 200 100 1)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or box-depth (setq box-depth 200.0))
      (or gap (setq gap 100.0))
      (or cord (setq cord 0))
      (mi-ensure-layer "mi-gj" 8 nil)
      (setvar "CLAYER" "mi-gj")
      (setq u (mi-perp (mi-vec p1 p2) 0))
      (setq ents (list (mi-rect p1 p2 box-depth 0)))
      (setq ents (append ents (list
            (mi-line (mi-off p1 (mi-scale u gap))
              (mi-off p2 (mi-scale u gap))))))
      (if (= 1 cord)
        (progn
          (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) 0.8)))
          (setq b (mi-off a (mi-scale u (* 1.3 gap))))
          (setq ents (append ents (list
                (mi-line a b)
                (mi-circle (list (car b) (- (cadr b) 20)) 15)))))
        ents)))
)

(defun at-interior:curtain ()
  "窗帘盒：指定窗线、盒深、间隙、是否拉绳"
  (startundo)
  (at-interior:ai-curtain
    (getpoint "\n指定窗线起点: ")
    (getpoint "\n指定窗线终点: ")
    (getreal (strcat "窗帘盒深度 <200>: "))
    (getreal (strcat "窗帘间隙 <100>: "))
    (getint (strcat "画拉绳 <0>: ")))
  (endundo)
  (princ)
)

;; ==================== 玻璃隔断 ====================

(defun at-interior:ai-glass-wall (p1 p2 thickness door-w door-pos
    / w v n t2 d1 d2 a1 a2 b1 b2 ents)
  "玻璃隔断：双线+斜纹玻璃带(门洞断开)+门扇+开启弧。
  p1 p2 隔墙线起终点；thickness 隔断厚(缺省100)；door-w 门宽(缺省900，
    0=无门)；door-pos 门洞中心位置比例 0~1(缺省0.5)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-glass-wall '(0 0) '(4000 0) 100 900 0.5)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or thickness (setq thickness 100.0))
      (or door-w (setq door-w 900.0))
      (or door-pos (setq door-pos 0.5))
      (mi-ensure-layer "mi-glass" 7 nil)
      (setvar "CLAYER" "mi-glass")
      (setq w (mi-len (mi-vec p1 p2))
            v  (mi-scale (mi-vec p1 p2) (/ 1.0 w))
            n  (mi-perp (mi-vec p1 p2) 0)
            t2 (/ thickness 2.0))
      ;; 双线
      (setq ents (list
          (mi-line (mi-off p1 (mi-scale n (- t2))) (mi-off p2 (mi-scale n (- t2))))
          (mi-line (mi-off p1 (mi-scale n t2)) (mi-off p2 (mi-scale n t2)))))
      (if (> door-w 0.0)
        (progn
          (setq d1 (mi-off p1 (mi-scale v (- (* door-pos w) (/ door-w 2.0)))))
          (setq d2 (mi-off d1 (mi-scale v door-w)))
          ;; 玻璃带（门洞两侧）
          (setq a1 (mi-off p1 (mi-scale n (- t2)))
                a2 (mi-off d1 (mi-scale n (- t2)))
                b1 (mi-off d2 (mi-scale n (- t2)))
                b2 (mi-off p2 (mi-scale n (- t2))))
          (if (> (mi-len (mi-vec a1 a2)) 1.0)
            (setq ents (append ents (list
                  (mi-hatch (mi-rect a1 a2 thickness 0) "SLANT" 2.0)))))
          (if (> (mi-len (mi-vec b1 b2)) 1.0)
            (setq ents (append ents (list
                  (mi-hatch (mi-rect b1 b2 thickness 0) "SLANT" 2.0)))))
          ;; 门框边梃
          (setq ents (append ents (list
                (mi-line (mi-off d1 (mi-scale n (- t2)))
                  (mi-off d1 (mi-scale n t2)))
                (mi-line (mi-off d2 (mi-scale n (- t2)))
                  (mi-off d2 (mi-scale n t2))))))
          ;; 门扇 + 开启弧
          (setq ents (append ents (list
                (mi-line d2 (mi-off d2 (mi-scale n door-w)))
                (mi-arc d1 door-w d2 (mi-off d1 (mi-scale n door-w)))))))
        (if (= 0 door-w)
          (progn
            (setq a1 (mi-off p1 (mi-scale n (- t2)))
              b1 (mi-off p2 (mi-scale n (- t2))))
            (if (> (mi-len (mi-vec a1 b1)) 1.0)
              (setq ents (append ents (list
                    (mi-hatch (mi-rect a1 b1 thickness 0) "SLANT" 2.0))))))))
      ents)))

(defun at-interior:glass-wall ()
  "玻璃隔断：指定隔墙线、厚度、门宽、门洞位置"
  (startundo)
  (at-interior:ai-glass-wall
    (getpoint "\n指定隔墙线起点: ")
    (getpoint "\n指定隔墙线终点: ")
    (getreal (strcat "隔断厚度 <100>: "))
    (getreal (strcat "门宽 <900> (0=无门): "))
    (getreal (strcat "门洞位置比例 0~1 <0.5>: ")))
  (endundo)
  (princ)
)
