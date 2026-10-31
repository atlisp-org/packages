;; ============================================================
;; at-interior sec — 板材/玻璃/石材/型钢截面
;; 木板、玻璃、石材、型钢、钢框、石材墙剖面
;; ============================================================

;; ==================== 木板截面 ====================

(defun at-interior:ai-sec-wood (p1 p2 thk kind / ents)
  "木板截面：沿 p1→p2 厚 thk 的填充矩形。
  p1 p2 底边起终点；thk 板厚(缺省18)；kind 材料：
  wood=实木(STITCH) / ply=多层板(细STITCH) / solid=实心(SOLID)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-sec-wood '(0 0) '(1200 0) 18 \"ply\")"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or thk (setq thk 18.0))
      (if (null kind) (setq kind "ply"))
      (mi-ensure-layer "mi-sec" 8 nil)
      (setvar "CLAYER" "mi-sec")
      (setq ents (mi-draw-sec p1 p2 thk kind))
      ents))
)

(defun at-interior:sec-wood (/ kw)
  "木板截面：指定底边、板厚、材料（中文选项映射为内部代号）"
  (command "_.UNDO" "Begin")
  (progn
    (setq kw (getkword "\n材料 [实木/多层板/实心] <多层板>: "))
    (if (eq "实木" kw)
      (setq kw "wood")
      (if (eq "实心" kw)
        (setq kw "solid")
        (setq kw "ply")))
    (at-interior:ai-sec-wood
      (getpoint "\n指定底边起点: ")
      (getpoint "\n指定底边终点: ")
      (getreal (strcat "板厚 <18>: "))
      kw))
  (command "_.UNDO" "End")
  (princ)
)

;; ==================== 玻璃截面 ====================

(defun at-interior:ai-sec-glass (p1 p2 typ / ents)
  "玻璃截面：单片6mm 或 双片6+6(片间空腔14)。
  p1 p2 底边起终点；typ 1=单片(缺省) / 2=双片。SLANT 斜纹填充。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-sec-glass '(0 0) '(1500 0) 2)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or typ (setq typ 1))
      (mi-ensure-layer "mi-sec" 8 nil)
      (setvar "CLAYER" "mi-sec")
      (setq ents (mi-draw-sec p1 p2 6.0 "glass"))
      (if (= 2 typ)
        (setq ents (append ents
            (mi-draw-sec
              (mi-off p1 (mi-scale (mi-perp (mi-vec p1 p2) 0) 14.0))
              (mi-off p2 (mi-scale (mi-perp (mi-vec p1 p2) 0) 14.0))
              6.0 "glass"))))
      ents))
)

(defun at-interior:sec-glass ()
  "玻璃截面：指定底边、类型(1单片/2双片)"
  (command "_.UNDO" "Begin")
  (at-interior:ai-sec-glass
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getint (strcat "类型 1=单片/2=双片 <1>: ")))
  (command "_.UNDO" "End")
  (princ)
)

;; ==================== 石材截面 ====================

(defun at-interior:ai-sec-stone (p1 p2 thk hanger-gap / w ents i a b)
  "石材截面：石板矩形(SAND)+背面挂件(30短线+r5圆，自 x=50 起均布)。
  p1 p2 底边起终点；thk 石板厚(缺省20)；hanger-gap 挂件间距(缺省600)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-sec-stone '(0 0) '(800 0) 20 600)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or thk (setq thk 20.0))
      (or hanger-gap (setq hanger-gap 600.0))
      (mi-ensure-layer "mi-sec" 8 nil)
      (setvar "CLAYER" "mi-sec")
      (setq w (mi-len (mi-vec p1 p2)))
      (setq ents (mi-draw-sec p1 p2 thk "stone"))
      (setq i 50.0)
      (while (< i (- w 50.0))
        (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i w)))
          b (mi-off a (mi-scale (mi-perp (mi-vec p1 p2) 0) (- thk -30.0))))
        (setq ents (append ents (list
              (mi-line a b)
              (mi-circle b 5.0))))
        (setq i (+ i hanger-gap)))
      ents))
)

(defun at-interior:sec-stone ()
  "石材截面：指定底边、石板厚、挂件间距"
  (command "_.UNDO" "Begin")
  (at-interior:ai-sec-stone
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "石板厚度 <20>: "))
    (getreal (strcat "挂件间距 <600>: ")))
  (command "_.UNDO" "End")
  (princ)
)

;; ==================== 型钢截面 ====================

(defun at-interior:ai-sec-steel (p1 p2 typ a b c d / r ents)
  "型钢截面：plate 板 / i 工字 / h 型钢（12点轮廓），SOLID 填充。
  p1 p2 底边起终点；typ plate/i/h；
  plate: a=板厚(缺省10)；i/h: a=总高(缺省200) b=翼缘宽(缺省100)
  c=翼缘厚(缺省10) d=腹板厚(缺省6)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-sec-steel '(0 0) '(200 0) \"h\" 200 100 10 6)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or typ (setq typ "h"))
      (or a (setq a (if (eq typ "plate") 10.0 200.0)))
      (or b (setq b 100.0))
      (or c (setq c 10.0))
      (or d (setq d 6.0))
      (mi-ensure-layer "mi-sec" 8 nil)
      (setvar "CLAYER" "mi-sec")
      (if (eq typ "plate")
        (progn
          (setq r (mi-rect p1 p2 a 0))
          (setq ents (list r (mi-hatch r "SOLID" 1.0))))
        (progn
          (setq r (mi-steel-profile p1 p2 a b c d))
          (setq ents (list r (mi-hatch r "SOLID" 1.0)))))
      ents))
)

(defun at-interior:sec-steel (/ kw)
  "型钢截面：指定底边、类型(中文选项映射)、尺寸"
  (command "_.UNDO" "Begin")
  (progn
    (setq kw (getkword "\n类型 [板/工字/型钢] <型钢>: "))
    (if (eq "板" kw)
      (setq kw "plate")
      (if (eq "工字" kw)
        (setq kw "i")
        (setq kw "h")))
    (at-interior:ai-sec-steel
      (getpoint "\n指定底边起点: ")
      (getpoint "\n指定底边终点: ")
      kw
      (getreal (strcat "总高/板厚 <200>: "))
      (getreal (strcat "翼缘宽 <100>: "))
      (getreal (strcat "翼缘厚 <10>: "))
      (getreal (strcat "腹板厚 <6>: "))))
  (command "_.UNDO" "End")
  (princ)
)

;; ==================== 钢框截面 ====================

(defun at-interior:ai-sec-steel-frame (p1 p2 height leg
    / w n outer inner p1i p2i ents)
  "钢框截面：外框矩形+内矩形+环带 SOLID 填充（框腿厚 leg）。
  p1 p2 底边起终点；height 框高(缺省150)；leg 框腿厚(缺省8)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-sec-steel-frame '(0 0) '(1500 0) 150 8)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or height (setq height 150.0))
      (or leg (setq leg 8.0))
      (setq w (mi-len (mi-vec p1 p2)))
      (if (or (<= height (* leg 3.0)) (<= w (* leg 3.0)))
        (progn (princ "\n尺寸过小，框腿放不下") nil)
        (progn
          (mi-ensure-layer "mi-sec" 8 nil)
          (setvar "CLAYER" "mi-sec")
          (setq n (mi-perp (mi-vec p1 p2) 0)
                outer (mi-rect p1 p2 height 0)
                p1i   (mi-off (mi-off p1 (mi-scale (mi-vec p1 p2) (/ leg w)))
                            (mi-scale n leg))
                p2i   (mi-off (mi-off p2 (mi-scale (mi-vec p1 p2) (/ (- w leg) w)))
                            (mi-scale n leg))
                inner (mi-rect p1i p2i (- height (* 2 leg)) 0))
          (setq ents (list outer inner
              (mi-hatch-ring outer inner "SOLID" 1.0)))
          ents))))
)

(defun at-interior:sec-steel-frame ()
  "钢框截面：指定底边、框高、框腿厚"
  (command "_.UNDO" "Begin")
  (at-interior:ai-sec-steel-frame
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "框高 <150>: "))
    (getreal (strcat "框腿厚 <8>: ")))
  (command "_.UNDO" "End")
  (princ)
)

;; ==================== 石材墙剖面 ====================

(defun at-interior:ai-sec-stone-wall (p1 p2 thk gap c-int
    / w v n i a b y0 y1 ch pts stone ents)
  "石材墙剖面：地线+120墙身(CONCRETE)+C型钢(高60/深40，自x=300起均布)
  +挂件线+石板(顶边海棠角，SAND)。
  p1 p2 底边(地线)起终点；thk 石板厚(缺省20)；gap C型钢与石板间隙
  (缺省20)；c-int C型钢间距(缺省1000)。"
  "成功返回图元名表，失败返回 nil"
  "示例: (at-interior:ai-sec-stone-wall '(0 0) '(3000 0) 20 20 1000)"
  (if (or (null p1) (null p2))
    (progn (princ "\n缺少起点或终点") nil)
    (progn
      (or thk (setq thk 20.0))
      (or gap (setq gap 20.0))
      (or c-int (setq c-int 1000.0))
      (mi-ensure-layer "mi-sec" 8 nil)
      (setvar "CLAYER" "mi-sec")
      (setq w (mi-len (mi-vec p1 p2))
            v  (mi-scale (mi-vec p1 p2) (/ 1.0 w))
            n  (mi-perp (mi-vec p1 p2) 0)
            y0 120.0
            y1 (+ 160.0 gap)
            ch (min 20.0 (- thk 2.0)))
      ;; 地线 + 墙身
      (setq ents (list (mi-line p1 p2))
        ents (append ents (mi-draw-sec p1 p2 120.0 "conc")))
      ;; C 型钢 + 挂件
      (setq i 300.0)
      (while (< (+ i 60.0) w)
        (setq a (mi-off p1 (mi-scale v i))
          b (mi-off a (mi-scale v 60.0)))
        (setq ents (append ents (list
              (mi-line (mi-off a (mi-scale n y0))
                (mi-off b (mi-scale n y0)))
              (mi-line (mi-off a (mi-scale n y0))
                (mi-off a (mi-scale n 160.0)))
              (mi-line (mi-off b (mi-scale n y0))
                (mi-off b (mi-scale n 160.0)))
              (mi-line (mi-off (mi-mid a b) (mi-scale n 160.0))
                (mi-off (mi-mid a b) (mi-scale n y1))))))
        (setq i (+ i c-int)))
      ;; 石板五边形（顶边海棠角切角）
      (setq pts (list
          (mi-off p1 (mi-scale n y1))
          (mi-off p2 (mi-scale n y1))
          (mi-off p2 (mi-scale n (+ y1 (- thk ch))))
          (mi-off (mi-off p2 (mi-scale v (- ch))) (mi-scale n (+ y1 thk)))
          (mi-off p1 (mi-scale n (+ y1 thk)))))
      (setq stone (mi-pline pts 1))
      (setq ents (append ents (list stone (mi-hatch stone "SAND" 1.0))))
      ents))
)

(defun at-interior:sec-stone-wall ()
  "石材墙剖面：指定底边、石板厚、间隙、C型钢间距"
  (command "_.UNDO" "Begin")
  (at-interior:ai-sec-stone-wall
    (getpoint "\n指定底边起点: ")
    (getpoint "\n指定底边终点: ")
    (getreal (strcat "石板厚度 <20>: "))
    (getreal (strcat "C型钢与石板间隙 <20>: "))
    (getreal (strcat "C型钢间距 <1000>: ")))
  (command "_.UNDO" "End")
  (princ)
)
