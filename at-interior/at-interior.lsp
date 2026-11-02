;; ============================================================
;; at-interior — 迷你装修
;; 复现 iMini 装修类命令：柜类平面/剖面、天花角线、灯具布置、
;; 玻璃隔断、板材截面、焊缝管线等。
;; 源码 UTF-8；函数命名 at-interior:<名>；
;; AI 无交互入口 at-interior:ai-<动作>。单位 mm。
;; 侧向约定：side=0 左侧(逆时针90°)，side=1 右侧(顺时针90°)。
;; ============================================================

;; ==================== 依赖加载 ====================

(if (null entity:make-line) (require 'entity:*))
(if (null layer:make) (require 'layer:*))

;; ==================== 菜单注册 ====================

(@:add-menus '(("装修工具"
      ("衣柜平面" "(at-interior:closet)")
      ("柜体平面" "(at-interior:cabinet)")
      ("柜体立面" "(at-interior:cabinet-elev)")
      ("柜体剖面" "(at-interior:cabinet-sec)")
      ("窗帘盒" "(at-interior:curtain)")
      ("玻璃隔断" "(at-interior:glass-wall)")
      ("--" "--")
      ("天花轮廓框" "(at-interior:elev-frame)")
      ("天花平面线" "(at-interior:jianxian-plan)")
      ("天花线立面" "(at-interior:jianxian-elev)")
      ("石膏板剖面" "(at-interior:gypsum-sec)")
      ("电线平面" "(at-interior:elec-line)")
      ("灯具平面" "(at-interior:light-plan)")
      ("--" "--")
      ("木板截面" "(at-interior:sec-wood)")
      ("玻璃截面" "(at-interior:sec-glass)")
      ("石材截面" "(at-interior:sec-stone)")
      ("型钢截面" "(at-interior:sec-steel)")
      ("钢框截面" "(at-interior:sec-steel-frame)")
      ("石材墙剖面" "(at-interior:sec-stone-wall)")
      ("--" "--")
      ("焊缝" "(at-interior:weld)")
      ("软管" "(at-interior:flex)")
      ("管线" "(at-interior:tube)")
      ("背板开孔" "(at-interior:back-holes)")
)))

;; ==================== 通用工具：向量运算 ====================

(defun mi-vec (p1 p2)
  "向量 p1→p2：(dx dy)"
  (list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)))
)

(defun mi-add (a b)
  "向量相加"
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b)))
)

(defun mi-scale (v s)
  "向量缩放"
  (list (* (car v) s) (* (cadr v) s))
)

(defun mi-len (v)
  "向量长度"
  (sqrt (+ (expt (car v) 2) (expt (cadr v) 2)))
)

(defun mi-dot (a b)
  "向量点积"
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)))
)

(defun mi-norm (v)
  "单位向量（零向量返回 (0 0)）"
  (if (zerop (mi-len v))
    (list 0.0 0.0)
    (mi-scale v (/ 1.0 (mi-len v))))
)

(defun mi-perp (v side / u)
  "v 的单位法向量。side=0 左侧(逆时针90°)，否则右侧(顺时针90°)"
  (setq u (mi-norm v))
  (if (= 0 side)
    (list (- (cadr u)) (car u))
    (list (cadr u) (- (car u))))
)

(defun mi-off (p v)
  "点 p 沿向量 v 偏移"
  (list (+ (car p) (car v)) (+ (cadr p) (cadr v)))
)

(defun mi-mid (p1 p2)
  "线段中点"
  (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0))
)

(defun mi-div (p1 p2 n / step pts i)
  "线段 p1→p2 等分 n 段，返回 n+1 个点表"
  (if (<= n 1)
    (list p1 p2)
    (progn
      (setq step (mi-scale (mi-vec p1 p2) (/ 1.0 n))
            pts  '()
            i    0)
      (while (<= i n)
        (setq pts (append pts (list (mi-off p1 (mi-scale step i)))))
        (setq i (1+ i)))
      pts))
)

;; ==================== 通用工具：图元绘制 ====================

(defun mi-line (p1 p2)
  "两点直线，返回图元名"
  (entity:make-line p1 p2)
)

(defun mi-lines (pts / i ents)
  "点表依次连线，返回图元名表"
  (setq i 0
    ents '())
  (while (< i (1- (length pts)))
    (setq ents (cons (mi-line (nth i pts) (nth (1+ i) pts)) ents)
      i (1+ i)))
  (reverse ents)
)

(defun mi-pline (pts closed)
  "LWPOLYLINE，closed=1 闭合，返回图元名"
  (entity:make-lwpolyline pts nil 0 closed 0)
)

(defun mi-lwp (pts width closed)
  "带宽度 LWPOLYLINE，返回图元名"
  (entity:make-lwpolyline pts nil width closed 0)
)

(defun mi-circle (cen rad)
  "圆，返回图元名"
  (entity:make-circle cen rad)
)

(defun mi-arc (cen rad spt ept)
  "圆弧（自 spt 逆时针到 ept），返回图元名"
  (entity:make-arc cen rad spt ept)
)

(defun mi-text (str pt h ang)
  "居中文本，返回图元名"
  (entity:make-text str pt h ang 0.8 0 "MM")
)

(defun mi-text-l (str pt h ang)
  "左下对齐文本，返回图元名"
  (entity:make-text str pt h ang 0.8 0 "LB")
)

(defun mi-rect (p1 p2 dist side / n)
  "以 p1→p2 为底边、向 side 侧偏移 dist 的四边形，返回图元名"
  (setq n (mi-scale (mi-perp (mi-vec p1 p2) side) dist))
  (entity:make-lwpolyline (list p1 (mi-off p1 n) (mi-off p2 n) p2) nil 0 1 0)
)

(defun mi-rect-pts (p1 p2)
  "p1 p2 为对角点时的轴对齐矩形四角"
  (list (list (car p1) (cadr p1))
    (list (car p2) (cadr p1))
    (list (car p2) (cadr p2))
    (list (car p1) (cadr p2)))
)

(defun mi-axis-rect (p1 p2)
  "轴对齐矩形（p1 p2 为对角点），返回图元名"
  (entity:make-lwpolyline (mi-rect-pts p1 p2) nil 0 1 0)
)

;; ==================== 通用工具：填充/图层 ====================

(defun mi-hatch (ent pat scale / h)
  "单外边界填充，返回填充图元名"
  (setq h (entity:addhatch (list ent) nil pat))
  (if scale (vla-put-patternscale h scale))
  (vlax-vla-object->ename h)
)

(defun mi-hatch-ring (out-ent in-ent pat scale / h)
  "环带填充（外边界+内边界），返回填充图元名"
  (setq h (entity:addhatch (list out-ent) (list in-ent) pat))
  (if scale (vla-put-patternscale h scale))
  (vlax-vla-object->ename h)
)

(defun mi-fill-spec (kind)
  "材料 → (填充名 . 比例)：glass/wood/ply/stone/conc/其他"
  (cond ((= "glass" kind) (list "SLANT" 2.0))
    ((= "wood" kind)  (list "STITCH" 1.0))
    ((= "ply" kind)   (list "STITCH" 0.5))
    ((= "stone" kind) (list "SAND" 1.0))
    ((= "conc" kind)  (list "CONCRETE" 1.0))
    (t (list "SOLID" 1.0)))
)

(defun mi-draw-sec (p1 p2 thk fill / r spec)
  "板材剖面：沿 p1→p2 厚 thk、带填充，返回 (轮廓图元 填充图元)"
  (setq r (mi-rect p1 p2 thk 0)
    spec (mi-fill-spec fill))
  (list r (mi-hatch r (car spec) (cadr spec)))
)

(defun mi-ensure-layer (name color ltype)
  "确保图层存在（无则创建），线型缺失时尝试加载。返回图层名"
  (if (and ltype
      (not (eq "CONTINUOUS" (strcase ltype)))
      (not (tblsearch "LTYPE" ltype)))
    (progn
      (vl-catch-all-apply 'command (list "._-LTYPE" "L" (strcase ltype)))
      (if (not (tblsearch "LTYPE" ltype)) (setq ltype "CONTINUOUS"))))
  (if (not (tblsearch "LAYER" name))
    (layer:make name color ltype nil))
  name
)

;; ==================== 通用工具：曲线采样/符号 ====================

(defun mi-pline-pts (ent / pts)
  "折线（LWPOLYLINE/POLYLINE）全部顶点"
  (setq pts '())
  (foreach code (entget ent)
    (if (= 10 (car code))
      (setq pts (append pts (list (cdr code))))))
  pts
)

(defun mi-curve-pts (ent dist / o l n remainder i pts p)
  "曲线按等距 dist 采样点表（非曲线实体返回 nil）"
  (if (vl-catch-all-error-p
      (vl-catch-all-apply 'vlax-curve-getendparam
        (list (vlax-ename->vla-object ent))))
    nil
    (progn
      (setq o (vlax-ename->vla-object ent)
        l (vlax-curve-getdistatparam o (vlax-curve-getendparam o)))
      (if (and (> dist 0.0) (>= l dist))
        (progn
          (setq n (fix (/ l dist))
                remainder (- l (* n dist))
                i   0
                pts '())
          (while (<= i n)
            (setq p (vlax-curve-getpointatdist o (* i dist)))
            (setq pts (append pts (list (list (car p) (cadr p))))
              i (1+ i)))
          (if (> remainder (/ dist 2.0))
            (progn
              (setq p (vlax-curve-getpointatdist o l))
              (setq pts (append pts (list (list (car p) (cadr p)))))))
          pts)
        nil)))
)

(defun mi-centroid (pts)
  "点集简单平均中心"
  (list (/ (apply '+ (mapcar 'car pts)) (float (length pts)))
    (/ (apply '+ (mapcar 'cadr pts)) (float (length pts))))
)

(defun mi-elev-symbol (pt h val / ents)
  "标高符号：倒三角+水平线+文本 val，返回图元名表"
  (setq ents (list
      (mi-pline (list pt
          (list (- (car pt) (* 0.6 h)) (+ (cadr pt) h))
          (list (+ (car pt) (* 0.6 h)) (+ (cadr pt) h))
          pt) 1)
      (mi-line pt (list (+ (car pt) (* 2.5 h)) (cadr pt)))
      (mi-text-l val (list (+ (car pt) (* 2.7 h)) (+ (cadr pt) (* 0.3 h))) h 0)))
  ents
)

(defun mi-light-symbol (pt blk / ents)
  "灯具符号：圆 r50+斜十字；blk 有效时 INSERT blk"
  (if (and blk (not (= blk "")) (tblobj blk))
    (progn
      (command "._INSERT" blk pt 1 1 0)
      (list (entlast)))
    (progn
      (setq ents (list
          (mi-circle (list (car pt) (+ (cadr pt) 50)) 50)
          (mi-line (list (- (car pt) 35) (+ (cadr pt) 15))
            (list (+ (car pt) 35) (+ (cadr pt) 85)))
          (mi-line (list (- (car pt) 35) (+ (cadr pt) 85))
            (list (+ (car pt) 35) (+ (cadr pt) 15)))))
      ents))
)

;; ==================== 通用工具：剖面/偏移 ====================

(defun mi-steel-profile (p1 p2 h b tf tw / w x0 n rel pts)
  "H/I 型钢轮廓（12点）：p1 p2 底宽，h 总高，b 翼缘宽，tf 翼缘厚，tw 腹板厚"
  (setq w (mi-len (mi-vec p1 p2))
        x0  (/ (- w b) 2.0)
        n   (mi-perp (mi-vec p1 p2) 0)
        rel (list (list 0 0) (list b 0) (list b tf)
                  (list (/ (+ b tw) 2.0) tf)
                  (list (/ (+ b tw) 2.0) (- h tf))
                  (list b (- h tf))
                  (list b h) (list 0 h)
                  (list 0 (- h tf))
                  (list (/ (- b tw) 2.0) (- h tf))
                  (list (/ (- b tw) 2.0) tf)
                  (list 0 tf))
        pts (mapcar (function (lambda (r)
                                (mi-off (mi-off p1
                                              (mi-scale (mi-vec p1 p2)
                                                        (/ (+ x0 (car r)) w)))
                                          (mi-scale n (cadr r)))))
                    rel))
  (entity:make-lwpolyline pts nil 0 1 0)
)

(defun mi-pline-offset (pts r side / i n1 n m d pts2)
  "折线向 side 侧偏移 r，顶点斜接，返回点表"
  (setq pts2 (list (mi-off (car pts)
        (mi-scale (mi-perp (mi-vec (car pts) (cadr pts)) side) r)))
    i 1)
  (while (< i (1- (length pts)))
    (setq n1 (mi-perp (mi-vec (nth (1- i) pts) (nth i pts)) side)
      n (mi-add n1 (mi-perp (mi-vec (nth i pts) (nth (1+ i) pts)) side)))
    (if (< (mi-len n) 0.0001)
      (setq m n1)
      (setq m (mi-scale n (/ 1.0 (mi-len n)))))
    (setq d (mi-dot m n1))
    (if (< (abs d) 0.0001) (setq d 0.0001))
    (setq pts2 (append pts2 (list (mi-off (nth i pts) (mi-scale m (/ r d))))))
    (setq i (1+ i)))
  (append pts2
    (list (mi-off (nth (1- (length pts)) pts)
        (mi-scale (mi-perp
            (mi-vec (nth (- (length pts) 2) pts)
              (nth (1- (length pts)) pts))
            side) r))))
)

(defun mi-div-lines (p1 p2 n dist side / i ents a b)
  "线段 p1→p2 均布 n-1 条分格线（向 side 侧长 dist），返回图元名表"
  (setq i 1
    ents '())
  (while (< i n)
    (setq a (mi-off p1 (mi-scale (mi-vec p1 p2) (/ i n)))
      b (mi-off a (mi-scale (mi-perp (mi-vec p1 p2) side) dist)))
    (setq ents (append ents (list (mi-line a b))))
    (setq i (1+ i)))
  ents
)

(defun mi-get-side ()
  "交互选择绘制侧：0 左 / 1 右"
  (if (eq "右" (getkword "\n绘制在哪一侧 [左/右] <左>: "))
    1 0)
)
