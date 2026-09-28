;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dim-leader - 圆心引线标注
;; 对"共圆心的两个半圆弧"的中心点，用多折线引线标注其旁边的单行文本。
;; 文本自动分层、可多列横向错开、并可按需旋转 90°，使标注文字互不重叠，
;; 同时尽量缩短引线。
;;
;; 用法：加载后在命令行执行 (dim-leader:run)
;; 或从 @lisp 面板 "标注" -> "圆心引线标注" 调用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:define-config 'dim-leader:layer "" "引线及文字所在图层，为空时用图元原图层")
(@:define-config 'dim-leader:offset 2.0 "文本相对圆心的水平距离")
(@:define-config 'dim-leader:match-tol 0.5 "文本匹配容差")
(@:define-config 'dim-leader:gap 1.0 "标注文字分层/分列时的间隙")
(@:define-config 'dim-leader:columns 4 "允许横向错开的最大列数")
(@:define-config 'dim-leader:avoid-leader 0 "尽量使文字不压引线(1开,较慢,密集时慎用)")
(@:define-config 'dim-leader:layout 0 "布局: 0=本地错开 1=右侧表格 2=分区表格 3=二维分区 4=四向 5=分向轨道 6=芯片式左右分区 7=紧凑双侧(均衡/居中/彩色循环) 8=通道布线(四带,引线零共线重叠)")
(@:define-config 'dim-leader:zones 8 "分区表格的分带数")
(@:define-config 'dim-leader:rotate 1 "允许把部分文本旋转为竖向(90°)，1 允许 0 不允许")
(@:define-config 'dim-leader:leader-width nil "引线宽度，nil 为默认细线")
(setq dim-leader:*chip-error* nil)

;; 各布局的菜单名，下标即 dim-leader:layout 的取值；菜单与配置共用这一张表
(setq dim-leader:*layout-names*
      '("0 本地错开" "1 右侧表格" "2 分区表格" "3 二维分区" "4 四向"
        "5 分向轨道" "6 芯片式左右分区" "7 紧凑双侧" "8 通道布线"))

;; ---------------------------------------------------------------------------
;; 工具函数
;; ---------------------------------------------------------------------------

;; 判断角度差 d 是否约为"半圆"(target 为半圆对应的角度，full 为一整圈的角度)
;; 兼容弧度(pi/2pi)与角度(180/360)两种单位，取较小夹角比较
(defun dim-leader:span-to-half (d full target tol / a)
  (setq a (rem d full))
  (if (< a 0.0) (setq a (+ a full)))
  (if (> a (/ full 2.0)) (setq a (- full a)))
  (< (abs (- a target)) tol))

;; 起始角到终止角是否构成半圆弧（兼容该 CAD 以弧度返回 ARC 角度的情况）
(defun dim-leader:half-circle-p (sa ea / d)
  (setq d (- ea sa))
  (or (dim-leader:span-to-half d (* 2.0 pi) pi 0.02)
      (dim-leader:span-to-half d 360.0 180.0 0.5)))

;; 生成"圆心键"字符串（3 位小数），用于把同圆心的圆弧分组
(defun dim-leader:center-key (p)
  (strcat (rtos (car p) 2 3) "," (rtos (cadr p) 2 3)))

;; 修改或新增 DXF 组码
(defun dim-leader:setd (dat code val)
  (if (assoc code dat)
    (subst (cons code val) (assoc code dat) dat)
    (append dat (list (cons code val)))))

;; 矩形碰撞检测：box=(bl br bb bt)，检查是否与已放置的任一矩形相交
(defun dim-leader:box-hit-p (placed bl br bb bt / found)
  (setq found nil)
  (foreach p placed
    (if (null found)
      (if (and (> (nth 1 p) bl) (< (nth 0 p) br) (> (nth 3 p) bb) (< (nth 2 p) bt))
        (setq found t))))
  found)

;; 点在矩形内（含边界）
(defun dim-leader:pt-in-rect (x y bl br bb bt)
  (and (>= x bl) (<= x br) (>= y bb) (<= y bt)))

;; 两线段是否相交（一般位置判定）
(defun dim-leader:seg-seg-hit (ax ay bx by cx cy dx dy / d1 d2 d3 d4)
  (setq d1 (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))
        d2 (- (* (- bx ax) (- dy ay)) (* (- by ay) (- dx ax)))
        d3 (- (* (- dx cx) (- ay cy)) (* (- dy cy) (- ax cx)))
        d4 (- (* (- dx cx) (- by cy)) (* (- dy cy) (- bx cx))))
  (and (or (and (> d1 0.0) (< d2 0.0)) (and (< d1 0.0) (> d2 0.0)))
       (or (and (> d3 0.0) (< d4 0.0)) (and (< d3 0.0) (> d4 0.0)))))

;; 线段与矩形是否相交（含端点落入）
(defun dim-leader:seg-rect-hit (x1 y1 x2 y2 bl br bb bt)
  (or (dim-leader:pt-in-rect x1 y1 bl br bb bt)
      (dim-leader:pt-in-rect x2 y2 bl br bb bt)
      (dim-leader:seg-seg-hit x1 y1 x2 y2 bl bb br bb)
      (dim-leader:seg-seg-hit x1 y1 x2 y2 br bb br bt)
      (dim-leader:seg-seg-hit x1 y1 x2 y2 br bt bl bt)
      (dim-leader:seg-seg-hit x1 y1 x2 y2 bl bt bl bb)))

;; 组装一个已放置项：文本盒 + 两条引线段 + x 范围(粗筛用)
(defun dim-leader:mk-placed (bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2)
  (list bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2
        (min bl s1x1 s1x2 s2x1 s2x2)
        (max br s1x1 s1x2 s2x1 s2x2)))

;; 评估候选放置，返回 (hit cross)：
;;   hit   = 文本盒与已放文本盒相交（硬约束，必须为 nil）
;;   cross = 候选引线穿过已放文本 或 已放引线穿过候选文本 的次数（软约束，计入代价）
(defun dim-leader:eval-place (placed bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2 / hit cross p minx maxx)
  (setq minx (min bl s1x1 s1x2 s2x1 s2x2)
        maxx (max br s1x1 s1x2 s2x1 s2x2)
        hit nil
        cross 0)
  (foreach p placed
    (if (and (> (nth 13 p) minx) (< (nth 12 p) maxx))
      (if (and (> (nth 1 p) bl) (< (nth 0 p) br) (> (nth 3 p) bb) (< (nth 2 p) bt))
        (setq hit t)
        (if (or (dim-leader:seg-rect-hit s1x1 s1y1 s1x2 s1y2 (nth 0 p) (nth 1 p) (nth 2 p) (nth 3 p))
                (dim-leader:seg-rect-hit s2x1 s2y1 s2x2 s2y2 (nth 0 p) (nth 1 p) (nth 2 p) (nth 3 p))
                (dim-leader:seg-rect-hit (nth 4 p) (nth 5 p) (nth 6 p) (nth 7 p) bl br bb bt)
                (dim-leader:seg-rect-hit (nth 8 p) (nth 9 p) (nth 10 p) (nth 11 p) bl br bb bt))
          (setq cross (1+ cross))))))
  (list hit cross))

;; 列表第 idx 位置设为 val
(defun dim-leader:setnth (lst idx val / res i)
  (setq res nil i 0)
  (foreach x lst
    (setq res (cons (if (= i idx) val x) res))
    (setq i (1+ i)))
  (reverse res))

;; 生成 n 个 nil 的列表
(defun dim-leader:list-nil (n / r)
  (setq r nil)
  (while (> n 0) (setq r (cons nil r) n (1- n)))
  r)

;; 去除折线中相邻重复点
(defun dim-leader:dedupe-pts (pts / res)
  (setq res nil)
  (foreach p pts
    (if (or (null res) (> (distance (car res) p) 1.0e-6))
      (setq res (cons p res))))
  (reverse res))

;; 线段穿过的文本数（带 x 粗筛）
(defun dim-leader:seg-cross-boxes (boxes px py qx qy / c bx minx maxx)
  (setq c 0
        minx (min px qx)
        maxx (max px qx))
  (foreach bx boxes
    (if (and (> (nth 2 bx) minx) (< (nth 0 bx) maxx))
      (if (dim-leader:seg-rect-hit px py qx qy (nth 0 bx) (nth 1 bx) (nth 2 bx) (nth 3 bx))
        (setq c (1+ c)))))
  c)

;; 折线穿过的文本总数
(defun dim-leader:route-cross (boxes pts / c n i a b)
  (setq c 0 n (length pts) i 0)
  (while (< i (1- n))
    (setq a (nth i pts) b (nth (1+ i) pts))
    (setq c (+ c (dim-leader:seg-cross-boxes boxes (car a) (cadr a) (car b) (cadr b))))
    (setq i (1+ i)))
  c)

(defun dim-leader:orient (ax ay bx by cx cy / d)
  (setq d (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))
  (if (< d -1.0e-9)
    -1
    (if (> d 1.0e-9) 1 0)))

(defun dim-leader:seg-seg-hit2 (ax ay bx by cx cy dx dy / o1 o2 o3 o4)
  (setq o1 (dim-leader:orient ax ay bx by cx cy)
        o2 (dim-leader:orient ax ay bx by dx dy)
        o3 (dim-leader:orient cx cy dx dy ax ay)
        o4 (dim-leader:orient cx cy dx dy bx by))
  (and (<= (* o1 o2) 0)
       (<= (* o3 o4) 0)
       (<= (min ax bx) (+ (max cx dx) 1.0e-9))
       (<= (min cx dx) (+ (max ax bx) 1.0e-9))
       (<= (min ay by) (+ (max cy dy) 1.0e-9))
       (<= (min cy dy) (+ (max ay by) 1.0e-9))))

(defun dim-leader:seg-box-hit2 (x1 y1 x2 y2 bl br bb bt)
  (or (dim-leader:pt-in-rect x1 y1 bl br bb bt)
      (dim-leader:pt-in-rect x2 y2 bl br bb bt)
      (dim-leader:seg-seg-hit2 x1 y1 x2 y2 bl bb br bb)
      (dim-leader:seg-seg-hit2 x1 y1 x2 y2 br bb br bt)
      (dim-leader:seg-seg-hit2 x1 y1 x2 y2 br bt bl bt)
      (dim-leader:seg-seg-hit2 x1 y1 x2 y2 bl bt bl bb)))

(defun dim-leader:route-hit-box (route box / hit a b i n)
  (setq hit nil i 0 n (length route))
  (while (and (not hit) (< i (1- n)))
    (setq a (nth i route) b (nth (1+ i) route))
    (if (dim-leader:seg-box-hit2 (car a) (cadr a) (car b) (cadr b)
                                 (nth 0 box) (nth 1 box) (nth 2 box) (nth 3 box))
      (setq hit t))
    (setq i (1+ i)))
  hit)

(defun dim-leader:route-hit-boxes (route boxes / hit)
  (setq hit nil)
  (foreach box boxes
    (if (and (not hit) (dim-leader:route-hit-box route box))
      (setq hit t)))
  hit)

(defun dim-leader:route-hit-route (a b / hit s1 s2 sa1 sa2 i j n m)
  (setq hit nil i 0 n (length a))
  (while (and (not hit) (< i (1- n)))
    (setq s1 (nth i a) s2 (nth (1+ i) a) j 0 m (length b))
    (while (and (not hit) (< j (1- m)))
      (setq sa1 (nth j b) sa2 (nth (1+ j) b))
      (if (dim-leader:seg-seg-hit2 (car s1) (cadr s1) (car s2) (cadr s2)
                                   (car sa1) (cadr sa1) (car sa2) (cadr sa2))
        (setq hit t))
      (setq j (1+ j)))
    (setq i (1+ i)))
  hit)

(defun dim-leader:route-keepouts (route clearance / boxes a b x1 y1 x2 y2 i n)
  (setq boxes nil i 0 n (length route))
  (while (< i (1- n))
    (setq a (nth i route) b (nth (1+ i) route)
          x1 (car a) y1 (cadr a) x2 (car b) y2 (cadr b))
    (if (= x1 x2)
      (setq boxes (cons (list (- x1 clearance) (+ x1 clearance)
                              (- (min y1 y2) clearance) (+ (max y1 y2) clearance))
                        boxes))
      (setq boxes (cons (list (- (min x1 x2) clearance) (+ (max x1 x2) clearance)
                              (- y1 clearance) (+ y1 clearance))
                        boxes)))
    (setq i (1+ i)))
  (reverse boxes))

(defun dim-leader:route-safe-p (route boxes keepouts)
  (and (not (dim-leader:route-hit-boxes route boxes))
       (not (dim-leader:route-hit-boxes route keepouts))))

(defun dim-leader:route-length (route / total a b i n)
  (setq total 0.0 i 0 n (length route))
  (while (< i (1- n))
    (setq a (nth i route) b (nth (1+ i) route))
    (setq total (+ total (distance a b)))
    (setq i (1+ i)))
  total)

(defun dim-leader:route-bends (route / count a b i n)
  (setq count 0 i 1 n (length route))
  (while (< i (1- n))
    (setq a (nth (1- i) route) b (nth (1+ i) route))
    (if (and (/= (car a) (car b)) (/= (cadr a) (cadr b)))
      (setq count (1+ count)))
    (setq i (1+ i)))
  count)

(defun dim-leader:unique-nums (vals / res v eps)
  (setq res nil eps 1.0e-7)
  (foreach v (vl-sort (vl-remove-if-not (function numberp) vals)
                      (function (lambda (a b) (< a b))))
    (if (or (null res) (> (abs (- v (car res))) eps))
      (setq res (cons v res))))
  (reverse res))

(defun dim-leader:expand-nums (vals gap / res)
  (setq res nil)
  (foreach v vals
    (setq res (cons (- v gap) (cons v (cons (+ v gap) res)))))
  (dim-leader:unique-nums res))

(defun dim-leader:thin-nums (vals limit / res n i idx)
  (setq vals (dim-leader:unique-nums vals) n (length vals))
  (if (<= n limit)
    vals
    (progn
      (setq res nil i 0)
      (while (< i limit)
        (setq idx (fix (+ 0.5 (* i (/ (1- n) (1- limit))))))
        (setq res (cons (nth idx vals) res))
        (setq i (1+ i)))
      (reverse res))))

(defun dim-leader:chip-route-cost (route gap)
  (+ (dim-leader:route-length route)
     (* (dim-leader:route-bends route) gap 2.0)))

(defun dim-leader:chip-consider (route boxes keepouts gap / cost)
  (setq route (dim-leader:dedupe-pts route))
  (setq cost nil)
  (if (and (> (length route) 1)
           (dim-leader:route-safe-p route boxes keepouts))
    (setq cost (dim-leader:chip-route-cost route gap)))
  cost)

(defun dim-leader:chip-find-route (start goal xs ys tracks boxes keepouts gap
                                   / sx sy gx gy x y cand cost best bestcost)
  (setq sx (car start) sy (cadr start)
        gx (car goal) gy (cadr goal)
        best nil bestcost nil)
  (if (or (= sx gx) (= sy gy))
    (progn
      (setq cand (list start goal))
      (setq cost (dim-leader:chip-consider cand boxes keepouts gap))
      (if (and cost (or (null bestcost) (< cost bestcost)))
        (setq best cand bestcost cost))))
  (setq cand (list start (list sx gy) goal))
  (setq cost (dim-leader:chip-consider cand boxes keepouts gap))
  (if (and cost (or (null bestcost) (< cost bestcost)))
    (setq best cand bestcost cost))
  (setq cand (list start (list gx sy) goal))
  (setq cost (dim-leader:chip-consider cand boxes keepouts gap))
  (if (and cost (or (null bestcost) (< cost bestcost)))
    (setq best cand bestcost cost))
  (foreach x xs
    (setq cand (list start (list x sy) (list x gy) goal))
    (setq cost (dim-leader:chip-consider cand boxes keepouts gap))
    (if (and cost (or (null bestcost) (< cost bestcost)))
      (setq best cand bestcost cost)))
  (foreach y ys
    (setq cand (list start (list sx y) (list gx y) goal))
    (setq cost (dim-leader:chip-consider cand boxes keepouts gap))
    (if (and cost (or (null bestcost) (< cost bestcost)))
      (setq best cand bestcost cost)))
  (foreach x xs
    (foreach y tracks
      (setq cand (list start (list x sy) (list x y) (list gx y) goal))
      (setq cost (dim-leader:chip-consider cand boxes keepouts gap))
      (if (and cost (or (null bestcost) (< cost bestcost)))
        (setq best cand bestcost cost))))
  best)

(defun dim-leader:chip-route-attempt (plans boxes xs ys tracks clearance gap / out ok keepouts route)
  (setq out nil ok t keepouts nil)
  (foreach plan plans
    (setq route (dim-leader:dedupe-pts
                  (dim-leader:chip-find-route (nth 8 plan) (nth 9 plan)
                                              xs ys tracks boxes keepouts gap)))
    (if route
      (progn
        (setq out (cons (append plan (list route)) out))
        (setq keepouts (append keepouts
                               (dim-leader:route-keepouts route clearance))))
      (setq ok nil)))
  (if ok (reverse out)))

(defun dim-leader:routes-valid-p (routes boxes / ok i j)
  (setq ok t i 0)
  (while (and ok (< i (length routes)))
    (if (dim-leader:route-hit-boxes (nth i routes) boxes)
      (setq ok nil))
    (setq j (1+ i))
    (while (and ok (< j (length routes)))
      (if (dim-leader:route-hit-route (nth i routes) (nth j routes))
        (setq ok nil))
      (setq j (1+ j)))
    (setq i (1+ i)))
  ok)

(defun dim-leader:chip-side-plans (g side edge rowLo rowPitch rowCap colPitch gap
                                   / plans i n colCount rowCount col row ctr ent ax ay hj box start goal)
  (setq plans nil i 0 n (length g))
  (setq colCount (max 1 (1+ (fix (/ (1- n) rowCap))))
        rowCount (max 1 (fix (/ (+ n colCount -1) colCount))))
  (while (< i n)
    (setq col (fix (/ i rowCount)) row (rem i rowCount))
    (setq ctr (nth 0 (nth i g)) ent (nth 1 (nth i g))
          ay (+ rowLo (* row rowPitch)))
    (if (= side 1)
      (setq ax (+ edge (* col colPitch))
            hj 0
            box (list ax (+ ax (nth 4 (nth i g)))
                      (- ay (/ (nth 5 (nth i g)) 2.0))
                      (+ ay (/ (nth 5 (nth i g)) 2.0)))
            start (list (car ctr) (cadr ctr))
            goal (list (- ax (/ gap 2.0)) ay))
      (setq ax (- edge (* col colPitch))
            hj 2
            box (list (- ax (nth 4 (nth i g))) ax
                      (- ay (/ (nth 5 (nth i g)) 2.0))
                      (+ ay (/ (nth 5 (nth i g)) 2.0)))
            start (list (car ctr) (cadr ctr))
            goal (list (+ ax (/ gap 2.0)) ay)))
    (setq plans (cons (list side ctr ent ax ay hj 2 box start goal) plans))
    (setq i (1+ i)))
  (reverse plans))

(defun dim-leader:layout-chip (matched obstacles
                               / gap maxw maxh items m ctr ent dat ref h w tb box boxes
                                 centerX minX maxX minY maxY cx cy ob rgroup lgroup
                                 margin rowPitch colPitch rowLo rowHi rowSpan rowCap
                                 rCount lCount rEdge lEdge vals xs ys tracks plans
                                 orderplans clearances clearance order routed routeplans
                                 results result k)
  (setq gap (@:get-config 'dim-leader:gap))
  (if (not (and (numberp gap) (> gap 0.0))) (setq gap 1.0))
  (setq maxw 0.0 maxh 0.0 items nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (if (null ref) (setq ref ctr))
    (setq h (cdr (assoc 40 dat)))
    (if (not (and (numberp h) (> h 0.0))) (setq h 2.5))
    (setq tb (vl-catch-all-apply 'textbox (list dat)))
    (if (and (listp tb) (car tb) (cadr tb) (listp (car tb)) (listp (cadr tb)))
      (setq w (abs (- (car (cadr tb)) (caar tb))))
      (setq w (dim-leader:text-fallback-width dat)))
    (if (not (and (numberp w) (> w 0.0))) (setq w (* h 4.0)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items)))
  (if (or (null items) (= 0.0 maxw) (= 0.0 maxh))
    (progn
      (setq dim-leader:*chip-error* "无可用文字尺寸")
      nil)
    (progn
      (setq centerX 0.0)
      (foreach m items
        (setq centerX (+ centerX (car (nth 0 m)))))
      (setq centerX (/ centerX (length items)))
      (setq rgroup nil lgroup nil)
      (foreach m items
        (if (>= (car (nth 0 m)) centerX)
          (setq rgroup (cons m rgroup))
          (setq lgroup (cons m lgroup))))
      (setq boxes nil minX 1.0e30 maxX -1.0e30 minY 1.0e30 maxY -1.0e30)
      (foreach m items
        (setq cx (car (nth 0 m)) cy (cadr (nth 0 m)))
        (if (< cx minX) (setq minX cx))
        (if (> cx maxX) (setq maxX cx))
        (if (< cy minY) (setq minY cy))
        (if (> cy maxY) (setq maxY cy)))
      (foreach ob obstacles
        (setq box (list (nth 0 ob) (nth 1 ob) (nth 2 ob) (nth 3 ob)))
        (setq boxes (cons box boxes))
        (if (< (nth 0 box) minX) (setq minX (nth 0 box)))
        (if (> (nth 1 box) maxX) (setq maxX (nth 1 box)))
        (if (< (nth 2 box) minY) (setq minY (nth 2 box)))
        (if (> (nth 3 box) maxY) (setq maxY (nth 3 box))))
      (setq margin (max (* gap 3.0) maxh)
            rowPitch (+ maxh gap)
            colPitch (+ maxw gap)
            rowLo (- (+ minY margin) (/ maxh 2.0))
            rowHi (+ (- maxY margin) (/ maxh 2.0))
            rowSpan (- rowHi rowLo)
            rowCap (max 1 (1+ (fix (/ rowSpan rowPitch)))))
      (setq rgroup (vl-sort rgroup (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))))
      (setq lgroup (vl-sort lgroup (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))))
      (setq rCount (length rgroup) lCount (length lgroup))
      (setq rEdge (+ maxX margin) lEdge (- minX margin))
      (setq plans nil)
      (if rgroup
        (setq plans (append plans
                            (dim-leader:chip-side-plans
                              rgroup 1 rEdge rowLo rowPitch rowCap
                              colPitch gap))))
      (if lgroup
        (setq plans (append plans
                            (dim-leader:chip-side-plans
                              lgroup 2 lEdge rowLo rowPitch rowCap
                              colPitch gap))))
      (setq boxes (append boxes
                          (mapcar (function (lambda (p) (nth 7 p))) plans)))
      (setq vals nil)
      (foreach p plans
        (setq vals (cons (car (nth 8 p)) (cons (car (nth 9 p)) vals))
              vals (cons (car (nth 7 p)) (cons (nth 1 (nth 7 p)) vals))))
      (foreach b boxes
        (setq vals (cons (nth 0 b) (cons (nth 1 b) vals))))
      (setq xs (dim-leader:thin-nums (dim-leader:expand-nums vals gap) 90))
      (setq vals nil)
      (foreach p plans
        (setq vals (cons (cadr (nth 8 p)) (cons (cadr (nth 9 p)) vals))
              vals (cons (nth 2 (nth 7 p)) (cons (nth 3 (nth 7 p)) vals))))
      (foreach b boxes
        (setq vals (cons (nth 2 b) (cons (nth 3 b) vals))))
      (setq ys (dim-leader:thin-nums (dim-leader:expand-nums vals gap) 90))
      (setq tracks nil k 0)
      (while (< k 4)
        (setq tracks (cons (- rowLo margin (* k gap)) tracks)
              tracks (cons (+ rowHi margin (* k gap)) tracks)
              k (1+ k)))
      (setq tracks (dim-leader:unique-nums tracks))
      (setq orderplans (list plans (reverse plans))
            clearances (list (/ gap 8.0) (/ gap 20.0) 0.001)
            routed nil)
      (while (and (null routed) clearances)
        (setq clearance (max 0.001 (car clearances)))
        (foreach order orderplans
          (if (and (null routed) order)
            (setq routed (dim-leader:chip-route-attempt order boxes xs ys tracks
                                                        clearance gap))))
        (setq clearances (cdr clearances)))
      (setq dim-leader:*chip-error* nil)
      (if (null routed)
        (setq dim-leader:*chip-error* "无可用正交通道，未修改图形")
        (progn
          (setq routeplans (mapcar (function (lambda (p) (nth 10 p))) routed))
          (if (not (dim-leader:routes-valid-p routeplans boxes))
            (setq routed nil
                  dim-leader:*chip-error* "引线交叉复核未通过，未修改图形"))))
      (if routed
        (progn
          (setq results nil)
          (foreach p routed
            (setq result (list (nth 1 p) (nth 2 p) (nth 3 p) (nth 4 p) 0
                               (nth 5 p) (nth 6 p) (nth 10 p)))
            (setq results (cons result results)))
          (reverse results))))))

(defun dim-leader:text-fallback-width (dat / txt h wf w)
  (setq txt (cdr (assoc 1 dat))
        h (cdr (assoc 40 dat))
        wf (cdr (assoc 41 dat)))
  (if (not (and (numberp h) (> h 0.0))) (setq h 2.5))
  (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
  (setq w (vl-catch-all-apply 'text:gettextwidth (list txt dat)))
  (if (not (numberp w))
    (setq w (* (abs (strlen (if txt txt "m"))) h wf 0.6)))
  (if (< w 0.0) (setq w (- 0.0 w)))
  w)

;; 计算一个文本当前的包围盒 (bl br bb bt)，用于作为障碍物
;; 依据实测：rot=0 盒=[x, x+w]x[y-h/2, y+h/2]；rot=90 盒=[x-h/2, x+h/2]x[y, y+w]
;; 其中 (x y) 为参考点(11/10)，角度组码以弧度计
(defun dim-leader:text-box (ent / dat ins ref h rot wf tb w)
  (setq dat (entget ent)
        ins (cdr (assoc 10 dat)))
  (setq ref (cdr (assoc 11 dat)))
  (if (null ref) (setq ref ins))
  (setq h (cdr (assoc 40 dat))
        rot (cdr (assoc 50 dat)))
  (if (null rot) (setq rot 0.0))
  (setq wf (cdr (assoc 41 dat)))
  (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
  (setq tb (textbox dat))
  (if tb
    (setq w (- (car (cadr tb)) (caar tb)))
    (setq w (dim-leader:text-fallback-width dat)))
  (if (< w 0.0) (setq w (- 0.0 w)))
  (if (> (rem (+ rot 0.001) pi) 1.5)
    (list (- (car ref) (/ h 2.0)) (+ (car ref) (/ h 2.0)) (cadr ref) (+ (cadr ref) w))
    (list (car ref) (+ (car ref) w) (- (cadr ref) (/ h 2.0)) (+ (cadr ref) (/ h 2.0)))))

;; ---------------------------------------------------------------------------
;; 1. 查找中心点：恰好两条各跨 180° 的圆弧，且圆心相同
;; ---------------------------------------------------------------------------
(defun dim-leader:find-centers (/ ss i ent dat cen key g groups centers arcs a sa ea ok)
  (setq groups nil
        centers nil)
  (if (setq ss (ssget "_X" '((0 . "ARC"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              dat (entget ent))
        (if (and (assoc 10 dat) (assoc 50 dat) (assoc 51 dat))
          (progn
            (setq cen (cdr (assoc 10 dat)))
            (setq key (dim-leader:center-key cen))
            (if (setq g (assoc key groups))
              (setq groups (subst (cons key (cons ent (cdr g))) g groups))
              (setq groups (cons (cons key (list ent)) groups)))))
        (setq i (1+ i)))))
  (foreach g groups
    (setq arcs (cdr g))
    (if (= 2 (length arcs))
      (progn
        (setq ok t)
        (foreach a arcs
          (setq dat (entget a)
                sa (cdr (assoc 50 dat))
                ea (cdr (assoc 51 dat)))
          (if (not (dim-leader:half-circle-p sa ea)) (setq ok nil)))
        (if ok
          (setq centers (cons (cdr (assoc 10 (entget (car arcs)))) centers))))))
  (reverse centers))

;; ---------------------------------------------------------------------------
;; 2. 匹配文本：文本参考点(11/10) 在 圆心±offset 处，一对一分配
;; ---------------------------------------------------------------------------
(defun dim-leader:match-texts (centers / offset tol ss i ent dat ap texts cands ctr cx cy s ex dist res usedc usedt ck)
  (setq offset (@:get-config 'dim-leader:offset)
        tol (@:get-config 'dim-leader:match-tol))
  (setq texts nil)
  (if (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              dat (entget ent))
        (if (= "TEXT" (cdr (assoc 0 dat)))
          (progn
            (setq ap (cdr (assoc 11 dat)))
            (if (null ap) (setq ap (cdr (assoc 10 dat))))
            (if ap (setq texts (cons (cons ent ap) texts)))))
        (setq i (1+ i)))))
  (setq cands nil)
  (foreach ctr centers
    (setq cx (car ctr) cy (cadr ctr))
    (foreach tpair texts
      (setq ent (car tpair) ap (cdr tpair))
      (foreach s (list 1 -1)
        (setq ex (+ cx (* s offset))
              dist (distance (list ex cy) ap))
        (if (< dist tol)
          (setq cands (cons (list dist ctr ent s) cands))))))
  (setq cands (vl-sort cands (function (lambda (a b) (< (car a) (car b))))))
  (setq res nil usedc nil usedt nil)
  (foreach c cands
    (setq ctr (nth 1 c) ent (nth 2 c) s (nth 3 c)
          ck (dim-leader:center-key ctr))
    (if (and (not (member ck usedc)) (not (member ent usedt)))
      (setq usedc (cons ck usedc)
            usedt (cons ent usedt)
            res (cons (list ctr ent s) res))))
  (reverse res))

;; ---------------------------------------------------------------------------
;; 3. 布局：每个文本尝试 (旋转 0/90) × (横向第 k 列) × (竖向最近空层)，
;;    取引线最短者。列间距分别取 文本宽/高 + gap，使不同列互不重叠。
;;    返回 ((圆心 文本图元 折点X 接点Y 锚点X 锚点Y 旋转) ...)
;; ---------------------------------------------------------------------------
(defun dim-leader:layout (matched obstacles / gap levelH maxw maxh pitch0 pitch90 rotallow cols klist
                          items m ctr ent dat ref h wf style txt w tb placed res it
                          base_x base_y rot k step ax found o oo oo2 ay bl br bb bt best bestcost
                          cand-cost by elbow_x attach_y anchor_x anchor_y cx cy ev cross0 avoid
                          s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2
                          boxes newres xms xm pl nx bestnx ex)
  (setq gap (@:get-config 'dim-leader:gap)
        rotallow (= 1 (@:get-config 'dim-leader:rotate))
        avoid (= 1 (@:get-config 'dim-leader:avoid-leader))
        cols (@:get-config 'dim-leader:columns)
        maxw 0.0 maxh 0.0 items nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m))
    (setq dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat))
          style (cdr (assoc 7 dat))
          txt (cdr (assoc 1 dat)))
    (setq wf (cdr (assoc 41 dat)))
    (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
    (setq tb (textbox dat))
    (if tb
      (setq w (- (car (cadr tb)) (caar tb)))
      (setq w (dim-leader:text-fallback-width dat)))
    (if (< w 0.0) (setq w (- 0.0 w)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items)))
  (setq levelH (+ maxh gap)
        pitch0 (+ maxw gap)
        pitch90 (+ maxh gap))
  ;; 处理顺序：按原始锚点 X，再按 Y
  (setq items (vl-sort items
                (function
                  (lambda (a b)
                    (if (/= (nth 2 a) (nth 2 b))
                      (< (nth 2 a) (nth 2 b))
                      (if (/= (nth 3 a) (nth 3 b))
                        (< (nth 3 a) (nth 3 b))
                        (< (nth 4 a) (nth 4 b))))))))
  ;; 列偏移序列：0, ±1, ±2, ... ±cols
  (setq klist (list 0) k 1)
  (while (<= k cols)
    (setq klist (append klist (list k (- k))))
    (setq k (1+ k)))
  (setq placed obstacles res nil)
  (foreach it items
    (setq ctr (nth 0 it) ent (nth 1 it)
          base_x (nth 2 it) base_y (nth 3 it)
          w (nth 4 it) h (nth 5 it)
          cx (car ctr) cy (cadr ctr))
    ;; 快速路径：原位(rot=0, 第0列, 第0层)且不压文本、不被引线穿过
    (setq bl base_x br (+ base_x w) bb (- base_y (/ h 2.0)) bt (+ base_y (/ h 2.0))
          s1x1 cx s1y1 cy s1x2 cx s1y2 base_y
          s2x1 cx s2y1 base_y s2x2 base_x s2y2 base_y
          best nil bestcost nil)
    (setq ev (if avoid
               (dim-leader:eval-place placed bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2)
               (list (dim-leader:box-hit-p placed bl br bb bt) 0)))
    (if (and (not (car ev)) (zerop (cadr ev)))
      (setq best (list 0 base_x base_y bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2)))
    (if (null best)
      (progn
        (foreach rot (if rotallow (list 0 90) (list 0))
          (setq step (if (= rot 90) (+ w gap) levelH))
          (foreach k klist
            (setq ax (+ base_x (* k (if (= rot 90) pitch90 pitch0))))
            (setq found nil o 0 oo nil cross0 0)
            (while (not found)
              (foreach oo2 (if (zerop o) (list 0) (list o (- o)))
                (if (not found)
                  (progn
                    (setq oo oo2)
                    (setq ay (+ base_y (* oo step)))
                    (if (= rot 0)
                      (setq bl ax br (+ ax w) bb (- ay (/ h 2.0)) bt (+ ay (/ h 2.0)))
                      (setq bl (- ax (/ h 2.0)) br (+ ax (/ h 2.0)) bb (- ay (/ w 2.0)) bt (+ ay (/ w 2.0))))
                    (if (not (dim-leader:box-hit-p placed bl br bb bt))
                      (setq found t)))))
              (setq o (1+ o))
              (if (> o 400) (setq found t)))
            (if oo
              (progn
                (setq cross0 0)
                (if avoid
                  (progn
                    (if (= rot 0)
                      (setq s1x1 cx s1y1 cy s1x2 cx s1y2 ay s2x1 cx s2y1 ay s2x2 ax s2y2 ay)
                      (setq s1x1 cx s1y1 cy s1x2 cx s1y2 ay
                            s2x1 cx s2y1 ay s2x2 (- ax (/ h 2.0)) s2y2 ay))
                    (setq ev (dim-leader:eval-place placed bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2)
                          cross0 (cadr ev))))
                (setq cand-cost (+ (abs (* k (if (= rot 90) pitch90 pitch0)))
                                   (abs (* oo step))
                                   (if (= rot 90) 3.0 0.0)
                                   (* cross0 8.0))))
              (setq cand-cost nil))
            (if (and oo (or (null bestcost) (< cand-cost bestcost)))
              (setq bestcost cand-cost
                    best (list rot ax ay bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2)))))))
    (setq rot (nth 0 best) ax (nth 1 best) by (nth 2 best)
          bl (nth 3 best) br (nth 4 best) bb (nth 5 best) bt (nth 6 best)
          s1x1 (nth 7 best) s1y1 (nth 8 best) s1x2 (nth 9 best) s1y2 (nth 10 best)
          s2x1 (nth 11 best) s2y1 (nth 12 best) s2x2 (nth 13 best) s2y2 (nth 14 best))
    (if (= rot 0)
      (setq elbow_x ax anchor_x ax
            attach_y by anchor_y by)
      (setq elbow_x (- ax (/ h 2.0))
            anchor_x ax
            attach_y by
            anchor_y (- by (/ w 2.0))))
    (setq placed (cons (dim-leader:mk-placed bl br bb bt s1x1 s1y1 s1x2 s1y2 s2x1 s2y1 s2x2 s2y2) placed))
    (setq res (cons (list ctr ent anchor_x anchor_y rot elbow_x attach_y) res)))
  ;; 绕行走廊后处理：为每条引线在若干竖向走廊 x 中，取穿越文本最少者(允许 3 段折线)
  (setq boxes (mapcar (function (lambda (p) (list (nth 0 p) (nth 1 p) (nth 2 p) (nth 3 p)))) placed))
  (setq newres nil)
  (foreach r (reverse res)
    (setq ctr (nth 0 r) ent (nth 1 r) ax (nth 2 r) ay (nth 3 r) rot (nth 4 r)
          ex (nth 5 r) cx (car ctr) cy (cadr ctr))
    (setq xms (list cx ex (/ (+ cx ex) 2.0)
                    (/ (+ cx ex ex) 3.0) (/ (+ cx cx ex) 3.0))
          best nil bestnx nil)
    (foreach xm xms
      (setq pl (dim-leader:dedupe-pts (list (list cx cy) (list xm cy) (list xm ay) (list ex ay)))
            nx (dim-leader:route-cross boxes pl))
      (if (or (null bestnx)
              (< nx bestnx)
              (and (= nx bestnx) (< (length pl) (length best))))
        (setq bestnx nx best pl)))
    (setq newres (cons (list ctr ent ax ay rot best) newres)))
  (reverse newres))

;; ---------------------------------------------------------------------------
;; 3b. 表格布局：把所有标签排到右侧的"列×行"网格上；引线走全局行列间隙，
;;     因此不与任何文字相交（代价：标签离圆心较远）。
;;     引线: 圆心 -> (cx,间隙y) -> (列左,间隙y) -> (列左,标签y) -> 标签
;; ---------------------------------------------------------------------------
(defun dim-leader:layout-table (matched obstacles / gap maxw maxh colPitch rowPitch
                                items m ctr ent dat ref h wf style txt w tb
                                 x0 yTop yBottom rows i o col row lx ly gy route res cx cy trk trkPitch)
  (setq gap (@:get-config 'dim-leader:gap) maxw 0.0 maxh 0.0 items nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m))
    (setq dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat)) style (cdr (assoc 7 dat)) txt (cdr (assoc 1 dat)))
    (setq wf (cdr (assoc 41 dat)))
    (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
    (setq tb (textbox dat))
    (if tb (setq w (abs (- (car (cadr tb)) (caar tb)))) (setq w (dim-leader:text-fallback-width dat)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items)))
  (setq colPitch (+ maxw gap) rowPitch (+ maxh gap))
  (setq x0 -1.0e30 yTop -1.0e30 yBottom 1.0e30)
  (foreach m items
    (setq cx (car (nth 0 m)) cy (cadr (nth 0 m)))
    (if (> cx x0) (setq x0 cx))
    (if (> cy yTop) (setq yTop cy))
    (if (< cy yBottom) (setq yBottom cy)))
  (foreach o obstacles
    (if (> (nth 1 o) x0) (setq x0 (nth 1 o)))
    (if (> (nth 3 o) yTop) (setq yTop (nth 3 o)))
    (if (< (nth 2 o) yBottom) (setq yBottom (nth 2 o))))
  (setq rows (max 1 (fix (/ (- yTop yBottom) rowPitch)))
        x0 (+ x0 colPitch 5.0)
        yBottom (- yBottom 2.0))
  (setq items (vl-sort items (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))))
  ;; 排线：为每条引线分配独占的水平轨道（互不重叠）
  (setq trkPitch (/ (- yTop yBottom) (max 1 (length items))))
  (if (< trkPitch 0.25) (setq trkPitch 0.25))
  (setq i 0 trk 0 res nil)
  (foreach m items
    (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) h (nth 5 m))
    (setq col (fix (/ i rows)) row (rem i rows))
    (setq lx (+ x0 (* col colPitch))
          ly (+ yBottom (* row rowPitch) (/ h 2.0))
          gy (+ yBottom (* trk trkPitch)))
    (setq route (dim-leader:dedupe-pts
                  (list (list cx cy) (list cx gy) (list (- lx 0.5) gy) (list (- lx 0.5) ly) (list lx ly))))
    (setq res (cons (list ctr ent lx ly 0 route) res))
    (setq i (1+ i) trk (1+ trk)))
  (reverse res))

;; ---------------------------------------------------------------------------
;; 3c. 分区表格：按 y 把圆心分成若干带，每带的标签排到该带"局部右侧"的小表格，
;;     引线仍走全局行/列间隙 → 不压字，且引线比全图右侧大表短。
;; ---------------------------------------------------------------------------
(defun dim-leader:layout-table-zone (matched obstacles nbands
                                     / gap maxw maxh colPitch rowPitch items m ctr ent dat ref h wf style txt w tb
                                     yTop yBottom x0 bandH band b0 b1 bvals bmaxx i col rr lx ly gy route res cx cy rows r0 r1)
  (setq gap (@:get-config 'dim-leader:gap) maxw 0.0 maxh 0.0 items nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat)) style (cdr (assoc 7 dat)) txt (cdr (assoc 1 dat)))
    (setq wf (cdr (assoc 41 dat)))
    (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
    (setq tb (textbox dat))
    (if tb (setq w (abs (- (car (cadr tb)) (caar tb)))) (setq w (dim-leader:text-fallback-width dat)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items)))
  (setq colPitch (+ maxw gap) rowPitch (+ maxh gap))
  (setq yTop -1.0e30 yBottom 1.0e30)
  (foreach m items
    (setq cy (cadr (nth 0 m)))
    (if (> cy yTop) (setq yTop cy))
    (if (< cy yBottom) (setq yBottom cy)))
  (foreach o obstacles
    (if (> (nth 3 o) yTop) (setq yTop (nth 3 o)))
    (if (< (nth 2 o) yBottom) (setq yBottom (nth 2 o))))
  (setq yBottom (- yBottom 2.0)
        bandH (/ (- yTop yBottom) nbands)
        res nil
        band 0)
  (while (< band nbands)
    (setq b0 (+ yBottom (* band bandH))
          b1 (+ b0 bandH)
          bvals nil
          bmaxx -1.0e30)
    (foreach m items
      (setq cy (cadr (nth 0 m)) cx (car (nth 0 m)))
      (if (and (>= cy b0) (< cy b1))
        (progn
          (setq bvals (cons m bvals))
          (if (> cx bmaxx) (setq bmaxx cx)))))
    (if bvals
      (progn
        (setq bvals (vl-sort bvals (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))))
        (setq r0 (fix (/ (- b0 yBottom) rowPitch))
              r1 (max (1+ r0) (fix (/ (- b1 yBottom) rowPitch)))
              rows (- r1 r0))
        (setq i 0)
        (foreach m bvals
          (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) h (nth 5 m))
          (setq col (fix (/ i rows)) rr (+ r0 (rem i rows)))
          (setq lx (+ bmaxx colPitch 3.0 (* col colPitch))
                ly (+ yBottom (* rr rowPitch) (/ h 2.0))
                gy (+ yBottom (* rr rowPitch) maxh (/ gap 2.0)))
          (setq route (dim-leader:dedupe-pts
                        (list (list cx cy) (list cx gy) (list (- lx 0.5) gy) (list (- lx 0.5) ly) (list lx ly))))
          (setq res (cons (list ctr ent lx ly 0 route) res))
          (setq i (1+ i)))))
    (setq band (1+ band)))
  (reverse res))

;; ---------------------------------------------------------------------------
;; 3d. 二维分区 + 空白区放置：先栅格化"已占"网格(避开已有文字)，把圆心按二维分区，
;;     每区的小表格放进离该区最近的一块空白矩形；引线仍走全局行/列间隙。
;; ---------------------------------------------------------------------------
(defun dim-leader:layout-zone2d (matched obstacles nz
                                 / gap maxw maxh colPitch rowPitch items m ctr ent dat ref h wf style txt w tb
                                 boxlist b minX maxX minY maxY nx ny grid i j row
                                 zx0 zy0 zoneW zoneH labList nLab tabRows tabCols
                                 i0 j0 i1 j1 best bestd ii jj ok c k rr cc lx ly gy route res cx cy z cxz cyz ci1 ci2 jc kk allLab)
  (setq gap (@:get-config 'dim-leader:gap) maxw 0.0 maxh 0.0 items nil boxlist nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat)) style (cdr (assoc 7 dat)) txt (cdr (assoc 1 dat)))
    (setq wf (cdr (assoc 41 dat)))
    (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
    (setq tb (textbox dat))
    (if tb (setq w (abs (- (car (cadr tb)) (caar tb)))) (setq w (dim-leader:text-fallback-width dat)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items))
    (setq boxlist (cons (list (car ref) (+ (car ref) w) (- (cadr ref) (/ h 2.0)) (+ (cadr ref) (/ h 2.0))) boxlist)))
  (foreach o obstacles
    (setq boxlist (cons (list (nth 0 o) (nth 1 o) (nth 2 o) (nth 3 o)) boxlist)))
  (setq colPitch (+ maxw gap) rowPitch (+ maxh gap))
  (setq minX 1.0e30 maxX -1.0e30 minY 1.0e30 maxY -1.0e30)
  (foreach b boxlist
    (if (< (nth 0 b) minX) (setq minX (nth 0 b)))
    (if (> (nth 1 b) maxX) (setq maxX (nth 1 b)))
    (if (< (nth 2 b) minY) (setq minY (nth 2 b)))
    (if (> (nth 3 b) maxY) (setq maxY (nth 3 b))))
  (foreach m items
    (setq cx (car (nth 0 m)) cy (cadr (nth 0 m)))
    (if (< cx minX) (setq minX cx))
    (if (> cx maxX) (setq maxX cx))
    (if (< cy minY) (setq minY cy))
    (if (> cy maxY) (setq maxY cy)))
  (setq minX (- minX colPitch) minY (- minY rowPitch)
        maxX (+ maxX colPitch) maxY (+ maxY rowPitch)
        nx (1+ (fix (/ (- maxX minX) colPitch)))
        ny (1+ (fix (/ (- maxY minY) rowPitch))))
  ;; 占用网格
  (setq grid nil j 0)
  (while (< j ny) (setq grid (cons (dim-leader:list-nil nx) grid)) (setq j (1+ j)))
  (setq grid (reverse grid))
  (foreach b boxlist
    (setq i (fix (/ (- (nth 0 b) minX) colPitch))
          i1 (fix (/ (- (nth 1 b) minX) colPitch))
          j (fix (/ (- (nth 2 b) minY) rowPitch))
          j1 (fix (/ (- (nth 3 b) minY) rowPitch)))
    (while (<= i i1)
      (setq j (fix (/ (- (nth 2 b) minY) rowPitch)))
      (while (<= j j1)
        (if (and (>= i 0) (< i nx) (>= j 0) (< j ny))
          (setq grid (dim-leader:setnth grid j (dim-leader:setnth (nth j grid) i t))))
        (setq j (1+ j)))
      (setq i (1+ i))))
  ;; 第一步：分区、找空白矩形、确定所有标签位置（先不改图元）
  (setq zoneW (/ (- maxX minX) nz) zoneH (/ (- maxY minY) nz))
  (setq allLab nil z 0)
  (while (< z (* nz nz))
    (setq cxz (+ minX (* (rem z nz) zoneW))
          cyz (+ minY (* (fix (/ z nz)) zoneH))
          labList nil)
    (foreach m items
      (setq cx (car (nth 0 m)) cy (cadr (nth 0 m)))
      (if (and (>= cx cxz) (< cx (+ cxz zoneW)) (>= cy cyz) (< cy (+ cyz zoneH)))
        (setq labList (cons m labList))))
    (if labList
      (progn
        (setq nLab (length labList)
              tabRows (max 1 (fix (/ zoneH rowPitch)))
              tabCols (max 1 (1+ (fix (/ (1- nLab) tabRows)))))
        (setq best nil bestd nil j0 0)
        (while (<= j0 (- ny tabRows))
          (setq i0 0)
          (while (<= i0 (- nx tabCols))
            (setq ok t ii 0)
            (while (and ok (< ii tabCols))
              (setq jj 0)
              (while (and ok (< jj tabRows))
                (if (nth (+ i0 ii) (nth (+ j0 jj) grid)) (setq ok nil))
                (setq jj (1+ jj)))
              (setq ii (1+ ii)))
            (if ok
              (progn
                (setq c (list (+ minX (* i0 colPitch)) (+ minY (* j0 rowPitch))))
                (setq d (distance c (list cxz (/ (+ cyz (+ cyz zoneH)) 2.0))))
                (if (or (null bestd) (< d bestd)) (setq bestd d best (list i0 j0)))))
            (setq i0 (1+ i0)))
          (setq j0 (1+ j0)))
        (if (null best) (setq best (list 0 0)))
        (setq i0 (car best) j0 (cadr best) k 0)
        (foreach m labList
          (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) h (nth 5 m) w (nth 4 m))
          (setq c (fix (/ k tabRows)) rr (rem k tabRows))
          (setq lx (+ minX (* (+ i0 c) colPitch))
                ly (+ minY (* (+ j0 rr) rowPitch) (/ h 2.0)))
          (setq allLab (cons (list ctr ent lx ly w h) allLab))
          (setq k (1+ k)))))
    (setq z (1+ z)))
  ;; 第二步：把所有已放置标签也标进占用网格，避免后面的通道压到它们
  (foreach L allLab
    (setq lx (nth 2 L) ly (nth 3 L) w (nth 4 L) h (nth 5 L))
    (setq i (fix (/ (- lx minX) colPitch)) i1 (fix (/ (- (+ lx w) minX) colPitch)))
    (while (<= i i1)
      (setq j (fix (/ (- (- ly (/ h 2.0)) minY) rowPitch))
            j1 (fix (/ (- (+ ly (/ h 2.0)) minY) rowPitch)))
      (while (<= j j1)
        (if (and (>= i 0) (< i nx) (>= j 0) (< j ny))
          (setq grid (dim-leader:setnth grid j (dim-leader:setnth (nth j grid) i t))))
        (setq j (1+ j)))
      (setq i (1+ i))))
  ;; 第三步：逐条找"无字横向通道"并布线
  (setq res nil)
  (foreach L allLab
    (setq ctr (nth 0 L) ent (nth 1 L) lx (nth 2 L) ly (nth 3 L) cx (car ctr) cy (cadr ctr))
    (setq ci1 (max 0 (fix (/ (- cx minX) colPitch)))
          ci2 (min (1- nx) (max ci1 (1- (fix (/ (- lx minX) colPitch)))))
          jc (max 0 (min (1- ny) (fix (/ (- cy minY) rowPitch))))
          gy nil kk 0)
    (while (and (null gy) (<= kk ny))
      (foreach jj (if (= kk 0) (list jc) (list (+ jc kk) (- jc kk)))
        (if (and (null gy) (>= jj 0) (< jj ny))
          (progn
            (setq ok t ii ci1)
            (while (and ok (<= ii ci2))
              (if (nth ii (nth jj grid)) (setq ok nil))
              (setq ii (1+ ii)))
            (if ok (setq gy (+ minY (* jj rowPitch) (/ rowPitch 2.0)))))))
      (setq kk (1+ kk)))
    (if (null gy) (setq gy (+ minY (* jc rowPitch) (/ rowPitch 2.0))))
    (setq route (dim-leader:dedupe-pts
                  (list (list cx cy) (list cx gy) (list (- lx 0.5) gy) (list (- lx 0.5) ly) (list lx ly))))
    (setq res (cons (list ctr ent lx ly 0 route) res)))
  (reverse res))

;; ---------------------------------------------------------------------------
;; 3e. 四向本地布局：标签可放在圆心的 右/左/上/下；引线为直线，四向散开以减少交叉。
;;     返回 (圆心 图元 锚点X 锚点Y 旋转 水平对齐 垂直对齐 折线点表)
;; ---------------------------------------------------------------------------
(defun dim-leader:layout-4dir (matched obstacles / gap levelH maxw maxh items m ctr ent dat ref h wf style txt w tb
                               placed res cx cy off d d0 bl br bb bt best ax ay hj vj route Cx Cy n dlist)
  (setq gap (@:get-config 'dim-leader:gap) maxw 0.0 maxh 0.0 items nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat)) style (cdr (assoc 7 dat)) txt (cdr (assoc 1 dat)))
    (setq wf (cdr (assoc 41 dat)))
    (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
    (setq tb (textbox dat))
    (if tb (setq w (abs (- (car (cadr tb)) (caar tb)))) (setq w (dim-leader:text-fallback-width dat)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items)))
  (setq levelH (+ maxh gap))
  ;; 重心（用于按方位分组，形成四向排线束）
  (setq Cx 0.0 Cy 0.0 n 0)
  (foreach m items
    (setq Cx (+ Cx (car (nth 0 m))) Cy (+ Cy (cadr (nth 0 m))) n (1+ n)))
  (setq Cx (/ Cx n) Cy (/ Cy n))
  (setq items (vl-sort items (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))))
  (setq placed obstacles res nil)
  (foreach m items
    (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) w (nth 4 m) h (nth 5 m))
    ;; 主方向：按相对重心的方位
    (if (>= (abs (- cx Cx)) (abs (- cy Cy)))
      (if (>= cx Cx) (setq d0 1) (setq d0 2))
      (if (>= cy Cy) (setq d0 3) (setq d0 4)))
    (setq dlist nil)
    (foreach d (list d0 1 2 3 4) (if (not (member d dlist)) (setq dlist (append dlist (list d)))))
    (setq best nil off 2.0)
    (while (and (null best) (< off 400.0))
      (foreach d dlist
        (if (null best)
          (progn
            (cond
              ((= d 1) (setq bl (+ cx off) br (+ cx off w) bb (- cy (/ h 2.0)) bt (+ cy (/ h 2.0)) ax (+ cx off) ay cy hj 0 vj 2))
              ((= d 2) (setq bl (- cx off w) br (- cx off) bb (- cy (/ h 2.0)) bt (+ cy (/ h 2.0)) ax (- cx off) ay cy hj 2 vj 2))
              ((= d 3) (setq bl (- cx (/ w 2.0)) br (+ cx (/ w 2.0)) bb (+ cy off) bt (+ cy off h) ax cx ay (+ cy off) hj 1 vj 0))
              (t       (setq bl (- cx (/ w 2.0)) br (+ cx (/ w 2.0)) bb (- cy off h) bt (- cy off) ax cx ay (- cy off) hj 1 vj 3)))
            (if (not (dim-leader:box-hit-p placed bl br bb bt))
              (setq best (list bl br bb bt ax ay hj vj))))))
      (setq off (+ off levelH)))
    (if (null best) (setq best (list bl br bb bt cx cy 0 2)))
    (setq bl (nth 0 best) br (nth 1 best) bb (nth 2 best) bt (nth 3 best)
          ax (nth 4 best) ay (nth 5 best) hj (nth 6 best) vj (nth 7 best))
    (setq placed (cons (list bl br bb bt) placed))
    (setq route (dim-leader:dedupe-pts (list (list cx cy) (list ax ay))))
    (setq res (cons (list ctr ent ax ay 0 hj vj route) res)))
  (reverse res))

;; ---------------------------------------------------------------------------
;; 3f. 分方向独立轨道：按相对重心把圆心分四向；每向标签排在该侧，按垂直轴排序；
;;     每条引线分配独占的"轨道"(竖轨道 R/L、横轨道 U/D)，故引线互不重叠。
;;     返回 (圆心 图元 锚点X 锚点Y 旋转 水平对齐 垂直对齐 折线点表)
;; ---------------------------------------------------------------------------
(defun dim-leader:layout-lanes (matched obstacles / gap levelH maxw maxh items m ctr ent dat ref h wf style txt w tb
                                Cx Cy n cx cy minX maxX minY maxY XR XL YU YD
                                groups dd g rows col row lx ly lane glx gly gy route res i)
  (setq gap (@:get-config 'dim-leader:gap) maxw 0.0 maxh 0.0 items nil
        minX 1.0e30 maxX -1.0e30 minY 1.0e30 maxY -1.0e30 Cx 0.0 Cy 0.0 n 0)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat)) style (cdr (assoc 7 dat)) txt (cdr (assoc 1 dat)))
    (setq wf (cdr (assoc 41 dat)))
    (if (not (and (numberp wf) (> wf 0.0))) (setq wf 1.0))
    (setq tb (textbox dat))
    (if tb (setq w (abs (- (car (cadr tb)) (caar tb)))) (setq w (dim-leader:text-fallback-width dat)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq cx (car ctr) cy (cadr ctr))
    (if (< cx minX) (setq minX cx)) (if (> cx maxX) (setq maxX cx))
    (if (< cy minY) (setq minY cy)) (if (> cy maxY) (setq maxY cy))
    (setq Cx (+ Cx cx) Cy (+ Cy cy) n (1+ n))
    (setq items (cons (list ctr ent (car ref) (cadr ref) w h) items)))
  (setq Cx (/ Cx n) Cy (/ Cy n) levelH (+ maxh gap))
  (setq XR (+ maxX (+ maxw gap) 5.0) XL (- minX (+ maxw gap) 5.0)
        YU (+ maxY (+ maxh gap) 5.0) YD (- minY (+ maxh gap) 5.0))
  (setq groups (list nil nil nil nil))
  (foreach m items
    (setq cx (car (nth 0 m)) cy (cadr (nth 0 m)))
    (if (>= (abs (- cx Cx)) (abs (- cy Cy)))
      (if (>= cx Cx) (setq dd 1) (setq dd 2))
      (if (>= cy Cy) (setq dd 3) (setq dd 4)))
    (setq groups (dim-leader:setnth groups (1- dd) (cons m (nth (1- dd) groups)))))
  (setq rows (max 1 (fix (/ (- maxY minY) (+ maxh gap)))) res nil)
  ;; ---- 右 (R): 竖轨道 ----
  (setq g (vl-sort (nth 0 groups) (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))) i 0)
  (foreach m g
    (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) h (nth 5 m))
    (setq col (fix (/ i rows)) row (rem i rows)
          lx (+ XR (* col (+ maxw gap))) ly (+ minY (* row (+ maxh gap)) (/ h 2.0))
          gy (+ minY (* (fix (/ (- cy minY) levelH)) levelH) maxh (/ gap 2.0))
          lane (- lx 0.5))
    (setq route (dim-leader:dedupe-pts (list (list cx cy) (list cx gy) (list lane gy) (list lane ly) (list lx ly))))
    (setq res (cons (list ctr ent lx ly 0 0 2 route) res)) (setq i (1+ i)))
  ;; ---- 左 (L): 竖轨道, 右对齐 ----
  (setq g (vl-sort (nth 1 groups) (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))) i 0)
  (foreach m g
    (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) h (nth 5 m))
    (setq col (fix (/ i rows)) row (rem i rows)
          lx (- XL (* col (+ maxw gap))) ly (+ minY (* row (+ maxh gap)) (/ h 2.0))
          gy (+ minY (* (fix (/ (- cy minY) levelH)) levelH) maxh (/ gap 2.0))
          lane (+ lx 0.5))
    (setq route (dim-leader:dedupe-pts (list (list cx cy) (list cx gy) (list lane gy) (list lane ly) (list lx ly))))
    (setq res (cons (list ctr ent lx ly 0 2 2 route) res)) (setq i (1+ i)))
  ;; ---- 上 (U): 横轨道, 底对齐 ----
  (setq g (vl-sort (nth 2 groups) (function (lambda (a b) (< (car (nth 0 a)) (car (nth 0 b)))))) i 0)
  (foreach m g
    (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) w (nth 4 m) h (nth 5 m))
    (setq col (fix (/ i rows)) row (rem i rows)
          ly (+ YU (* col (+ maxh gap))) lx (+ minX (* row (+ maxw gap)) (/ w 2.0))
          glx (- lx (/ w 2.0) 0.5)
          gly (+ minY (* (fix (/ (- cy minY) levelH)) levelH) maxh (/ gap 2.0)))
    (setq route (dim-leader:dedupe-pts (list (list cx cy) (list cx gly) (list glx gly) (list glx ly) (list lx ly))))
    (setq res (cons (list ctr ent lx ly 0 1 0 route) res)) (setq i (1+ i)))
  ;; ---- 下 (D): 横轨道, 顶对齐 ----
  (setq g (vl-sort (nth 3 groups) (function (lambda (a b) (< (car (nth 0 a)) (car (nth 0 b)))))) i 0)
  (foreach m g
    (setq ctr (nth 0 m) ent (nth 1 m) cx (car ctr) cy (cadr ctr) w (nth 4 m) h (nth 5 m))
    (setq col (fix (/ i rows)) row (rem i rows)
          ly (- YD (* col (+ maxh gap))) lx (+ minX (* row (+ maxw gap)) (/ w 2.0))
          glx (- lx (/ w 2.0) 0.5)
          gly (+ minY (* (fix (/ (- cy minY) levelH)) levelH) maxh (/ gap 2.0)))
    (setq route (dim-leader:dedupe-pts (list (list cx cy) (list cx gly) (list glx gly) (list glx ly) (list lx ly))))
    (setq res (cons (list ctr ent lx ly 0 1 3 route) res)) (setq i (1+ i)))
  (reverse res))

;; ---------------------------------------------------------------------------
;; 4. 放置文本：把锚点(10/11) 移到 (ax ay)，并设置旋转角 rot
;; ---------------------------------------------------------------------------
(defun dim-leader:place-text (ent ax ay rot / dat z rad)
  (setq dat (entget ent))
  (setq z (caddr (cdr (assoc 10 dat))))
  (if (null z) (setq z 0.0))
  ;; 该 CAD 的角度组码以弧度计，90° 需写 pi/2
  (setq rad (if (= rot 90) (/ pi 2.0) 0.0))
  (setq dat (dim-leader:setd dat 10 (list ax ay z)))
  (setq dat (dim-leader:setd dat 11 (list ax ay z)))
  (setq dat (dim-leader:setd dat 50 rad))
  (entmod dat)
  ent)

;; 放置文本并指定水平/垂直对齐(72/73)，用于四向布局
(defun dim-leader:place-text-j (ent ax ay rot hj vj / dat z rad)
  (setq dat (entget ent))
  (setq z (caddr (cdr (assoc 10 dat))))
  (if (null z) (setq z 0.0))
  (setq rad (if (= rot 90) (/ pi 2.0) 0.0))
  (setq dat (dim-leader:setd dat 10 (list ax ay z)))
  (setq dat (dim-leader:setd dat 11 (list ax ay z)))
  (setq dat (dim-leader:setd dat 50 rad))
  (setq dat (dim-leader:setd dat 72 hj))
  (setq dat (dim-leader:setd dat 73 vj))
  (entmod dat)
  ent)

;; ---------------------------------------------------------------------------
;; 5. 创建多段线引线（点表任意段）
;; ---------------------------------------------------------------------------
(defun dim-leader:make-polyline (pts layer lw)
  (entmake
    (append
      (list '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            '(100 . "AcDbPolyline")
            (cons 90 (length pts))
            '(70 . 0))
      (if (and layer (= (type layer) 'STR) (/= layer "")) (list (cons 8 layer)))
      (if (numberp lw) (list (cons 43 lw)))
      (mapcar (function (lambda (p) (cons 10 p))) pts)))
  (entlast))

;; ---------------------------------------------------------------------------
;; 主命令
;; ---------------------------------------------------------------------------
(defun dim-leader:run (/ centers matched laid layer lw r ctr ent ax ay rot route nlead
                          sst i e matchedef obstacles bx occx occy pal pl)
  (setq centers (dim-leader:find-centers))
  (cond
    ((null centers)
     (princ "\n未找到中心点(共圆心的两个半圆弧)。"))
    ((null (setq matched (dim-leader:match-texts centers)))
     (princ (strcat "\n中心点: " (itoa (length centers)) "  未匹配到文本。")))
    (t
     (princ (strcat "\n中心点: " (itoa (length centers))
                    "  匹配文本: " (itoa (length matched))))
     ;; 收集未参与标注的文本作为障碍物，避免新位置压到它们
     (setq matchedef (mapcar (function (lambda (x) (nth 1 x))) matched)
           obstacles nil)
     (if (setq sst (ssget "_X" '((0 . "TEXT"))))
       (progn
         (setq i 0)
         (while (< i (sslength sst))
           (setq e (ssname sst i))
           (if (not (member e matchedef))
             (progn
               (setq bx (dim-leader:text-box e)
                     occx (/ (+ (nth 0 bx) (nth 2 bx)) 2.0)
                     occy (/ (+ (nth 1 bx) (nth 3 bx)) 2.0))
               (setq obstacles
                     (cons (dim-leader:mk-placed (nth 0 bx) (nth 1 bx) (nth 2 bx) (nth 3 bx)
                                                 occx occy occx occy occx occy occx occy)
                           obstacles))))
           (setq i (1+ i)))))
     (setq laid (cond
                  ((= 1 (@:get-config 'dim-leader:layout))
                   (dim-leader:layout-table matched obstacles))
                  ((= 2 (@:get-config 'dim-leader:layout))
                   (dim-leader:layout-table-zone matched obstacles (@:get-config 'dim-leader:zones)))
                   ((= 3 (@:get-config 'dim-leader:layout))
                    (dim-leader:layout-zone2d matched obstacles 3))
                   ((= 4 (@:get-config 'dim-leader:layout))
                    (dim-leader:layout-4dir matched obstacles))
                    ((= 5 (@:get-config 'dim-leader:layout))
                     (dim-leader:layout-lanes matched obstacles))
                     ((= 6 (@:get-config 'dim-leader:layout))
                      (dim-leader:layout-chip matched obstacles))
                     ((= 7 (@:get-config 'dim-leader:layout))
                      (dim-leader:layout-compact matched obstacles))
                     ((= 8 (@:get-config 'dim-leader:layout))
                      (dim-leader:layout-channels matched obstacles))
                     (t (dim-leader:layout matched obstacles)))
             layer (@:get-config 'dim-leader:layer)
             lw (@:get-config 'dim-leader:leader-width)
             pal (dim-leader:parse-colors (@:get-config 'dim-leader:leader-colors))
             nlead 0)
      (if (and (= 6 (@:get-config 'dim-leader:layout)) (null laid))
        (princ (strcat "\n芯片式布局: "
                       (if dim-leader:*chip-error*
                         dim-leader:*chip-error*
                         "生成失败，未修改图形"))))
     (foreach r laid
       (setq ctr (nth 0 r)
             ent (nth 1 r)
             ax (nth 2 r)
             ay (nth 3 r)
             rot (nth 4 r))
       (if (nth 7 r)
         (progn
           (dim-leader:place-text-j ent ax ay rot (nth 5 r) (nth 6 r))
           (setq pl (dim-leader:make-polyline (nth 7 r) layer lw)))
         (progn
           (dim-leader:place-text ent ax ay rot)
           (setq pl (dim-leader:make-polyline (nth 5 r) layer lw))))
       (if (and pal pl)
         (entmod (dim-leader:setd (entget pl) 62 (nth (rem nlead (length pal)) pal))))
       (setq nlead (1+ nlead)))
     (princ (strcat "  生成引线: " (itoa nlead) " 条。"))))
  (princ))

;; 命令行别名
(defun c:dimleader () (dim-leader:run))
(defun c:dl () (dim-leader:run))

;; ---------------------------------------------------------------------------
;; 菜单：按布局编号分项，免去手工改 dim-leader:layout
;; ---------------------------------------------------------------------------

(defun dim-leader:run-layout (n)
  "按布局编号 n 执行圆心引线标注，并把 dim-leader:layout 设为 n。返回 dim-leader:run 的结果。"
  (if (and (numberp n) (>= n 0) (< n (length dim-leader:*layout-names*)))
    (progn
      (@::set-config 'dim-leader:layout n)
      (dim-leader:run))
    (princ (strcat "\n布局编号需在 0-" (itoa (1- (length dim-leader:*layout-names*))) " 之间。"))))

(defun dim-leader:menu-items (/ lst i)
  "构造圆心引线标注的菜单项表，每项对应一种布局。返回 ((项名 命令) ...)。"
  (setq lst nil i 0)
  (foreach nm dim-leader:*layout-names*
    ;; 命令以列表形式交给 @:add-menu，由其转成 "(dim-leader:run-layout i)"
    (setq lst (cons (list nm (list 'dim-leader:run-layout i)) lst))
    (setq i (1+ i)))
  (reverse lst))

(defun dim-leader:add-menu (/ lst)
  "注册菜单项到 圆心引线标注 分类：9 种布局 + 分隔线 + 参数设置。返回布局项数。"
  (setq lst (dim-leader:menu-items))
  (@:add-menus
    (list
      (cons "圆心引线标注"
            (append lst
                    (list (list "--" "--")
                          (list "参数设置…" "(@::edit-config-dialog)"))))))
  (length lst))

(dim-leader:add-menu)
