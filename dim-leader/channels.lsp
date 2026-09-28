;; dim-leader 通道布线器(布局 8, F7): 区域切分 + 每带 left-edge 独占泳道, 引线零共线重叠
;; 依赖 dim-leader.lsp 中的几何/落图工具
;;
;; 思路:
;;   1. 圆心按到点群外接矩形四边的最近距离分到 上/下/左/右 四个带;
;;   2. 每带沿带方向按 字高+gap 分列成网格, 列顺序与圆心沿带方向顺序一致;
;;   3. 每行在靠点群一侧留泳道区, 泳道由 left-edge 分配(同行同泳道区间不重叠);
;;   4. 引线 = 圆心 ->(深度) 泳道 ->(沿带) 列 ->(深度) 文字边, 全正交。
;;   竖直段只在圆心坐标或列坐标上出现: 圆心坐标基本互异, 同行列坐标互异,
;;   不同行泳道区不重叠, 故引线无共线重叠。
;;
;; 返回记录 (圆心 图元 锚点X 锚点Y 旋转 水平对齐 垂直对齐 折线点表), 与布局 4/5/6 一致。

(@:define-config 'dim-leader:lane-gap 1.0 "布局8: 相邻平行引线的通道间距")
(@:define-config 'dim-leader:channels-lines 0 "布局8: 每带行/列数(0=按点群范围自动, >0 手动指定以压缩布局)")
(@:define-config 'dim-leader:channels-columns 1 "布局8: 列位算法 1=最小位移保序(引线更短) 0=等距")
(@:define-config 'dim-leader:channels-blocks 10 "布局8: 文本带分块数(0/1=单带; >=2 块间留贯穿缝隙, 长引线穿缝, 减少穿越文字盒)")

;; 向上取整除法
(defun dim-leader:ch-ceil (a b)
  "向上取整除法: 返回 ceil(a/b)。a,b 为整数。"
  (if (<= a 0) 1 (fix (/ (+ a b -1) b))))

(defun dim-leader:ch-sum (l / s)
  "求和: 返回实数列表 l 的元素之和。"
  (setq s 0.0) (foreach x l (setq s (+ s x))) s)

;; 按圆心坐标分量无损升序排序。vl-sort 会删除比较结果为 nil 的重复元素,
;; 故用原始序号打破平局, 避免同坐标的圆心被静默丢弃。
(defun dim-leader:ch-sort-order (idx lst / tag i)
  "按圆心坐标分量 idx(0=x,1=y) 对 (圆心 …) 记录表 lst 无损升序排序, 返回新表。"
  (setq tag nil i 0)
  (foreach z lst
    (setq tag (cons (list (nth idx (nth 0 z)) i z) tag) i (1+ i)))
  (setq tag (vl-sort tag
              (function (lambda (a b)
                (or (< (car a) (car b))
                    (and (not (< (car b) (car a))) (< (cadr a) (cadr b))))))))
  (mapcar (function caddr) tag))

;; 分块带布线(horizontal: 上/下带; positive: 上/右)。
;; 带沿带方向分 blocks 块, 块间缝宽按该缝深行引线数自适应; 深行引线经缝上行,
;; 缝内独占通道, 每行水平段 left-edge 泳道。块的排布沿带方向错开(上带右对齐、
;; 下带左对齐、右带下对齐、左带上对齐), 使相邻两带的文字盒在角部错开, 避免重叠。
(defun dim-leader:ch-band-blocks (g horizontal positive gap lane blocks maxh minX maxX minY maxY
  / ridx oidx pitch runmin runmax range sgn m gw grid cols gapleft edge lanet lane_t cursor
    outer per0 bs cap rows extent mid xcur b c i z r ci u bi cand gi cnt cntl ngw
    k idxs spans le asg nt tracks riser depth key seen j sx sy rx e lanev route res
    cx cy w h ii)
  "分块通道布线: 对同一带 g 上的圆心分配文字网格与正交引线, 返回 8 元素布局记录表。"
  (if (null g) nil
    (progn
      ;; 沿带方向排序, 使列序与圆心顺序一致(与不分块模式一致, 缩短引线)
      (setq g (dim-leader:ch-sort-order (if horizontal 0 1) g)
            ridx (if horizontal 0 1) oidx (- 1 ridx)
            pitch (+ maxh gap)
            runmin (if horizontal minX minY)
            runmax (if horizontal maxX maxY)
            range (- runmax runmin)
            sgn (if positive 1 -1)
            m (length g)
            gw nil b 0)
      (while (< b (1- blocks)) (setq gw (append gw (list gap))) (setq b (1+ b)))
      (setq outer 0 grid nil cols nil gapleft nil)
      (while (< outer 8)
        (setq per0 (max 1 (fix (/ range pitch)))
              bs (dim-leader:ch-ceil per0 blocks)
              cap (* bs blocks)
              rows (dim-leader:ch-ceil m cap)
              extent (+ (* cap pitch) (dim-leader:ch-sum gw))
              mid (if horizontal (/ (+ minX maxX) 2.0) (/ (+ minY maxY) 2.0))
              cols nil gapleft nil xcur (- mid (/ extent 2.0)))
        (setq b 0)
        (while (< b blocks)
          (setq c 0)
          (while (< c bs)
            (setq cols (append cols (list (list (+ xcur (/ pitch 2.0)) b))))
            (setq xcur (+ xcur pitch) c (1+ c)))
          (if (< b (1- blocks))
            (progn (setq gapleft (append gapleft (list xcur)))
                   (setq xcur (+ xcur (nth b gw)))))
          (setq b (1+ b)))
        ;; 沿带方向错开(上右/下左/右下/左上), 使角部相邻带文字盒错开
        (setq b (if horizontal
                   (if positive (- runmax (+ (car (last cols)) (/ pitch 2.0)))
                               (- runmin (- (caar cols) (/ pitch 2.0))))
                   (if positive (- runmin (- (caar cols) (/ pitch 2.0)))
                               (- runmax (+ (car (last cols)) (/ pitch 2.0))))))
        (setq cols (mapcar (function (lambda (cc) (list (+ (car cc) b) (cadr cc)))) cols))
        (setq gapleft (mapcar (function (lambda (x) (+ x b))) gapleft))
        ;; 网格分配
        (setq grid nil i 0)
        (foreach z g
          (setq r (fix (/ i cap)) ci (rem i cap))
          (if (>= ci (length cols)) (setq ci (1- (length cols))))
          (setq u (car (nth ci cols)) bi (cadr (nth ci cols)))
          (setq cand nil)
          (if (> bi 0) (setq cand (append cand (list (1- bi)))))
          (if (< bi (1- blocks)) (setq cand (append cand (list bi))))
          (setq gi -1)
          (if cand
            (progn
              (setq gi (car cand))
              (foreach cc cand
                (if (< (abs (- (+ (nth cc gapleft) (/ (nth cc gw) 2.0)) (nth ridx (nth 0 z))))
                       (abs (- (+ (nth gi gapleft) (/ (nth gi gw) 2.0)) (nth ridx (nth 0 z)))))
                  (setq gi cc)))))
          (setq grid (append grid (list (list z r u gi))))
          (setq i (1+ i)))
        ;; 每缝深行引线数 -> 缝宽
        (setq cntl nil b 0)
        (while (< b (length gapleft)) (setq cntl (append cntl (list 0))) (setq b (1+ b)))
        (foreach it grid
          (if (and (> (cadr it) 0) (>= (cadddr it) 0))
            (setq cntl (dim-leader:setnth cntl (cadddr it) (1+ (nth (cadddr it) cntl))))))
        (setq ngw nil)
        (foreach cc cntl (setq ngw (append ngw (list (max gap (+ (* cc lane) gap))))))
        (if (equal ngw gw)
          (setq outer 8)
          (setq gw ngw outer (1+ outer))))
      ;; 缝内独占通道(按列序)
      (setq tracks (dim-leader:list-nil (length grid)) riser (dim-leader:list-nil (length grid)))
      (setq gi 0)
      (while (< gi (length gapleft))
        (setq idxs nil k 0)
        (foreach it grid
          (if (and (> (cadr it) 0) (= (cadddr it) gi)) (setq idxs (append idxs (list k))))
          (setq k (1+ k)))
        (setq idxs (vl-sort idxs (function (lambda (a b) (< (caddr (nth a grid)) (caddr (nth b grid)))))))
        (setq k 0)
        (foreach ii idxs
          (setq tracks (dim-leader:setnth tracks ii (+ (nth gi gapleft) (* (+ k 0.5) lane))))
          (setq k (1+ k)))
        (setq gi (1+ gi)))
      (setq k 0)
      (foreach it grid
        (if (= (cadr it) 0)
          (setq riser (dim-leader:setnth riser k (nth ridx (nth 0 (car it)))))
          (setq riser (dim-leader:setnth riser k (nth k tracks))))
        (setq k (1+ k)))
      ;; 每行水平段泳道
      (setq lanet (dim-leader:list-nil rows) lane_t (dim-leader:list-nil (length grid)))
      (setq r 0)
      (while (< r rows)
        (setq idxs nil k 0)
        (foreach it grid (if (= (cadr it) r) (setq idxs (append idxs (list k)))) (setq k (1+ k)))
        (setq spans nil)
        (foreach ii idxs
          (setq spans (append spans (list (list (min (nth ii riser) (caddr (nth ii grid)))
                                          (max (nth ii riser) (caddr (nth ii grid))))))))
        (setq le (dim-leader:ch-left-edge spans) asg (car le) nt (cadr le))
        (setq lanet (dim-leader:setnth lanet r nt))
        (setq j 0)
        (foreach ii idxs (setq lane_t (dim-leader:setnth lane_t ii (nth j asg))) (setq j (1+ j)))
        (setq r (1+ r)))
      ;; 文字边与深度
      (setq depth 0.0)
      (foreach z g (if (> (nth 2 z) depth) (setq depth (nth 2 z))))
      (setq edge (dim-leader:list-nil rows)
            cursor (if horizontal (if positive (+ maxY gap) (- minY gap))
                              (if positive (+ maxX gap) (- minX gap)))
            r 0)
      (while (< r rows)
        (setq e (+ cursor (* sgn (+ (* (nth r lanet) lane) gap))))
        (setq edge (dim-leader:setnth edge r e))
        (setq cursor (+ e (* sgn (+ depth gap))))
        (setq r (1+ r)))
      ;; 去重起点坐标, 避免共线
      (setq seen nil)
      (setq k 0)
      (foreach it grid
        (setq key (rtos (nth oidx (nth 0 (car it))) 2 4))
        (setq cnt 0)
        (if (assoc key seen) (setq cnt (cdr (assoc key seen))))
        (setq lane_t (dim-leader:setnth lane_t k (list (nth k lane_t) cnt)))
        (setq seen (if (assoc key seen)
                     (subst (cons key (1+ cnt)) (assoc key seen) seen)
                     (append seen (list (cons key (1+ cnt))))))
        (setq k (1+ k)))
      ;; 生成记录
      (setq res nil k 0)
      (foreach it grid
        (setq z (car it) r (cadr it) u (caddr it) w (nth 2 z) h (nth 3 z)
              cx (car (nth 0 z)) cy (cadr (nth 0 z))
              j (cadr (nth k lane_t))
              lanev (+ (- (nth r edge) (* sgn (+ (* (nth r lanet) lane) gap)))
                       (* sgn (* (+ (car (nth k lane_t)) 0.5) lane)))
              rx (nth k riser))
        (if horizontal
          (progn
            (setq sy (+ cy (* j lane))
                  route (dim-leader:dedupe-pts
                          (list (list cx cy) (list cx sy) (list rx sy)
                                (list rx lanev) (list u lanev) (list u (nth r edge)))))
            (setq res (cons (list (nth 0 z) (nth 1 z) u (+ (nth r edge) (/ w 2.0)) 90 1 2 route) res)))
          (progn
            (setq sx (+ cx (* j lane))
                  route (dim-leader:dedupe-pts
                          (list (list cx cy) (list sx cy) (list sx rx)
                                (list lanev rx) (list lanev u) (list (nth r edge) u))))
            (setq res (cons (list (nth 0 z) (nth 1 z) (nth r edge) u 0 (if positive 0 2) 2 route) res))))
        (setq k (1+ k)))
      (reverse res))))

;; 去掉表尾元素
(defun dim-leader:ch-buttlast (l)
  "返回去掉最后一个元素后的表 l。"
  (reverse (cdr (reverse l))))

;; 保序回归(PAVA, L2): p 为坐标表, 返回满足 相邻差>=pitch 且尽量贴近 p 的列位表。
;; 做法: v_k=p_k-k*pitch, 对 v 求非降的块均值(PAVA), 再 u_k=v_k+k*pitch。
(defun dim-leader:ch-isotonic (p pitch / v blocks b c k res vk)
  "等间距约束(pitch)下最贴近 p 的列位(保序回归), 返回坐标表。"
  (setq v nil k 0)
  (foreach x p (setq v (append v (list (- x (* k pitch))))) (setq k (1+ k)))
  (setq blocks nil)
  (foreach x v
    (setq blocks (append blocks (list (list x 1))))
    (while (and (>= (length blocks) 2)
                (> (/ (float (car (nth (- (length blocks) 2) blocks)))
                      (cadr (nth (- (length blocks) 2) blocks)))
                   (/ (float (car (last blocks))) (cadr (last blocks)))))
      (setq c (last blocks) b (nth (- (length blocks) 2) blocks)
            blocks (dim-leader:ch-buttlast (dim-leader:ch-buttlast blocks))
            blocks (append blocks (list (list (+ (car b) (car c)) (+ (cadr b) (cadr c))))))))
  (setq res nil k 0)
  (foreach b blocks
    (setq vk (/ (float (car b)) (cadr b)))
    (repeat (cadr b)
      (setq res (append res (list (+ vk (* k pitch)))) k (1+ k))))
  res)

;; left-edge 通道分配: spans 为 (起 止) 列表(保持原始顺序),
;; 返回 (分配轨道号表 轨道总数); 同一轨道上的区间互不重叠。
(defun dim-leader:ch-left-edge (spans / lst n i ends assign found s e idx tr)
  "left-edge 区间分配: 返回 (轨道号表 轨道总数), 同轨道区间不重叠。"
  (setq n (length spans) i 0 lst nil)
  (foreach sp spans
    (setq lst (cons (list (min (car sp) (cadr sp)) (max (car sp) (cadr sp)) i) lst)
          i (1+ i)))
  (setq lst (vl-sort lst (function (lambda (a b)
              (or (< (car a) (car b))
                  (and (not (< (car b) (car a)))
                       (< (cadr a) (cadr b)))
                  (and (not (< (car b) (car a))) (not (< (cadr b) (cadr a)))
                       (< (caddr a) (caddr b))))))))
  (setq ends nil assign (dim-leader:list-nil n))
  (foreach it lst
    (setq s (car it) e (cadr it) idx (caddr it) found nil tr 0)
    (while (and (null found) (< tr (length ends)))
      (if (<= (nth tr ends) (+ s 1.0e-9))
        (progn (setq ends (dim-leader:setnth ends tr e)
                     assign (dim-leader:setnth assign idx tr)
                     found t))
        (setq tr (1+ tr))))
    (if (null found)
      (progn (setq ends (append ends (list e))
                   assign (dim-leader:setnth assign idx (1- (length ends)))))))
  (list assign (length ends)))

;; 把列表按每段 n 个拆成多段
(defun dim-leader:ch-chunks (lst n / res seg k)
  "把表 lst 按每段 n 个拆分成多段, 返回段列表。"
  (setq res nil)
  (while lst
    (setq seg nil k 0)
    (while (and lst (< k n))
      (setq seg (append seg (list (car lst))) lst (cdr lst) k (1+ k)))
    (setq res (append res (list seg))))
  res)

;; n 个点在 [lo hi] 上等距分布
(defun dim-leader:ch-spread (n lo hi / res k)
  "在 [lo hi] 上等距生成 n 个点, 返回坐标表。"
  (setq res nil k 0)
  (if (= n 1)
    (setq res (list (/ (+ lo hi) 2.0)))
    (while (< k n)
      (setq res (append res (list (+ lo (* (/ (- hi lo) (1- n)) k)))))
      (setq k (1+ k))))
  res)

;; 水平带(上/下): 文字旋转 90°, 沿 x 排布, above=t 在上方否则在下方。
;; 返回记录表。g 为 (圆心 图元 宽 高) 列表。
(defun dim-leader:ch-band-h (g above gap lane maxh minX maxX minY maxY lines cols
                             / gs pitch per line n us spans le assign nt depth
                               lo hi span mid cursor k z cx cy w h u ly textb textt res)
  "水平带(上/下)不分块布线, 返回 8 元素布局记录表。"
  (if g
    (progn
      (setq gs (dim-leader:ch-sort-order 0 g))
      (setq pitch (+ maxh gap))
      (if (> lines 0)
        (setq per (max 1 (fix (/ (+ (length gs) lines -1) lines))))
        (setq per (max 1 (fix (/ (- maxX minX) pitch)))))
      (setq lo minX hi maxX)
      (if (> lines 0)
        (progn
          (setq span (max (- maxX minX) (* per pitch))
                mid (/ (+ minX maxX) 2.0)
                lo (- mid (/ span 2.0)) hi (+ mid (/ span 2.0)))))
      (setq cursor (if above (+ maxY gap) (- minY gap)) res nil)
      (foreach line (dim-leader:ch-chunks gs per)
        (setq n (length line))
        (if (and (= cols 1) (= lines 0))
          (setq us (dim-leader:ch-isotonic
                     (mapcar (function (lambda (z) (car (nth 0 z)))) line) pitch))
          (setq us (dim-leader:ch-spread n lo hi)))
        (setq spans nil k 0)
        (foreach z line
          (setq u (nth k us))
          (setq spans (append spans (list (list (min (car (nth 0 z)) u) (max (car (nth 0 z)) u)))))
          (setq k (1+ k)))
        (setq le (dim-leader:ch-left-edge spans) assign (car le) nt (cadr le))
        (setq depth 0.0)
        (foreach z line (if (> (nth 2 z) depth) (setq depth (nth 2 z))))
        (if above
          (setq textb (+ cursor (* nt lane) gap))
          (setq textt (- cursor (* nt lane) gap)))
        (setq k 0)
        (foreach z line
          (setq u (nth k us) w (nth 2 z) h (nth 3 z)
                cx (car (nth 0 z)) cy (cadr (nth 0 z))
                ly (if above (+ cursor (* (+ (nth k assign) 0.5) lane))
                           (- cursor (* (+ (nth k assign) 0.5) lane))))
          (if above
            (setq res (cons (list (nth 0 z) (nth 1 z) u (+ textb (/ w 2.0)) 90 1 2
                                  (dim-leader:dedupe-pts
                                    (list (list cx cy) (list cx ly) (list u ly) (list u textb))))
                            res))
            (setq res (cons (list (nth 0 z) (nth 1 z) u (- textt (/ w 2.0)) 90 1 2
                                  (dim-leader:dedupe-pts
                                    (list (list cx cy) (list cx ly) (list u ly) (list u textt))))
                            res)))
          (setq k (1+ k)))
        (setq cursor (if above (+ textb depth gap) (- textt depth gap))))
      (reverse res))))

;; 竖直带(右/左): 文字水平, 沿 y 排布, right=t 在右侧否则在左侧。
(defun dim-leader:ch-band-v (g right gap lane maxh minX maxX minY maxY lines cols
                             / gs pitch per line n us spans le assign nt depth
                               lo hi span mid cursor k z cx cy w h u lx textl textr res)
  "竖直带(左/右)不分块布线, 返回 8 元素布局记录表。"
  (if g
    (progn
      (setq gs (dim-leader:ch-sort-order 1 g))
      (setq pitch (+ maxh gap))
      (if (> lines 0)
        (setq per (max 1 (fix (/ (+ (length gs) lines -1) lines))))
        (setq per (max 1 (fix (/ (- maxY minY) pitch)))))
      (setq lo minY hi maxY)
      (if (> lines 0)
        (progn
          (setq span (max (- maxY minY) (* per pitch))
                mid (/ (+ minY maxY) 2.0)
                lo (- mid (/ span 2.0)) hi (+ mid (/ span 2.0)))))
      (setq cursor (if right (+ maxX gap) (- minX gap)) res nil)
      (foreach line (dim-leader:ch-chunks gs per)
        (setq n (length line))
        (if (and (= cols 1) (= lines 0))
          (progn
            (setq us (dim-leader:ch-isotonic
                       (mapcar (function (lambda (z) (cadr (nth 0 z)))) line) pitch))
            (if (< (car us) minY)
              (setq us (mapcar (function (lambda (v) (+ v (- minY (car us))))) us)))
            (if (> (last us) maxY)
              (setq us (mapcar (function (lambda (v) (+ v (- maxY (last us))))) us))))
          (setq us (dim-leader:ch-spread n lo hi)))
        (setq spans nil k 0)
        (foreach z line
          (setq u (nth k us))
          (setq spans (append spans (list (list (min (cadr (nth 0 z)) u) (max (cadr (nth 0 z)) u)))))
          (setq k (1+ k)))
        (setq le (dim-leader:ch-left-edge spans) assign (car le) nt (cadr le))
        (setq depth 0.0)
        (foreach z line (if (> (nth 2 z) depth) (setq depth (nth 2 z))))
        (if right
          (setq textl (+ cursor (* nt lane) gap))
          (setq textr (- cursor (* nt lane) gap)))
        (setq k 0)
        (foreach z line
          (setq u (nth k us) w (nth 2 z) h (nth 3 z)
                cx (car (nth 0 z)) cy (cadr (nth 0 z))
                lx (if right (+ cursor (* (+ (nth k assign) 0.5) lane))
                           (- cursor (* (+ (nth k assign) 0.5) lane))))
          (if right
            (setq res (cons (list (nth 0 z) (nth 1 z) textl u 0 0 2
                                  (dim-leader:dedupe-pts
                                    (list (list cx cy) (list lx cy) (list lx u) (list textl u))))
                            res))
            (setq res (cons (list (nth 0 z) (nth 1 z) textr u 0 2 2
                                  (dim-leader:dedupe-pts
                                    (list (list cx cy) (list lx cy) (list lx u) (list textr u))))
                            res)))
          (setq k (1+ k)))
        (setq cursor (if right (+ textl depth gap) (- textr depth gap))))
      (reverse res))))

;; 布局 8: 四带通道布线。matched 来自 dim-leader:match-texts,
;; obstacles 未使用(与其它布局保持同一调用签名)。
;; 返回 8 元素记录表; 无数据时返回 nil。
(defun dim-leader:layout-channels (matched obstacles
                                   / gap lane lines cols blocks maxh maxw minX maxX minY maxY
                                     items groups m ctr ent dat ref w h tb
                                     cx cy d side res)
  "布局 8: 四带通道布线, 返回 8 元素布局记录表; 无数据时返回 nil。"
  (setq gap (@:get-config 'dim-leader:gap))
  (if (not (and (numberp gap) (> gap 0.0))) (setq gap 1.0))
  (setq lane (@:get-config 'dim-leader:lane-gap))
  (if (not (and (numberp lane) (> lane 0.0))) (setq lane gap))
  (setq lines (@:get-config 'dim-leader:channels-lines))
  (if (not (and (numberp lines) (>= lines 0))) (setq lines 0))
  (setq cols (@:get-config 'dim-leader:channels-columns))
  (if (not (member cols '(0 1))) (setq cols 1))
  (setq blocks (@:get-config 'dim-leader:channels-blocks))
  (if (not (and (numberp blocks) (>= blocks 0))) (setq blocks 0))
  (setq maxh 0.0 maxw 0.0
        minX 1.0e30 maxX -1.0e30 minY 1.0e30 maxY -1.0e30
        items nil)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (setq h (cdr (assoc 40 dat)))
    (if (not (and (numberp h) (> h 0.0))) (setq h 2.5))
    (setq w (dim-leader:text-fallback-width dat))
    (setq tb (vl-catch-all-apply 'textbox (list dat)))
    (if (and (listp tb) (car tb) (cadr tb) (listp (car tb)) (listp (cadr tb)))
      (setq w (abs (- (car (cadr tb)) (caar tb)))))
    (if (not (and (numberp w) (> w 0.0))) (setq w (* h 4.0)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq cx (car ctr) cy (cadr ctr))
    (if (< cx minX) (setq minX cx)) (if (> cx maxX) (setq maxX cx))
    (if (< cy minY) (setq minY cy)) (if (> cy maxY) (setq maxY cy))
    (setq items (cons (list ctr ent w h) items)))
  (setq items (reverse items))
  (if (< (length items) 2)
    nil
    (progn
      (setq groups (list nil nil nil nil))
      (foreach z items
        (setq cx (car (nth 0 z)) cy (cadr (nth 0 z))
              d (list (- maxY cy) (- cy minY) (- cx minX) (- maxX cx)))
        (setq side 0)
        (if (< (nth 1 d) (nth side d)) (setq side 1))
        (if (< (nth 2 d) (nth side d)) (setq side 2))
        (if (< (nth 3 d) (nth side d)) (setq side 3))
        (setq groups (dim-leader:setnth groups side (cons z (nth side groups)))))
      (setq res nil)
      (if (>= blocks 2)
        (progn
          (setq res (append res (dim-leader:ch-band-blocks (nth 0 groups) t t gap lane blocks maxh minX maxX minY maxY)))
          (setq res (append res (dim-leader:ch-band-blocks (nth 1 groups) t nil gap lane blocks maxh minX maxX minY maxY)))
          (setq res (append res (dim-leader:ch-band-blocks (nth 3 groups) nil t gap lane blocks maxh minX maxX minY maxY)))
          (setq res (append res (dim-leader:ch-band-blocks (nth 2 groups) nil nil gap lane blocks maxh minX maxX minY maxY))))
        (progn
          (setq res (append res (dim-leader:ch-band-h (nth 0 groups) t gap lane maxh minX maxX minY maxY lines cols)))
          (setq res (append res (dim-leader:ch-band-h (nth 1 groups) nil gap lane maxh minX maxX minY maxY lines cols)))
          (setq res (append res (dim-leader:ch-band-v (nth 3 groups) t gap lane maxh minX maxX minY maxY lines cols)))
          (setq res (append res (dim-leader:ch-band-v (nth 2 groups) nil gap lane maxh minX maxX minY maxY lines cols)))))
      res)))
