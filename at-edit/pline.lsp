;; ============================================================
;; at-edit 多段线 / 转换 / 布尔 / 块 类命令
;; 依赖 at-edit.lsp 的公共辅助函数
;;   pline-data / curve-segs / make-pl / ent-segs / block2ents
;; 菜单已在 at-edit.lsp 的"迷你编辑"菜单中统一注册，本文件不重复注册
;; 命令一览（交互入口 / AI 入口）
;;   pljoin / ai-pljoin              连接首尾相接的线弧为多段线
;;   pwidth / ai-pwidth              批量修改多段线宽度
;;   add-vertex / ai-add-vertex      边上插入顶点
;;   delete-vertex / ai-delete-vertex 删除顶点
;;   lw-arc / ai-lw-arc              线弧转换
;;   pline-batch / ai-pline-batch    连接/闭合/反转/清理/圆角
;;   curve2pline / ai-curve2pline    曲线转多段线
;;   region2pline / ai-region2pline  面域转多段线
;;   circle2donut / ai-circle2donut  圆变圆环
;;   donut2circle / ai-donut2circle  圆环转圆
;;   pline-bool / ai-pline-bool      多段线布尔运算
;;   boundary / ai-boundary          智能外轮廓
;;   quick-block / ai-quick-block    快速作块
;;   explode-block / ai-explode-block 递归炸块
;;   wipeout / ai-wipeout            由边界生成遮罩
;;   erase-wipeout / ai-erase-wipeout 删除遮罩
;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; 内部辅助
;; ------------------------------------------------------------

(defun at-edit:pl-ents (sel / res)
  "把选择集/图元/图元列表统一为有效图元名列表"
  "图元名列表或 nil"
  "(at-edit:pl-ents (ssget))"
  (cond
    ((null sel) nil)
    ((= 'PICKSET (type sel)) (pickset:to-list sel))
    ((= 'ENAME (type sel)) (if (entget sel) (list sel) nil))
    ((listp sel)
     (foreach x sel
       (if (and (= 'ENAME (type x)) (entget x))
         (setq res (append res (list x)))))
     res)
    (t nil)))

(defun at-edit:pl-near (p1 p2 tol)
  "两点在容差内重合判断（2D）"
  "T 或 nil"
  "(at-edit:pl-near '(0 0) '(0.001 0) 0.01) => T"
  (and p1 p2
       (< (at-edit:2d-dist (at-edit:2d p1) (at-edit:2d p2))
          (if tol tol 1e-6))))

(defun at-edit:pl-undo-begin ()
  "开始撤销记录，失败静默"
  "nil"
  "(at-edit:pl-undo-begin)"
  (vl-catch-all-apply
   'vla-startundomark
   (list (vla-get-activedocument (vlax-get-acad-object))))
  nil)

(defun at-edit:pl-undo-end ()
  "结束撤销记录，失败静默"
  "nil"
  "(at-edit:pl-undo-end)"
  (vl-catch-all-apply
   'vla-endundomark
   (list (vla-get-activedocument (vlax-get-acad-object))))
  nil)

(defun at-edit:pl-ssget (msg types / ss)
  "按类型过滤交互选择图元"
  "图元名列表或 nil"
  "(at-edit:pl-ssget \"选择对象\" \"LINE,ARC\")"
  (prompt msg)
  (setq ss (ssget (list (cons 0 types))))
  (if ss (pickset:to-list ss) nil))

(defun at-edit:pl-drop-nth (lst n / res i)
  "返回去掉第 n 项（0 基）的列表"
  "list"
  "(at-edit:pl-drop-nth '(1 2 3) 1) => (1 3)"
  (setq i 0)
  (foreach x lst
    (if (/= i n) (setq res (append res (list x))))
    (setq i (1+ i)))
  res)

(defun at-edit:pl-ins-nth (lst n v / res i)
  "在第 n 项（0 基）前插入 v，n 越界则追加"
  "list"
  "(at-edit:pl-ins-nth '(1 3) 1 2) => (1 2 3)"
  (setq i 0)
  (foreach x lst
    (if (= i n) (setq res (append res (list v))))
    (setq res (append res (list x)))
    (setq i (1+ i)))
  (if (>= n (length lst)) (setq res (append res (list v))))
  res)

(defun at-edit:pl-set-nth (lst n v / res i)
  "把第 n 项（0 基）替换为 v"
  "list"
  "(at-edit:pl-set-nth '(1 2 3) 1 9) => (1 9 3)"
  (setq i 0)
  (foreach x lst
    (setq res (append res (list (if (= i n) v x))))
    (setq i (1+ i)))
  res)

(defun at-edit:pl-rev-seg (seg)
  "反转一段 (pt1 pt2 bulge)"
  "段"
  "(at-edit:pl-rev-seg '((0 0)(1 0) 0))"
  (list (cadr seg) (car seg) (- (caddr seg))))

(defun at-edit:pl-flat-seg (seg tol / p1 p2 b o r a1 a2 sweep n i a da res da2)
  "把一段曲线按弦高 tol 离散为直线段列表，tol 为 nil/0 时原样返回"
  "段列表"
  "(at-edit:pl-flat-seg '((0 0)(1 0) 0) nil)"
  (setq p1 (car seg)
        p2 (cadr seg)
        b (caddr seg))
  (if (or (null tol) (<= tol 0.0) (zerop b))
    (list seg)
    (progn
      (setq o (at-edit:2d (curve:bulge2o p1 p2 b)))
      (if (null o)
        (list seg)
        (progn
          (setq r (at-edit:2d-dist o p1))
          (if (or (<= r tol) (zerop r))
            (list seg)
            (progn
              (setq a1 (angle o p1)
                    a2 (angle o p2))
              (setq sweep (if (> b 0)
                            (at-edit:ang-diff a1 a2)
                            (- (at-edit:ang-diff a2 a1))))
              (setq da (sqrt (* 8.0 (/ tol r))))
              (if (<= da 0.0) (setq da 0.001))
              (setq n (min 2048 (max 2 (fix (1+ (/ (abs sweep) da)))))
                    da2 (/ sweep (float n))
                    i 0
                    res '())
              (while (< i n)
                (setq a (+ a1 (* (float i) da2)))
                (setq res (append res
                            (list (list
                                   (list (+ (car o) (* r (cos a)))
                                         (+ (cadr o) (* r (sin a))))
                                   (list (+ (car o) (* r (cos (+ a da2))))
                                         (+ (cadr o) (* r (sin (+ a da2)))))
                                   0))))
                (setq i (1+ i)))
              res)))))))

(defun at-edit:pl-flat-segs (segs tol / res)
  "整条段列表按弦高离散"
  "段列表"
  "(at-edit:pl-flat-segs segs 1.0)"
  (foreach s segs
    (setq res (append res (at-edit:pl-flat-seg s tol))))
  res)

(defun at-edit:pl-chain (segs tol / pool chains cur s i endp found s0)
  "把首尾相接的段按容差串成连通链"
  "链列表（每条链为段列表）"
  "(at-edit:pl-chain segs 1e-6)"
  (setq pool segs
        chains '())
  (while pool
    (setq cur (list (car pool))
          pool (cdr pool)
          found t)
    (while (and found pool)
      (setq found nil
            endp (cadr (car (reverse cur)))
            i 0)
      (while (and (not found) (< i (length pool)))
        (setq s (nth i pool))
        (cond
          ((at-edit:pl-near endp (car s) tol)
           (setq cur (append cur (list s)) found t))
          ((at-edit:pl-near endp (cadr s) tol)
           (setq cur (append cur (list (at-edit:pl-rev-seg s))) found t)))
        (setq i (1+ i)))
      (if found
        (setq pool (at-edit:pl-drop-nth pool (1- i)))
        (progn
          (setq s0 (car cur)
                endp (car s0)
                i 0)
          (while (and (not found) (< i (length pool)))
            (setq s (nth i pool))
            (cond
              ((at-edit:pl-near endp (cadr s) tol)
               (setq cur (cons s cur) found t))
              ((at-edit:pl-near endp (car s) tol)
               (setq cur (cons (at-edit:pl-rev-seg s) cur) found t)))
            (setq i (1+ i)))
          (if found (setq pool (at-edit:pl-drop-nth pool (1- i)))))))
    (setq chains (append chains (list cur))))
  chains)

(defun at-edit:pl-segs2data (segs closed / pts bulges)
  "段列表转 (点表 凸度表)，closed 为 T 时去掉重复终点"
  "list (pts bulges)"
  "(at-edit:pl-segs2data segs nil)"
  (if (null segs) nil
    (progn
      (setq pts (list (car (car segs))))
      (foreach s segs
        (setq pts (append pts (list (cadr s)))
              bulges (append bulges (list (caddr s)))))
      (if closed (setq pts (reverse (cdr (reverse pts)))))
      (list pts bulges))))

(defun at-edit:pl-circle-arcs (cen rad / segs i a1 a2 p1 p2)
  "圆转 4 段 90 度弧段（首尾相接，可直接成闭合多段线）"
  "段列表"
  "(at-edit:pl-circle-arcs '(0 0) 5)"
  (setq segs '()
        i 0)
  (repeat 4
    (setq a1 (* i pi 0.5)
          a2 (* (1+ i) pi 0.5))
    (setq p1 (list (+ (car cen) (* rad (cos a1)))
                   (+ (cadr cen) (* rad (sin a1))))
          p2 (list (+ (car cen) (* rad (cos a2)))
                   (+ (cadr cen) (* rad (sin a2)))))
    (setq segs (append segs (list (list p1 p2 (at-edit:tan (/ pi 8.0))))))
    (setq i (1+ i)))
  segs)

(defun at-edit:pl-segs-of (ent / cen rad)
  "取图元的 2D 段列表（圆用 4 段 90 度弧，其余交给 ent-segs）"
  "段列表或 nil"
  "(at-edit:pl-segs-of (car (entsel)))"
  (if (= "CIRCLE" (at-edit:ent-type ent))
    (progn
      (setq cen (entity:getdxf ent 10)
            rad (entity:getdxf ent 40))
      (at-edit:pl-circle-arcs (at-edit:2d cen) rad))
    (at-edit:ent-segs ent)))

(defun at-edit:pl-segs2pl (segs tol gw layer / closed data)
  "段列表→多段线（首尾重合判为闭合），返回新图元名"
  "图元名或 nil"
  "(at-edit:pl-segs2pl segs 1e-6 0.0 \"0\")"
  (if segs
    (progn
      (setq closed (and (>= (length segs) 3)
                        (at-edit:pl-near (car (car segs))
                                         (cadr (car (reverse segs))) tol)))
      (setq data (at-edit:pl-segs2data segs closed))
      (at-edit:make-pl (car data) (cadr data) nil nil gw closed
                        (if layer layer "0") nil 0.0))
    nil))

(defun at-edit:pl-put-verts (ent pts bulges w1s w2s closed gw
                                 / d new i)
  "把点表/凸度表/宽度写回 LWPOLYLINE（图元名不变）"
  "图元名或 nil"
  "(at-edit:pl-put-verts ent pts bulges nil nil nil nil)"
  (if (not (= "LWPOLYLINE" (at-edit:ent-type ent)))
    nil
    (progn
      (setq d (vl-remove-if
               '(lambda (x) (member (car x) '(10 40 41 42 43 70 90)))
               (entget ent))
            new (list (cons 90 (length pts))
                      (cons 70 (if closed 1 0))
                      (if gw (cons 43 gw) nil))
            i 0)
      (foreach p pts
        (setq new (append new
                    (list (cons 10 (list (car p) (cadr p) 0.0))
                          (cons 42 (if (and bulges (> (length bulges) i))
                                     (nth i bulges) 0.0))
                          (if (and w1s (> (length w1s) i))
                            (cons 40 (nth i w1s)) nil)
                          (if (and w2s (> (length w2s) i))
                            (cons 41 (nth i w2s)) nil))))
        (setq i (1+ i)))
      (if (entmod (append d new))
        (progn (entupd ent) ent)
        nil))))

(defun at-edit:pl-collinear-p (p1 p2 p3 tol / d l)
  "p2 到 p1p3 连线距离小于 tol 判为共线"
  "T 或 nil"
  "(at-edit:pl-collinear-p '(0 0) '(1 0) '(2 0) 0.001) => T"
  (setq d (at-edit:v2sub (at-edit:2d p3) (at-edit:2d p1))
        l (at-edit:v2len d))
  (and (> l 1e-9)
       (< (abs (at-edit:v2cross d (at-edit:v2sub (at-edit:2d p2)
                                                  (at-edit:2d p1))))
          (* tol l))))

(defun at-edit:pl-bulge-3pt (p1 p2 p3 / ax ay bx by cx cy dd ux uy o a1 a2
                               am sweep)
  "三点定弧：p1 到 p3 过 p2 的凸度，共线/重合返回 0"
  "number"
  "(at-edit:pl-bulge-3pt '(0 0) '(1 1) '(2 0)) => -1.0"
  (setq p1 (at-edit:2d p1)
        p2 (at-edit:2d p2)
        p3 (at-edit:2d p3))
  (if (or (equal p1 p3) (equal p1 p2) (equal p2 p3))
    0.0
    (progn
      (setq ax (car p1) ay (cadr p1)
            bx (car p2) by (cadr p2)
            cx (car p3) cy (cadr p3)
            dd (* 2.0 (+ (* ax (- by cy)) (* bx (- cy ay))
                        (* cx (- ay by)))))
      (if (< (abs dd) 1e-12)
        0.0
        (progn
          (setq ux (/ (+ (* (+ (* ax ax) (* ay ay)) (- by cy))
                        (* (+ (* bx bx) (* by by)) (- cy ay))
                        (* (+ (* cx cx) (* cy cy)) (- ay by)))
                     dd)
                uy (/ (+ (* (+ (* ax ax) (* ay ay)) (- cx bx))
                        (* (+ (* bx bx) (* by by)) (- ax cx))
                        (* (+ (* cx cx) (* cy cy)) (- bx ax)))
                     dd)
                o (list ux uy)
                a1 (angle o p1)
                a2 (angle o p3)
                am (angle o p2)
                sweep (at-edit:ang-diff a1 a2))
          (if (>= (at-edit:ang-diff a1 am) sweep)
            (setq sweep (- sweep (* 2 pi))))
          (at-edit:tan (/ sweep 4.0)))))))

(defun at-edit:pl-tan-bulge (p1 p2 tdir / d n den r o a1 a2 sweep len)
  "已知 p1 处切线方向 tdir，求 p1→p2 的弧凸度，近共线返回 0"
  "number"
  "(at-edit:pl-tan-bulge '(0 0) '(1 1) '(1 0)) => 0.414"
  (setq p1 (at-edit:2d p1)
        p2 (at-edit:2d p2)
        len (at-edit:2d-dist p1 p2))
  (if (or (< len 1e-9) (null tdir))
    0.0
    (progn
      (setq d (at-edit:v2sub p2 p1)
            n (list (- (cadr tdir)) (car tdir))
            den (* 2.0 (at-edit:v2dot d n)))
      (if (< (abs den) 1e-12)
        0.0
        (progn
          (setq r (/ (at-edit:v2dot d d) den))
          (if (> (abs r) (* 100.0 len))
            0.0
            (progn
              (setq o (at-edit:v2add p1 (at-edit:v2scale n r))
                    a1 (angle o p1)
                    a2 (angle o p2)
                    sweep (if (> r 0)
                            (at-edit:ang-diff a1 a2)
                            (- (at-edit:ang-diff a2 a1))))
              (at-edit:tan (/ sweep 4.0)))))))))

(defun at-edit:pl-closest-on-seg (pt seg / p1 p2 b o r a1 ap sw t2)
  "段上离 pt 最近的点（弧段径向投影，超出弧范围取端点）"
  "二维点或 nil"
  "(at-edit:pl-closest-on-seg '(1 1) '((0 0)(2 0) 0))"
  (setq pt (at-edit:2d pt)
        p1 (at-edit:2d (car seg))
        p2 (at-edit:2d (cadr seg))
        b (caddr seg))
  (if (zerop b)
    (or (at-edit:pt-on-seg pt (list p1 p2))
        (if (< (at-edit:2d-dist pt p1) (at-edit:2d-dist pt p2)) p1 p2))
    (progn
      (setq o (at-edit:2d (curve:bulge2o p1 p2 b)))
      (if (null o)
        p1
        (progn
          (setq r (at-edit:2d-dist o p1)
                a1 (angle o p1)
                ap (angle o pt)
                sw (if (> b 0)
                     (at-edit:ang-diff a1 (angle o p2))
                     (- (at-edit:ang-diff (angle o p2) a1)))
                t2 (if (> b 0) (at-edit:ang-diff a1 ap)
                       (- (at-edit:ang-diff ap a1))))
          (if (and (> (abs t2) 1e-9) (< (abs t2) (- (abs sw) 1e-9)))
            (list (+ (car o) (* r (cos ap)))
                  (+ (cadr o) (* r (sin ap))))
            (if (< (at-edit:2d-dist pt p1) (at-edit:2d-dist pt p2)) p1 p2)))))))

(defun at-edit:pl-nearest-seg (segs pt / i best bi bp p d)
  "找离 pt 最近的段，返回 (段号 最近点 距离)"
  "list 或 nil"
  "(at-edit:pl-nearest-seg segs '(1 1))"
  (setq i 0)
  (foreach s segs
    (setq p (at-edit:pl-closest-on-seg pt s)
          d (if p (at-edit:2d-dist (at-edit:2d pt) p) nil))
    (if (and d (or (null best) (< d best)))
      (setq best d bi i bp p))
    (setq i (1+ i)))
  (if bi (list bi bp best) nil))

(defun at-edit:pl-nearest-vertex (pts pt / i best bi d)
  "找离 pt 最近的顶点索引（0 基）"
  "整数或 nil"
  "(at-edit:pl-nearest-vertex '((0 0)(1 0)) '(0.9 0.1)) => 1"
  (setq i 0)
  (foreach p pts
    (setq d (at-edit:2d-dist (at-edit:2d p) (at-edit:2d pt)))
    (if (or (null best) (< d best)) (setq best d bi i))
    (setq i (1+ i)))
  bi)

(defun at-edit:pl-set-closed (ent flag / d old v)
  "设置多段线闭合标志，返回图元名"
  "图元名或 nil"
  "(at-edit:pl-set-closed ent T)"
  (setq d (entget ent)
        old (if (assoc 70 d) (cdr (assoc 70 d)) 0))
  (setq v (if flag (logior old 1) (logand old -2)))
  (setq d (if (assoc 70 d)
            (subst (cons 70 v) (assoc 70 d) d)
            (append d (list (cons 70 v)))))
  (if (entmod d)
    (progn (entupd ent) ent)
    nil))

;; ------------------------------------------------------------
;; 1. 连接多段线
;; ------------------------------------------------------------

(defun at-edit:pl-join (ents tol del / segs res e lay chains c used s)
  "把首尾相接的图元按容差连接为多段线，返回新图元列表"
  "图元名列表或 nil"
  "(at-edit:pl-join (at-edit:pl-ents (ssget)) 0.01 T)"
  (setq ents (at-edit:pl-ents ents)
        segs '())
  (if (null ents) nil
    (progn
      (setq lay (entity:getdxf (car ents) 8))
      (foreach e ents
        (setq s (at-edit:pl-segs-of e))
        (if s
          (setq segs (append segs s)
                used (append used (list e)))))
      (if (null segs) nil
        (progn
          (setq chains (at-edit:pl-chain segs tol))
          (foreach c chains
            (setq e (at-edit:pl-segs2pl c tol nil lay))
            (if e (setq res (append res (list e)))))
          (if (and res del)
            (foreach e used (vl-catch-all-apply 'entdel (list e))))
          res)))))

(defun at-edit:ai-pljoin (ents tol del)
  "AI 调用：把首尾相接的线/弧/多段线连接为多段线，返回新多段线列表
参数: ents 图元或图元列表, tol 容差(nil=1e-6), del 删除原对象(nil=默认删除)
示例: (at-edit:ai-pljoin (ssget) 0.01)"
  (if (null tol) (setq tol 1e-6))
  (if (null del) (setq del t))
  (at-edit:pl-join ents tol del))

(defun at-edit:pljoin (/ ents tol res)
  "连接首尾相接的线/弧/多段线为一条多段线"
  "新多段线图元列表或 nil"
  "(at-edit:pljoin)"
  (setq ents (at-edit:pl-ssget "\n选择要连接的对象(线/弧/多段线): "
                              "LINE,ARC,LWPOLYLINE,POLYLINE"))
  (if (null ents)
    (progn (princ "\n未选择对象。") nil)
    (progn
      (setq tol (getreal "\n连接容差 <0.01>: "))
      (if (null tol) (setq tol 0.01))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:pl-join ents tol t))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已连接为 " (itoa (if res (length res) 0))
                     " 条多段线。"))
      res)))

;; ------------------------------------------------------------
;; 2. 宽度批量修改
;; ------------------------------------------------------------

(defun at-edit:pl-set-width (ent w1 w2 / ty d new i v vd)
  "设置单条多段线宽度：w2=nil 统一宽度 w1，否则每段头 w1 尾 w2"
  "图元名或 nil"
  "(at-edit:pl-set-width (car (entsel)) 2.0 nil)"
  (setq ty (at-edit:ent-type ent))
  (cond
    ((= ty "LWPOLYLINE")
     (setq d (vl-remove-if '(lambda (x) (member (car x) '(40 41 43)))
                           (entget ent)))
     (if w2
       (progn
         (setq new '() i 0)
         (foreach x d
           (setq new (append new (list x)))
           (if (= (car x) 10)
             (setq new (append new (list (cons 40 w1) (cons 41 w2))))))
         (setq d new))
       (setq d (append d (list (cons 43 w1)))))
     (if (entmod d)
       (progn (entupd ent) ent)
       nil))
    ((= ty "POLYLINE")
     (setq v (entnext ent))
     (while (and v (= "VERTEX" (at-edit:ent-type v)))
       (setq vd (vl-remove-if '(lambda (x) (member (car x) '(40 41)))
                              (entget v)))
       (entmod (append vd (list (cons 40 w1)
                                (cons 41 (if w2 w2 w1)))))
       (setq v (entnext v)))
     (entupd ent)
     ent)
    (t nil)))

(defun at-edit:ai-pwidth (ents width head-tail / res e w2)
  "AI 调用：批量修改多段线宽度，返回修改成功的图元列表
参数: ents 图元或列表, width 宽度, head-tail nil=统一宽度,
      数字=尾宽(首宽取 width), (首宽 尾宽)=逐段变宽
示例: (at-edit:ai-pwidth (ssget) 2.0) / (at-edit:ai-pwidth (ssget) 0.0 3.0)"
  (setq ents (at-edit:pl-ents ents))
  (if (or (null ents) (null width))
    nil
    (progn
      (setq w2 (cond ((listp head-tail) (cadr head-tail))
                     ((numberp head-tail) head-tail)
                     (t nil)))
      (foreach e ents
        (if (at-edit:pl-set-width e (float width)
                                  (if w2 (float w2) nil))
          (setq res (append res (list e)))))
      res)))

(defun at-edit:pwidth (/ ents w tw mode res)
  "批量修改多段线宽度"
  "修改的图元列表或 nil"
  "(at-edit:pwidth)"
  (setq ents (at-edit:pl-ssget "\n选择多段线: " "LWPOLYLINE,POLYLINE"))
  (if (null ents)
    (progn (princ "\n未选择多段线。") nil)
    (progn
      (initget "Uniform HeadTail")
      (setq mode (getkword "\n宽度方式 [统一/首尾] <统一>: "))
      (if (null mode) (setq mode "Uniform"))
      (setq w (getreal "\n宽度 <2.0>: "))
      (if (null w) (setq w 2.0))
      (if (= mode "HeadTail")
        (progn
          (setq tw (getreal "\n尾宽 <0.0>: "))
          (if (null tw) (setq tw 0.0))))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-pwidth ents w (if (= mode "HeadTail") tw nil)))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已修改 " (itoa (if res (length res) 0))
                     " 条多段线宽度。"))
      res)))

;; ------------------------------------------------------------
;; 3. 增加顶点
;; ------------------------------------------------------------

(defun at-edit:pl-to-lw (ent / data new)
  "把重多段线(POLYLINE)转成 LWPOLYLINE，LWPOLYLINE 原样返回"
  "图元名或 nil"
  "(at-edit:pl-to-lw (car (entsel)))"
  (cond
    ((= "LWPOLYLINE" (at-edit:ent-type ent)) ent)
    ((= "POLYLINE" (at-edit:ent-type ent))
     (setq data (at-edit:pline-data ent))
     (if data
       (progn
         (setq new (at-edit:make-pl (car data) (cadr data) (caddr data)
                                    (cadddr data) (nth 4 data)
                                    (nth 5 data) (nth 6 data) (nth 7 data)
                                    0.0))
         (if new (entdel ent))
         new)
       nil))
    (t nil)))

(defun at-edit:ai-add-vertex (ent pt / data pts bulges w1s w2s gw closed
                                 segs hit i np subs nb newpts nw1)
  "AI 调用：在多段线离 pt 最近的边处插入顶点并保持形状，返回图元名
参数: ent 多段线图元, pt 参考点(2D/3D)
示例: (at-edit:ai-add-vertex (car (entsel)) '(1.0 1.0))"
  (setq ent (at-edit:pl-to-lw ent)
        data (at-edit:pline-data ent))
  (if (or (null data) (null pt))
    nil
    (progn
      (setq pts (car data)
            bulges (cadr data)
            w1s (caddr data)
            w2s (cadddr data)
            gw (nth 4 data)
            closed (nth 5 data))
      (setq segs (at-edit:curve-segs ent))
      (setq hit (at-edit:pl-nearest-seg segs pt))
      (if (null hit)
        nil
        (progn
          (setq i (car hit)
                np (cadr hit))
          (setq subs (at-edit:split-seg (nth i segs) (list np)))
          (if (/= 2 (length subs))
            nil
            (progn
              (setq nb (mapcar 'caddr subs)
                    newpts (at-edit:pl-ins-nth pts (1+ i) np))
              (setq nb (at-edit:pl-ins-nth (at-edit:pl-set-nth
                                            bulges i (car nb))
                                           (1+ i) (cadr nb)))
              (if w1s
                (setq nw1 (at-edit:pl-ins-nth w1s (1+ i) (nth i w1s))
                      w2s (if w2s (at-edit:pl-ins-nth w2s (1+ i)
                                                     (nth i w2s)) nil)))
              (at-edit:pl-put-verts ent newpts nb
                                    (if w1s nw1 nil) w2s closed gw))))))))

(defun at-edit:add-vertex (/ ent pt)
  "在多段线上离指定点最近的边处插入顶点"
  "图元名或 nil"
  "(at-edit:add-vertex)"
  (setq ent (car (entsel "\n选择多段线: ")))
  (cond
    ((null ent) (princ "\n未选择对象。") nil)
    ((not (member (at-edit:ent-type ent)
                  '("LWPOLYLINE" "POLYLINE")))
     (princ "\n不是多段线。") nil)
    (t
     (setq pt (getpoint "\n指定插入位置: "))
     (if (null pt)
       nil
       (progn
         (at-edit:pl-undo-begin)
         (setq ent (at-edit:ai-add-vertex ent pt))
         (at-edit:pl-undo-end)
         (if ent (princ "\n已插入顶点。") (princ "\n插入失败。"))
         ent)))))

;; ------------------------------------------------------------
;; 4. 删除顶点
;; ------------------------------------------------------------

(defun at-edit:ai-delete-vertex (ent pt idx / data pts bulges w1s w2s gw
                                   closed n segs m a b p1 p2 nb k news data2)
  "AI 调用：删除多段线离 pt 最近的顶点（或指定索引 idx），返回图元名
参数: ent 多段线图元, pt 参考点, idx 顶点索引(0 基, 给 idx 时忽略 pt)
示例: (at-edit:ai-delete-vertex (car (entsel)) '(0.0 0.0)) 或 idx=2"
  (setq ent (at-edit:pl-to-lw ent)
        data (at-edit:pline-data ent))
  (cond
    ((or (null data) (< (length (car data)) 3)) nil)
    (t
     (setq pts (car data)
           bulges (cadr data)
           w1s (caddr data)
           w2s (cadddr data)
           gw (nth 4 data)
           closed (nth 5 data)
           n (length pts))
     (if (and (null idx) pt)
       (setq idx (at-edit:pl-nearest-vertex pts pt)))
     (if (or (null idx) (< idx 0) (>= idx n))
       nil
       (progn
         (setq segs (at-edit:pts2segs
                     (if closed (append pts (list (car pts))) pts)
                     bulges)
               m (length segs))
         (cond
           ((and (not closed) (zerop idx))
            (setq segs (cdr segs)))
           ((and (not closed) (= idx (1- n)))
            (setq segs (reverse (cdr (reverse segs)))))
           (t
            (setq a (if (> idx 0) (1- idx) (1- m))
                  b idx)
            (setq p1 (car (nth a segs))
                  p2 (cadr (nth b segs))
                  nb (at-edit:pl-bulge-3pt p1 (nth idx pts) p2))
            (setq news '()
                  k 0)
            (foreach s segs
              (cond
                ((= k (min a b))
                 (setq news (append news (list (list p1 p2 nb)))))
                ((member k (list a b)) nil)
                (t (setq news (append news (list s)))))
              (setq k (1+ k)))
            (setq segs news)))
         (setq data2 (at-edit:pl-segs2data segs closed))
         (if (or (null data2) (< (length (car data2)) 2))
           (progn (princ "\n删除后顶点过少。") nil)
           (at-edit:pl-put-verts ent (car data2) (cadr data2)
                                 (if w1s (at-edit:pl-drop-nth w1s idx))
                                 (if w2s (at-edit:pl-drop-nth w2s idx))
                                 closed gw)))))))

(defun at-edit:delete-vertex (/ ent pt)
  "删除多段线上离指定点最近的顶点（关键词 I 改为按索引）"
  "图元名或 nil"
  "(at-edit:delete-vertex)"
  (setq ent (car (entsel "\n选择多段线: ")))
  (cond
    ((null ent) (princ "\n未选择对象。") nil)
    ((not (member (at-edit:ent-type ent)
                  '("LWPOLYLINE" "POLYLINE")))
     (princ "\n不是多段线。") nil)
    (t
     (initget "Index Point")
     (setq pt (getpoint "\n指定要删除的顶点(或输入 I 按索引): "))
     (cond
       ((null pt) (princ "\n未指定位置。") nil)
       ((= 'STR (type pt))
        (at-edit:pl-undo-begin)
        (setq ent (at-edit:ai-delete-vertex
                   ent nil (getint "\n顶点索引(0 基): ")))
        (at-edit:pl-undo-end)
        (if ent (princ "\n已删除顶点。") (princ "\n删除失败。"))
        ent)
       (t
        (at-edit:pl-undo-begin)
        (setq ent (at-edit:ai-delete-vertex ent pt nil))
        (at-edit:pl-undo-end)
        (if ent (princ "\n已删除顶点。") (princ "\n删除失败。"))
        ent)))))

;; ------------------------------------------------------------
;; 5. 线弧转换
;; ------------------------------------------------------------

(defun at-edit:ai-lw-arc (ent mode / data pts bulges w1s w2s gw closed
                            n i newb b p0 p1 p2 nb tdir nxt)
  "AI 调用：多段线线弧转换，返回图元名
参数: ent 多段线图元, mode \"a2l\" 弧段改直段 / \"l2a\" 直段改切向弧
示例: (at-edit:ai-lw-arc (car (entsel)) \"l2a\")"
  (setq ent (at-edit:pl-to-lw ent)
        data (at-edit:pline-data ent))
  (if (or (null data) (null mode))
    nil
    (progn
      (setq pts (car data)
            bulges (cadr data)
            w1s (caddr data)
            w2s (cadddr data)
            gw (nth 4 data)
            closed (nth 5 data)
            n (length pts))
      (cond
        ((= (strcase mode) "A2L")
         (setq newb (mapcar '(lambda (x) 0.0) pts))
         (at-edit:pl-put-verts ent pts newb w1s w2s closed gw))
        ((= (strcase mode) "L2A")
         (setq i 0
               newb '())
         (foreach b bulges
           (setq p1 (nth i pts)
                 p2 (nth (if (< i (1- n)) (1+ i) 0) pts)
                 p0 (cond ((> i 0) (nth (1- i) pts))
                          (closed (nth (1- n) pts))
                          (t nil)))
           (if (/= 0.0 b)
             (setq newb (append newb (list b)))
             (progn
               (setq tdir (if p0 (at-edit:v2unit (at-edit:v2sub p1 p0))))
               (setq nb (at-edit:pl-tan-bulge p1 p2 tdir))
               (if (and (null p0) (= nb 0.0))
                 (progn
                   (setq nxt (nth (if (< (1+ i) n) (1+ i) 0) pts))
                   (if nxt
                     (setq nb (- (at-edit:pl-tan-bulge
                                  p2 p1
                                  (at-edit:v2unit
                                   (at-edit:v2sub nxt p2))))))))
               (setq newb (append newb (list nb)))))
           (setq i (1+ i)))
         (at-edit:pl-put-verts ent pts newb w1s w2s closed gw))
        (t nil)))))

(defun at-edit:lw-arc (/ ents mode res e)
  "多段线的弧段改直段或直段改切向弧"
  "修改的图元列表或 nil"
  "(at-edit:lw-arc)"
  (setq ents (at-edit:pl-ssget "\n选择多段线: " "LWPOLYLINE,POLYLINE"))
  (if (null ents)
    (progn (princ "\n未选择多段线。") nil)
    (progn
      (initget "Arc2Line Line2Arc")
      (setq mode (getkword "\n转换方式 [弧改直/直改弧] <弧改直>: "))
      (if (null mode) (setq mode "Arc2Line"))
      (at-edit:pl-undo-begin)
      (foreach e ents
        (if (at-edit:ai-lw-arc e (if (= mode "Arc2Line") "a2l" "l2a"))
          (setq res (append res (list e)))))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已转换 " (itoa (if res (length res) 0))
                     " 条多段线。"))
      res)))

;; ------------------------------------------------------------
;; 6. 多段线批处理
;; ------------------------------------------------------------

(defun at-edit:pl-reverse (ent / data pts bulges w1s w2s gw closed
                             newpts newb)
  "反转多段线方向（保持形状）"
  "图元名或 nil"
  "(at-edit:pl-reverse (car (entsel)))"
  (setq ent (at-edit:pl-to-lw ent)
        data (at-edit:pline-data ent))
  (if (null data)
    nil
    (progn
      (setq pts (car data)
            bulges (cadr data)
            w1s (caddr data)
            w2s (cadddr data)
            gw (nth 4 data)
            closed (nth 5 data))
      (if closed
        (setq newpts (append (list (car pts)) (reverse (cdr pts)))
              newb (mapcar '- (reverse bulges)))
        (setq newpts (reverse pts)
              newb (mapcar '- (reverse bulges))))
      (at-edit:pl-put-verts ent newpts newb
                            (if w1s (reverse w1s))
                            (if w2s (reverse w2s))
                            closed gw))))

(defun at-edit:pl-clean (ent tol / data pts bulges gw closed segs res
                           last-s s data2)
  "删除多段线中的重复/共线顶点"
  "图元名或 nil"
  "(at-edit:pl-clean (car (entsel)) 0.001)"
  (setq ent (at-edit:pl-to-lw ent)
        data (at-edit:pline-data ent))
  (if (null data)
    nil
    (progn
      (setq pts (car data)
            bulges (cadr data)
            gw (nth 4 data)
            closed (nth 5 data))
      (setq segs (at-edit:pts2segs
                  (if closed (append pts (list (car pts))) pts)
                  bulges))
      (foreach s segs
        (setq last-s (car (reverse res)))
        (if (and last-s (zerop (caddr s)) (zerop (caddr last-s))
                 (at-edit:pl-collinear-p (car last-s) (cadr last-s)
                                         (cadr s) tol))
          (setq res (append (reverse (cdr (reverse res)))
                            (list (list (car last-s) (cadr s) 0.0))))
          (setq res (append res (list s)))))
      (setq data2 (at-edit:pl-segs2data res closed))
      (if (or (null data2) (< (length (car data2)) 3))
        ent
        (at-edit:pl-put-verts ent (car data2) (cadr data2)
                              nil nil closed gw)))))

(defun at-edit:pl-corner (prev v next rad / t1 t2 cr dt del tk)
  "直段转角的圆角切点与凸度，无法倒角返回 nil"
  "list (切点1 切点2 凸度) 或 nil"
  "(at-edit:pl-corner '(0 0) '(1 0) '(1 1) 0.2)"
  (setq t1 (at-edit:v2unit (at-edit:v2sub v prev))
        t2 (at-edit:v2unit (at-edit:v2sub next v)))
  (if (or (null t1) (null t2))
    nil
    (progn
      (setq cr (at-edit:v2cross t1 t2)
            dt (at-edit:v2dot t1 t2)
            del (if (and (< (abs cr) 1e-12) (<= dt 0.0))
                  pi
                  (atan (abs cr) dt)))
      (setq tk (if (> del 1e-6) (* rad (at-edit:tan (/ del 2.0))) 0.0))
      (if (or (<= tk 1e-9)
              (>= (* 2.0 tk) (at-edit:2d-dist prev v))
              (>= (* 2.0 tk) (at-edit:2d-dist v next)))
        nil
        (list (at-edit:v2sub v (at-edit:v2scale t1 tk))
              (at-edit:v2add v (at-edit:v2scale t2 tk))
              (* (at-edit:tan (/ del 4.0)) (if (< cr 0.0) -1.0 1.0)))))))

(defun at-edit:pl-emit (st pt b)
  "在 (点表 凸度表) 结构尾端追加一点一段"
  "list"
  "(at-edit:pl-emit (list '((0 0)) '(0)) '(1 0) 0.0)"
  (list (append (car st) (list pt))
        (append (cadr st) (list b))))

(defun at-edit:pl-fillet (ent rad / data pts bulges gw closed n segs cor
                            k c st cur out-pts)
  "给多段线各直段转角加圆角"
  "图元名或 nil"
  "(at-edit:pl-fillet (car (entsel)) 5.0)"
  (setq ent (at-edit:pl-to-lw ent)
        data (at-edit:pline-data ent))
  (if (or (null data) (null rad) (<= rad 0.0))
    nil
    (progn
      (setq pts (car data)
            bulges (cadr data)
            gw (nth 4 data)
            closed (nth 5 data)
            n (length pts)
            segs (at-edit:pts2segs
                  (if closed (append pts (list (car pts))) pts)
                  bulges))
      (setq k 0
            cor '())
      (while (< k n)
        (setq c (at-edit:pl-corner
                 (cond ((> k 0) (nth (1- k) pts))
                       (closed (nth (1- n) pts))
                       (t nil))
                 (nth k pts)
                 (cond ((< k (1- n)) (nth (1+ k) pts))
                       (closed (car pts))
                       (t nil))
                 rad))
        (if (and c
                 (zerop (if (> k 0) (caddr (nth (1- k) segs))
                          (if closed (caddr (nth (1- n) segs)) 1.0)))
                 (zerop (if (< k (length segs))
                          (caddr (nth k segs)) 1.0)))
          (setq cor (append cor (list c)))
          (setq cor (append cor (list nil))))
        (setq k (1+ k)))
      (setq c (car cor)
            cur (if (and closed c) (cadr c) (car pts))
            st (list (list cur) '())
            k 1)
      (while (< k (if closed n (1- n)))
        (setq c (nth k cor))
        (if c
          (progn
            (setq st (at-edit:pl-emit st (car c) 0.0)
                  st (at-edit:pl-emit st (cadr c) (caddr c))
                  cur (cadr c))))
        (setq k (1+ k)))
      (cond
        (closed
         (setq c (car cor))
         (cond
           (c (setq st (at-edit:pl-emit st (car c) 0.0)
                    st (at-edit:pl-emit st (cadr c) (caddr c))))
           ((not (at-edit:pl-near cur (car pts) 1e-9))
            (setq st (at-edit:pl-emit st (car pts) 0.0)))))
        ((not (at-edit:pl-near cur (car (reverse pts)) 1e-9))
         (setq st (at-edit:pl-emit st (car (reverse pts)) 0.0))))
      (setq out-pts (car st))
      (if closed
        (setq out-pts (reverse (cdr (reverse out-pts)))))
      (at-edit:pl-put-verts ent out-pts (cadr st) nil nil closed gw))))

(defun at-edit:ai-pline-batch (ents op arg / ents2 res e tol rad)
  "AI 调用：多段线批处理，返回处理后的图元列表
参数: ents 图元或列表, op \"join\"/\"close\"/\"reverse\"/\"clean\"/\"fillet\",
      arg join=容差 clean=共线容差 fillet=圆角半径
示例: (at-edit:ai-pline-batch (ssget) \"fillet\" 5.0)"
  (setq ents2 (at-edit:pl-ents ents))
  (if (or (null ents2) (null op))
    nil
    (progn
      (setq op (strcase op))
      (cond
        ((= op "JOIN")
         (at-edit:pl-join ents2 (if arg arg 1e-6) t))
        ((= op "CLOSE")
         (foreach e ents2
           (if (at-edit:pl-set-closed e t)
             (setq res (append res (list e))))))
        ((= op "REVERSE")
         (foreach e ents2
           (if (at-edit:pl-reverse e)
             (setq res (append res (list e))))))
        ((= op "CLEAN")
         (setq tol (if arg arg 0.001))
         (foreach e ents2
           (if (at-edit:pl-clean e tol)
             (setq res (append res (list e))))))
        ((= op "FILLET")
         (setq rad (if arg arg 5.0))
         (foreach e ents2
           (if (at-edit:pl-fillet e rad)
             (setq res (append res (list e)))))))
      res)))

(defun at-edit:pline-batch (/ ents op arg res)
  "多段线批处理：连接/闭合/反转/删共线点/圆角"
  "处理结果列表或 nil"
  "(at-edit:pline-batch)"
  (setq ents (at-edit:pl-ssget "\n选择多段线: " "LWPOLYLINE,POLYLINE"))
  (if (null ents)
    (progn (princ "\n未选择多段线。") nil)
    (progn
      (initget "Join Close Reverse Clean Fillet")
      (setq op (getkword
                "\n操作 [连接/闭合/反转/清理/圆角] <清理>: "))
      (if (null op) (setq op "Clean"))
      (cond
        ((= op "Join")
         (setq arg (getreal "\n连接容差 <0.01>: "))
         (if (null arg) (setq arg 0.01)))
        ((= op "Clean")
         (setq arg (getreal "\n共线容差 <0.001>: "))
         (if (null arg) (setq arg 0.001)))
        ((= op "Fillet")
         (setq arg (getreal "\n圆角半径 <5.0>: "))
         (if (null arg) (setq arg 5.0))))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-pline-batch ents op arg))
      (at-edit:pl-undo-end)
      (princ (strcat "\n处理完成，共 " (itoa (if res (length res) 0))
                     " 个对象。"))
      res)))

;; ------------------------------------------------------------
;; 7. 曲线转多段线
;; ------------------------------------------------------------

(defun at-edit:ai-curve2pline (ents tol del / ents2 res e segs ty pl lay
                                 used)
  "AI 调用：曲线(线/弧/圆/样条/椭圆/多段线)转多段线，返回新多段线列表
参数: ents 图元或列表, tol 弦高(nil=1.0, 0=保留弧段), del 删除原对象
示例: (at-edit:ai-curve2pline (ssget) 0.5 T)"
  (setq ents2 (at-edit:pl-ents ents))
  (if (null ents2)
    nil
    (progn
      (if (null tol) (setq tol 1.0))
      (foreach e ents2
        (setq ty (at-edit:ent-type e))
        (if (not (member ty '("LWPOLYLINE" "INSERT" "WIPEOUT")))
          (progn
            (setq segs (at-edit:pl-segs-of e))
            (if segs
              (progn
                (if (> tol 0.0)
                  (setq segs (at-edit:pl-flat-segs segs tol)))
                (setq lay (entity:getdxf e 8))
                (setq pl (at-edit:pl-segs2pl segs 1e-6 nil lay))
                (if pl
                  (setq res (append res (list pl))
                        used (append used (list e)))))))))
      (if (and res del)
        (foreach e used (vl-catch-all-apply 'entdel (list e))))
      res)))

(defun at-edit:curve2pline (/ ents tol res)
  "曲线(弧/圆/样条/椭圆/多段线)转多段线"
  "新多段线列表或 nil"
  "(at-edit:curve2pline)"
  (setq ents (at-edit:pl-ssget
              "\n选择曲线(线/弧/圆/样条/椭圆): "
              "LINE,ARC,CIRCLE,SPLINE,ELLIPSE,POLYLINE"))
  (if (null ents)
    (progn (princ "\n未选择曲线。") nil)
    (progn
      (setq tol (getreal "\n弦高容差 <1.0>: "))
      (if (null tol) (setq tol 1.0))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-curve2pline ents tol t))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已转换 " (itoa (if res (length res) 0))
                     " 条曲线。"))
      res)))

;; ------------------------------------------------------------
;; 8. 面域转多段线
;; ------------------------------------------------------------

(defun at-edit:pl-vla-list (x / r)
  "把 vla 调用返回的 safearray/variant/单个对象统一为对象列表"
  "list"
  "(at-edit:pl-vla-list objs)"
  (cond
    ((null x) nil)
    ((listp x) x)
    ((= 'SAFEARRAY (type x)) (vlax-safearray->list x))
    ((= 'VARIANT (type x))
     (setq r (vl-catch-all-apply
              'vlax-safearray->list
              (list (vl-catch-all-apply 'vlax-variant-value (list x)))))
     (if (vl-catch-all-error-p r) nil r))
    (t (list x))))

(defun at-edit:ai-region2pline (ents tol / ents2 res e obj objs sub
                                  segs lay pl ob c)
  "AI 调用：面域炸开并连接为多段线，返回新多段线列表
参数: ents 面域图元或列表, tol 弦高(nil=1.0)
示例: (at-edit:ai-region2pline (ssget) 1.0)"
  (setq ents2 (at-edit:pl-ents ents))
  (if (null ents2)
    nil
    (progn
      (if (null tol) (setq tol 1.0))
      (foreach e ents2
        (if (= "REGION" (at-edit:ent-type e))
          (progn
            (setq obj (vlax-ename->vla-object e)
                  lay (entity:getdxf e 8))
            (setq objs (vl-catch-all-apply 'vlax-invoke (list obj 'Explode)))
            (if (not (vl-catch-all-error-p objs))
              (progn
                (vla-delete obj)
                (setq segs '())
                (foreach ob (at-edit:pl-vla-list objs)
                  (setq sub (vl-catch-all-apply
                             'vlax-vla-object->ename (list ob)))
                  (if (= 'ENAME (type sub))
                    (progn
                      (setq segs (append segs (at-edit:pl-segs-of sub)))
                      (vl-catch-all-apply 'entdel (list sub)))))
                (if (> tol 0.0)
                  (setq segs (at-edit:pl-flat-segs segs tol)))
                (foreach c (at-edit:pl-chain segs 1e-4)
                  (setq pl (at-edit:pl-segs2pl c 1e-4 nil lay))
                  (if pl (setq res (append res (list pl))))))))))
      res)))

(defun at-edit:region2pline (/ ents tol res)
  "面域转多段线"
  "新多段线列表或 nil"
  "(at-edit:region2pline)"
  (setq ents (at-edit:pl-ssget "\n选择面域: " "REGION"))
  (if (null ents)
    (progn (princ "\n未选择面域。") nil)
    (progn
      (setq tol (getreal "\n弦高容差 <1.0>: "))
      (if (null tol) (setq tol 1.0))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-region2pline ents tol))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已转换 " (itoa (if res (length res) 0))
                     " 个面域。"))
      res)))

;; ------------------------------------------------------------
;; 9. 圆变圆环
;; ------------------------------------------------------------

(defun at-edit:ai-circle2donut (ents width del / ents2 res e cen r rc lay
                                  z new)
  "AI 调用：圆改圆环(外径=圆半径, 内径=半径-2*宽度)，返回圆环列表
参数: ents 圆图元或列表, width 环宽, del 删除原圆(nil=默认删除)
示例: (at-edit:ai-circle2donut (ssget) 3.0)"
  (setq ents2 (at-edit:pl-ents ents))
  (if (or (null ents2) (null width) (<= width 0.0))
    nil
    (progn
      (foreach e ents2
        (if (= "CIRCLE" (at-edit:ent-type e))
          (progn
            (setq cen (entity:getdxf e 10)
                  r (entity:getdxf e 40)
                  rc (- r width)
                  lay (entity:getdxf e 8)
                  z (if (caddr cen) (caddr cen) 0.0))
            (if (> rc 0.0)
              (progn
                (setq new (entmakex
                           (list '(0 . "LWPOLYLINE")
                                 '(100 . "AcDbEntity")
                                 (cons 8 lay)
                                 '(100 . "AcDbPolyline")
                                 '(90 . 2)
                                 '(70 . 1)
                                 (cons 43 (* 2.0 width))
                                 (cons 38 z)
                                 (cons 10 (list (- (car cen) rc)
                                                (cadr cen) z))
                                 '(42 . 1.0)
                                 (cons 10 (list (+ (car cen) rc)
                                                (cadr cen) z))
                                 '(42 . 1.0))))
                (if new (setq res (append res (list new)))))))))
      (if (and res (if (null del) t del))
        (foreach e ents2
          (if (= "CIRCLE" (at-edit:ent-type e))
            (vl-catch-all-apply 'entdel (list e)))))
      res)))

(defun at-edit:circle2donut (/ ents w res)
  "圆变圆环"
  "圆环列表或 nil"
  "(at-edit:circle2donut)"
  (setq ents (at-edit:pl-ssget "\n选择圆: " "CIRCLE"))
  (if (null ents)
    (progn (princ "\n未选择圆。") nil)
    (progn
      (setq w (getreal "\n圆环宽度 <3.0>: "))
      (if (null w) (setq w 3.0))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-circle2donut ents w t))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已转换 " (itoa (if res (length res) 0))
                     " 个圆。"))
      res)))

;; ------------------------------------------------------------
;; 10. 圆环转圆
;; ------------------------------------------------------------

(defun at-edit:ai-donut2circle (ents del / res e data pts bulges w closed
                                  cen rc lay z c1 c2 r1 r2)
  "AI 调用：圆环(带宽度的两顶点闭合多段线)转两个同心圆，返回圆列表
参数: ents 圆环图元或列表, del 删除原圆环(nil=默认删除)
示例: (at-edit:ai-donut2circle (ssget))"
  (foreach e (at-edit:pl-ents ents)
    (if (= "LWPOLYLINE" (at-edit:ent-type e))
      (progn
        (setq data (at-edit:pline-data e))
        (if data
          (setq pts (car data)
                bulges (cadr data)
                w (entity:getdxf e 43)
                closed (nth 5 data)))
        (if (and data (= 2 (length pts)) closed w (> w 0.0)
                 (= 2 (length bulges))
                 (equal 1.0 (abs (car bulges)) 1e-6)
                 (equal 1.0 (abs (cadr bulges)) 1e-6))
          (progn
            (setq cen (at-edit:v2scale
                       (at-edit:v2add (car pts) (cadr pts)) 0.5)
                  rc (* 0.5 (at-edit:2d-dist (car pts) (cadr pts)))
                  lay (entity:getdxf e 8)
                  z 0.0
                  r1 (+ rc (* 0.5 w))
                  r2 (- rc (* 0.5 w))
                  c1 nil
                  c2 nil)
            (setq c1 (entmakex (list '(0 . "CIRCLE")
                                     '(100 . "AcDbEntity")
                                     (cons 8 lay)
                                     '(100 . "AcDbCircle")
                                     (cons 10 (list (car cen) (cadr cen) z))
                                     (cons 40 r1))))
            (if (> r2 0.0)
              (setq c2 (entmakex (list '(0 . "CIRCLE")
                                       '(100 . "AcDbEntity")
                                       (cons 8 lay)
                                       '(100 . "AcDbCircle")
                                       (cons 10 (list (car cen)
                                                      (cadr cen) z))
                                       (cons 40 r2)))))
            (if c1 (setq res (append res (list c1))))
            (if c2 (setq res (append res (list c2))))
            (if (and c1 (if (null del) t del))
              (vl-catch-all-apply 'entdel (list e))))))))
  res)

(defun at-edit:donut2circle (/ ents res)
  "圆环转同心圆"
  "同心圆列表或 nil"
  "(at-edit:donut2circle)"
  (setq ents (at-edit:pl-ssget "\n选择圆环: " "LWPOLYLINE"))
  (if (null ents)
    (progn (princ "\n未选择圆环。") nil)
    (progn
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-donut2circle ents t))
      (at-edit:pl-undo-end)
      (if res
        (princ (strcat "\n已生成 " (itoa (length res)) " 个圆。"))
        (princ "\n未找到符合条件的圆环。"))
      res)))

;; ------------------------------------------------------------
;; 11. 多段线布尔运算
;; ------------------------------------------------------------

(defun at-edit:pl-regions (ents / ms objs regs rst)
  "由封闭曲线创建面域，返回面域 VLA 对象列表"
  "VLA 对象列表或 nil"
  "(at-edit:pl-regions (at-edit:pl-ents (ssget)))"
  (setq ms (vla-get-modelspace
            (vla-get-activedocument (vlax-get-acad-object))))
  (setq objs (mapcar '(lambda (e) (vlax-ename->vla-object e)) ents))
  (setq regs (vl-catch-all-apply 'vlax-invoke (list ms 'AddRegion objs)))
  (cond
    ((vl-catch-all-error-p regs) nil)
    ((= 'VLA-OBJECT (type regs)) (list regs))
    ((= 'SAFEARRAY (type regs)) (vlax-safearray->list regs))
    ((= 'VARIANT (type regs))
     (setq rst (vl-catch-all-apply
                'vlax-safearray->list
                (list (vl-catch-all-apply 'vlax-variant-value
                                          (list regs)))))
     (if (vl-catch-all-error-p rst)
       (vl-catch-all-apply 'vlax-safearray->list (list regs))
       rst))
    ((listp regs) regs)
    (t nil)))

(defun at-edit:ai-pline-bool (ents mode del / ents2 regs acc objs segs
                                lay pl res op e ob c)
  "AI 调用：封闭曲线 union/subtract/intersect，返回结果多段线列表
参数: ents 闭合曲线列表(第一个为被减对象), mode union/subtract/intersect,
      del 删除原对象(nil=默认删除)
示例: (at-edit:ai-pline-bool (ssget) \"union\")"
  (setq ents2 (at-edit:pl-ents ents))
  (if (or (null ents2) (null mode))
    nil
    (progn
      (setq regs (at-edit:pl-regions ents2))
      (if (null regs)
        (progn (princ "\n无法生成面域(请选择闭合曲线)。") nil)
        (progn
          (setq op (cond ((= (strcase mode) "SUBTRACT") 3)
                         ((= (strcase mode) "INTERSECT") 2)
                         (t 1)))
          (setq acc (car regs))
          (foreach ob (cdr regs)
            (vl-catch-all-apply 'vla-boolean (list acc op ob)))
          (setq lay (entity:getdxf (car ents2) 8))
          (setq objs (vl-catch-all-apply 'vlax-invoke (list acc 'Explode)))
          (if (not (vl-catch-all-error-p objs))
            (progn
              (foreach ob (at-edit:pl-vla-list objs)
                (setq e (vl-catch-all-apply 'vlax-vla-object->ename
                                            (list ob)))
                (if (= 'ENAME (type e))
                  (progn
                    (setq segs (append segs (at-edit:pl-segs-of e)))
                    (vl-catch-all-apply 'entdel (list e)))))
              (foreach c (at-edit:pl-chain segs 1e-4)
                (setq pl (at-edit:pl-segs2pl c 1e-4 nil lay))
                (if pl (setq res (append res (list pl)))))))
          (foreach ob regs
            (vl-catch-all-apply 'vla-delete (list ob)))
          (if (and res (if (null del) t del))
            (foreach e ents2 (vl-catch-all-apply 'entdel (list e))))
          res)))))

(defun at-edit:pline-bool (/ ents mode res)
  "封闭多段线布尔运算(并/交/差)"
  "结果多段线列表或 nil"
  "(at-edit:pline-bool)"
  (setq ents (at-edit:pl-ssget
              "\n选择封闭多段线(先选的为被减对象): "
              "LWPOLYLINE,POLYLINE,CIRCLE"))
  (if (null ents)
    (progn (princ "\n未选择对象。") nil)
    (progn
      (initget "Union Subtract Intersect")
      (setq mode (getkword "\n运算方式 [并集/差集/交集] <并集>: "))
      (if (null mode) (setq mode "Union"))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-pline-bool ents mode t))
      (at-edit:pl-undo-end)
      (if res
        (princ (strcat "\n布尔运算完成，生成 " (itoa (length res))
                       " 条多段线。"))
        (princ "\n布尔运算失败。"))
      res)))

;; ------------------------------------------------------------
;; 12. 智能外轮廓
;; ------------------------------------------------------------

(defun at-edit:pl-make-layer (name)
  "确保图层存在，返回图层名"
  "string"
  "(at-edit:pl-make-layer \"OUTLINE\")"
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   '(70 . 0)
                   '(62 . 7)
                   '(6 . "Continuous"))))
  name)

(defun at-edit:pl-put-layer (ent layer / d)
  "把图元改到指定图层，返回图元名"
  "图元名或 nil"
  "(at-edit:pl-put-layer ent \"OUTLINE\")"
  (setq d (entget ent))
  (if (and d (assoc 8 d))
    (progn (entmod (subst (cons 8 layer) (assoc 8 d) d))
           (entupd ent)
           ent)
    nil))

(defun at-edit:pl-boundary-at (pt layer / last-ent e)
  "用 -BOUNDARY 在内部点生成边界，返回图元名"
  "图元名或 nil"
  "(at-edit:pl-boundary-at '(5 5 0) \"0\")"
  (setq last-ent (entlast))
  (vl-catch-all-apply 'command (list "_.-BOUNDARY" pt ""))
  (setq e (entlast))
  (if (and e (not (equal e last-ent)) (entget e))
    (progn (if layer (at-edit:pl-put-layer e layer)) e)
    nil))

(defun at-edit:ai-boundary (ents pt layer / ents2 segs res c pl lay)
  "AI 调用：由选中实体围成的区域生成外轮廓多段线，返回多段线列表
参数: ents 边界图元或列表(可为 nil), pt 内部点(nil=按实体连接求闭合链),
      layer 结果图层(nil=沿用源对象图层)
示例: (at-edit:ai-boundary (ssget) nil \"OUTLINE\")"
  (setq ents2 (at-edit:pl-ents ents))
  (cond
    (ents2
     (setq lay (if layer
                 (at-edit:pl-make-layer layer)
                 (entity:getdxf (car ents2) 8)))
     (foreach e ents2
       (setq segs (append segs (at-edit:pl-segs-of e))))
     (foreach c (at-edit:pl-chain segs 1e-4)
       (if (at-edit:pl-near (car (car c)) (cadr (car (reverse c))) 1e-4)
         (progn
           (setq pl (at-edit:pl-segs2pl c 1e-4 nil lay))
           (if pl (setq res (append res (list pl)))))))
     (cond
       (res
        (if (> (length res) 1)
          (setq res (if (at-edit:ai-pline-bool res "union" t) (at-edit:ai-pline-bool res "union" t) res)))
        (if layer (foreach pl res (at-edit:pl-put-layer pl layer)))
        res)
       (pt
        (setq pl (at-edit:pl-boundary-at pt (if layer
                                            (at-edit:pl-make-layer layer))))
        (if pl (list pl) nil))
       (t
        (princ "\n未能围成闭合区域。")
        nil)))
    (pt
     (setq pl (at-edit:pl-boundary-at pt (if layer
                                         (at-edit:pl-make-layer layer))))
     (if pl (list pl) nil))
    (t nil)))

(defun at-edit:boundary (/ ents pt layer res)
  "由选中实体围成的区域生成外轮廓多段线"
  "多段线列表或 nil"
  "(at-edit:boundary)"
  (setq ents (at-edit:pl-ssget
              "\n选择围成区域的实体(回车改为指定内部点): "
              "LINE,ARC,LWPOLYLINE,POLYLINE,CIRCLE,SPLINE,ELLIPSE"))
  (if (null ents)
    (setq pt (getpoint "\n指定区域内一点: "))
    (progn
      (setq layer (getstring "\n结果图层(回车沿用源图层): "))
      (if (= layer "") (setq layer nil))))
  (if (and (null ents) (null pt))
    (progn (princ "\n未指定边界。") nil)
    (progn
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-boundary ents pt layer))
      (at-edit:pl-undo-end)
      (if res
        (princ (strcat "\n生成外轮廓 " (itoa (length res)) " 条。"))
        (princ "\n生成失败。"))
      res)))

;; ------------------------------------------------------------
;; 13. 快速作块
;; ------------------------------------------------------------

(defun at-edit:ai-quick-block (ents name basept del-src / ents2 ss e res
                                 ins)
  "AI 调用：把实体做成块并原地替换，返回块引用图元名
参数: ents 图元或列表, name 块名, basept 基点(nil=首实体起点),
      del-src 删除原实体(nil=t)
示例: (at-edit:ai-quick-block (ssget) \"MYBLK\" '(0.0 0.0 0.0))"
  (setq ents2 (at-edit:pl-ents ents))
  (cond
    ((or (null ents2) (null name) (/= 'STR (type name))) nil)
    ((tblsearch "BLOCK" name)
     (princ (strcat "\n块名已存在: " name))
     nil)
    (t
     (if (null basept)
       (setq basept (entity:getdxf (car ents2) 10)))
     (if (null basept) (setq basept '(0.0 0.0 0.0)))
     (if (null del-src) (setq del-src t))
     (if (not del-src)
       (foreach e ents2
         (vl-catch-all-apply 'vla-copy
                             (list (vlax-ename->vla-object e)))))
     (setq ss (ssadd))
     (foreach e ents2 (ssadd e ss))
     (setq res (vl-catch-all-apply 'command
                                   (list "_.-BLOCK" name basept ss "")))
     (cond
       ((vl-catch-all-error-p res)
        (princ "\n创建块失败。")
        nil)
       ((and (setq ins (entlast))
             (= "INSERT" (at-edit:ent-type ins))
             (= name (entity:getdxf ins 2)))
        ins)
       ((tblsearch "BLOCK" name)
        (vl-catch-all-apply 'command
                            (list "_.-INSERT" name basept 1.0 1.0 0.0))
        (setq ins (entlast))
        (if (and del-src (= "INSERT" (at-edit:ent-type ins)))
          (foreach e ents2
            (if (and (entget e) (/= e ins))
              (vl-catch-all-apply 'entdel (list e)))))
        (if (= "INSERT" (at-edit:ent-type ins)) ins nil))
       (t nil)))))

(defun at-edit:quick-block (/ ents name basept ins)
  "选择实体快速作块并原地替换"
  "块引用图元名或 nil"
  "(at-edit:quick-block)"
  (setq ents (at-edit:pl-ssget "\n选择要做块的实体: "*))
  (if (null ents)
    (progn (princ "\n未选择实体。") nil)
    (progn
      (setq name (getstring "\n块名: "))
      (if (= name "")
        (progn (princ "\n块名为空。") nil)
        (progn
          (setq basept (getpoint "\n指定基点: "))
          (if (null basept) (setq basept '(0.0 0.0 0.0)))
          (at-edit:pl-undo-begin)
          (setq ins (at-edit:ai-quick-block ents name basept t))
          (at-edit:pl-undo-end)
          (if ins
            (princ (strcat "\n已创建块: " name))
            (princ "\n创建失败。"))
          ins)))))

;; ------------------------------------------------------------
;; 14. 全能炸块
;; ------------------------------------------------------------

(defun at-edit:pl-xref-p (ent / name rec)
  "判断块引用是否来自外部参照"
  "T 或 nil"
  "(at-edit:pl-xref-p (car (entsel)))"
  (setq name (entity:getdxf ent 2)
        rec (tblsearch "BLOCK" name))
  (if (and rec (logand 4 (if (assoc 70 rec) (cdr (assoc 70 rec)) 0)))
    t
    nil))

(defun at-edit:pl-explode-rec (ent depth / obj objs res e)
  "递归炸开图块（内部函数），返回炸开后的图元列表"
  "图元名列表"
  "(at-edit:pl-explode-rec (car (entsel)) 10)"
  (if (and (= "INSERT" (at-edit:ent-type ent))
           depth (> depth 0)
           (not (at-edit:pl-xref-p ent)))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq objs (vl-catch-all-apply 'vlax-invoke (list obj 'Explode)))
      (if (vl-catch-all-error-p objs)
        (list ent)
        (progn
          (vla-delete obj)
          (foreach ob objs
            (setq e (vl-catch-all-apply 'vlax-vla-object->ename
                                        (list ob)))
            (if (= 'ENAME (type e))
              (setq res (append res
                                (at-edit:pl-explode-rec e (1- depth))))))
          res)))
    (list ent)))

(defun at-edit:ai-explode-block (ents depth / res e)
  "AI 调用：递归炸开图块(含嵌套块与属性)，返回炸开后的图元列表
参数: ents 块引用或列表, depth 最大递归层数(nil=10)
示例: (at-edit:ai-explode-block (ssget) 10)"
  (if (null depth) (setq depth 10))
  (foreach e (at-edit:pl-ents ents)
    (setq res (append res (at-edit:pl-explode-rec e depth))))
  res)

(defun at-edit:explode-block (/ ents depth res)
  "全能炸块：递归炸开嵌套图块"
  "炸开后的图元列表或 nil"
  "(at-edit:explode-block)"
  (setq ents (at-edit:pl-ssget "\n选择图块: " "INSERT"))
  (if (null ents)
    (progn (princ "\n未选择图块。") nil)
    (progn
      (setq depth (getint "\n最大递归层数 <10>: "))
      (if (null depth) (setq depth 10))
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-explode-block ents depth))
      (at-edit:pl-undo-end)
      (princ (strcat "\n已炸开为 " (itoa (if res (length res) 0))
                     " 个图元。"))
      res)))

;; ------------------------------------------------------------
;; 15. 边界生成遮罩
;; ------------------------------------------------------------

(defun at-edit:ai-wipeout (ent del / last-ent e res)
  "AI 调用：由闭合边界生成遮罩(WIPEOUT)，返回遮罩图元名
参数: ent 闭合多段线/圆/椭圆/样条, del 删除边界对象(nil=保留)
示例: (at-edit:ai-wipeout (car (entsel)))"
  (if (or (null ent)
          (not (member (at-edit:ent-type ent)
                       '("LWPOLYLINE" "POLYLINE" "CIRCLE" "ELLIPSE"
                         "SPLINE"))))
    (progn (princ "\n需要闭合边界对象。") nil)
    (progn
      (setq last-ent (entlast))
      (setq res (vl-catch-all-apply 'command
                                    (list "_.WIPEOUT" "_P" ent)))
      (if (vl-catch-all-error-p res)
        (progn (princ "\n遮罩创建失败。") nil)
        (progn
          (setq e (entlast))
          (if (and e (not (equal e last-ent))
                   (= "WIPEOUT" (at-edit:ent-type e)))
            (progn
              (if del (vl-catch-all-apply 'entdel (list ent)))
              e)
            nil))))))

(defun at-edit:wipeout (/ ent res)
  "由闭合边界生成遮罩"
  "遮罩图元名或 nil"
  "(at-edit:wipeout)"
  (setq ent (car (entsel "\n选择闭合边界(多段线/圆): ")))
  (if (null ent)
    (progn (princ "\n未选择对象。") nil)
    (progn
      (at-edit:pl-undo-begin)
      (setq res (at-edit:ai-wipeout ent t))
      (at-edit:pl-undo-end)
      (if res (princ "\n已生成遮罩。") (princ "\n生成失败。"))
      res)))

;; ------------------------------------------------------------
;; 16. 删除遮罩
;; ------------------------------------------------------------

(defun at-edit:ai-erase-wipeout (ents / ents2 ss res e)
  "AI 调用：删除遮罩，ents=nil 时删除全部遮罩，返回被删图元列表
参数: ents 遮罩图元或列表(可为 nil)
示例: (at-edit:ai-erase-wipeout nil)"
  (cond
    (ents (setq ents2 (at-edit:pl-ents ents)))
    (t
     (setq ss (ssget "X" '((0 . "WIPEOUT"))))
     (setq ents2 (if ss (pickset:to-list ss) nil))))
  (foreach e ents2
    (if (and (= "WIPEOUT" (at-edit:ent-type e))
             (vl-catch-all-apply 'entdel (list e)))
      (setq res (append res (list e)))))
  res)

(defun at-edit:erase-wipeout (/ ss res)
  "删除遮罩（未选择时删除全部遮罩）"
  "被删图元列表或 nil"
  "(at-edit:erase-wipeout)"
  (prompt "\n选择要删除的遮罩(回车删除全部): ")
  (setq ss (ssget '((0 . "WIPEOUT"))))
  (at-edit:pl-undo-begin)
  (setq res (if ss
              (at-edit:ai-erase-wipeout (pickset:to-list ss))
              (at-edit:ai-erase-wipeout nil)))
  (at-edit:pl-undo-end)
  (princ (strcat "\n已删除 " (itoa (if res (length res) 0)) " 个遮罩。"))
  res)

(princ)
