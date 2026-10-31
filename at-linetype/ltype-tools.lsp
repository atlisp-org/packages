;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-linetype 线型工具 —— 主文件：菜单注册 + 公共函数
;; 复现 iMini 线型类命令：批量改线型 / 局部变虚线 / 反转线型方向 /
;; 输出线型定义 / 炸线型为块 / 变线为树 / 成片布树 / 制作形文件 / 线型快捷键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'entity:*)
(require 'curve:*)
(require 'line:*)

(@:add-menus
 '(("线型工具"
    ("批量改线型" (at-linetype:ltype))
    ("局部变虚线" (at-linetype:jb-vd))
    ("反转线型方向" (at-linetype:reverse))
    ("输出线型定义" (at-linetype:lt-out))
    ("炸线型为块" (at-linetype:shp2blk))
    ("变线为树" (at-linetype:tree-style))
    ("成片布树" (at-linetype:tree-group))
    ("制作形文件" (at-linetype:mkshape))
    ("线型快捷命令" (at-linetype:lt-key))
    )))

(defun at-linetype:tan (x)
  "弧度 x 的正切值（本方言无全局 tan，自备实现）。
  参数: x:弧度"
  "数值"
  "(at-linetype:tan 1.0)"
  (/ (sin x) (cos x)))

(defun at-linetype:load-lin (path / cm)
  "加载 .lin 线型文件（-LINETYPE LOAD 方式）。
  参数: path:.lin 文件路径"
  "成功 t，文件不存在 nil"
  "(at-linetype:load-lin \"c:/a.lin\")"
  (if (findfile path)
      (progn
        (setq cm (getvar "cmdecho"))
        (setvar "cmdecho" 0)
        (command "_.-linetype" "_.l" "*" path)
        (while (> (getvar "cmdactive") 0) (command ""))
        (setvar "cmdecho" cm)
        t)
      (progn
        (princ (strcat "\n文件不存在: " path))
        nil)))

(defun at-linetype:check-ltype (name lin-file)
  "检查线型是否可用，缺失时尝试加载自定义 .lin。
  参数: name:线型名; lin-file:.lin 文件路径（可 nil）"
  "可用 t，不可用 nil"
  "(at-linetype:check-ltype \"DASHED\" nil)"
  (cond
   ((entity:ltype-exists name) t)
   ((and lin-file (at-linetype:load-lin lin-file)
         (entity:ltype-exists name))
    t)
   (t
    (progn
      (princ (strcat "\n线型不存在: " name))
      nil))))

(defun at-linetype:set-ltype (ename ltype / data)
  "设置单个图元的线型（替换已有 6 组码）。
  参数: ename:图元; ltype:线型名"
  "成功 t，失败 nil"
  "(at-linetype:set-ltype (car (entsel)) \"DASHED\")"
  (if (entity:ltype-exists ltype)
      (progn
        (setq data (entget ename))
        (entmod (append (vl-remove (assoc 6 data) data)
                        (list (cons 6 ltype))))
        (entupd ename)
        t)
      nil))

(defun at-linetype:apply-ltype (entlist ltype / n)
  "批量修改实体列表的线型。
  参数: entlist:ename 列表; ltype:线型名"
  "成功修改的数量"
  "(at-linetype:apply-ltype (ssget) \"DASHED\")"
  (setq n 0)
  (foreach en entlist
    (if (at-linetype:set-ltype en ltype)
        (setq n (1+ n))))
  n)

(defun at-linetype:collect-ents (sel layers / en)
  "收集待处理实体列表。
  参数: sel:选择集或 ename 列表（可 nil）; layers:图层名列表（sel 为 nil 时按层取）"
  "ename 列表"
  "(at-linetype:collect-ents (ssget) nil)"
  (cond
   ((and sel (= "SELSET" (type sel)))
    (pickset:to-list sel))
   ((= 'ename (type sel))
    (list sel))
   ((listp sel)
    (vl-remove-if-not (function (lambda (x) (= 'ename (type x)))) sel))
   (layers
    (if (ssget "_X" (list (cons 8 layers)))
        (pickset:to-list (ssget "_X" (list (cons 8 layers))))
        nil))
   (t nil)))

(defun at-linetype:pline-verts (ent / data pts bul w0 w1 cw n i lst p)
  "获取多段线顶点表（含凸度和宽度）。
  参数: ent:LWPOLYLINE 或 POLYLINE 图元"
  "列表 (点 凸度 起点宽 终点宽)..."
  "(at-linetype:pline-verts (car (entsel)))"
  (setq data (entget ent))
  (if (= "LWPOLYLINE" (cdr (assoc 0 data)))
      (progn
        (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 10)) data)))
        (setq bul (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 42)) data)))
        (setq w0 (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 40)) data)))
        (setq w1 (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 41)) data)))
        (setq cw (cdr (assoc 43 data)))
        (setq n (length pts))
        (setq i 0)
        (setq lst nil)
        (foreach p pts
          (setq lst (append lst
                       (list (list p
                                   (if (< i (length bul)) (nth i bul) 0.0)
                                   (if (if (< i (length w0)) (nth i w0) nil) (if (< i (length w0)) (nth i w0) nil) (if cw cw 0.0))
                                   (if (if (< i (length w1)) (nth i w1) nil) (if (< i (length w1)) (nth i w1) nil) (if cw cw 0.0)))))
          (setq i (1+ i)))
        lst)
      (progn
        ;; POLYLINE：顶点在子实体中
        (setq lst nil)
        (setq i (entnext ent))
        (while (and i (not (= "SEQEND" (cdr (assoc 0 (entget i))))))
          (if (= "VERTEX" (cdr (assoc 0 (entget i))))
              (progn
                (setq data (entget i))
                (setq lst (append lst
                             (list (list (cdr (assoc 10 data))
                                         (if (cdr (assoc 42 data)) (cdr (assoc 42 data)) 0.0)
                                         (if (cdr (assoc 40 data)) (cdr (assoc 40 data)) 0.0)
                                         (if (cdr (assoc 41 data)) (cdr (assoc 41 data)) 0.0)))))))
          (setq i (entnext i))))
        lst)))

(defun at-linetype:pline-closed-p (ent / data)
  "判断多段线是否闭合。
  参数: ent:多段线图元"
  "t 或 nil"
  "(at-linetype:pline-closed-p (car (entsel)))"
  (setq data (entget ent))
  (if (and (assoc 70 data)
           (= 1 (logand 1 (cdr (assoc 70 data)))))
      t
      nil))

(defun at-linetype:pline-edge-count (ent / n)
  "多段线的段数（闭合=顶点数，开口=顶点数-1）。
  参数: ent:多段线图元"
  "整数"
  "(at-linetype:pline-edge-count (car (entsel)))"
  (setq n (length (at-linetype:pline-verts ent)))
  (if (at-linetype:pline-closed-p ent)
      n
      (1- n)))

(defun at-linetype:tree-at (pt r / e1 e2)
  "在指定位置放一棵树（树冠圆 + 树干点）。
  参数: pt:中心点; r:树冠半径"
  "两个 ename 的列表，失败 nil"
  "(at-linetype:tree-at '(100 200 0) 50)"
  (setq e1 (entity:make-circle pt r))
  (if e1
      (progn
        (setq e2 (entity:make-point pt))
        (if e2
            (list e1 e2)
            (progn
              (entdel e1)
              nil)))
      nil))

(defun at-linetype:make-lwpl (pts bulges widths closed src-ent / d layer color lt n i w0 w1)
  "创建 LWPOLYLINE（支持逐点宽度，从源图元继承图层/颜色/线型）。
  参数: pts:点表; bulges:凸度表（与 pts 等长）; widths:((起点宽 终点宽)...)（可 nil）;
       closed:0/1; src-ent:继承属性的源图元（可 nil）"
  "ename，失败 nil"
  "(at-linetype:make-lwpl '((0 0) (10 0)) '(0 0) nil 0 nil)"
  (setq d (list (cons 0 "LWPOLYLINE")
                (cons 100 "AcDbEntity")
                (cons 100 "AcDbPolyline")
                (cons 90 (length pts))
                (cons 70 (if (= closed 1) 1 0))
                (cons 38 0.0)))
  (if src-ent
      (progn
        (setq layer (cdr (assoc 8 (entget src-ent))))
        (setq color (cdr (assoc 62 (entget src-ent))))
        (setq lt (cdr (assoc 6 (entget src-ent))))
        (if layer (setq d (append d (list (cons 8 layer)))))
        (if color (setq d (append d (list (cons 62 color)))))
        (if lt (setq d (append d (list (cons 6 lt)))))))
  (setq n (length pts))
  (setq i 0)
  (foreach p pts
    (setq w0 0.0)
    (setq w1 0.0)
    (if (and widths (< i (length widths)))
        (progn
          (setq w0 (car (nth i widths)))
          (setq w1 (cadr (nth i widths)))))
    (setq d (append d
                 (list (cons 10 p)
                       (cons 40 w0)
                       (cons 41 w1)
                       (cons 42 (if (< i (length bulges)) (nth i bulges) 0.0)))))
    (setq i (1+ i)))
  (entmakex (vl-remove nil d)))

(defun at-linetype:pt-seg-dist (pt p1 p2 / t2 dx dy)
  "点到线段的距离（二维投影）。
  参数: pt/p1/p2:二维或三维点"
  "距离"
  "(at-linetype:pt-seg-dist '(5 2 0) '(0 0 0) '(10 0 0))"
  (if (equal p1 p2 1e-9)
      (distance pt p1)
      (progn
        (setq dx (- (car p2) (car p1)))
        (setq dy (- (cadr p2) (cadr p1)))
        (setq t2 (/ (+ (* (- (cadr pt) (cadr p1)) dy)
                       (* (- (car pt) (car p1)) dx))
                    (+ (* dx dx) (* dy dy))))
        (if (< t2 0.0)
            (distance pt p1)
            (if (> t2 1.0)
                (distance pt p2)
                (distance pt (list (+ (car p1) (* t2 dx))
                                   (+ (cadr p1) (* t2 dy))
                                   0.0)))))))

(defun at-linetype:edge-at (verts closed i / p p1 p2)
  "取多段线第 i 段（0 基）的起点和终点。
  参数: verts:顶点表（见 pline-verts）; closed:是否闭合; i:段索引"
  "列表 (p1 p2)，越界 nil"
  "(at-linetype:edge-at (at-linetype:pline-verts e) nil 0)"
  (setq p (length verts))
  (if (and (> p 1) (>= i 0) (< i (if closed p (1- p))))
      (progn
        (setq p1 (car (nth i verts)))
        (setq p2 (car (nth (rem (1+ i) p) verts)))
        (list p1 p2))
      nil))

(defun at-linetype:pt-in-poly (pt pts / inside n i p1 p2)
  "判断点是否在多边形内（射线法）。
  参数: pt:点; pts:顶点表（无需闭合重复）"
  "t 或 nil"
  "(at-linetype:pt-in-poly '(5 5 0) '((0 0 0) (10 0 0) (10 10 0) (0 10 0)))"
  (setq inside nil)
  (setq n (length pts))
  (setq i 0)
  (while (< i n)
    (setq p1 (nth i pts))
    (setq p2 (nth (rem (1+ i) n) pts))
    (if (and (> (max (cadr p1) (cadr p2)) (cadr pt))
             (< (min (cadr p1) (cadr p2)) (cadr pt)))
        (progn
          (setq inside (xor inside
                       (> (car pt)
                          (+ (car p1)
                             (* (/ (- (cadr pt) (cadr p1))
                                   (- (cadr p2) (cadr p1)))
                                (- (car p2) (car p1)))))))))
    (setq i (1+ i)))
  inside)

(defun at-linetype:poly-area (pts / area n i p1 p2)
  "多边形面积（鞋带公式，取绝对值）。
  参数: pts:二维或三维顶点表"
  "面积"
  "(at-linetype:poly-area '((0 0 0) (10 0 0) (10 10 0) (0 10 0)))"
  (setq area 0.0)
  (setq n (length pts))
  (setq i 0)
  (while (< i n)
    (setq p1 (nth i pts))
    (setq p2 (nth (rem (1+ i) n) pts))
    (setq area (+ area (* (- (car p2) (car p1))
                          (+ (cadr p2) (cadr p1)))))
    (setq i (1+ i)))
  (/ (abs area) 2.0))

(defun at-linetype:bulge2pts (p1 bulge p2 n / dx dy c th d ctr r a1 pts i a)
  "把弧段（凸度）拆成 n 段弦，返回中间点。
  参数: p1/p2:起止点; bulge:凸度（0 返回 nil）; n:拆分段数（>=2）"
  "中间点列表（n-1 个，不含端点）"
  "(at-linetype:bulge2pts '(0 0 0) 1.0 '(10 0 0) 8)"
  (if (or (equal bulge 0.0 1e-9) (< n 2))
      nil
      (progn
        (setq dx (- (car p2) (car p1)))
        (setq dy (- (cadr p2) (cadr p1)))
        (setq c (distance p1 p2))
        (if (> c 1e-9)
            (progn
              (setq th (* 4.0 (atan bulge)))
              (setq d (/ (* 0.5 c) (at-linetype:tan (/ th 2.0))))
              (setq ctr (list (+ (/ (+ (car p1) (car p2)) 2.0) (* d (/ (- dy) c)))
                              (+ (/ (+ (cadr p1) (cadr p2)) 2.0) (* d (/ dx c)))
                              0.0))
              (setq r (distance ctr p1))
              (setq a1 (angle ctr p1))
              (setq i 1)
              (setq pts nil)
              (while (< i n)
                (setq a (+ a1 (* th (/ (float i) (float n)))))
                (setq pts (append pts
                             (list (list (+ (car ctr) (* r (cos a)))
                                         (+ (cadr ctr) (* r (sin a)))
                                         0.0))))
                (setq i (1+ i)))
              pts)
            nil))))
