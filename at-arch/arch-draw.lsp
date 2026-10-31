;; ============================================================
;; mini-arch — 迷你建筑绘图
;; 复现 iMini 建筑专业绘图命令：轴网、墙体、柱、梁、门窗、
;; 楼梯、标高坐标、剖断线、符号等。
;; 源码 UTF-8；函数命名 at-arch:<名>；AI 无交互入口 at-arch:ai-<动作>。
;; ============================================================

;; ==================== 菜单注册 ====================

(@:add-menus '(("建筑绘图"
    ("直线轴网" "(at-arch:axis-line)")
    ("轴线编号" "(at-arch:axis-text)")
    ("修改轴线编号" "(at-arch:axis-update)")
    ("--" "--")
    ("画墙" "(at-arch:wall)")
    ("单线变墙" "(at-arch:line2wall)")
    ("批量修改墙宽" "(at-arch:wall-width)")
    ("墙体完成面线" "(at-arch:wall-cover)")
    ("删除墙柱门窗" "(at-arch:erase-wall)")
    ("--" "--")
    ("画柱" "(at-arch:column)")
    ("多段线变柱" "(at-arch:pline2column)")
    ("--" "--")
    ("画梁" "(at-arch:beam)")
    ("单线变梁" "(at-arch:line2beam)")
    ("批量修改梁宽" "(at-arch:beam-width)")
    ("--" "--")
    ("两点画门" "(at-arch:door)")
    ("两点画窗" "(at-arch:win)")
    ("墙上开门" "(at-arch:hole-door)")
    ("门窗编号" "(at-arch:win-text)")
    ("门窗表" "(at-arch:win-list)")
    ("--" "--")
    ("普通楼梯平面" "(at-arch:stair-plan)")
    ("电梯轿厢平面" "(at-arch:elevator)")
    ("--" "--")
    ("建筑标高" "(at-arch:elev)")
    ("建筑坐标" "(at-arch:arch-coord)")
    ("折断线" "(at-arch:break-line)")
    ("剖面符号" "(at-arch:sec-symbol)")
    ("指北针" "(at-arch:north)")
)))

;; ==================== 公共变量 ====================

;; 出图比例系数：标注文字/符号尺寸 = 基准值 × 本值
;; 1:50/1:100 建筑图（mm 单位）保持 1.0 即可；小比例图可改小
(defvar *at-arch:scale* 1.0
  "迷你建筑绘图：标注与符号尺寸比例系数")

;; 常用图层名
(defvar *at-arch:layer-axis* "AXIS")
(defvar *at-arch:layer-wall* "WALL")
(defvar *at-arch:layer-column* "COLUMN")
(defvar *at-arch:layer-beam* "BEAM")
(defvar *at-arch:layer-door* "DOOR")
(defvar *at-arch:layer-window* "WINDOW")
(defvar *at-arch:layer-elev* "ELEV")
(defvar *at-arch:layer-coord* "COORD")
(defvar *at-arch:layer-stair* "STAIR")
(defvar *at-arch:layer-elevator* "ELEVATOR")
(defvar *at-arch:layer-symbol* "SYMBOL")
(defvar *at-arch:layer-hatch* "HATCH")
(defvar *at-arch:layer-cover* "COVER")

;; ==================== 通用辅助函数 ====================

(defun at-arch:ts (base-h)
  "尺寸缩放：基准值 × *at-arch:scale*。"
  "number"
  "(at-arch:ts 250)"
  (* base-h (if *at-arch:scale* *at-arch:scale* 1.0)))

(defun at-arch:layer-exists-p (name)
  "判断图层是否存在。"
  "t/nil"
  "(at-arch:layer-exists-p \"WALL\")"
  (if (null layer:make) (require 'layer:*))
  (member (strcase name) (mapcar (function strcase) (layer:list))))

(defun at-arch:ensure-layer (name color)
  "确保图层存在（不存在则创建），返回图层名。"
  "string"
  "(at-arch:ensure-layer \"WALL\" 7)"
  (if (null layer:make) (require 'layer:*))
  (if (at-arch:layer-exists-p name)
      name
      (progn
        (layer:make name (if color color 7) "continuous" 0)
        name)))

(defun at-arch:set-layer (name color)
  "确保图层存在并设为当前图层，返回图层名。"
  "string"
  "(at-arch:set-layer \"WALL\" 7)"
  (at-arch:ensure-layer name color)
  (setvar "clayer" name)
  name)

(defun at-arch:2d (pt)
  "取点前两坐标（忽略 Z）。"
  "list"
  "(at-arch:2d '(1 2 3))"
  (list (car pt) (cadr pt)))

(defun at-arch:vadd (a b)
  "二维向量加法。"
  "list"
  "(at-arch:vadd '(1 2) '(3 4))"
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))

(defun at-arch:vscale (v k)
  "二维向量数乘。"
  "list"
  "(at-arch:vscale '(1 2) 3)"
  (list (* (car v) k) (* (cadr v) k)))

(defun at-arch:vec (p1 p2)
  "向量 p1→p2。"
  "list"
  "(at-arch:vec '(0 0) '(3 4))"
  (list (- (car p2) (car p1)) (- (cadr p2) (cadr p1))))

(defun at-arch:normal (p1 p2 / v l)
  "线段 p1→p2 的单位法向量（行进方向左侧）。"
  "list"
  "(at-arch:normal '(0 0) '(1 0))"
  (setq v (at-arch:vec p1 p2))
  (setq l (distance '(0 0) v))
  (if (< l 1e-9) '(0 1)
      (list (/ (- (cadr v)) l) (/ (car v) l))))

(defun at-arch:ang-diff (a1 a2 / d)
  "两角度差（弧度，0~pi）。"
  "number"
  "(at-arch:ang-diff 0 (/ pi 2))"
  (setq d (- a1 a2))
  (while (> d pi) (setq d (- d (* 2 pi))))
  (while (< d -pi) (setq d (+ d (* 2 pi))))
  (abs d))

(defun at-arch:iota (n)
  "0 到 n-1 的整数列表。"
  "list"
  "(at-arch:iota 4)"
  (if (> n 0) (append (at-arch:iota (1- n)) (list (1- n)))))

(defun at-arch:setn (lst i val)
  "把列表第 i 个元素置为 val，返回新列表。"
  "list"
  "(at-arch:setn '(1 2 3) 1 9)"
  (if (= i 0)
      (cons val (cdr lst))
      (cons (car lst) (at-arch:setn (cdr lst) (1- i) val))))

(defun at-arch:first-false (lst / i)
  "列表中第一个 nil 的下标。"
  "number/nil"
  "(at-arch:first-false '(t nil t))"
  (setq i 0)
  (while (and (< i (length lst)) (nth i lst))
    (setq i (1+ i)))
  (if (< i (length lst)) i nil))

(defun at-arch:parse-numbers (str / s)
  "解析数字列表字符串（分隔符：逗号/分号/空格/中文标点），返回数字列表。"
  "list"
  "(at-arch:parse-numbers \"3600,3600;4200\")"
  (if (null str)
      nil
      (progn
        (setq s (vl-string-subst "，" "," str))
        (setq s (vl-string-subst "；" ";" s))
        (setq s (vl-string-subst " " ";" s))
        (setq s (vl-string-subst "\t" ";" s))
        (vl-remove-if-not
         (function numberp)
         (mapcar (function (lambda (x)
                    (vl-catch-all-apply 'read (list x))))
                 (at-arch:split-str s ";"))))))

(defun at-arch:letter (n / res i)
  "数字（从 1 起）转轴号字母：1=>A，26=>Z，27=>AA。"
  "string"
  "(at-arch:letter 27)"
  (setq res "")
  (setq i n)
  (while (> i 0)
    (setq i (1- i))
    (setq res (strcat (chr (+ 65 (rem i 26))) res))
    (setq i (fix (/ i 26))))
  res)

(defun at-arch:letter-index (s / res)
  "轴号字母转数字：\"A\"=>1，\"Z\"=>26，\"AA\"=>27。"
  "number"
  "(at-arch:letter-index \"C\")"
  (setq res 0)
  (setq s (strcase s))
  (while (/= s "")
    (setq res (+ (* res 26) (- (ord (substr s 1 1)) 64)))
    (setq s (if (> (strlen s) 1) (substr s 2) "")))
  res)

(defun at-arch:str-contain-p (s sub)
  "判断字符串 s 是否包含子串 sub（不依赖 wcmatch，其参数形式跨 CAD 不一致）。
  命中位置为 0 时 vl-string-search 返回 0（布尔上下文中作 nil），需显式处理。"
  "t/nil"
  "(at-arch:str-contain-p \"WINDOW\" \"WIN\")"
  (if (vl-string-search sub s)
      t
      (if (= (vl-string-search sub s) 0) t nil)))

;; ==================== 实体读取与创建 ====================

(defun at-arch:ent-list (ss / n i res)
  "选择集/ename 列表/vlax 对象 统一转 ename 列表（nil 返回空表）。"
  "list"
  "(at-arch:ent-list (ssget '((0 . \"LINE\"))))"
  (cond
    ((null ss) nil)
    ((= 'vlax (type ss)) (list (vlax-vla-object->ename ss)))
    ((= 'ename (type ss)) (list ss))
    ((= 'list (type ss))
     (mapcar
      (function (lambda (x)
         (if (= 'vlax (type x)) (vlax-vla-object->ename x) x)))
      ss))
    (t
     (setq n (sslength ss))
     (setq i 0)
     (setq res nil)
     (while (< i n)
       (setq res (cons (ssname ss i) res))
       (setq i (1+ i)))
     (reverse res))))

(defun at-arch:alive-p (ent / r)
  "判断实体是否仍然存在。"
  "t/nil"
  "(at-arch:alive-p (entlast))"
  (setq r (vl-catch-all-apply 'vlax-erased-p (list ent)))
  (if (or (vl-catch-all-error-p r) r) nil t))

(defun at-arch:ent-pts (ent / e obj n i p pts)
  "读取直线/多段线为二维点列表（弧段近似直线）；闭合多段线末尾补首点。"
  "list"
  "(at-arch:ent-pts (car (entsel)))"
  (if (null ent)
      nil
      (progn
        (setq e (entget ent))
        (cond
          ((eq "LINE" (cdr (assoc 0 e)))
           (list (at-arch:2d (cdr (assoc 10 e)))
                 (at-arch:2d (cdr (assoc 11 e)))))
          ((member (cdr (assoc 0 e)) '("LWPOLYLINE" "POLYLINE"))
           (setq obj (vlax-ename->vla-object ent))
           (setq n (fix (vla-get-PointCount obj)))
           (setq pts nil)
           (setq i 0)
           (while (< i n)
             (setq p (vlax-safearray->list (vla-get-Point obj i)))
             (setq pts (cons (at-arch:2d p) pts))
             (setq i (1+ i)))
           (setq pts (reverse pts))
           (if (vlax-curve-isClosed obj)
             (setq pts (append pts (list (car pts)))))
           pts)
          (t nil)))))

(defun at-arch:line (p1 p2 layer lwidth)
  "在指定图层创建直线。"
  "ename"
  "(at-arch:line '(0 0) '(100 0) \"AXIS\" 0.18)"
  (entmakex (list (cons 0 "LINE")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbLine")
                  (cons 8 layer)
                  (cons 10 (list (car p1) (cadr p1)
                            (if (caddr p1) (caddr p1) 0)))
                  (cons 11 (list (car p2) (cadr p2)
                            (if (caddr p2) (caddr p2) 0)))
                  (cons 42 (if lwidth lwidth 0.18))))
  (entlast))

(defun at-arch:pline (pts closed layer / dxf p)
  "在指定图层创建二维多段线。"
  "ename"
  "(at-arch:pline '((0 0)(100 0)(100 100)) 0 \"WALL\")"
  (setq dxf (list (cons 0 "LWPOLYLINE")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbPolyline")
                  (cons 8 layer)
                  (cons 90 (length pts))
                  (cons 70 (if closed 1 0))
                  (cons 38 0)
                  (cons 39 0)))
  (foreach p pts
    (setq dxf (append dxf
             (list (cons 10 (at-arch:2d p))
                   (cons 40 0) (cons 41 0) (cons 42 0)
                   (cons 91 0)))))
  (entmakex dxf)
  (entlast))

(defun at-arch:circle (pt r layer / e)
  "在指定图层创建圆。"
  "ename"
  "(at-arch:circle '(0 0) 300 \"AXIS\")"
  (setq e (entmakex (list (cons 0 "CIRCLE")
                  (cons 100 "AcDbEntity")
                  (cons 100 "AcDbCircle")
                  (cons 8 layer)
                  (cons 10 (list (car pt) (cadr pt)
                            (if (caddr pt) (caddr pt) 0)))
                  (cons 40 r))))
  (entlast))

(defun at-arch:arc (cen r pt1 pt2 layer / a)
  "在指定图层创建 90° 以内圆弧（自动选择短弧方向）。"
  "ename"
  "(at-arch:arc '(0 0) 100 '(100 0) '(0 100) \"DOOR\")"
  (if (null entity:make-arc) (require 'entity:*))
  (setq a (at-arch:ang-diff (angle cen pt1) (angle cen pt2)))
  (if (> a 1.7)
      (entity:make-arc cen r pt2 pt1)
      (entity:make-arc cen r pt1 pt2)))

(defun at-arch:text (str pt h ang align layer / e)
  "在指定图层创建单行文字。"
  "ename"
  "(at-arch:text \"M1\" '(0 0) 250 0 \"M\" \"DOOR\")"
  (if (null entity:make-text) (require 'entity:*))
  (if (null entity:putdxf) (require 'entity:*))
  (setq e (entity:make-text str (list (car pt) (cadr pt) 0)
           (if h h 250) (if ang ang 0) 0.8 0
           (if align align "M")))
  (if e (progn (entity:putdxf e 8 layer) e) nil))

(defun at-arch:text-w (str h)
  "估算文字宽度（字高 h，宽比 0.8）。"
  "number"
  "(at-arch:text-w \"M1\" 250)"
  (if (null text:gettextwidth) (require 'text:*))
  (text:gettextwidth
   str
   (list (cons 1 str) (cons 10 '(0 0)) (cons 40 h)
         (cons 41 0.8) (cons 50 0) (cons 51 0))))

;; ==================== 几何工具 ====================

(defun at-arch:offset-poly (pts dist / res i n a b m lm)
  "多段线 pts 各点整体偏移 dist（角点取角平分线），返回新点表。"
  "list"
  "(at-arch:offset-poly '((0 0)(1000 0)(1000 500)) 120)"
  (if (< (length pts) 2)
      (mapcar (function at-arch:2d) pts)
      (progn
        (setq res nil)
        (setq n (length pts))
        (setq i 0)
        (while (< i n)
          (setq a (at-arch:normal
                   (if (= i 0) (nth 0 pts) (nth (1- i) pts))
                   (nth i pts)))
          (setq b (at-arch:normal
                   (nth i pts)
                   (if (= i (1- n)) (nth (1- n) pts) (nth (1+ i) pts))))
          (if (= i 0)
              (setq m b)
              (if (= i (1- n))
                  (setq m a)
                  (progn
                    (setq m (list (+ (car a) (car b))
                             (+ (cadr a) (cadr b))))
                    (setq lm (distance '(0 0) m))
                    (if (< lm 1e-9) (setq m a)))))
          (setq res (cons
                     (list (+ (car (nth i pts)) (* (car m) (/ dist lm)))
                           (+ (cadr (nth i pts)) (* (cadr m) (/ dist lm))))
                     res))
          (setq i (1+ i)))
        (reverse res))))

(defun at-arch:proj-pt (pt p1 p2 / v l f)
  "点 pt 在直线 p1→p2 上的投影点（不限制在线段内）。"
  "list"
  "(at-arch:proj-pt '(5 3) '(0 0) '(10 0))"
  (setq v (at-arch:vec p1 p2))
  (setq l (distance '(0 0) v))
  (if (< l 1e-9)
      (at-arch:2d p1)
      (progn
        (setq f (/ (+ (* (- (car pt) (car p1)) (car v))
                      (* (- (cadr pt) (cadr p1)) (cadr v)))
                  l l))
        (at-arch:2d (at-arch:vadd p1 (at-arch:vscale v f))))))

(defun at-arch:seg-int (p1 p2 q1 q2 / res)
  "线段 p1p2 与直线 q1q2 的交点（无限延长）。"
  "list/nil"
  "(at-arch:seg-int '(0 0) '(10 10) '(5 0) '(5 10))"
  (setq res (vl-catch-all-apply 'inters
           (list p1 p2 q1 q2 t)))
  (if (vl-catch-all-error-p res) nil res))

(defun at-arch:frac (p a b / la)
  "点 p 在线段 a→b 上的比例（0~1）。"
  "number"
  "(at-arch:frac '(5 0) '(0 0) '(10 0))"
  (setq la (distance a b))
  (if (< la 1e-9) 0.0
      (/ (distance a p) la)))

(defun at-arch:sd (p a b / v)
  "点 p 到无限直线 a→b 的有符号距离（左侧为正，右侧为负）。"
  "number"
  "(at-arch:sd '(1 0) '(0 0) '(0 1))"
  (setq v (at-arch:vec a b))
  (- (* (car v) (- (cadr p) (cadr a)))
     (* (cadr v) (- (car p) (car a)))))

(defun at-arch:between-p (p c1a c1b c2a c2b)
  "判断点 p 是否位于两条无限直线 c1/c2 之间。"
  "t/nil"
  "(at-arch:between-p '(500 0) '(400 -100) '(400 100) '(600 100) '(600 -100))"
  (and (>= (* (at-arch:sd c2a c1a c1b)
              (at-arch:sd p c1a c1b))
           0)
       (>= (* (at-arch:sd c1a c2a c2b)
              (at-arch:sd p c2a c2b))
           0)))

(defun at-arch:split-poly (pts c1a c1b c2a c2b / res i n p1 p2
                                       xings cur f q mid)
  "按两条无限切割线（c1a-c1b 与 c2a-c2b）拆分多段线点表，
  删除落在两线之间的线段（用于在墙上开门窗洞口）。
pts: 二维点表。c1a/c1b: 切割线 1 两端点；c2a/c2b: 切割线 2 两端点。
return: 多段线点表列表；无法拆分返回 nil。"
  "list"
  "(at-arch:split-poly '((0 0)(1000 0)) '(400 -100) '(400 100) '(600 100) '(600 -100))"
  (if (< (length pts) 2) nil
      (progn
        (setq cur (list (car pts)))
        (setq res nil)
        (setq i 0)
        (setq n (1- (length pts)))
        (while (< i n)
          (setq p1 (nth i pts))
          (setq p2 (nth (1+ i) pts))
          (setq xings nil)
          (if (setq q (at-arch:seg-int p1 p2 c1a c1b))
              (setq xings (cons (list (at-arch:frac q p1 p2) q) xings)))
          (if (setq q (at-arch:seg-int p1 p2 c2a c2b))
              (setq xings (cons (list (at-arch:frac q p1 p2) q) xings)))
          (if xings
              (progn
                (setq xings (vl-sort xings
                        (function (lambda (a b) (< (car a) (car b))))))
                (setq f 0.0)
                (foreach x xings
                  (setq q (cadr x))
                  (setq mid (list
                             (+ (car p1)
                                (* (car (at-arch:vec p1 p2))
                                   (/ (+ f (car x)) 2.0)))
                             (+ (cadr p1)
                                (* (cadr (at-arch:vec p1 p2))
                                   (/ (+ f (car x)) 2.0)))))
                  (if (at-arch:between-p mid c1a c1b c2a c2b)
                      (progn
                        (if (> (length cur) 1)
                            (setq res (cons cur res)))
                        (setq cur (list q)))
                      (setq cur (if (and cur
                                          (> (distance q (car (reverse cur)))
                                             1e-6))
                                    (append cur (list q))
                                    (list q))))
                  (setq f (car x)))
                (setq mid (list
                           (+ (car p1)
                              (* (car (at-arch:vec p1 p2))
                                 (/ (+ f 1.0) 2.0)))
                           (+ (cadr p1)
                              (* (cadr (at-arch:vec p1 p2))
                                 (/ (+ f 1.0) 2.0)))))
                (if (at-arch:between-p mid c1a c1b c2a c2b)
                    (progn
                      (if (> (length cur) 1)
                          (setq res (cons cur res)))
                      (setq cur nil))
                    (setq cur (if (and cur
                                        (> (distance p2 (car (reverse cur)))
                                           1e-6))
                                  (append cur (list p2))
                                  cur))))
              (if (at-arch:between-p p2 c1a c1b c2a c2b)
                  (progn
                    (if (> (length cur) 1)
                        (setq res (cons cur res)))
                    (setq cur nil))
                  (setq cur (if (and cur
                                      (> (distance p2 (car (reverse cur)))
                                         1e-6))
                                (append cur (list p2))
                                cur))))
          (setq i (1+ i)))
        (if (> (length cur) 1) (setq res (cons cur res)))
        (vl-remove-if
         (function (lambda (r) (< (length r) 2)))
         (reverse res)))))

(defun at-arch:pair-geo-p (pts1 pts2 / ds mn mx a0 i)
  "判断两点表是否为两条平行偏移线（同顶点数、逐点距离近似相等、
偏移方向一致）。"
  "t/nil"
  (if (or (/= (length pts1) (length pts2)) (< (length pts1) 2))
      nil
      (progn
        (setq ds (mapcar
                  (function (lambda (i)
                     (distance (at-arch:2d (nth i pts1))
                               (at-arch:2d (nth i pts2)))))
                  (at-arch:iota (length pts1))))
        (setq mn (apply (function min) ds))
        (setq mx (apply (function max) ds))
        (and (> mn 1.0)
             (< (- mx mn) 80.0)
             (setq a0 (angle (at-arch:2d (car pts1))
                             (at-arch:2d (car pts2))))
             (vl-every
              (function (lambda (i)
                 (< (at-arch:ang-diff
                     a0
                     (angle (at-arch:2d (nth i pts1))
                           (at-arch:2d (nth i pts2))))
                    0.5)))
              (at-arch:iota (length pts1)))))))

(defun at-arch:pair-dist (pts1 pts2 / i n s)
  "两点表逐点距离均值。"
  "number"
  (setq n (length pts1))
  (setq s 0.0)
  (setq i 0)
  (while (< i n)
    (setq s (+ s (distance (at-arch:2d (nth i pts1))
                           (at-arch:2d (nth i pts2)))))
    (setq i (1+ i)))
  (/ s n))

(defun at-arch:pair-lines (lines / res i j best best-d n used d)
  "把线两两配成双线对（每条线找最近未配对的平行线）。
lines: ((ename pts) ...)。return: ((e1 pts1 e2 pts2) ...)，未配对的线不返回。"
  "list"
  (setq res nil)
  (setq n (length lines))
  (setq used (mapcar (function (lambda (x) nil)) lines))
  (while (< i n)
    (if (not (nth i used))
        (progn
          (setq best nil)
          (setq best-d 1e30)
          (setq j 0)
          (while (< j n)
            (if (and (not (nth j used)) (/= i j)
                     (at-arch:pair-geo-p
                      (cadr (nth i lines))
                      (cadr (nth j lines))))
                (progn
                  (setq d (at-arch:pair-dist
                           (cadr (nth i lines))
                           (cadr (nth j lines))))
                  (if (< d best-d)
                      (progn (setq best-d d) (setq best j)))))
            (setq j (1+ j)))
          (if best
              (progn
                (setq res (cons (list (nth i lines) (nth best lines)) res))
                (setq used (at-arch:setn used i 1))
                (setq used (at-arch:setn used best 1))))))
    (setq i (1+ i)))
  (reverse res))

(defun at-arch:draw-wall (pts t layer / s1 s2 e1 e2 o1 o2)
  "由中心线点表与墙厚画双线墙（两端封口），返回 4 个图元 ename 列表。"
  "list"
  (if (< (length pts) 2) nil
      (progn
        (setq o1 (at-arch:offset-poly pts (/ t 2.0)))
        (setq o2 (at-arch:offset-poly pts (- (/ t 2.0))))
        (setq s1 (at-arch:pline o1 0 layer))
        (setq s2 (at-arch:pline o2 0 layer))
        (setq e1 (at-arch:line (car o1) (car o2) layer 0))
        (setq e2 (at-arch:line (cadr (reverse o1)) (cadr (reverse o2))
                  layer 0))
        (list s1 s2 e1 e2))))

(defun at-arch:str (v)
  "值转字符串（整数不带小数点，浮点取整）。"
  "string"
  "(at-arch:str 240)"
  (cond
    ((null v) "")
    ((integerp v) (itoa v))
    ((numberp v) (rtos v 2 0))
    (t v)))

(defun at-arch:bbox (ents / res e d p mn mx r)
  "实体的包围盒，返回 ((min-x min-y) (max-x max-y))。"
  "list"
  "(at-arch:bbox (list (entlast)))"
  (if (null ents)
      nil
      (progn
        (setq mn (list 1e30 1e30))
        (setq mx (list -1e30 -1e30))
        (foreach e ents
          (if (setq d (vl-catch-all-apply 'entget (list e)))
              (progn
                (foreach p (mapcar (function cdr)
                           (vl-remove-if-not
                            (function (lambda (x) (= (car x) 10)))
                            d))
                  (if (and p (cadr p))
                      (progn
                        (setq mn (list (min (car mn) (car p))
                                (min (cadr mn) (cadr p))))
                        (setq mx (list (max (car mx) (car p))
                                (max (cadr mx) (cadr p)))))))
                (if (and (eq "CIRCLE" (cdr (assoc 0 d)))
                         (setq r (cdr (assoc 40 d)))
                         (setq p (cdr (assoc 10 d))))
                    (progn
                      (setq mn (list (min (car mn) (- (car p) r))
                              (min (cadr mn) (- (cadr p) r))))
                      (setq mx (list (max (car mx) (+ (car p) r))
                              (max (cadr mx) (+ (cadr p) r)))))))))
        (list mn mx))))

(defun at-arch:draw-table (pt title headers rows / cw rh W H i x y i2 n-c n-r)
  "画简单网格表格（pt 为左下角），返回 (宽 高)。
  title: 标题（可 nil）。headers: 表头字符串表。rows: 行（字符串/数值）。"
  "list"
  "(at-arch:draw-table '(0 0) \"门窗表\" '(\"编号\" \"数量\") '((\"M1\" 2)))"
  (if (or (null headers) (null rows))
      (progn (princ "draw-table: 表头或数据为空。") nil)
      (progn
        (setq rh (at-arch:ts 500))
        (setq cw (mapcar (function (lambda (h)
                       (+ (at-arch:text-w (at-arch:str h) rh) 300)))
                   headers))
        (setq W (apply (function +) cw))
        (setq n-r (length rows))
        (setq n-c (length headers))
        (setq H (* rh (+ n-r 1 (if title 1 0))))
        (setq i 0)
        (setq i2 0)
        (setq x 0)
        (setq y 0)
        (setq x (car pt))
        (repeat (1+ n-c)
          (at-arch:line (list x (cadr pt)) (list x (+ (cadr pt) H))
            (getvar "clayer") 0.18)
          (setq x (+ x (nth i cw)))
          (setq i (1+ i)))
        (setq y (cadr pt))
        (repeat (1+ n-r)
          (at-arch:line (list (car pt) y) (list (+ (car pt) W) y)
            (getvar "clayer") 0.18)
          (setq y (+ y rh)))
        (if title
            (at-arch:text title
              (list (+ (car pt) (/ W 2)) (+ (cadr pt) (- H (/ rh 2.0))))
              (at-arch:ts 350) 0 "M" (getvar "clayer")))
        ;; 表头行
        (setq i 0)
        (foreach h headers
          (at-arch:text (at-arch:str h)
            (list (+ (car pt) (apply (function +)
                     (at-arch:take cw i))
                   (/ (nth i cw) 2.0))
                  (+ (cadr pt) (+ (* rh n-r) (/ rh 2.0))))
            (at-arch:ts 300) 0 "M" (getvar "clayer"))
          (setq i (1+ i)))
        ;; 数据行（rows 第 0 行在最上方）
        (setq i 0)
        (foreach row rows
          (setq i2 0)
          (foreach cell row
            (at-arch:text (at-arch:str cell)
              (list (+ (car pt) (apply (function +)
                       (at-arch:take cw i2))
                     (/ (nth i2 cw) 2.0))
                    (+ (cadr pt) (+ (* rh (- (1- n-r) i)) (/ rh 2.0))))
              (at-arch:ts 300) 0 "M" (getvar "clayer"))
            (setq i2 (1+ i2)))
          (setq i (1+ i)))
        (list W H))))

(defun at-arch:take (lst n / res i)
  "取列表前 n 个元素。"
  "list"
  "(at-arch:take '(1 2 3) 2)"
  (setq res nil)
  (setq i 0)
  (while (and (< i n) (consp lst))
    (setq res (cons (car lst) res))
    (setq lst (cdr lst))
    (setq i (1+ i)))
  (reverse res))

(defun at-arch:center-of (pts1 pts2 / res i n)
  "两点表逐点中点（同顶点数）。"
  "list"
  "(at-arch:center-of '((0 0)(10 0)) '((0 240)(10 240)))"
  (progn
    (setq res nil)
    (setq i 0)
    (setq n (length pts1))
    (while (< i n)
      (setq res (cons
                 (list (/ (+ (car (nth i pts1)) (car (nth i pts2))) 2.0)
                       (/ (+ (cadr (nth i pts1)) (cadr (nth i pts2))) 2.0))
                 res))
      (setq i (1+ i)))
    (reverse res)))

(defun at-arch:offset-side (center pts d / res i n v len)
  "把线 pts 沿远离 center 的方向偏移 d（同顶点数），返回新点表。"
  "list"
  (progn
    (setq res nil)
    (setq i 0)
    (setq n (length pts))
    (while (< i n)
      (setq v (at-arch:vec (nth i center) (nth i pts)))
      (setq len (distance '(0 0) v))
      (if (> len 1e-9)
          (setq res (cons
                     (list (+ (car (nth i pts)) (* (car v) (/ d len)))
                           (+ (cadr (nth i pts)) (* (cadr v) (/ d len))))
                     res)))
      (setq i (1+ i)))
    (reverse res)))

(defun at-arch:rebuild-pairs (pairs tnew layer / res pr c o1 o2 s1 s2 e1 e2)
  "按新厚度重画各线对的双线（删除原线），返回新图元 ename 列表。
  pairs: ((e1 pts1 e2 pts2) ...)。"
  "list"
  (progn
    (setq res nil)
    (foreach pr pairs
      (setq c (at-arch:center-of (cadr (car pr)) (cadr (cadr pr))))
      (setq o1 (at-arch:offset-poly c (/ tnew 2.0)))
      (setq o2 (at-arch:offset-poly c (- (/ tnew 2.0))))
      (setq s1 (at-arch:pline o1 0 layer))
      (setq s2 (at-arch:pline o2 0 layer))
      (setq e1 (at-arch:line (car o1) (car o2) layer 0))
      (setq e2 (at-arch:line (cadr (reverse o1)) (cadr (reverse o2))
                layer 0))
      (if (at-arch:alive-p (car (car pr))) (entdel (car (car pr))))
      (if (at-arch:alive-p (car (cadr pr))) (entdel (car (cadr pr))))
      (setq res (append res (list s1 s2 e1 e2))))
    (reverse res)))

(defun at-arch:del-near (pt r layer / ss i)
  "删除指定图层上靠近 pt（半径 r 内）的圆/文字。"
  "无"
  (if (null entity:getdxf) (require 'entity:*))
  (if (setq ss (ssget "x" (list (cons 0 "CIRCLE TEXT") (cons 8 layer))))
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (if (and
               (at-arch:alive-p (ssname ss i))
               (<= (distance pt
                    (at-arch:2d (entity:getdxf (ssname ss i) 10)))
                   r))
              (entdel (ssname ss i)))
          (setq i (1+ i))))))

(defun at-arch:axis-number (pt label layer / r c th)
  "在 pt 画轴号（圆圈+居中编号），返回 (圆 en 文字 en)。"
  "list"
  "(at-arch:axis-number '(0 0) \"1\" \"AXIS\")"
  (setq r (at-arch:ts 300))
  (setq c (at-arch:circle pt r layer))
  (setq th (at-arch:text label pt (at-arch:ts 150) 0 "M" layer))
  (list c th))

(defun at-arch:split-str (str seps / res cur i n c)
  "按分隔符集合拆分字符串（seps 中任一字符都算分隔符），忽略空段"
  "字符串列表"
  "(at-arch:split-str \"a,b;c\" \",;\")"
  (setq res '() cur "" i 1 n (strlen str))
  (while (<= i n)
    (setq c (substr str i 1))
    (if (vl-string-search c seps)
        (progn
          (if (/= cur "") (setq res (append res (list cur))))
          (setq cur ""))
        (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (if (/= cur "") (setq res (append res (list cur))))
  res)
