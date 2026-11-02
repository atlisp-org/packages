;; ============================================================
;; at-edit 迷你全能编辑 - 主文件
;; 菜单注册 + 公共辅助函数（几何/曲线图元/块内容）
;; ============================================================

;; 注册菜单
(@:add-menus
 '(("迷你编辑"
    ("全能缩放" "(at-edit:exscale)")
    ("移动增强" "(at-edit:exmove)")
    ("从图块中复制图元" "(at-edit:excopy)")
    ("镜像增强" "(at-edit:exmirror)")
    ("删除增强" "(at-edit:exerase)")
    ("多窗口拉伸" "(at-edit:mstretch)")
    ("批量中心旋转" "(at-edit:rotate-center)")
    ("--" "--")
    ("单选动态偏移" "(at-edit:dy-offset)")
    ("多选多重偏移" "(at-edit:multi-offset)")
    ("--" "--")
    ("全能修剪" "(at-edit:extrim)")
    ("交点断开" "(at-edit:break-inter)")
    ("连续点打断" "(at-edit:break-at)")
    ("延长或修剪到两点连线" "(at-edit:extend2point)")
    ("框选修补单线" "(at-edit:fix-line)")
    ("--" "--")
    ("局部放大" "(at-edit:zoom-detail)")
    ("--" "--")
    ("连接多段线" "(at-edit:pljoin)")
    ("宽度批量修改" "(at-edit:pwidth)")
    ("增加顶点" "(at-edit:add-vertex)")
    ("删除顶点" "(at-edit:delete-vertex)")
    ("线弧转换" "(at-edit:lw-arc)")
    ("扩展批处理多义线" "(at-edit:pline-batch)")
    ("--" "--")
    ("转换曲线为多段线" "(at-edit:curve2pline)")
    ("面域转多段线" "(at-edit:region2pline)")
    ("圆变圆环" "(at-edit:circle2donut)")
    ("圆环转圆" "(at-edit:donut2circle)")
    ("--" "--")
    ("多段线布尔运算" "(at-edit:pline-bool)")
    ("智能外轮廓" "(at-edit:boundary)")
    ("--" "--")
    ("快速作块" "(at-edit:quick-block)")
    ("全能炸块" "(at-edit:explode-block)")
    ("由边界生成遮罩" "(at-edit:wipeout)")
    ("删除遮罩" "(at-edit:erase-wipeout)"))))

;; 按需加载函数库
(defun at-edit:ensure-libs ()
  "加载本包依赖的函数库（curve/entity/geometry/pickset/point/list）"
  "nil"
  "内部调用"
  (if (null curve:join) (require 'curve:*))
  (if (null entity:make-line) (require 'entity:*))
  (if (null geometry:on-segment) (require 'geometry:*))
  (if (null pickset:to-list) (require 'pickset:*))
  (if (null point:centroid) (require 'point:*))
  (if (null layer:make) (require 'layer:*))
  (if (null list:flatten) (require 'list:*))
  nil)
(at-edit:ensure-libs)

;; ------------------------------------------------------------
;; 2D 向量运算（点格式 (x y)）
;; ------------------------------------------------------------

(defun at-edit:2d (pt)
  "点转二维坐标，丢弃 z"
  "二维点 (x y)"
  "(at-edit:2d '(1 2 3)) => (1 2)"
  (list (car pt) (cadr pt)))

(defun at-edit:2d-dist (p1 p2)
  "两点二维距离"
  "number"
  "(at-edit:2d-dist '(0 0) '(3 4)) => 5"
  (sqrt (+ (expt (- (car p2) (car p1)) 2)
           (expt (- (cadr p2) (cadr p1)) 2))))

(defun at-edit:v2sub (p1 p2)
  "向量差 p1-p2"
  "二维点"
  "(at-edit:v2sub '(1 1) '(0 0)) => (1 1)"
  (list (- (car p1) (car p2)) (- (cadr p1) (cadr p2))))

(defun at-edit:v2add (p1 p2)
  "向量加 p1+p2"
  "二维点"
  "(at-edit:v2add '(1 1) '(1 1)) => (2 2)"
  (list (+ (car p1) (car p2)) (+ (cadr p1) (cadr p2))))

(defun at-edit:v2scale (p s)
  "向量缩放"
  "二维点"
  "(at-edit:v2scale '(1 2) 3) => (3 6)"
  (list (* (car p) s) (* (cadr p) s)))

(defun at-edit:v2dot (p1 p2)
  "向量点积"
  "number"
  "(at-edit:v2dot '(1 0) '(0 1)) => 0"
  (+ (* (car p1) (car p2)) (* (cadr p1) (cadr p2))))

(defun at-edit:v2cross (p1 p2)
  "向量叉积（z 分量）"
  "number"
  "(at-edit:v2cross '(1 0) '(0 1)) => 1"
  (- (* (car p1) (cadr p2)) (* (cadr p1) (car p2))))

(defun at-edit:v2len (p)
  "向量长度"
  "number"
  "(at-edit:v2len '(3 4)) => 5"
  (sqrt (at-edit:v2dot p p)))

(defun at-edit:v2unit (p / l)
  "单位向量，零向量返回 (1 0)"
  "二维点"
  "(at-edit:v2unit '(0 5)) => (0 1)"
  (setq l (at-edit:v2len p))
  (if (> l 1e-12)
    (at-edit:v2scale p (/ 1.0 l))
    '(1 0)))

(defun at-edit:ang-diff (a1 a2 / d)
  "a2-a1 归一化到 (0 2pi]，逆时针角差"
  "number (弧度)"
  "(at-edit:ang-diff 0 pi) => pi"
  (setq d (- a2 a1))
  (while (<= d 0.0) (setq d (+ d (* 2 pi))))
  (if (= d 0.0) (* 2 pi) d))

(defun at-edit:seg-inter (seg1 seg2 / d1 d2 c)
  "两直线（无限延长）交点，平行返回 nil"
  "二维点或 nil"
  "(at-edit:seg-inter '((0 0)(2 0)) '((1 0)(1 5))) => (1 0)"
  (setq d1 (at-edit:v2sub (cadr seg1) (car seg1)))
  (setq d2 (at-edit:v2sub (cadr seg2) (car seg2)))
  (setq c (at-edit:v2cross d1 d2))
  (if (> (abs c) 1e-10)
    (at-edit:v2add
     (car seg1)
     (at-edit:v2scale
      d1
      (/ (at-edit:v2cross (at-edit:v2sub (car seg2) (car seg1)) d2) c)))
    nil))

(defun at-edit:set-nth (lst n v / res i)
  "替换列表第 n 项（0 基），越界原样返回"
  "新列表"
  "(at-edit:set-nth '(1 2 3) 1 9)"
  (if (or (< n 0) (>= n (length lst)))
    lst
    (progn
      (setq res '()
            i 0)
      (foreach x lst
        (setq res (append res (list (if (= i n) v x))))
        (setq i (1+ i)))
      res)))

(defun at-edit:pt-on-seg (pt seg / d tt)
  "点到直线的投影，在线段内返回投影点，否则 nil"
  "二维点或 nil"
  "(at-edit:pt-on-seg '(1 1) '((0 0)(2 0))) => (1 0)"
  (setq d (at-edit:v2sub (cadr seg) (car seg)))
  (if (> (at-edit:v2dot d d) 1e-20)
    (progn
      (setq tt (/ (at-edit:v2dot (at-edit:v2sub pt (car seg)) d)
                  (at-edit:v2dot d d)))
      (if (and (>= tt -1e-9) (<= tt (+ 1.0 1e-9)))
        (at-edit:v2add (car seg) (at-edit:v2scale d tt))
        nil))
    nil))

(defun at-edit:seg2-inter (seg1 seg2 / p)
  "两线段（不延长）交点，无交点或平行/共线返回 nil"
  "二维点或 nil"
  "(at-edit:seg2-inter '((0 0)(2 2)) '((0 2)(2 0))) => (1 1)"
  (setq p (at-edit:seg-inter seg1 seg2))
  (if p
    (if (and (at-edit:pt-on-seg p seg1)
             (at-edit:pt-on-seg p seg2))
      p
      nil)
    nil))

(defun at-edit:pt-in-poly (pt poly / inside i n a b)
  "偶数环绕规则判断点是否在多边形内（2D 点，闭合）"
  "T 或 nil"
  "(at-edit:pt-in-poly '(1 1) '((0 0)(2 0)(2 2)(0 2))) => T"
  (setq inside nil
        n (length poly)
        i 0)
  (repeat n
    (setq a (nth i poly)
          b (nth (1+ (mod i n)) poly))
    (if (and (> (max (cadr a) (cadr b)) (cadr pt))
             (< (min (cadr a) (cadr b)) (cadr pt)))
      (if (< (car pt)
             (+ (car a)
                (/ (* (- (cadr pt) (cadr a))
                      (- (cadr b) (cadr a)))
                   (- (car b) (car a)))))
        (setq inside (not inside))))
    (setq i (1+ i)))
  inside)

(defun at-edit:poly-area (pts / sum i n a b)
  "多边形有向面积（2D 点，闭合），逆时针为正"
  "number"
  "(at-edit:poly-area '((0 0)(2 0)(2 2)(0 2))) => 4"
  (setq sum 0.0
        n (length pts)
        i 0)
  (repeat n
    (setq a (nth i pts)
          b (nth (1+ (mod i n)) pts))
    (setq sum (+ sum (- (* (car a) (cadr b)) (* (cadr a) (car b)))))
    (setq i (1+ i)))
  (* 0.5 sum))

;; ------------------------------------------------------------
;; 图元读取
;; ------------------------------------------------------------

(defun at-edit:ent-type (ent)
  "获取图元类型字符串，无效图元返回 nil"
  "string 或 nil"
  "(at-edit:ent-type (car (entsel)))"
  (vl-catch-all-apply 'entity:getdxf (list ent 0)))

(defun at-edit:pline-data (ent / g pts bs w1s w2s closed i)
  "读取 LINE/LWPOLYLINE/POLYLINE 数据
  返回 (list pts bulges w1s w2s globalw closed layer color)
  pts: 2D 点（闭合时不含重复终点）
  bulges: 每段凸度
  w1s/w2s: 每段头/尾宽，无变宽时为 nil
  globalw: 全局宽度(43)，无则 nil
  closed: T 或 nil"
  "list"
  "(at-edit:pline-data (car (entsel)))"
  (setq g (entget ent))
  (cond
    ((= "LINE" (entity:getdxf ent 0))
     (list (list (at-edit:2d (entity:getdxf ent 10))
                 (at-edit:2d (entity:getdxf ent 11)))
           (list 0) nil nil (entity:getdxf ent 43) nil
           (entity:getdxf ent 8) (entity:getdxf ent 62)))
    ((= "LWPOLYLINE" (entity:getdxf ent 0))
     (setq pts (mapcar 'at-edit:2d
                (mapcar 'cdr (vl-remove-if-not
                   '(lambda (x) (= (car x) 10)) g)))
           bs (vl-remove-if-not '(lambda (x) (= (car x) 42)) g)
           w1s (mapcar 'cdr (vl-remove-if-not
                   '(lambda (x) (= (car x) 40)) g))
           w2s (mapcar 'cdr (vl-remove-if-not
                   '(lambda (x) (= (car x) 41)) g))
           closed (= 1 (logand 1 (if (entity:getdxf ent 70) (entity:getdxf ent 70) 0))))
     (list pts
           (if bs (mapcar 'cdr bs) (mapcar '(lambda (x) 0.0) pts))
           (if w1s w1s nil) (if w2s w2s nil)
           (entity:getdxf ent 43) closed
           (entity:getdxf ent 8) (entity:getdxf ent 62)))
    ((= "POLYLINE" (entity:getdxf ent 0))
     (setq pts '() bs '() w1s '() w2s '())
     (setq i (entnext ent))
     (while (and i (= "VERTEX" (entity:getdxf i 0)))
       (setq pts (append pts (list (at-edit:2d (entity:getdxf i 10))))
             bs (append bs (list (if (entity:getdxf i 42) (entity:getdxf i 42) 0.0)))
             w1s (append w1s
                   (if (entity:getdxf i 40)
                     (list (entity:getdxf i 40)) nil))
             w2s (append w2s
                   (if (entity:getdxf i 41)
                     (list (entity:getdxf i 41)) nil)))
       (setq i (entnext i)))
     (setq closed (and i (= 1 (logand 1 (if (entity:getdxf i 70) (entity:getdxf i 70) 0)))))
     (if closed
       (progn
         (setq pts (reverse (cdr (reverse pts)))
               bs (reverse (cdr (reverse bs))))
         (if w1s
           (setq w1s (reverse (cdr (reverse w1s)))
                 w2s (reverse (cdr (reverse w2s))))))
       (setq w1s nil w2s nil))
     (list pts (if bs bs nil) (if w1s w1s nil) (if w2s w2s nil)
           (entity:getdxf ent 43) closed
           (entity:getdxf ent 8) (entity:getdxf ent 62)))
    (t nil)))

(defun at-edit:curve-segs (ent / ty pts bulges o r a1 a2 sweep
                             len n i p1 p2 segs data obj sp ep)
  "曲线图元转 2D 段列表，段格式 (pt1 pt2 bulge)
  支持 LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE/SPLINE/ELLIPSE
  CIRCLE 拆成 4 段弧，SPLINE/ELLIPSE 按采样折线处理"
  "段列表或 nil"
  "(at-edit:curve-segs (car (entsel)))"
  (setq ty (at-edit:ent-type ent))
  (cond
    ((null ty) nil)
    ((= ty "LINE")
     (list (list (at-edit:2d (entity:getdxf ent 10))
                 (at-edit:2d (entity:getdxf ent 11)) 0)))
    ((member ty '("LWPOLYLINE" "POLYLINE"))
     (setq data (at-edit:pline-data ent))
     (setq pts (car data)
           bulges (cadr data))
     (if (nth 5 data)
       (setq pts (append pts (list (car pts)))))
     (setq segs '()
           i 0)
     (repeat (1- (length pts))
       (setq p1 (nth i pts)
             p2 (nth (1+ i) pts))
       (if (not (equal p1 p2))
         (setq segs (append segs (list (list p1 p2 (nth i bulges))))))
       (setq i (1+ i)))
     segs)
    ((= ty "ARC")
     (setq o (entity:getdxf ent 10)
           r (entity:getdxf ent 40)
           a1 (entity:getdxf ent 50)
           a2 (entity:getdxf ent 51))
     (setq sweep (at-edit:ang-diff a1 a2))
     (list (list (list (+ (car o) (* r (cos a1)))
                       (+ (cadr o) (* r (sin a1))))
                 (list (+ (car o) (* r (cos a2)))
                       (+ (cadr o) (* r (sin a2))))
                 (at-edit:tan (/ sweep 4.0)))))
    ((= ty "CIRCLE")
     (setq o (entity:getdxf ent 10)
           r (entity:getdxf ent 40))
     (at-edit:circle-segs (at-edit:2d o) r))
    ((member ty '("SPLINE" "ELLIPSE"))
     (setq obj (vlax-ename->vla-object ent)
           n 64)
     (setq len (vl-catch-all-apply 'curve:length (list ent)))
     (if (numberp len)
       (setq n (max 64 (fix (/ len 25.0)))))
     (setq sp (vlax-curve-getstartparam obj)
           ep (vlax-curve-getendparam obj)
           segs '()
           i 0)
     (repeat (1- n)
       (setq p1 (vlax-curve-getpointatparam
                  obj (+ sp (* (- ep sp) (/ (float i) (float (1- n))))))
             p2 (vlax-curve-getpointatparam
                 obj (+ sp (* (- ep sp)
                              (/ (float (1+ i)) (float (1- n)))))))
       (if (and p1 p2)
         (setq segs (append segs (list (list (at-edit:2d p1)
                                              (at-edit:2d p2) 0)))))
       (setq i (1+ i)))
     segs)
    (t nil)))

(defun at-edit:seg-of-pt (segs pt tol / i p1 p2 b tt o r sweep)
  "找包含 pt 的段（在 tol 容差内），返回 (list 段号 吸附点)，无则 nil"
  "list 或 nil"
  "(at-edit:seg-of-pt segs '(1 0) 0.01)"
  (setq i 0)
  (while (< i (length segs))
    (setq p1 (car (nth i segs))
          p2 (cadr (nth i segs))
          b (caddr (nth i segs)))
    (cond
      ((zerop b)
       (setq tt (at-edit:pt-on-seg pt (list p1 p2)))
       (if (and tt (> (at-edit:2d-dist tt p1) 1e-9)
                  (< (at-edit:2d-dist tt p1)
                     (- (at-edit:2d-dist p1 p2) 1e-9))
                  (< (at-edit:2d-dist pt tt) tol))
         (list i tt)))
      (t
       (setq o (curve:bulge2o p1 p2 b)
             r (at-edit:2d-dist (at-edit:2d o) p1))
       (if (< (abs (- (at-edit:2d-dist (at-edit:2d pt)
                                  (at-edit:2d o)) r)) tol)
         (progn
           (setq sweep (if (> b 0)
                       (at-edit:ang-diff (angle (at-edit:2d o) p1)
                                           (angle (at-edit:2d o) p2))
                       (- (at-edit:ang-diff (angle (at-edit:2d o) p2)
                                               (angle (at-edit:2d o) p1)))))
           (setq tt (if (> b 0)
                     (at-edit:ang-diff (angle (at-edit:2d o) p1)
                                         (angle (at-edit:2d o) pt))
                    (- (at-edit:ang-diff (angle (at-edit:2d o) pt)
                                            (angle (at-edit:2d o) p1)))))
           (if (and (> tt 1e-7) (< tt (- (abs sweep) 1e-7)))
             (list i
                   (list (+ (car (at-edit:2d o))
                            (* r (cos (angle (at-edit:2d o) pt))))
                         (+ (cadr (at-edit:2d o))
                            (* r (sin (angle (at-edit:2d o) pt)))))))))))
    (setq i (1+ i))))

(defun at-edit:split-seg (seg pts / p1 p2 b d o r a1 sweep a q b2 res
                            tt order)
  "按点表打断一段曲线 (pt1 pt2 bulge)，点须在段上，返回子段列表"
  "段列表"
  "(at-edit:split-seg '((0 0)(2 0) 0) '((1 0)))"
  (setq p1 (car seg)
        p2 (cadr seg)
        b (caddr seg)
        res '())
  (if (zerop b)
    (progn
      (setq d (at-edit:v2sub p2 p1))
      (setq order (sort
                   pts
                   '(lambda (x y)
                      (< (at-edit:v2dot (at-edit:v2sub x p1) d)
                         (at-edit:v2dot (at-edit:v2sub y p1) d)))))
      (setq a p1)
      (foreach q (append order (list p2))
        (if (not (equal a q))
          (setq res (append res (list (list a q 0)))))
        (setq a q)))
    (progn
      (setq o (curve:bulge2o p1 p2 b)
            o (at-edit:2d o)
            r (at-edit:2d-dist o p1)
            a1 (angle o p1))
      (setq sweep (if (> b 0)
                  (at-edit:ang-diff a1 (angle o p2))
                  (- (at-edit:ang-diff (angle o p2) a1))))
      (setq tt (mapcar
                '(lambda (x)
                  (if (> b 0)
                    (at-edit:ang-diff a1 (angle o x))
                    (- (at-edit:ang-diff (angle o x) a1))))
               pts))
      (setq order (mapcar 'cdr
                  (sort (mapcar 'cons tt pts)
                        '(lambda (x y) (< (car x) (car y))))))
      (setq a p1)
      (foreach q (append order (list p2))
        (if (not (equal a q))
          (progn
            (setq b2 (at-edit:tan
                      (/ (at-edit:ang-diff (angle o a) (angle o q))
                         4.0)))
            (setq res (append res (list (list a q b2)))))
        (setq a q)))))
  res)

(defun at-edit:make-pl (pts bulges w1s w2s globalw closed layer color
                          z / dxf i)
  "由 2D 点表/凸度表创建 LWPOLYLINE
  参数: pts 2D 点(闭合不重复首点) bulges 凸度 w1s/w2s 每段头尾宽
        globalw 全局宽 closed 闭合 layer/color 图层颜色 z 标高"
  "图元名"
  "(at-edit:make-pl '((0 0)(2 0)(2 2)) '(0 0 0) nil nil 0 nil 0 7 0)"
  (setq dxf (list (cons 0 "LWPOLYLINE")
                  (cons 100 "AcDbEntity")
                  (cons 8 (if layer layer "0"))
                  (cons 62 (if color color 256))
                  (cons 100 "AcDbPolyline")
                  (cons 90 (length pts))
                  (cons 70 (if closed 1 0))
                  (if globalw (cons 43 globalw) nil)
                  (cons 38 (if z z 0.0))
                  (cons 39 0.0)))
  (setq i 0)
  (foreach pt pts
    (setq dxf (append
               dxf
               (list (cons 10 (list (car pt) (cadr pt) (if z z 0.0)))
                     (cons 42 (if (and bulges (> (length bulges) i))
                           (nth i bulges) 0.0))
                     (if (and w1s (> (length w1s) i))
                       (cons 40 (nth i w1s)) nil)
                     (if (and w2s (> (length w2s) i))
                       (cons 41 (nth i w2s)) nil))))
    (setq i (1+ i)))
  (entmakex (vl-remove nil dxf)))

(defun at-edit:pline-from-segs (segs / pts bulges closed)
  "由段列表 (pt1 pt2 bulge) 创建多段线，返回图元名"
  "图元名"
  "(at-edit:pline-from-segs (list (list '(0 0) '(2 0) 0)))"
  (setq pts (list (car (car segs)))
        bulges '())
  (foreach s segs
    (setq pts (append pts (list (cadr s)))
          bulges (append bulges (list (caddr s)))))
  (setq closed (and (>= (length segs) 3)
                    (equal (car (car segs))
                           (cadr (car (reverse segs))))))
  (at-edit:make-pl
   (reverse (cdr (reverse pts))) bulges
   nil nil nil closed nil nil 0.0))

(defun at-edit:break-ent (ent pts / data segs tol box diag seg-pts
                            i j s sp k subs chains cur w1s w2s globalw
                            layer color closed z0 res n ch pts2 bulges2
                            w1s2 w2s2 ch-closed)
  "按点表(2D，自动吸附)打断曲线图元，返回新图元列表
  支持 LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE/SPLINE/ELLIPSE
  调用方负责删除原图元"
  "图元列表"
  "(at-edit:break-ent (car (entsel)) '((1 0)))"
  (setq data (at-edit:pline-data ent))
  (setq segs (at-edit:curve-segs ent))
  (if (or (null segs) (null data))
    (progn
      (princ (strcat "at-edit:break-ent: 不支持的图元 "
                     (at-edit:ent-type ent)))
      nil)
    (progn
      (setq box (entity:getbox ent 0)
            diag (at-edit:2d-dist (car box) (cadr box)))
      (setq tol (max 1e-6 (* 0.001 (if diag diag 1.0))))
      ;; 每段上的打断点（带源点）
      (setq seg-pts (mapcar '(lambda (x) '()) segs))
      (foreach p pts
        (setq sp (at-edit:seg-of-pt segs (at-edit:2d p) tol))
        (if sp
          (setq j (car sp)
                seg-pts (at-edit:set-nth seg-pts j
                          (append (nth j seg-pts) (list (cadr sp)))))))
      ;; 打断成子段
      (setq subs '()
            i 0)
      (foreach s segs
        (setq k (nth i seg-pts))
        (if (null k)
          (setq subs (append subs (list (list s))))
          (setq subs (append subs
                        (list (at-edit:split-seg s k)))))
        (setq i (1+ i)))
      ;; 子段按原段顺序串成链
      (setq chains '()
            cur '()
            i 0)
      (foreach g subs
        (setq cur (append cur g))
        (if (and (nth i seg-pts) (> (length g) 1))
          (progn
            (setq chains (cons cur chains))
            (setq cur '())))
        (setq i (1+ i)))
      (if cur (setq chains (cons cur chains)))
      (setq layer (nth 6 data)
            color (nth 7 data)
            globalw (nth 4 data)
            w1s (nth 2 data)
            w2s (nth 3 data)
            closed (nth 5 data)
            z0 (caddr (cdr (assoc 10 (entget ent)))))
      ;; 由链生成图元（宽度取各段尾宽近似继承）
      (setq res '())
      (foreach ch chains
        (if (> (length ch) 0)
          (progn
            (setq pts2 (list (car (car ch)))
                  bulges2 '()
                  w1s2 '()
                  w2s2 '()
                  i 0)
            (foreach s ch
              (setq pts2 (append pts2 (list (cadr s)))
                    bulges2 (append bulges2 (list (caddr s)))
                    w1s2 (append w1s2
                          (list (if w2s (nth i w2s)
                                (if globalw globalw 0.0))))
                    w2s2 (append w2s2
                          (list (if w2s (nth i w2s)
                                (if globalw globalw 0.0)))))
              (setq i (1+ i)))
            (setq ch-closed (and closed
                                 (= (length chains) 1)
                                 (equal (car (car (car ch)))
                                        (cadr (car (reverse ch))))))
            (setq res (append res
                      (list (at-edit:make-pl
                             (reverse (cdr (reverse pts2)))
                             bulges2
                             (if w1s w1s2 nil)
                             (if w1s w2s2 nil)
                             (if (null w1s) globalw nil)
                             ch-closed layer color z0)))))))
      (reverse res))))

(defun at-edit:ent-bbox-diag (ent / box)
  "图元包围盒对角线长度（2D），失败返回 1"
  "number"
  "(at-edit:ent-bbox-diag (car (entsel)))"
  (setq box (entity:getbox ent 0))
  (if box (at-edit:2d-dist (car box) (cadr box)) 1.0))

(defun at-edit:ss-ents (msg / ss)
  "交互选择图元，返回图元列表，空选返回 nil"
  "图元列表或 nil"
  "(at-edit:ss-ents \"请选择图元\")"
  (prompt msg)
  (setq ss (ssget))
  (if ss (pickset:to-list ss) nil))

;; ------------------------------------------------------------
;; 块内容处理（excopy / explode-block / boundary 共用）
;; *me-xf* 变换表: (list 基点 (x y z) cos sin sx sy)
;; ------------------------------------------------------------

(defun at-edit:xf-pt (pt / x y z)
  "按当前 *me-xf* 变换（基点+旋转+比例）变换点"
  "三维点"
  "(at-edit:xf-pt '(1 0 0))"
  (setq x (car pt)
        y (cadr pt))
  (setq x (+ (caar *me-xf*)
             (* (cadr *me-xf*) x)
             (- (* (caddr *me-xf*) y)))
        y (+ (cadar *me-xf*)
             (* (caddr *me-xf*) x)
             (* (cadr *me-xf*) y))
        z (+ (caddar *me-xf*)
             (if (caddr pt) (caddr pt) 0.0)))
  (list x y z))

(defun at-edit:xf-radius (r)
  "按当前 *me-xf* 比例缩放半径（非等比用几何平均近似）"
  "number"
  "(at-edit:xf-radius 50)"
  (* (if r r 0.0)
     (sqrt (abs (if (* (nth 3 *me-xf*) (nth 4 *me-xf*)) (* (nth 3 *me-xf*) (nth 4 *me-xf*)) 0.0)))))

(defun at-edit:xf-rot ()
  "当前 *me-xf* 的旋转角（弧度）"
  "number"
  "(at-edit:xf-rot)"
  (atan (caddr *me-xf*) (cadr *me-xf*)))

(defun at-edit:xf-compose (xf1 xf2 / x)
  "复合变换 xf1∘xf2（先 xf2 后 xf1）"
  "变换表"
  "(at-edit:xf-compose xfy xfz)"
  (setq x (list
            (caar xf1)
            (- (* (cadr xf1) (cadr xf2))
               (* (caddr xf1) (caddr xf2)))
            (+ (* (caddr xf1) (cadr xf2))
               (* (cadr xf1) (caddr xf2)))
            (* (nth 3 xf1) (nth 3 xf2))
            (* (nth 4 xf1) (nth 4 xf2))))
  x)

(defun at-edit:block-xf (ent)
  "图块引用 dxf 表的插入变换表（基点+旋转+比例）"
  "变换表"
  "(at-edit:block-xf (entget ins))"
  (list (cdr (assoc 2 ent))
        (cos (if (cdr (assoc 50 ent)) (cdr (assoc 50 ent)) 0.0))
        (sin (if (cdr (assoc 50 ent)) (cdr (assoc 50 ent)) 0.0))
        (if (cdr (assoc 41 ent)) (cdr (assoc 41 ent)) 1.0)
        (if (cdr (assoc 42 ent)) (cdr (assoc 42 ent)) 1.0)))

(defun at-edit:block2ents (ins / d name ang sx sy n r c rr res v
                             blk ent)
  "把图块引用内的实体（含嵌套，递归；含行/列阵列）复制为
  模型空间独立图元，返回新图元列表"
  "图元列表"
  "(at-edit:block2ents (car (entsel)))"
  (setq d (entget ins)
        name (entity:getdxf ins 6)
        ang (if (entity:getdxf ins 50) (entity:getdxf ins 50) 0.0)
        sx (if (entity:getdxf ins 41) (entity:getdxf ins 41) 1.0)
        sy (if (entity:getdxf ins 42) (entity:getdxf ins 42) 1.0)
        n (if (entity:getdxf ins 68) (entity:getdxf ins 68) 1)
        r (if (entity:getdxf ins 69) (entity:getdxf ins 69) 1)
        res '())
  (setq blk (entnext (tblobjname "BLOCK" (strcat "*" name))))
  (setq c 0)
  (repeat n
    (setq rr 0)
    (repeat r
      (setq *me-xf* (list
                      (list (+ (car (entity:getdxf ins 2))
                               (* c (if (entity:getdxf ins 70) (entity:getdxf ins 70) 0.0)))
                            (+ (cadr (entity:getdxf ins 2))
                               (* rr (if (entity:getdxf ins 71) (entity:getdxf ins 71) 0.0)))
                            (caddr (entity:getdxf ins 2)))
                      (cos ang) (sin ang) sx sy))
      (if blk
        (progn
          (setq v (entnext blk))
          (while (and v (/= (car (entget v)) '(0 . "ENDSEQ")))
            (setq ent (at-edit:ent2ent v))
            (if ent (setq res (append res ent)))
            (setq v (entnext v)))))
      (setq rr (1+ rr)))
    (setq c (1+ c)))
  res)

(defun at-edit:ent2ent (ent / d ty res new pts bulges closed cen rad)
  "按当前 *me-xf* 变换把块内图元复制到模型空间
  返回新图元列表（不支持的图元类型返回 nil）"
  "图元列表"
  "(at-edit:ent2ent v)"
  (setq d (entget ent))
  (setq ty (cdr (assoc 0 d)))
  (cond
    ((= ty "LINE")
     (setq new (entity:make-line
                (at-edit:xf-pt (entity:getdxf ent 10))
                (at-edit:xf-pt (entity:getdxf ent 11))))
     (if new (setq res (list new))))
    ((= ty "LWPOLYLINE")
     (progn
       (setq pts (mapcar 'at-edit:xf-pt
                 (mapcar 'cdr (vl-remove-if-not
                    '(lambda (x) (= (car x) 10)) d)))
             bulges (mapcar 'cdr (vl-remove-if-not
                    '(lambda (x) (= (car x) 42)) d))
             closed (= 1 (logand 1 (if (entity:getdxf ent 70) (entity:getdxf ent 70) 0))))
       (if (and pts (> (length pts) 1))
         (setq new (at-edit:make-pl
                    (mapcar 'at-edit:2d pts)
                    (if bulges bulges nil)
                    nil nil (entity:getdxf ent 43) closed
                    (entity:getdxf ent 8) (entity:getdxf ent 62)
                    (caddr (car pts))))
         (setq new nil))
       (if new (setq res (list new)))))
    ((= ty "CIRCLE")
     (progn
       (setq new (entity:make-circle
                  (at-edit:xf-pt (entity:getdxf ent 10))
                  (at-edit:xf-radius (entity:getdxf ent 40))))
       (if new (setq res (list new)))))
    ((= ty "ARC")
     (progn
       (setq cen (at-edit:xf-pt (entity:getdxf ent 10))
             rad (at-edit:xf-radius (entity:getdxf ent 40)))
       (if (> rad 1e-9)
         (setq new (entity:make-arc
                    cen rad
                    (at-edit:xf-pt
                     (polar (entity:getdxf ent 10)
                            (entity:getdxf ent 50) (entity:getdxf ent 40)))
                    (at-edit:xf-pt
                     (polar (entity:getdxf ent 10)
                            (entity:getdxf ent 51) (entity:getdxf ent 40))))))
       (if new (setq res (list new)))))
    ((= ty "TEXT")
     (progn
       (setq new (entmakex
                  (append
                   (list (cons 0 "TEXT")
                         (cons 100 "AcDbEntity")
                         (cons 8 (entity:getdxf ent 8))
                         (cons 100 "AcDbText")
                         (cons 1 (entity:getdxf ent 1))
                         (cons 10
                               (at-edit:xf-pt (entity:getdxf ent 10)))
                         (cons 40
                               (* (abs (nth 4 *me-xf*))
                                  (if (entity:getdxf ent 40) (entity:getdxf ent 40) 0.0)))
                         (cons 50
                               (+ (if (entity:getdxf ent 50) (entity:getdxf ent 50) 0.0)
                                  (at-edit:xf-rot))))
                   (if (entity:getdxf ent 41)
                     (list (cons 41 (entity:getdxf ent 41))) nil)
                   (if (entity:getdxf ent 72)
                     (list (cons 72 (entity:getdxf ent 72))
                           (cons 73 (if (entity:getdxf ent 73) (entity:getdxf ent 73) 0)))
                     nil)
                   (if (entity:getdxf ent 74)
                     (list (cons 74 (entity:getdxf ent 74))
                           (cons 75 (if (entity:getdxf ent 75) (entity:getdxf ent 75) 0)))
                     nil))))
       (if new (setq res (list new)))))
    ((= ty "MTEXT")
     (progn
       (setq new (entmakex
                  (list (cons 0 "MTEXT")
                        (cons 100 "AcDbEntity")
                        (cons 8 (entity:getdxf ent 8))
                        (cons 100 "AcDbMText")
                        (cons 10 (at-edit:xf-pt (entity:getdxf ent 10)))
                        (cons 40 (* (abs (nth 4 *me-xf*))
                                    (if (entity:getdxf ent 40) (entity:getdxf ent 40) 0.0)))
                        (cons 50 (+ (if (entity:getdxf ent 50) (entity:getdxf ent 50) 0.0)
                                    (at-edit:xf-rot)))
                        (cons 1 (entity:getdxf ent 1))
                        (cons 71 (if (entity:getdxf ent 71) (entity:getdxf ent 71) 1))
                        (cons 72 (if (entity:getdxf ent 72) (entity:getdxf ent 72) 1))
                        (cons 47 (if (entity:getdxf ent 47) (entity:getdxf ent 47) 1.0)))))
       (if new (setq res (list new)))))
    ((= ty "INSERT")
     (progn
       (setq new (entmakex
                  (list (cons 0 "INSERT")
                        (cons 100 "AcDbEntity")
                        (cons 8 (entity:getdxf ent 8))
                        (cons 100 "AcDbBlockReference")
                        (cons 2 (at-edit:xf-pt (entity:getdxf ent 2)))
                        (cons 68 (if (entity:getdxf ent 68) (entity:getdxf ent 68) 1))
                        (cons 69 (if (entity:getdxf ent 69) (entity:getdxf ent 69) 1))
                        (cons 41 (* (if (entity:getdxf ent 41) (entity:getdxf ent 41) 1.0)
                                    (nth 3 *me-xf*)))
                        (cons 42 (* (if (entity:getdxf ent 42) (entity:getdxf ent 42) 1.0)
                                    (nth 4 *me-xf*)))
                        (cons 50 (+ (if (entity:getdxf ent 50) (entity:getdxf ent 50) 0.0)
                                    (at-edit:xf-rot))))))
       (if new (setq res (list new)))))
    ((= ty "POINT")
     (progn
       (setq new (entmakex
                  (list (cons 0 "POINT")
                        (cons 100 "AcDbEntity")
                        (cons 8 (entity:getdxf ent 8))
                        (cons 100 "AcDbPoint")
                        (cons 10 (at-edit:xf-pt (entity:getdxf ent 10)))
                        (cons 40 (if (entity:getdxf ent 40) (entity:getdxf ent 40) 0.0)))))
       (if new (setq res (list new)))))
    ((= ty "SOLID")
     (progn
       (setq pts (mapcar 'at-edit:xf-pt
                 (mapcar 'cdr (vl-remove-if-not
                    '(lambda (x) (= (car x) 10)) d))))
       (if (>= (length pts) 3)
         (setq new (entmakex
                    (append
                     (list (cons 0 "SOLID")
                           (cons 100 "AcDbEntity")
                           (cons 8 (entity:getdxf ent 8))
                           (cons 100 "AcDbPolyline"))
                     (mapcar '(lambda (p) (cons 10 p)) pts)))))
       (if new (setq res (list new)))))
    ((= ty "WIPEOUT")
     (progn
       (setq pts (mapcar 'at-edit:xf-pt
                 (mapcar 'cdr (vl-remove-if-not
                    '(lambda (x) (= (car x) 10)) d))))
       (if (= (length pts) 4)
         (setq new (entmakex
                    (append
                     (list (cons 0 "WIPEOUT")
                           (cons 100 "AcDbEntity")
                           (cons 8 (entity:getdxf ent 8))
                           (cons 100 "AcDbWipeout"))
                     (mapcar '(lambda (p) (cons 10 p)) pts)
                     (list (cons 70 0) (cons 71 0))))))
       (if new (setq res (list new)))))
    (t nil))
  res)

(defun at-edit:ent-segs (ent / d segs blk v)
  "图元的 2D 段列表（块引用递归取内容，含变换），用于轮廓/布尔
  支持 LINE/PLINE/ARC/CIRCLE/SPLINE/ELLIPSE/INSERT/HATCH"
  "段列表或 nil"
  "(at-edit:ent-segs (car (entsel)))"
  (cond
    ((null (at-edit:ent-type ent)) nil)
    ((= "INSERT" (at-edit:ent-type ent))
     (progn
       (setq *me-xf* (at-edit:block-xf (entget ent)))
       (setq blk (entnext (tblobjname "BLOCK"
                     (strcat "*" (entity:getdxf ent 6)))))
       (setq segs '())
       (if blk
         (progn
           (setq v (entnext blk))
           (while (and v (/= (car (entget v)) '(0 . "ENDSEQ")))
             (setq segs (append segs (at-edit:ent-segs-t (entget v))))
             (setq v (entnext v))))))
       segs)
    (t (at-edit:ent-segs-t (entget ent)))))

(defun at-edit:ent-segs-t (d / ty tmp)
  "图元 dxf 表按当前 *me-xf* 变换转 2D 段列表"
  "段列表或 nil"
  "(at-edit:ent-segs-t (entget ent))"
  (setq ty (cdr (assoc 0 d)))
  (cond
    ((member ty '("LINE" "LWPOLYLINE" "ARC" "CIRCLE" "SPLINE"
                    "ELLIPSE"))
     (progn
       (setq tmp (at-edit:make-pl-from-dxf d))
       (if tmp (at-edit:curve-segs tmp))))
    ((= ty "INSERT")
     (at-edit:insert-segs d))
    ((= ty "HATCH")
     (at-edit:hatch-segs d))
    (t nil)))

(defun at-edit:make-pl-from-dxf (d / new)
  "临时创建图元（用于读取采样），调用方必须删除"
  "图元名或 nil"
  "(at-edit:make-pl-from-dxf d)"
  (setq new (vl-catch-all-apply 'entmakex (list d)))
  (if (and (eq 'ename (type new)) (not (vl-catch-all-error-p new)))
    new nil))

(defun at-edit:insert-segs (d / sub res v name old-xf)
  "嵌套图块引用 dxf 的 2D 段列表（递归）"
  "段列表或 nil"
  "(at-edit:insert-segs d)"
  (setq name (cdr (assoc 2 d))
        sub (at-edit:xf-compose *me-xf* (at-edit:block-xf d)))
  (setq old-xf *me-xf*
        *me-xf* sub)
  (setq v (entnext (tblobjname "BLOCK" (strcat "*" name)))
        res '())
  (if v
    (progn
      (setq v (entnext v))
      (while (and v (/= (car (entget v)) '(0 . "ENDSEQ")))
        (setq res (append res (at-edit:ent-segs-t (entget v))))
        (setq v (entnext v)))))
  (setq *me-xf* old-xf)
  res)

(defun at-edit:pts2segs (pts bulges / segs i n p1 p2)
  "2D 点表+凸度表转段列表"
  "段列表"
  "(at-edit:pts2segs '((0 0)(1 0)(1 1)) '(0 0 0))"
  (setq segs '()
        n (length pts)
        i 0)
  (repeat (1- n)
    (setq p1 (nth i pts)
          p2 (nth (1+ i) pts))
    (if (not (equal p1 p2))
      (setq segs (append segs
                (list (list p1 p2 (if (nth i bulges) (nth i bulges) 0.0))))))
    (setq i (1+ i)))
  segs)

(defun at-edit:tan (x)
  "正切（AutoLISP 无内置 tan）"
  "数值"
  "(at-edit:tan 0.5)"
  (/ (sin x) (cos x)))

(defun at-edit:circle-segs (cen rad / segs i p1 p2 a1 a2)
  "圆心半径转 4 段 90 度弧（2D）"
  "段列表"
  "(at-edit:circle-segs '(0 0) 5)"
  (setq segs '()
        i 0)
  (repeat 4
    (setq a1 (* i pi 0.5)
          a2 (* (1+ i) pi 0.5)
          p1 (list (+ (car cen) (* rad (cos a1)))
                   (+ (cadr cen) (* rad (sin a1))))
          p2 (list (+ (car cen) (* rad (cos a2)))
                   (+ (cadr cen) (* rad (sin a2)))))
    (setq segs (append segs (list (list p1 p2 (at-edit:tan (/ pi 8.0))))))
    (setq i (1+ i)))
  segs)

(defun at-edit:hatch-segs (d / segs path pts bulges)
  "填充 dxf 表的外边界 2D 段列表（path 70=1 为外环）"
  "段列表"
  "(at-edit:hatch-segs (entget ent))"
  (setq segs '())
  (foreach path d
    (if (and (listp path)
             (consp (car path))
             (= (caar path) 70)
             (= (cdar path) 1))
      (progn
        (setq pts (mapcar 'at-edit:2d
                  (mapcar 'cdr (vl-remove-if-not
                     '(lambda (x) (= (car x) 10)) path)))
              bulges (mapcar 'cdr (vl-remove-if-not
                     '(lambda (x) (= (car x) 42)) path)))
        (if (>= (length pts) 2)
          (setq segs (append segs (at-edit:pts2segs
                                   (append pts (list (car pts)))
                                   (if bulges bulges nil)))))))
  segs))

(princ)
