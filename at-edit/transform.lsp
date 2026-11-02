;; ============================================================
;; at-edit 变换类命令 - transform.lsp
;; 全能缩放/移动/复制/镜像/删除增强、多窗口拉伸、批量中心旋转、
;; 单选动态偏移/多选多重偏移、全能修剪、交点断开/连续点打断、
;; 延长或修剪到两点连线、框选修补单线
;; ============================================================

(vl-load-com)

;; 注册菜单（at-edit.lsp 已注册同名菜单时跳过，避免重复项）
(if (not (member "迷你编辑" (mapcar 'car @::*menu*)))
  (@:add-menus
   '(("迷你编辑"
      ("全能缩放" "(at-edit:exscale)")
      ("移动增强" "(at-edit:exmove)")
      ("复制增强" "(at-edit:excopy)")
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
      ("框选修补单线" "(at-edit:fix-line)")))))

(at-edit:ensure-libs)

;; ------------------------------------------------------------
;; 内部工具：图元/点/段
;; ------------------------------------------------------------

(defun at-edit:e2o (ent / o)
  "图元名转 VLA 对象，无效图元返回 nil"
  "VLA 对象或 nil"
  "(at-edit:e2o (car (entsel)))"
  (if (eq (type ent) 'ename)
    (progn
      (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p o) nil o))
    nil))

(defun at-edit:3d (pt)
  "二维点补 z=0，三维点原样返回"
  "三维点"
  "(at-edit:3d '(1 2)) => (1 2 0.0)"
  (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0)))

(defun at-edit:valid-ent (ent / d)
  "图元是否仍有效（未被删除）"
  "T 或 nil"
  "(at-edit:valid-ent (entlast))"
  (if (eq (type ent) 'ename)
    (progn
      (setq d (vl-catch-all-apply 'entget (list ent)))
      (if (or (null d) (vl-catch-all-error-p d)) nil t))
    nil))

(defun at-edit:norm-ents (ents / res e)
  "把选择集/单个图元名/嵌套图元名列表统一成有效图元名列表"
  "图元名列表"
  "(at-edit:norm-ents (ssget))"
  (if ents
    (cond
      ((eq (type ents) 'pickset) (pickset:to-list ents))
      ((eq (type ents) 'ename) (if (at-edit:valid-ent ents) (list ents) nil))
      ((listp ents)
       (setq res '())
       (foreach e ents
         (if (at-edit:valid-ent e)
           (setq res (append res (list e)))))
       res)
      (t nil))
    nil))

(defun at-edit:expand-ents (ents / res e)
  "图元列表展开：块引用→块内图元（复制到模型空间），其余原样返回"
  "图元名列表"
  "(at-edit:expand-ents (list (car (entsel))))"
  (setq res '())
  (foreach e (at-edit:norm-ents ents)
    (if (eq "INSERT" (at-edit:ent-type e))
      (setq res (append res (at-edit:block2ents e)))
      (setq res (append res (list e)))))
  res)

(defun at-edit:del-ent (ent / o)
  "删除图元，成功返回 T"
  "T 或 nil"
  "(at-edit:del-ent (entlast))"
  (setq o (at-edit:e2o ent))
  (if o
    (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list o))))
    nil))

(defun at-edit:undo-start (/ r)
  "打开撤消记录标记（失败忽略）"
  "T 或 nil"
  "(at-edit:undo-start)"
  (setq r (vl-catch-all-apply
           'vla-startundomark
           (list (vla-get-activedocument (vlax-get-acad-object)))))
  (if (vl-catch-all-error-p r) nil t))

(defun at-edit:undo-end (/ r)
  "关闭撤消记录标记（失败忽略）"
  "T 或 nil"
  "(at-edit:undo-end)"
  (setq r (vl-catch-all-apply
           'vla-endundomark
           (list (vla-get-activedocument (vlax-get-acad-object)))))
  (if (vl-catch-all-error-p r) nil t))

(defun at-edit:chunk3 (lst / res)
  "把 (x y z x y z ...) 平铺表转二维点表"
  "二维点列表"
  "(at-edit:chunk3 '(0 0 0 1 1 0)) => ((0 0) (1 1))"
  (setq res '())
  (while (and lst (caddr lst))
    (setq res (append res (list (list (car lst) (cadr lst)))))
    (setq lst (cdddr lst)))
  res)

(defun at-edit:int-pts (ent1 ent2 / o1 o2 res)
  "两图元的交点列表（二维点），无法求交返回 nil"
  "二维点列表或 nil"
  "(at-edit:int-pts (car (entsel)) (car (entsel)))"
  (setq o1 (at-edit:e2o ent1)
        o2 (at-edit:e2o ent2))
  (if (and o1 o2 (not (equal ent1 ent2)))
    (progn
      (setq res (vl-catch-all-apply
                 (function
                  (lambda (a b)
                    (vlax-safearray->list
                     (vlax-variant-value (vla-IntersectWith a b 0)))))
                 (list o1 o2)))
      (if (vl-catch-all-error-p res) nil (at-edit:chunk3 res)))
    nil))

(defun at-edit:uniq-pts (pts tol / res p)
  "按容差去重点表"
  "二维点列表"
  "(at-edit:uniq-pts '((0 0) (0 0) (1 1)) 1e-6)"
  (setq res '())
  (foreach p pts
    (if (not (vl-some (function (lambda (q) (< (at-edit:2d-dist p q) tol))) res))
      (setq res (append res (list p)))))
  res)

(defun at-edit:pt-in-rect (pt p1 p2)
  "点是否在两点定义的矩形内（含边界，忽略 z）"
  "T 或 nil"
  "(at-edit:pt-in-rect '(1 1) '(0 0) '(2 2))"
  (and (>= (car pt) (min (car p1) (car p2)))
       (<= (car pt) (max (car p1) (car p2)))
       (>= (cadr pt) (min (cadr p1) (cadr p2)))
       (<= (cadr pt) (max (cadr p1) (cadr p2)))))

(defun at-edit:segs-cross (pt segs / c s p1 p2 x)
  "射线法统计点向右射线与段列表的交叉次数"
  "整数"
  "(at-edit:segs-cross '(1 1) (at-edit:ent-segs (car (entsel))))"
  (setq c 0)
  (foreach s segs
    (setq p1 (car s)
          p2 (cadr s))
    (if (and (or (> (cadr p2) (cadr pt)) (> (cadr p1) (cadr pt)))
             (or (<= (cadr p2) (cadr pt)) (<= (cadr p1) (cadr pt))))
      (progn
        (setq x (+ (car p1)
                   (/ (* (- (car p2) (car p1)) (- (cadr pt) (cadr p1)))
                      (- (cadr p2) (cadr p1)))))
        (if (> x (car pt)) (setq c (1+ c))))))
  c)

(defun at-edit:pt-inside (pt segs)
  "点是否在闭合边界（段列表）内部"
  "T 或 nil"
  "(at-edit:pt-inside '(1 1) (at-edit:ent-segs (car (entsel))))"
  (= 1 (logand 1 (at-edit:segs-cross pt segs))))

(defun at-edit:segs-mid (segs / n s)
  "段列表中部段的中间点（用于判断片段在内/外）"
  "二维点或 nil"
  "(at-edit:segs-mid (at-edit:ent-segs (car (entsel))))"
  (if (null segs)
    nil
    (progn
      (setq n (length segs)
            s (nth (/ n 2) segs))
      (list (/ (+ (car (car s)) (car (cadr s))) 2.0)
            (/ (+ (cadr (car s)) (cadr (cadr s))) 2.0)))))

(defun at-edit:seg-rev (s)
  "段反向（起终点互换，凸度取反）"
  "段"
  "(at-edit:seg-rev (list '(0 0) '(1 0) 0.5))"
  (list (cadr s) (car s) (- (if (caddr s) (caddr s) 0.0))))

(defun at-edit:list-set (lst idx val / i res x)
  "返回把表 lst 第 idx 个元素替换为 val 的新表（不受重复元素影响）"
  "list"
  "(at-edit:list-set '(a b c) 1 'x) => (a x c)"
  (setq i 0
        res '())
  (foreach x lst
    (setq res (append res (list (if (= i idx) val x))))
    (setq i (1+ i)))
  res)

(defun at-edit:last-item (lst / r)
  "取列表最后一个元素（不依赖 last 的返回形式）"
  "列表元素或 nil"
  "(at-edit:last-item '(1 2 3)) => 3"
  (setq r (reverse lst))
  (car r))

(defun at-edit:ents-segs (ent / ty)
  "图元的二维段列表（INSERT/HATCH 走 ent-segs，其余走 curve-segs）"
  "段列表或 nil"
  "(at-edit:ents-segs (car (entsel)))"
  (setq ty (at-edit:ent-type ent))
  (if (member ty '("INSERT" "HATCH"))
    (at-edit:ent-segs ent)
    (at-edit:curve-segs ent)))

(defun at-edit:cut-ent (ent pts / segs box diag tol seg-pts sp j p i s k
                        chains cur res layer color z closed pts2 bgs cl new
                        subs ns si)
  "按点表把图元打断成多段（自包含实现，不删除原图元），无法打断返回 nil
   pts: 二维点表（自动吸附到图元）"
  "新图元列表或 nil"
  "(at-edit:cut-ent (car (entsel)) '((5 0)))"
  (setq segs (at-edit:ent2segs ent))
  (if (null segs)
    nil
    (progn
      (setq box (entity:getbox ent 0))
      (setq diag (if box (at-edit:2d-dist (car box) (cadr box)) 1.0))
      (setq tol (max 1e-6 (* 0.001 (if diag diag 1.0))))
      (setq seg-pts (mapcar (function (lambda (x) '())) segs))
      (foreach p pts
        (setq sp (at-edit:seg-pos segs (at-edit:2d p) tol))
        (if sp
          (setq j (car sp)
                seg-pts (at-edit:list-set
                         seg-pts j
                         (append (nth j seg-pts) (list (cadr sp)))))))
      (if (vl-every (function (lambda (x) (null x))) seg-pts)
        nil
        (progn
          (setq chains '()
                cur '()
                i 0)
          (foreach s segs
            (setq k (nth i seg-pts))
            (if k
              (progn
                (if (vl-some
                     (function (lambda (q) (< (at-edit:2d-dist q (car s)) tol)))
                     k)
                  (if cur
                    (setq chains (append chains (list cur))
                          cur '())))
                (setq subs (at-edit:split-seg2 s k)
                      ns (length subs)
                      si 0)
                (foreach sub subs
                  (setq cur (append cur (list sub))
                        si (1+ si))
                  (if (and (< si ns) cur)
                    (setq chains (append chains (list cur))
                          cur '())))
                (if (vl-some
                     (function (lambda (q) (< (at-edit:2d-dist q (cadr s)) tol)))
                     k)
                  (if cur
                    (setq chains (append chains (list cur))
                          cur '()))))
              (setq cur (append cur (list s))))
            (setq i (1+ i)))
          (if cur (setq chains (append chains (list cur))))
          (setq layer (cdr (assoc 8 (entget ent)))
                color (cdr (assoc 62 (entget ent)))
                z (if (cdr (assoc 38 (entget ent)))
                    (cdr (assoc 38 (entget ent)))
                    0.0)
                closed (= 1 (logand 1 (if (cdr (assoc 70 (entget ent)))
                                        (cdr (assoc 70 (entget ent)))
                                        0)))
                res '())
          (foreach cur chains
            (if cur
              (progn
                (setq pts2 (cons (car (car cur)) (mapcar 'cadr cur))
                      bgs (mapcar 'caddr cur)
                      cl (and closed
                              (> (length cur) 2)
                              (< (at-edit:2d-dist
                                  (car (car cur))
                                  (cadr (at-edit:last-item cur))) tol)))
                (if cl (setq pts2 (reverse (cdr (reverse pts2)))))
                (setq new (at-edit:make-pl-z pts2 bgs cl layer color z))
                (if new (setq res (append res (list new)))))))
          res)))))

(defun at-edit:own-segs (ent / ty data pts bulges o r a1 a2 sweep)
  "自包含曲线取段：LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE，其余回退 curve-segs
   不依赖 pt-on-seg/seg-of-pt 等可能绑定保留符号 t 的旧辅助函数"
  "段列表或 nil"
  "(at-edit:own-segs (car (entsel)))"
  (setq ty (at-edit:ent-type ent))
  (cond
    ((null ty) nil)
    ((= ty "LINE")
     (list (list (at-edit:2d (entity:getdxf ent 10))
                 (at-edit:2d (entity:getdxf ent 11)) 0.0)))
    ((member ty '("LWPOLYLINE" "POLYLINE"))
     (setq data (at-edit:pline-data ent))
     (if data
       (progn
         (setq pts (car data)
               bulges (cadr data))
         (if (nth 5 data) (setq pts (append pts (list (car pts)))))
         (at-edit:pts2segs pts bulges))
       nil))
    ((= ty "ARC")
     (setq o (at-edit:2d (entity:getdxf ent 10))
           r (entity:getdxf ent 40)
           a1 (entity:getdxf ent 50)
           a2 (entity:getdxf ent 51)
           sweep (at-edit:ang-diff a1 a2))
     (list (list (list (+ (car o) (* r (cos a1)))
                       (+ (cadr o) (* r (sin a1))))
                 (list (+ (car o) (* r (cos a2)))
                       (+ (cadr o) (* r (sin a2))))
                 (at-edit:tan (/ sweep 4.0)))))
    ((= ty "CIRCLE")
     (at-edit:circle-segs (at-edit:2d (entity:getdxf ent 10))
                          (entity:getdxf ent 40)))
    (t (at-edit:curve-segs ent))))

(defun at-edit:ent2segs (ent / segs)
  "取图元二维段列表：优先用包内 helper，失败时用自包含实现"
  "段列表或 nil"
  "(at-edit:ent2segs (car (entsel)))"
  (setq segs (vl-catch-all-apply 'at-edit:ents-segs (list ent)))
  (if (or (null segs) (vl-catch-all-error-p segs))
    (setq segs (at-edit:own-segs ent)))
  segs)

(defun at-edit:seg-pos (segs pt tol / i p1 p2 b q o r ap a1 a2 sweep tt res)
  "在段表中找包含 pt 的段，返回 (段号 吸附点)，无则 nil
   不绑定保留符号 t"
  "list 或 nil"
  "(at-edit:seg-pos segs '(1 0) 0.01)"
  (setq i 0
        res nil)
  (foreach s segs
    (if (null res)
      (progn
        (setq p1 (car s)
              p2 (cadr s)
              b (if (caddr s) (caddr s) 0.0))
        (if (equal b 0.0 1e-12)
          (progn
            (setq q (at-edit:pt-on-line pt p1 p2))
            (if (and q (< (at-edit:2d-dist q pt) tol))
              (setq res (list i q))))
          (progn
            (setq o (at-edit:2d (curve:bulge2o p1 p2 b))
                  r (at-edit:2d-dist o p1)
                  ap (angle o pt))
            (if (and (> r 1e-9)
                     (<= (abs (- (at-edit:2d-dist o pt) r)) tol))
              (progn
                (setq q (list (+ (car o) (* r (cos ap)))
                              (+ (cadr o) (* r (sin ap))))
                      a1 (angle o p1)
                      a2 (angle o p2)
                      sweep (if (> b 0)
                              (at-edit:ang-diff a1 a2)
                              (- (at-edit:ang-diff a2 a1)))
                      tt (if (> b 0)
                         (at-edit:ang-diff a1 ap)
                         (- (at-edit:ang-diff ap a1))))
                (if (and (>= tt -1e-6) (<= tt (+ sweep 1e-6)))
                  (setq res (list i q)))))))
        (setq i (1+ i)))))
  res)

(defun at-edit:pt-on-line (pt p1 p2 / d k)
  "点到线段的投影，投影落在线段内返回投影点（不含容差扩展）"
  "二维点或 nil"
  "(at-edit:pt-on-line '(1 1) '(0 0) '(2 0)) => (1 0)"
  (setq d (at-edit:v2sub p2 p1))
  (if (> (at-edit:v2dot d d) 1e-20)
    (progn
      (setq k (/ (at-edit:v2dot (at-edit:v2sub pt p1) d)
                 (at-edit:v2dot d d)))
      (if (and (>= k -1e-9) (<= k (+ 1.0 1e-9)))
        (at-edit:v2add p1 (at-edit:v2scale d k))
        nil))
    nil))

(defun at-edit:split-seg2 (seg pts / p1 p2 b d o r a1 a q b2 res tt order)
  "按点表打断一段曲线(seg=(p1 p2 bulge))，点须在段上，返回子段列表
   不绑定 t 等保留符号，可安全用于各版本 CAD"
  "段列表"
  "(at-edit:split-seg2 (list '(0 0) '(2 0) 0.0) '((1 0)))"
  (setq p1 (car seg)
        p2 (cadr seg)
        b (if (caddr seg) (caddr seg) 0.0)
        res '())
  (if (equal b 0.0 1e-12)
    (progn
      (setq d (at-edit:v2sub p2 p1)
            order (vl-sort
                   pts
                   (function
                    (lambda (x y)
                      (< (at-edit:v2dot (at-edit:v2sub x p1) d)
                         (at-edit:v2dot (at-edit:v2sub y p1) d)))))
            a p1)
      (foreach q (append order (list p2))
        (if (not (equal a q))
          (setq res (append res (list (list a q 0.0)))))
        (setq a q)))
    (progn
      (setq o (at-edit:2d (curve:bulge2o p1 p2 b))
            r (at-edit:2d-dist o p1)
            a1 (angle o p1)
            tt (mapcar
                (function
                 (lambda (x)
                   (if (> b 0)
                     (at-edit:ang-diff a1 (angle o x))
                     (- (at-edit:ang-diff (angle o x) a1)))))
                pts)
            order (mapcar
                   'cdr
                   (vl-sort (mapcar 'cons tt pts)
                            (function (lambda (x y) (< (car x) (car y))))))
            a p1)
      (foreach q (append order (list p2))
        (if (not (equal a q))
          (progn
            (setq b2 (at-edit:tan (/ (at-edit:ang-diff (angle o a) (angle o q)) 4.0)))
            (setq res (append res (list (list a q b2))))))
        (setq a q))))
  res)

(defun at-edit:break-or-cut (ent pts / pieces)
  "打断图元：先用本包自包含实现，失败再用 at-edit:break-ent"
  "新图元列表或 nil"
  "(at-edit:break-or-cut (car (entsel)) '((5 0)))"
  (setq pieces (at-edit:cut-ent ent pts))
  (if (or (null pieces) (= 1 (length pieces)))
    (progn
      (setq pieces (vl-catch-all-apply 'at-edit:break-ent (list ent pts)))
      (if (vl-catch-all-error-p pieces) (setq pieces nil))))
  pieces)

(defun at-edit:line-dist (pt p1 p2 / d)
  "点到无限直线（p1 p2）的距离"
  "number"
  "(at-edit:line-dist '(1 1) '(0 0) '(2 0)) => 1.0"
  (setq d (at-edit:v2sub p1 p2))
  (/ (abs (at-edit:v2cross d (at-edit:v2sub pt p1)))
     (max (at-edit:v2len d) 1e-12)))

(defun at-edit:ent-len (ent / o l)
  "图元曲线长度，失败返回 0"
  "number"
  "(at-edit:ent-len (car (entsel)))"
  (setq o (at-edit:e2o ent))
  (if o
    (progn
      (setq l (vl-catch-all-apply
               (function
                (lambda (x) (vlax-curve-getdistatparam x (vlax-curve-getendparam x))))
               (list o)))
      (if (numberp l) l 0.0))
    0.0))

(defun at-edit:obj-dist (ent pt / o p)
  "图元到点的最近距离，失败返回 nil"
  "number 或 nil"
  "(at-edit:obj-dist (car (entsel)) '(0 0))"
  (setq o (at-edit:e2o ent))
  (if o
    (progn
      (setq p (vl-catch-all-apply 'vlax-curve-getclosestpointto
                                  (list o (at-edit:3d pt))))
      (if (and p (not (vl-catch-all-error-p p)))
        (at-edit:2d-dist (at-edit:2d p) (at-edit:2d pt))
        nil))
    nil))

;; ------------------------------------------------------------
;; 1. 全能缩放
;; ------------------------------------------------------------

(defun at-edit:text-h-pairs (ents / res e h)
  "收集 TEXT/MTEXT/ATTRIB 图元的 (图元 . 字高) 表"
  "(图元 . 字高) 列表"
  "(at-edit:text-h-pairs (pickset:to-list (ssget)))"
  (setq res '())
  (foreach e (at-edit:norm-ents ents)
    (if (member (at-edit:ent-type e) '("TEXT" "MTEXT" "ATTRIB"))
      (progn
        (setq h (entity:getdxf e 40))
        (if (and h (> h 0.0))
          (setq res (append res (list (cons e h))))))))
  res)

(defun at-edit:text-h-fix (pairs factor / d h)
  "缩放后修正字高：字高未随缩放变化时按因子修正"
  "nil"
  "(at-edit:text-h-fix pairs 2.0)"
  (foreach p pairs
    (if (at-edit:valid-ent (car p))
      (progn
        (setq d (entget (car p))
              h (cdr (assoc 40 d)))
        (if (and h (equal h (cdr p) 1e-6))
          (entmod (subst (cons 40 (* (cdr p) factor)) (assoc 40 d) d))))))
  nil)

(defun at-edit:ai-exscale (ents basept factor scale-text / lst ss e pairs)
  "AI 入口：按基点缩放图元，文字高度随比例同步缩放，返回缩放后图元列表，失败 nil
   ents: 图元（选择集/图元名/列表） basept: 基点 factor: 比例
   scale-text: 字高是否同步缩放，缺省 T"
  "图元名列表或 nil"
  "(at-edit:ai-exscale (ssget) '(0 0) 2.0 t)"
  (if (and (numberp factor) (/= factor 0.0) basept)
    (progn
      (setq lst (at-edit:norm-ents ents)
            scale-text (if (null scale-text) t scale-text)
            ss (ssadd))
      (foreach e lst (ssadd e ss))
      (if (> (sslength ss) 0)
        (progn
          (setq pairs (if scale-text (at-edit:text-h-pairs lst) nil))
          (at-edit:undo-start)
          (command "_.SCALE" ss "" (at-edit:3d basept) factor)
          (if scale-text (at-edit:text-h-fix pairs factor))
          (at-edit:undo-end)
          lst)
        nil))
    nil))

(defun at-edit:exscale (/ ents bp f)
  "交互入口：全能缩放，文字高度同步缩放"
  "缩放后图元列表或 nil"
  "(at-edit:exscale)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要缩放的图元: "))
    (if (setq bp (getpoint "\n缩放基点: "))
      (progn
        (setq f (getreal "\n缩放比例 <2.0>: "))
        (at-edit:ai-exscale ents bp (if f f 2.0) t)))))

;; ------------------------------------------------------------
;; 2. 移动增强
;; ------------------------------------------------------------

(defun at-edit:ai-exmove (ents p1 p2 / lst b1 b2 o)
  "AI 入口：移动图元（块引用自动展开为块内图元），返回移动后图元列表，失败 nil
   ents: 图元 p1: 基点 p2: 目标点"
  "图元名列表或 nil"
  "(at-edit:ai-exmove (list (car (entsel))) '(0 0) '(10 0))"
  (setq lst (at-edit:expand-ents ents))
  (if (and lst p1 p2)
    (progn
      (setq b1 (vlax-3d-point (at-edit:3d p1))
            b2 (vlax-3d-point (at-edit:3d p2)))
      (at-edit:undo-start)
      (foreach e lst
        (setq o (at-edit:e2o e))
        (if o (vl-catch-all-apply 'vla-move (list o b1 b2))))
      (at-edit:undo-end)
      lst)
    nil))

(defun at-edit:exmove (/ ents p1 p2)
  "交互入口：移动增强（可选中图块整体展开后移动）"
  "移动后图元列表或 nil"
  "(at-edit:exmove)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要移动的图元: "))
    (if (setq p1 (getpoint "\n基点: "))
      (if (setq p2 (getpoint p1 "\n目标点: "))
        (at-edit:ai-exmove ents p1 p2)))))

;; ------------------------------------------------------------
;; 3. 复制增强
;; ------------------------------------------------------------

(defun at-edit:ai-excopy (ents p1 p2 n / lst srcs news res c b1 b2 o)
  "AI 入口：复制图元（块引用自动展开），按位移向量逐次递进复制，返回新图元列表
   ents: 图元 p1: 基点 p2: 目标点 n: 复制次数，缺省 1"
  "图元名列表或 nil"
  "(at-edit:ai-excopy (ssget) '(0 0) '(10 0) 3)"
  (setq lst (at-edit:expand-ents ents)
        n (if (and (numberp n) (> n 0)) (fix n) 1))
  (if (and lst p1 p2)
    (progn
      (setq b1 (vlax-3d-point (at-edit:3d p1))
            b2 (vlax-3d-point (at-edit:3d p2))
            srcs lst
            res '())
      (at-edit:undo-start)
      (repeat n
        (setq news '())
        (foreach e srcs
          (setq o (at-edit:e2o e))
          (if o
            (progn
              (setq c (vl-catch-all-apply 'vla-copy (list o)))
              (if (not (vl-catch-all-error-p c))
                (progn
                  (vl-catch-all-apply 'vla-move (list c b1 b2))
                  (setq news (append news
                                     (list (vlax-vla-object->ename c)))))))))
        (setq res (append res news)
              srcs news))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:excopy (/ ents p1 p2 n)
  "交互入口：复制增强"
  "新图元列表或 nil"
  "(at-edit:excopy)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要复制的图元: "))
    (if (setq p1 (getpoint "\n基点: "))
      (if (setq p2 (getpoint p1 "\n目标点: "))
        (progn
          (setq n (getint "\n复制次数 <1>: "))
          (at-edit:ai-excopy ents p1 p2 (if n n 1)))))))

;; ------------------------------------------------------------
;; 4. 镜像增强
;; ------------------------------------------------------------

(defun at-edit:ai-exmirror (ents mp1 mp2 copy / lst b1 b2 res o c)
  "AI 入口：镜像图元，返回镜像生成的新图元列表，失败 nil
   ents: 图元 mp1/mp2: 镜像轴两点 copy: 保留源对象，缺省 T；传 0 表示删除源对象"
  "图元名列表或 nil"
  "(at-edit:ai-exmirror (ssget) '(0 0) '(0 10) t)"
  (setq lst (at-edit:expand-ents ents))
  (if (and lst mp1 mp2)
    (progn
      (setq b1 (vlax-3d-point (at-edit:3d mp1))
            b2 (vlax-3d-point (at-edit:3d mp2))
            res '())
      (at-edit:undo-start)
      (foreach e lst
        (setq o (at-edit:e2o e))
        (if o
          (progn
            (setq c (vl-catch-all-apply 'vla-Mirror (list o b1 b2)))
            (if (not (vl-catch-all-error-p c))
              (progn
                (if (equal copy 0) (at-edit:del-ent e))
                (setq res (append res
                                  (list (vlax-vla-object->ename c)))))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:exmirror (/ ents mp1 mp2 kw keep)
  "交互入口：镜像增强，可选是否保留源对象"
  "新图元列表或 nil"
  "(at-edit:exmirror)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要镜像的图元: "))
    (if (setq mp1 (getpoint "\n镜像线第一点: "))
      (if (setq mp2 (getpoint mp1 "\n镜像线第二点: "))
        (progn
          (setq kw (getkword "\n保留源对象? [是/否] <是>: "))
          (setq keep (if (eq kw "否") 0 t))
          (at-edit:ai-exmirror ents mp1 mp2 keep))))))

;; ------------------------------------------------------------
;; 5. 删除增强
;; ------------------------------------------------------------

(defun at-edit:ai-exerase (ents / lst n)
  "AI 入口：删除图元（选择集/图元名/列表混合均可），返回删除数量，失败 nil
   ents: 图元；块引用整块删除"
  "整数或 nil"
  "(at-edit:ai-exerase (ssget))"
  (setq lst (at-edit:norm-ents ents)
        n 0)
  (if lst
    (progn
      (at-edit:undo-start)
      (foreach e lst
        (if (at-edit:del-ent e) (setq n (1+ n))))
      (at-edit:undo-end))
    nil)
  (if (> n 0) n nil))

(defun at-edit:exerase (/ ents n)
  "交互入口：删除增强"
  "删除数量或 nil"
  "(at-edit:exerase)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要删除的图元: "))
    (progn
      (setq n (at-edit:ai-exerase ents))
      (if n (princ (strcat "\n已删除 " (itoa n) " 个图元")))
      n)))

;; ------------------------------------------------------------
;; 6. 多窗口拉伸
;; ------------------------------------------------------------

(defun at-edit:stretch-ent (ent p1 p2 disp / d res x moved)
  "按 STRETCH 语义把图元在窗口内的顶点按 disp 位移，成功返回 T
   窗口内的顶点搬走，窗口外的顶点不动"
  "T 或 nil"
  "(at-edit:stretch-ent (entlast) '(0 0) '(5 5) '(0 1))"
  (setq d (entget ent)
        res '()
        moved nil)
  (foreach x d
    (if (and (member (car x) '(10 11))
             (listp (cdr x))
             (at-edit:pt-in-rect (cdr x) p1 p2))
      (progn
        (setq moved t)
        (setq res (cons (cons (car x) (at-edit:v2add (cdr x) disp)) res)))
      (setq res (cons x res))))
  (if moved
    (not (vl-catch-all-error-p
          (vl-catch-all-apply 'entmod (list (reverse res)))))
    nil))

(defun at-edit:ai-mstretch (ents windows disp / lst res sel w p1 p2 e)
  "AI 入口：多窗口拉伸，对每个窗口（含交叉）内的顶点施加位移，返回被拉伸图元列表
   ents: 图元，nil 时按窗口自动交叉选择 windows: 窗口对角点表 ((p1 p2) ...)
   disp: 位移向量"
  "图元名列表或 nil"
  "(at-edit:ai-mstretch (ssget) '(((0 0) (5 5))) '(0 1))"
  (setq lst (at-edit:norm-ents ents)
        res '())
  (if (and windows disp)
    (progn
      (at-edit:undo-start)
      (foreach w windows
        (setq p1 (car w)
              p2 (cadr w))
        (if (and p1 p2)
          (progn
            (setq sel (if lst lst (at-edit:norm-ents (ssget "_C" p1 p2))))
            (foreach e sel
              (if (at-edit:stretch-ent e p1 p2 disp)
                (if (not (member e res))
                  (setq res (append res (list e)))))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:mstretch (/ ents wins w1 w2 bp dp)
  "交互入口：多窗口拉伸，连续指定窗口后给定位移"
  "被拉伸图元列表或 nil"
  "(at-edit:mstretch)"
  (at-edit:ensure-libs)
  (setq ents (at-edit:norm-ents (ssget)))
  (setq wins '())
  (while (setq w1 (getpoint "\n窗口第一角点 <回车结束>: "))
    (if (setq w2 (getpoint w1 "\n窗口对角点: "))
      (setq wins (append wins (list (list w1 w2))))))
  (if wins
    (if (setq bp (getpoint "\n位移基点: "))
      (if (setq dp (getpoint bp "\n位移目标点: "))
        (at-edit:ai-mstretch ents wins
                             (at-edit:v2sub (at-edit:2d dp) (at-edit:2d bp)))))))

;; ------------------------------------------------------------
;; 7. 批量中心旋转
;; ------------------------------------------------------------

(defun at-edit:ai-rotate-center (ents ang / lst res e box cen o)
  "AI 入口：各图元绕自身包围盒中心旋转，返回图元列表，失败 nil
   ents: 图元 ang: 旋转角度（度，逆时针为正）"
  "图元名列表或 nil"
  "(at-edit:ai-rotate-center (ssget) 45)"
  (setq lst (at-edit:norm-ents ents))
  (if (and lst (numberp ang))
    (progn
      (at-edit:undo-start)
      (setq res '())
      (foreach e lst
        (setq box (entity:getbox e 0))
        (if (and box (car box) (cadr box))
          (progn
            (setq cen (list (/ (+ (car (car box)) (car (cadr box))) 2.0)
                            (/ (+ (cadr (car box)) (cadr (cadr box))) 2.0)
                            (if (caddr (car box)) (caddr (car box)) 0.0)))
            (setq o (at-edit:e2o e))
            (if (and o
                     (not (vl-catch-all-error-p
                           (vl-catch-all-apply
                            'vla-rotate
                            (list o (vlax-3d-point cen)
                                  (* pi (/ ang 180.0)))))))
              (setq res (append res (list e)))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:rotate-center (/ ents a)
  "交互入口：批量绕各自中心旋转"
  "图元列表或 nil"
  "(at-edit:rotate-center)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要旋转的图元: "))
    (progn
      (setq a (getreal "\n旋转角度(度) <90>: "))
      (at-edit:ai-rotate-center ents (if a a 90.0)))))

;; ------------------------------------------------------------
;; 8. 单选动态偏移
;; ------------------------------------------------------------

(defun at-edit:offset-1 (ent dist / o res lst)
  "对图元按带符号距离偏移，返回新图元名，失败 nil"
  "图元名或 nil"
  "(at-edit:offset-1 (car (entsel)) 1.0)"
  (setq o (at-edit:e2o ent))
  (if o
    (progn
      (setq res (vl-catch-all-apply 'vla-Offset (list o (float dist))))
      (if (or (vl-catch-all-error-p res) (null res))
        nil
        (progn
          (setq lst (vl-catch-all-apply
                     'vlax-safearray->list
                     (list (vlax-variant-value res))))
          (if (and lst (not (vl-catch-all-error-p lst)) (car lst))
            (vlax-vla-object->ename (car lst))
            nil))))
    nil))

(defun at-edit:ai-dy-offset (ent dirpt dist side / d0 d1 new)
  "AI 入口：单图元动态偏移（直线/圆弧/圆/多段线通用），返回新图元，失败 nil
   ent: 图元 dirpt: 方向点（决定偏移侧） dist: 距离
   side: 偏移侧 1/-1，缺省时按方向点自动判定"
  "图元名或 nil"
  "(at-edit:ai-dy-offset (car (entsel)) '(0 5) 1.0 nil)"
  (if (and ent (numberp dist) (> (abs dist) 0.0))
    (progn
      (setq d0 (if dirpt (at-edit:obj-dist ent dirpt) nil))
      (if (numberp side)
        (setq new (at-edit:offset-1 ent (* side (abs dist))))
        (progn
          (setq new (at-edit:offset-1 ent (abs dist)))
          (if (and new d0)
            (progn
              (setq d1 (at-edit:obj-dist new dirpt))
              (if (and d1 (>= d1 d0))
                (progn
                  (at-edit:del-ent new)
                  (setq new (at-edit:offset-1 ent (- (abs dist))))))))))
      new)
    nil))

(defun at-edit:dy-offset (/ ent p d)
  "交互入口：单选动态偏移"
  "新图元名或 nil"
  "(at-edit:dy-offset)"
  (at-edit:ensure-libs)
  (if (setq ent (car (entsel "\n选择要偏移的图元: ")))
    (if (setq p (getpoint "\n指定偏移方向点: "))
      (progn
        (setq d (getreal "\n偏移距离 <10>: "))
        (at-edit:ai-dy-offset ent p (if d d 10.0) nil)))))

;; ------------------------------------------------------------
;; 9. 多选多重偏移
;; ------------------------------------------------------------

(defun at-edit:ai-multi-offset (ents dists side / lst dlist s res e d n)
  "AI 入口：多图元多重偏移，返回新图元列表，失败 nil
   ents: 图元 dists: 距离列表（或单个距离） side: 偏移侧 1/-1，缺省 1"
  "图元名列表或 nil"
  "(at-edit:ai-multi-offset (ssget) '(1 2 3) -1)"
  (setq lst (at-edit:norm-ents ents)
        dlist (if (listp dists) dists (if (numberp dists) (list dists) nil))
        s (if (and (numberp side) (/= side 0.0)) side 1.0)
        res '())
  (if (and lst dlist)
    (progn
      (at-edit:undo-start)
      (foreach e lst
        (foreach d dlist
          (if (numberp d)
            (setq n (at-edit:offset-1 e (* s (abs d))))
            (setq n nil))
          (if n (setq res (append res (list n))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:parse-nums (str / lst)
  "把空格或逗号分隔的数字串转数字表，失败返回 nil"
  "number 列表或 nil"
  "(at-edit:parse-nums \"1 2 3\") => (1.0 2.0 3.0)"
  (if (and str (/= str ""))
    (progn
      (setq lst (vl-catch-all-apply
                 'read
                 (list (strcat "(" (vl-string-translate ", " "  " str) ")"))))
      (if (and (listp lst) lst (vl-every 'numberp lst))
        (mapcar 'float lst)
        nil))
    nil))

(defun at-edit:multi-offset (/ ents str dists kw side)
  "交互入口：多选多重偏移"
  "新图元列表或 nil"
  "(at-edit:multi-offset)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要偏移的图元: "))
    (progn
      (setq str (getstring t "\n偏移距离(空格分隔，如 1 2 3) <1>: ")
            dists (at-edit:parse-nums str))
      (if (null dists) (setq dists (list 1.0)))
      (setq kw (getkword "\n偏移侧 [正向/反向] <正向>: ")
            side (if (eq kw "反向") -1.0 1.0))
      (at-edit:ai-multi-offset ents dists side))))

;; ------------------------------------------------------------
;; 10. 全能修剪
;; ------------------------------------------------------------

(defun at-edit:ai-extrim (targets boundaries extend / tlst blst bsegs res
                          e b pts pieces mid pc inside)
  "AI 入口：以边界图元修剪目标图元，返回保留的目标片段列表，失败 nil
   targets: 目标图元 boundaries: 边界图元
   extend: T 保留边界内部（修剪外部），缺省修剪内部（保留外部）"
  "图元名列表或 nil"
  "(at-edit:ai-extrim (list (car (entsel))) (list (car (entsel))) nil)"
  (setq tlst (at-edit:norm-ents targets)
        blst (at-edit:norm-ents boundaries)
        bsegs '()
        res '())
  (foreach b blst
    (setq bsegs (append bsegs (at-edit:ent2segs b))))
  (if (and tlst bsegs)
    (progn
      (at-edit:undo-start)
      (foreach e tlst
        (setq pts '())
        (foreach b blst
          (setq pts (append pts (at-edit:int-pts e b))))
        (setq pts (at-edit:uniq-pts pts 1e-6))
        (setq pieces (if pts (at-edit:break-or-cut e pts) nil))
        (if (and pieces (> (length pieces) 1))
          (progn
            (at-edit:del-ent e)
            (foreach pc pieces
              (setq mid (at-edit:segs-mid (at-edit:ent2segs pc))
                    inside (if mid
                             (= 1 (logand 1 (at-edit:segs-cross mid bsegs)))
                             nil))
              (if (if extend inside (not inside))
                (setq res (append res (list pc)))
                (at-edit:del-ent pc))))
          (if (at-edit:valid-ent e)
            (setq res (append res (list e))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:extrim (/ bnd tg kw)
  "交互入口：全能修剪"
  "保留的目标片段列表或 nil"
  "(at-edit:extrim)"
  (at-edit:ensure-libs)
  (if (setq bnd (at-edit:ss-ents "\n选择边界图元: "))
    (if (setq tg (at-edit:ss-ents "\n选择要修剪的图元: "))
      (progn
        (setq kw (getkword "\n修剪方式 [修剪内部/修剪外部] <修剪内部>: "))
        (at-edit:ai-extrim tg bnd (eq kw "修剪外部"))))))

;; ------------------------------------------------------------
;; 11. 交点断开
;; ------------------------------------------------------------

(defun at-edit:make-pl-z (pts bulges closed layer color z / w)
  "以零宽度创建多段线（显式给出宽度组码，避免 make-pl 传 nil 组码）"
  "图元名或 nil"
  "(at-edit:make-pl-z '((0 0) (1 0)) '(0) nil \"0\" 256 0.0)"
  (setq w (mapcar (function (lambda (x) 0.0)) pts))
  (at-edit:make-pl pts bulges w w 0.0 closed layer color (if z z 0.0)))

(defun at-edit:ai-break-inter (ents / lst res e o pts pieces cuts)
  "AI 入口：图元两两求交并在交点处断开，返回新生成的图元列表，失败 nil
   ents: 图元列表（先全部求交，再统一打断，避免互相影响）"
  "图元名列表或 nil"
  "(at-edit:ai-break-inter (pickset:to-list (ssget)))"
  (setq lst (at-edit:norm-ents ents))
  (if (cdr lst)
    (progn
      (setq cuts '())
      (foreach e lst
        (setq pts '())
        (foreach o lst
          (if (not (equal o e))
            (setq pts (append pts (at-edit:int-pts e o)))))
        (setq cuts (append cuts (list (cons e (at-edit:uniq-pts pts 1e-6))))))
      (at-edit:undo-start)
      (setq res '())
      (foreach c cuts
        (setq e (car c)
              pieces (if (cdr c) (at-edit:break-or-cut e (cdr c)) nil))
        (if pieces
          (progn
            (at-edit:del-ent e)
            (setq res (append res pieces)))
          (if (at-edit:valid-ent e)
            (setq res (append res (list e))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:break-inter (/ ents)
  "交互入口：交点断开"
  "新生成的图元列表或 nil"
  "(at-edit:break-inter)"
  (at-edit:ensure-libs)
  (if (setq ents (at-edit:ss-ents "\n选择要互相断开的图元: "))
    (at-edit:ai-break-inter ents)))

;; ------------------------------------------------------------
;; 12. 连续点打断
;; ------------------------------------------------------------

(defun at-edit:ai-break-at (ent pts / plist pieces)
  "AI 入口：沿图元在指定点打断，返回新图元列表，失败 nil
   ent: 图元 pts: 断点表（二维点列表或单个点，自动吸附到图元）"
  "图元名列表或 nil"
  "(at-edit:ai-break-at (car (entsel)) '((5 0)))"
  (if (and ent pts)
    (progn
      (setq plist (if (numberp (car pts)) (list pts) pts)
            pieces (at-edit:break-or-cut ent plist))
      (if (and pieces (> (length pieces) 1))
        (progn
          (at-edit:undo-start)
          (at-edit:del-ent ent)
          (at-edit:undo-end)
          pieces)
        nil))
    nil))

(defun at-edit:break-at (/ ent pts p)
  "交互入口：连续点打断"
  "新图元列表或 nil"
  "(at-edit:break-at)"
  (at-edit:ensure-libs)
  (if (setq ent (car (entsel "\n选择要打断的图元: ")))
    (progn
      (setq pts '())
      (while (setq p (getpoint "\n断点位置 <回车结束>: "))
        (setq pts (append pts (list p))))
      (if pts (at-edit:ai-break-at ent pts)))))

;; ------------------------------------------------------------
;; 13. 延长或修剪到两点连线
;; ------------------------------------------------------------

(defun at-edit:ai-extend2point (p1 p2 targets / lst bseg e d pt1 pt2 ip d1 d2
                               dt pv pa pb new res)
  "AI 入口：把目标线的近端延长或剪短到 p1-p2 两点连线，返回处理过的图元列表
   p1/p2: 界线两点 targets: 目标图元（LINE/LWPOLYLINE）"
  "图元名列表或 nil"
  "(at-edit:ai-extend2point '(0 0) '(10 0) (list (car (entsel))))"
  (setq lst (at-edit:norm-ents targets)
        bseg (list (at-edit:2d p1) (at-edit:2d p2))
        res '())
  (if (and lst (not (equal (car bseg) (cadr bseg))))
    (progn
      (at-edit:undo-start)
      (foreach e lst
        (cond
          ((eq "LINE" (at-edit:ent-type e))
           (setq d (entget e)
                 pt1 (cdr (assoc 10 d))
                 pt2 (cdr (assoc 11 d))
                 ip (at-edit:seg-inter
                     (list (at-edit:2d pt1) (at-edit:2d pt2)) bseg))
           (if ip
             (progn
               (setq d1 (at-edit:line-dist (at-edit:2d pt1) (car bseg) (cadr bseg))
                     d2 (at-edit:line-dist (at-edit:2d pt2) (car bseg) (cadr bseg)))
               (if (<= d1 d2)
                 (entmod (subst (cons 10 (at-edit:3d ip)) (assoc 10 d) d))
                 (entmod (subst (cons 11 (at-edit:3d ip)) (assoc 11 d) d)))
               (setq res (append res (list e))))))
          ((eq "LWPOLYLINE" (at-edit:ent-type e))
           (setq dt (at-edit:pline-data e))
           (if (and dt (not (nth 5 dt)) (>= (length (car dt)) 2))
             (progn
               (setq pv (car dt)
                     pa (car pv)
                     pb (at-edit:last-item pv)
                     ip (at-edit:seg-inter
                         (list (at-edit:2d pa) (at-edit:2d pb)) bseg))
               (if ip
                 (progn
                   (if (<= (at-edit:line-dist (at-edit:2d pa)
                                              (car bseg) (cadr bseg))
                           (at-edit:line-dist (at-edit:2d pb)
                                              (car bseg) (cadr bseg)))
                     (setq pv (cons (at-edit:2d ip) (cdr pv)))
                     (setq pv (append (reverse (cdr (reverse pv)))
                                      (list (at-edit:2d ip)))))
                   (setq new (at-edit:make-pl-z
                              pv (nth 1 dt) nil (nth 6 dt) (nth 7 dt) 0.0))
                   (if new
                     (progn
                       (at-edit:del-ent e)
                       (setq res (append res (list new))))))))))
          (t nil)))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:extend2point (/ p1 p2 ents)
  "交互入口：延长或修剪到两点连线"
  "处理过的图元列表或 nil"
  "(at-edit:extend2point)"
  (at-edit:ensure-libs)
  (if (setq p1 (getpoint "\n界线第一点: "))
    (if (setq p2 (getpoint p1 "\n界线第二点: "))
      (if (setq ents (at-edit:ss-ents "\n选择要延长/修剪的线: "))
        (at-edit:ai-extend2point p1 p2 ents)))))

;; ------------------------------------------------------------
;; 14. 框选修补单线
;; ------------------------------------------------------------

(defun at-edit:chain-find (ch rest lay tol / sp ep s res)
  "在段项表中找与链端点相接的段，返回 (段 反向标志 接端)，无则 nil
   接端 'e 接链尾 's 接链首"
  "list 或 nil"
  "(at-edit:chain-find ch rest \"0\" 0.01)"
  (setq sp (car (car ch))
        ep (cadr (at-edit:last-item ch))
        res nil)
  (foreach s rest
    (if (and (null res) (equal lay (cadr s)))
      (cond
        ((<= (at-edit:2d-dist (car (car s)) ep) tol)
         (setq res (list (car s) nil 'e)))
        ((<= (at-edit:2d-dist (cadr (car s)) ep) tol)
         (setq res (list (car s) t 'e)))
        ((<= (at-edit:2d-dist (cadr (car s)) sp) tol)
         (setq res (list (car s) nil 's)))
        ((<= (at-edit:2d-dist (car (car s)) sp) tol)
         (setq res (list (car s) t 's))))))
  res)

(defun at-edit:chain-items (items tol / rest chains it ch lay col found s2)
  "把段项表 (段 图层 颜色) 按端点容差连成链，返回 ((段表 图层 颜色) ...)"
  "list"
  "(at-edit:chain-items items 0.01)"
  (setq rest items
        chains '())
  (while rest
    (setq it (car rest)
          rest (cdr rest)
          ch (list (car it))
          lay (cadr it)
          col (caddr it)
          found t)
    (while found
      (setq found (at-edit:chain-find ch rest lay tol))
      (if found
        (progn
          (setq s2 (if (cadr found)
                     (at-edit:seg-rev (car found))
                     (car found)))
          (if (eq (caddr found) 'e)
            (setq ch (append ch (list s2)))
            (setq ch (cons s2 ch)))
          (setq rest (vl-remove-if
                      (function
                       (lambda (x) (equal (car x) (car found))))
                      rest)))))
    (setq chains (append chains (list (list ch lay col)))))
  chains)

(defun at-edit:ai-fix-line (corner1 corner2 minlen tol / lst ss items res e segs
                            lay col s new ch pts bgs closed minl toln)
  "AI 入口：框选修补单线。删除框内短于 minlen 的线段，把端点相接的线连成多段线
   corner1/corner2: 框选对角点 minlen: 最小线段长度，缺省 0
   tol: 端点连接容差，缺省 0.01 返回新生成的图元列表"
  "图元名列表或 nil"
  "(at-edit:ai-fix-line '(0 0) '(100 100) 5.0 0.01)"
  (setq minl (if (numberp minlen) minlen 0.0)
        toln (if (and (numberp tol) (> tol 0.0)) tol 0.01)
        items '()
        res '())
  (setq ss (ssget "_C" (at-edit:3d corner1) (at-edit:3d corner2)
                  '((0 . "LINE,LWPOLYLINE"))))
  (setq lst (at-edit:norm-ents ss))
  (if lst
    (progn
      (foreach e lst
        (setq lay (entity:getdxf e 8)
              col (entity:getdxf e 62))
        (foreach s (at-edit:ent2segs e)
          (if (>= (at-edit:2d-dist (car s) (cadr s)) minl)
            (setq items (append items (list (list s lay col)))))))
      (at-edit:undo-start)
      (foreach e lst (at-edit:del-ent e))
      (foreach ch (at-edit:chain-items items toln)
        (setq segs (car ch)
              lay (cadr ch)
              col (caddr ch))
        (if segs
          (progn
            (setq pts (cons (car (car segs)) (mapcar 'cadr segs))
                  bgs (mapcar 'caddr segs)
                  closed nil)
            (if (and (> (length segs) 2)
                     (<= (at-edit:2d-dist (car (car segs))
                                          (cadr (at-edit:last-item segs))) toln))
              (setq pts (reverse (cdr (reverse pts)))
                    closed t))
            (setq new (at-edit:make-pl-z pts bgs closed lay col 0.0))
            (if new (setq res (append res (list new)))))))
      (at-edit:undo-end)
      res)
    nil))

(defun at-edit:fix-line (/ c1 c2 minl tol)
  "交互入口：框选修补单线"
  "新生成的图元列表或 nil"
  "(at-edit:fix-line)"
  (at-edit:ensure-libs)
  (if (setq c1 (getpoint "\n框选第一角点: "))
    (if (setq c2 (getpoint c1 "\n框选对角点: "))
      (progn
        (setq minl (getreal "\n删除短于该长度的线段 <5>: ")
              tol (getreal "\n端点连接容差 <0.01>: "))
        (at-edit:ai-fix-line c1 c2 (if minl minl 5.0) (if tol tol 0.01))))))

(princ)
