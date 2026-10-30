;; ============================================================
;; at-landscape/path.lsp
;; 封闭多段线搭桥 / 挖路 / 路线里程标注 / 界址点编号
;; ============================================================
(require '(entity:* curve:* pickset:*))

;; 注册菜单
(@:add-menus
 '("景观路径"
   ("封闭多段线搭桥" "(at-landscape:add-path)")
   ("封闭多段线挖路" "(at-landscape:remove-path)")
   ("路线里程标注" "(at-landscape:mileage)")
   ("界址点编号" "(at-landscape:jzd)")))

;; ------------------------------------------------------------
;; 公共辅助：点表偏移 / 有向面积 / 桩号格式
;; ------------------------------------------------------------
(defun at-landscape:signed-area (pts / s p1 p2)
  "闭合点表的有向面积（逆时针为正）。"
  "实数值"
  "(at-landscape:signed-area '((0 0) (10 0) (10 10) (0 10)))"
  (setq s 0.0)
  (setq pts (append pts (list (car pts))))
  (while (cdr pts)
    (setq p1 (car pts))
    (setq p2 (cadr pts))
    (setq s (+ s (- (* (car p1) (cadr p2))
                    (* (cadr p1) (car p2)))))
    (setq pts (cdr pts)))
  (/ s 2.0))

(defun at-landscape:left-normal (a b)
  "线段 a→b 的左法向单位向量。"
  "单位向量"
  "(at-landscape:left-normal '(0 0) '(10 0))"
  (at-landscape:unit (list (- (cadr b) (cadr a))
                           (- (car a) (car b)))))

(defun at-landscape:offset-pts (pts d closed / n i cur prev next n1 n2 u k res)
  "把点表按距离 d 偏移（正值偏向左侧法向，顶点处取斜接）。"
  "偏移后的点表，点数不足返回 nil"
  "(at-landscape:offset-pts '((0 0) (10 0) (10 10) (0 10)) 1.0 1)"
  (setq n (length pts))
  (if (< n 2)
      nil
      (progn
        (setq res nil)
        (setq i 0)
        (while (< i n)
          (setq cur (nth i pts))
          (if (> i 0)
              (setq prev (nth (1- i) pts))
              (if closed (setq prev (nth (1- n) pts)) (setq prev nil)))
          (if (< i (1- n))
              (setq next (nth (1+ i) pts))
              (if closed (setq next (nth 0 pts)) (setq next nil)))
          (cond
           ((and prev next)
            (setq n1 (at-landscape:left-normal prev cur))
            (setq n2 (at-landscape:left-normal cur next)))
           (next
            (setq n1 (at-landscape:left-normal cur next))
            (setq n2 n1))
           (prev
            (setq n1 (at-landscape:left-normal prev cur))
            (setq n2 n1))
           (t
            (setq n1 '(0.0 0.0))
            (setq n2 n1)))
          (setq u (at-landscape:unit (list (+ (car n1) (car n2))
                                           (+ (cadr n1) (cadr n2)))))
          (setq k (if (> (distance '(0 0) u) 1e-6)
                      (/ 1.0 (max 0.2 (+ (* (car u) (car n1))
                                         (* (cadr u) (cadr n1)))))
                      1.0))
          (setq k (min k 5.0))
          (setq res (cons (list (+ (car cur) (* d k (car u)))
                                (+ (cadr cur) (* d k (cadr u))))
                          res))
          (setq i (1+ i)))
        (reverse res))))

(defun at-landscape:path-sides (ent w / pts s d p1 p2)
  "内部：按路宽取路径两侧的偏移点表。"
  "(内点表 外点表)，失败返回 nil"
  "(at-landscape:path-sides (car (entsel)) 2000)"
  (setq pts (at-landscape:pts ent))
  (if (< (length pts) 2)
      nil
      (progn
        (setq d (/ (max 1.0 (if (numberp w) w 2000.0)) 2.0))
        (if (at-landscape:closed-p ent)
            (progn
              (setq s (at-landscape:signed-area pts))
              (setq p1 (at-landscape:offset-pts pts d 1))
              (setq p2 (at-landscape:offset-pts pts (- d) 1))
              (if (< s 0.0) (list p1 p2) (list p2 p1)))
            (list (at-landscape:offset-pts pts d nil)
                  (at-landscape:offset-pts pts (- d) nil))))))

(defun at-landscape:ring-pts (inner outer / o)
  "内部：由内、外两条闭合点表拼成环形闭合点表（用于填充）。"
  "闭合点表"
  "(at-landscape:ring-pts inner-pts outer-pts)"
  (setq o (if (>= (at-landscape:signed-area outer) 0.0)
              outer
              (reverse outer)))
  (setq inner (if (>= (at-landscape:signed-area inner) 0.0)
                  (reverse inner)
                  inner))
  (append o inner))

(defun at-landscape:km-str (dist / km m)
  "把里程数值格式化为桩号字符串（如 K12+345）。"
  "字符串"
  "(at-landscape:km-str 12345.0)"
  (setq km (fix (/ dist 1000.0)))
  (setq m (fix (- dist (* (float km) 1000.0))))
  (strcat "K" (itoa km) "+"
          (cond
           ((< m 10) (strcat "00" (itoa m)))
           ((< m 100) (strcat "0" (itoa m)))
           (t (itoa m)))))

;; ------------------------------------------------------------
;; 1. 封闭多段线搭桥
;; ------------------------------------------------------------
(defun at-landscape:do-add-path (ent w gap / sides inner outer res len d p u nv p1 p2 ln hw)
  "核心：沿路径生成桥面（双线边界 + 等距桥板线）。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:do-add-path (car (entsel)) 2000 500)"
  (if (and (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC")
           (setq sides (at-landscape:path-sides ent w)))
      (progn
        (setq inner (car sides))
        (setq outer (cadr sides))
        (setq w (max 1.0 (if (numberp w) w 2000.0)))
        (setq gap (max 1.0 (if (numberp gap) gap 500.0)))
        (setq hw (/ w 2.0))
        (setq res (list (at-landscape:pl outer (at-landscape:closed-p ent))
                        (at-landscape:pl inner (at-landscape:closed-p ent))))
        (setq len (curve:length ent))
        (setq d 0.0)
        (while (< d len)
          (setq p (at-landscape:pt-at ent d))
          (setq u (at-landscape:unit (at-landscape:tangent ent d)))
          (setq nv (list (- (cadr u)) (car u)))
          (setq p1 (list (+ (car p) (* hw (car nv)))
                         (+ (cadr p) (* hw (cadr nv)))))
          (setq p2 (list (- (car p) (* hw (car nv)))
                         (- (cadr p) (* hw (cadr nv)))))
          (if (setq ln (entity:make-line p1 p2))
              (setq res (cons (at-landscape:on-layer ln) res)))
          (setq d (+ d gap)))
        (reverse res))
      (progn
        (princ "at-landscape:do-add-path 需要多段线/线状图元")
        nil)))

(defun at-landscape:add-path (/ ent w gap res)
  "封闭多段线搭桥：沿多段线生成桥面双线边界与等距桥板线。"
  "生成的图元 ename 列表"
  "(at-landscape:add-path)"
  (if (and (setq ent (car (entsel "\n请选择路径多段线:")))
           (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (progn
        (setq w (getdist "\n桥面宽度 <2000>:"))
        (if (null w) (setq w 2000.0))
        (setq gap (getdist "\n桥板间距 <500>:"))
        (if (null gap) (setq gap 500.0))
        (startundo)
        (setq res (at-landscape:do-add-path ent w gap))
        (endundo)
        (if res
            (princ (strcat "\n已生成桥面，共 " (itoa (length res)) " 个图元。"))
            (princ "\n搭桥失败。")))
      (princ "\n请选择路径多段线。"))
  (princ))

(defun at-landscape:ai-add-path (pline-ent w gap / res)
  "AI 调用入口：沿路径生成桥面（双线边界 + 等距桥板线）。
  pline-ent: 路径 ename；w: 桥面宽度（缺省 2000）；gap: 桥板间距（缺省 500）。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:ai-add-path (car (entsel)) 2000 500)"
  (if (and pline-ent
           (wcmatch (entity:getdxf pline-ent 0)
                    "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (setq res (at-landscape:do-add-path pline-ent w gap))
      (princ "at-landscape:ai-add-path pline-ent 必须是线状图元"))
  res)

;; ------------------------------------------------------------
;; 2. 封闭多段线挖路
;; ------------------------------------------------------------
(defun at-landscape:do-remove-path (ent w pattern / sides inner outer res ring hl)
  "核心：沿路径生成路面（双线边界 + 铺装填充）。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:do-remove-path (car (entsel)) 2000 \"ANSI31\")"
  (if (and (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE")
           (at-landscape:closed-p ent)
           (setq sides (at-landscape:path-sides ent w)))
      (progn
        (setq inner (car sides))
        (setq outer (cadr sides))
        (setq pattern (if (= 'str (type pattern)) pattern "ANSI31"))
        (setq res (list (at-landscape:pl outer 1)
                        (at-landscape:pl inner 1)))
        (setq ring (at-landscape:pl (at-landscape:ring-pts inner outer) 1))
        (if ring
            (progn
              (setq hl (at-landscape:hatch ring 8 pattern 40.0))
              (if hl
                  (setq res (cons hl res))
                  (progn
                    (entdel ring)
                    (princ (strcat "\n未找到填充图案 " pattern "，仅生成边界。"))))))
        (reverse res))
      (progn
        (princ "at-landscape:do-remove-path 需要闭合多段线")
        nil)))

(defun at-landscape:remove-path (/ ent w pattern res)
  "封闭多段线挖路：沿闭合多段线生成路面双线边界与铺装填充。"
  "生成的图元 ename 列表"
  "(at-landscape:remove-path)"
  (if (and (setq ent (car (entsel "\n请选择闭合多段线:")))
           (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE")
           (at-landscape:closed-p ent))
      (progn
        (setq w (getdist "\n路面宽度 <2000>:"))
        (if (null w) (setq w 2000.0))
        (setq pattern (getstring "\n填充图案 <ANSI31>:"))
        (if (= "" pattern) (setq pattern "ANSI31"))
        (startundo)
        (setq res (at-landscape:do-remove-path ent w pattern))
        (endundo)
        (if res
            (princ (strcat "\n已生成路面，共 " (itoa (length res)) " 个图元。"))
            (princ "\n挖路失败。")))
      (princ "\n请选择闭合多段线。"))
  (princ))

(defun at-landscape:ai-remove-path (pline-ent w pattern / res)
  "AI 调用入口：沿闭合多段线生成路面（双线边界 + 铺装填充）。
  pline-ent: 闭合多段线 ename；w: 路面宽度（缺省 2000）；
  pattern: 填充图案名（缺省 ANSI31，找不到图案时只画边界）。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:ai-remove-path (car (entsel)) 2000 \"ANSI31\")"
  (if (and pline-ent
           (wcmatch (entity:getdxf pline-ent 0) "LWPOLYLINE,POLYLINE,SPLINE")
           (at-landscape:closed-p pline-ent))
      (setq res (at-landscape:do-remove-path pline-ent w pattern))
      (princ "at-landscape:ai-remove-path pline-ent 必须是闭合多段线"))
  res)

;; ------------------------------------------------------------
;; 3. 路线里程标注
;; ------------------------------------------------------------
(defun at-landscape:do-mileage (ent dist start / e len step res d p u nv dot ts txt)
  "核心：沿路线按间隔布里程桩号（小圆点 + 桩号文字）。"
  "桩号文字 ename 列表，失败返回 nil"
  "(at-landscape:do-mileage (car (entsel)) 20000 0)"
  (if (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC")
      (progn
        (setq e (e2o ent))
        (setq len (curve:length ent))
        (setq step (max 1.0 (if (numberp dist) dist 20000.0)))
        (setq start (if (numberp start) start 0.0))
        (setq ts (at-landscape:text-size))
        (setq d 0.0)
        (setq res nil)
        (while (<= d len)
          (setq p (at-landscape:pt-at ent d))
          (setq u (at-landscape:unit (at-landscape:tangent ent d)))
          (setq nv (list (- (cadr u)) (car u)))
          (if (setq dot (entity:make-circle p (* 0.4 ts)))
              (at-landscape:on-layer dot))
          (setq p (list (+ (car p) (* 1.5 ts (car nv)))
                        (+ (cadr p) (* 1.5 ts (cadr nv)))))
          (if (setq txt (at-landscape:text
                         (at-landscape:km-str (+ start d))
                         p ts "MM"))
              (setq res (cons txt res)))
          (setq d (+ d step)))
        (reverse res))
      (progn
        (princ "at-landscape:do-mileage 需要线状图元")
        nil)))

(defun at-landscape:mileage (/ ent dist start res)
  "路线里程标注：沿路线按间隔标注里程桩号（K0+000 形式）。"
  "桩号文字 ename 列表"
  "(at-landscape:mileage)"
  (if (and (setq ent (car (entsel "\n请选择路线:")))
           (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (progn
        (setq dist (getdist "\n桩号间隔 <20000>:"))
        (if (null dist) (setq dist 20000.0))
        (setq start (getreal "\n起始里程 <0>:"))
        (if (null start) (setq start 0.0))
        (startundo)
        (setq res (at-landscape:do-mileage ent dist start))
        (endundo)
        (if res
            (princ (strcat "\n已标注 " (itoa (length res)) " 个桩号。"))
            (princ "\n里程标注失败。")))
      (princ "\n请选择路线。"))
  (princ))

(defun at-landscape:ai-mileage (pline-ent dist start / res)
  "AI 调用入口：沿路线按间隔标注里程桩号（K0+000 形式）。
  pline-ent: 路线 ename；dist: 桩号间隔（缺省 20000）；
  start: 起始里程（缺省 0）。"
  "桩号文字 ename 列表，失败返回 nil"
  "(at-landscape:ai-mileage (car (entsel)) 20000 0)"
  (if (and pline-ent
           (wcmatch (entity:getdxf pline-ent 0)
                    "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (setq res (at-landscape:do-mileage pline-ent dist start))
      (princ "at-landscape:ai-mileage pline-ent 必须是线状图元"))
  res)

;; ------------------------------------------------------------
;; 4. 界址点编号
;; ------------------------------------------------------------
(defun at-landscape:do-jzd (ents prefix / res idx e pts p c r off pt circ txt)
  "核心：沿边界线在拐点画界址点符号（小圆 + 点）并连续编号。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:do-jzd (list (car (entsel))) \"JZ\")"
  (setq prefix (if (= 'str (type prefix)) prefix "JZ"))
  (setq idx 1)
  (setq res nil)
  (foreach e ents
    (if (and e (setq pts (at-landscape:pts e)))
        (progn
          (setq c (at-landscape:centroid pts))
          (setq r (* 0.5 (at-landscape:text-size)))
          (foreach p pts
            (if (setq circ (entity:make-circle p r))
                (setq res (cons (at-landscape:on-layer circ) res)))
            (if (setq pt (entity:make-point p))
                (setq res (cons (at-landscape:on-layer pt) res)))
            (setq off (at-landscape:unit (list (- (car p) (car c))
                                              (- (cadr p) (cadr c)))))
            (if (< (distance '(0 0) off) 1e-6)
                (setq off '(0.0 1.0)))
            (setq pt (list (+ (car p) (* 3.0 r (car off)))
                           (+ (cadr p) (* 3.0 r (cadr off)))))
            (if (setq txt (at-landscape:text
                           (strcat prefix (itoa idx)) pt
                           (at-landscape:text-size) "MM"))
                (setq res (cons txt res)))
            (setq idx (1+ idx))))))
  (reverse res))

(defun at-landscape:jzd (/ ss ents prefix res)
  "界址点编号：沿边界线在拐点画界址点符号并连续编号（JZ1、JZ2…）。"
  "生成的图元 ename 列表"
  "(at-landscape:jzd)"
  (if (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,SPLINE,LINE"))))
      (progn
        (setq ents (pickset:to-entlist ss))
        (sssetfirst nil nil)
        (setq prefix (getstring "\n编号前缀 <JZ>:"))
        (if (= "" prefix) (setq prefix "JZ"))
        (startundo)
        (setq res (at-landscape:do-jzd ents prefix))
        (endundo)
        (if res
            (princ (strcat "\n已标注界址点，共 " (itoa (length res)) " 个图元。"))
            (princ "\n界址点编号失败。")))
      (princ "\n未选择边界线。"))
  (princ))

(defun at-landscape:ai-jzd (pline-ent prefix / ents res)
  "AI 调用入口：沿边界线拐点画界址点符号并连续编号。
  pline-ent: 边界线 ename 或 ename 列表；prefix: 编号前缀（缺省 JZ）。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:ai-jzd (car (entsel)) \"JZ\")"
  (cond
   ((= 'ename (type pline-ent)) (setq ents (list pline-ent)))
   ((listp pline-ent) (setq ents pline-ent)))
  (if ents
      (setq res (at-landscape:do-jzd ents prefix))
      (princ "at-landscape:ai-jzd pline-ent 必须是 ename 或 ename 列表"))
  res)
