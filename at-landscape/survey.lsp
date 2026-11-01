;; ============================================================
;; at-landscape/survey.lsp
;; 测绘辅助：范围内随机高程点、沿路线断面线、高程点分层设色
;; 需求来源：明经论坛 2026-10 热帖
;;   「多段线范围内输入高程区间随机生成高程点」
;;   「自动生成断面线及桩号」
;;   「更改零散高程点的颜色」
;; ============================================================
(require '(entity:* curve:* pickset:*))

;; 注册菜单
(@:add-menus
 '("景观测绘"
   ("范围内随机高程点" "(at-landscape:random-elevation)")
   ("沿路线断面线" "(at-landscape:section-lines)")
   ("高程点分层设色" "(at-landscape:elev-color)")))

;; ------------------------------------------------------------
;; 公共辅助
;; ------------------------------------------------------------
(defun at-landscape:elev-layer ()
  "高程点专用图层名，不存在时自动创建。"
  "图层名"
  "(at-landscape:elev-layer)"
  (if (null (tblsearch "LAYER" "ELEV-POINT"))
      (layer:make "ELEV-POINT" 2 nil nil))
  "ELEV-POINT")

(defun at-landscape:num-str (v prec / old s)
  "把实数格式化为指定小数位数的字符串（不受 DIMZIN 影响）。"
  "字符串"
  "(at-landscape:num-str 12.3456 2) => \"12.35\""
  (setq old (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq s (rtos v 2 prec))
  (setvar "DIMZIN" old)
  s)

(defun at-landscape:inside-p (pt pts / px py n i a b ya yb xin inside)
  "判断点是否在闭合点表内（射线法，含边界视为在内）。"
  "T 或 nil"
  "(at-landscape:inside-p '(5 5) '((0 0) (10 0) (10 10) (0 10)))"
  (setq px (car pt) py (cadr pt) n (length pts) i 0 inside nil)
  (while (< i n)
    (setq a (nth i pts))
    (setq b (nth (rem (1+ i) n) pts))
    (setq ya (cadr a) yb (cadr b))
    (if (and (not (equal ya yb 1e-12))
             (or (> ya py) (> yb py))
             (<= (min ya yb) py))
        (progn
          (setq xin (+ (car a)
                       (* (/ (float (- py ya)) (float (- yb ya)))
                          (- (car b) (car a)))))
          (if (< px xin) (setq inside (not inside)))))
    (setq i (1+ i)))
  inside)

(defun at-landscape:rand-point-in (pts / bb mnx mny mxx mxy i p found)
  "在闭合点表内随机取一点（包围盒剔除，最多尝试 5000 次）。"
  "二维点，失败返回 nil"
  "(at-landscape:rand-point-in '((0 0) (10 0) (10 10) (0 10)))"
  (setq bb (at-landscape:box pts))
  (if bb
      (progn
        (setq mnx (car (car bb)) mny (cadr (car bb)))
        (setq mxx (car (cadr bb)) mxy (cadr (cadr bb)))
        (setq i 0 found nil)
        (while (and (< i 5000) (null found))
          (setq p (list (at-landscape:rand-in mnx mxx)
                        (at-landscape:rand-in mny mxy)))
          (if (at-landscape:inside-p p pts) (setq found p))
          (setq i (1+ i)))
        found)))

(defun at-landscape:loop-pts (bounds / pts)
  "把闭合图元/点表统一成二维点表。"
  "二维点表，失败返回 nil"
  "(at-landscape:loop-pts (car (entsel)))"
  (cond
   ((= 'ename (type bounds)) (setq pts (at-landscape:pts bounds)))
   ((listp bounds) (setq pts (mapcar 'at-landscape:2d bounds))))
  (if (and pts (>= (length pts) 3)) pts))

(defun at-landscape:put-layer (ename lyr)
  "把图元放到指定图层，颜色随层。"
  "图元名"
  "(at-landscape:put-layer (entlast) \"ELEV-POINT\")"
  (if (and ename lyr)
      (entity:putdxf ename (list 8 62) (list lyr 256)))
  ename)

;; ------------------------------------------------------------
;; 1. 范围内随机高程点
;; ------------------------------------------------------------
(defun at-landscape:do-random-elevation
  (bounds n lo hi prec / pts res i p z pt txt ts lyr tmp)
  "核心：在闭合范围内随机生成高程点（POINT + 高程文字）。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:do-random-elevation (car (entsel)) 20 0.0 10.0 2)"
  (setq pts (at-landscape:loop-pts bounds))
  (if pts
      (progn
        (setq n (max 1 (fix (if (numberp n) n 20))))
        (setq lo (if (numberp lo) (float lo) 0.0))
        (setq hi (if (numberp hi) (float hi) 10.0))
        (if (> lo hi)
            (progn (setq tmp lo) (setq lo hi) (setq hi tmp)))
        (setq prec (max 0 (min 6 (fix (if (numberp prec) prec 2)))))
        (setq ts (at-landscape:text-size))
        (setq lyr (at-landscape:elev-layer))
        (setq res nil)
        (setq i 0)
        (while (< i n)
          (if (setq p (at-landscape:rand-point-in pts))
              (progn
                (setq z (at-landscape:rand-in lo hi))
                (setq pt (entity:make-point (list (car p) (cadr p) z)))
                (if pt
                    (setq res
                          (cons (at-landscape:put-layer pt lyr) res)))
                (setq txt (at-landscape:text
                           (at-landscape:num-str z prec)
                           (list (+ (car p) (* 1.2 ts))
                                 (+ (cadr p) (* 0.6 ts)))
                           ts "MM"))
                (if txt
                    (setq res
                          (cons (at-landscape:put-layer txt lyr) res)))))
          (setq i (1+ i)))
        (reverse res))
      (progn
        (princ "at-landscape:do-random-elevation 需要闭合多段线或点表")
        nil)))

(defun at-landscape:random-elevation (/ ent n lo hi prec res)
  "范围内随机高程点：在闭合多段线内随机布点，并标注随机高程。"
  "生成的图元 ename 列表"
  "(at-landscape:random-elevation)"
  (if (setq ent (car (entsel "\n请选择闭合范围:")))
      (if (at-landscape:closed-p ent)
          (progn
            (setq n (getint "\n高程点数量 <20>:"))
            (if (null n) (setq n 20))
            (setq lo (getreal "\n最小高程 <0>:"))
            (if (null lo) (setq lo 0.0))
            (setq hi (getreal "\n最大高程 <10>:"))
            (if (null hi) (setq hi 10.0))
            (setq prec (getint "\n高程小数位 <2>:"))
            (if (null prec) (setq prec 2))
            (command "_.UNDO" "Begin")
            (setq res (at-landscape:do-random-elevation
                       ent n lo hi prec))
            (command "_.UNDO" "End")
            (if res
                (princ (strcat "\n已生成高程点，共 "
                               (itoa (length res)) " 个图元。"))
                (princ "\n生成高程点失败。")))
          (princ "\n请选择闭合多段线。"))
      (princ))
  (princ))

(defun at-landscape:ai-random-elevation
  (bounds n lo hi prec / res)
  "AI 调用入口：在闭合范围内随机生成高程点（POINT + 高程文字）。
  bounds: 闭合多段线 ename 或闭合点表；
  n: 点数（缺省 20）；lo/hi: 高程区间（缺省 0~10）；
  prec: 高程小数位（缺省 2，0~6）。
  示例: (at-landscape:ai-random-elevation (car (entsel)) 30 5.0 15.0 2)"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:ai-random-elevation (car (entsel)) 20 0.0 10.0 2)"
  (if (at-landscape:loop-pts bounds)
      (setq res (at-landscape:do-random-elevation
                 bounds n lo hi prec))
      (princ "at-landscape:ai-random-elevation bounds 必须是闭合多段线或闭合点表"))
  res)

;; ------------------------------------------------------------
;; 2. 沿路线断面线及桩号
;; ------------------------------------------------------------
(defun at-landscape:do-section-lines
  (ent dist half-len start / e len step res d p u nv p1 p2 ln txt ts)
  "核心：沿路线按间隔画垂直于路线的断面线并标注桩号。"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:do-section-lines (car (entsel)) 20000 10000 0)"
  (if (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC")
      (progn
        (setq e (e2o ent))
        (setq len (curve:length ent))
        (setq step (max 1.0 (if (numberp dist) dist 20000.0)))
        (setq half-len (max 1.0 (if (numberp half-len) half-len 10000.0)))
        (setq start (if (numberp start) start 0.0))
        (setq ts (at-landscape:text-size))
        (setq d 0.0)
        (setq res nil)
        (while (<= d len)
          (setq p (at-landscape:pt-at ent d))
          (setq u (at-landscape:unit (at-landscape:tangent ent d)))
          (setq nv (list (- (cadr u)) (car u)))
          (setq p1 (list (- (car p) (* half-len (car nv)))
                         (- (cadr p) (* half-len (cadr nv)))))
          (setq p2 (list (+ (car p) (* half-len (car nv)))
                         (+ (cadr p) (* half-len (cadr nv)))))
          (if (setq ln (entity:make-line p1 p2))
              (setq res (cons (at-landscape:on-layer ln) res)))
          (setq p2 (list (+ (car p2) (* 1.2 ts (car nv)))
                         (+ (cadr p2) (* 1.2 ts (cadr nv)))))
          (if (setq txt (at-landscape:text
                         (at-landscape:km-str (+ start d))
                         p2 ts "MM"))
              (setq res (cons txt res)))
          (setq d (+ d step)))
        (reverse res))
      (progn
        (princ "at-landscape:do-section-lines 需要线状图元")
        nil)))

(defun at-landscape:section-lines (/ ent dist half-len start res)
  "沿路线断面线：按间隔画横断面线，并在端点标注桩号。"
  "生成的图元 ename 列表"
  "(at-landscape:section-lines)"
  (if (and (setq ent (car (entsel "\n请选择路线:")))
           (wcmatch (entity:getdxf ent 0)
                    "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (progn
        (setq dist (getdist "\n断面间距 <20000>:"))
        (if (null dist) (setq dist 20000.0))
        (setq half-len (getdist "\n断面线半长 <10000>:"))
        (if (null half-len) (setq half-len 10000.0))
        (setq start (getreal "\n起始里程 <0>:"))
        (if (null start) (setq start 0.0))
        (command "_.UNDO" "Begin")
        (setq res (at-landscape:do-section-lines
                   ent dist half-len start))
        (command "_.UNDO" "End")
        (if res
            (princ (strcat "\n已生成断面线，共 "
                           (itoa (length res)) " 个图元。"))
            (princ "\n生成断面线失败。")))
      (princ "\n请选择路线。"))
  (princ))

(defun at-landscape:ai-section-lines
  (pline-ent dist half-len start / res)
  "AI 调用入口：沿路线按间隔画横断面线并标注桩号。
  pline-ent: 路线 ename；dist: 断面间距（缺省 20000）；
  half-len: 断面线半长（缺省 10000）；start: 起始里程（缺省 0）。
  示例: (at-landscape:ai-section-lines (car (entsel)) 20000 10000 0)"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-landscape:ai-section-lines (car (entsel)) 20000 10000 0)"
  (if (and pline-ent
           (wcmatch (entity:getdxf pline-ent 0)
                    "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (setq res (at-landscape:do-section-lines
                 pline-ent dist half-len start))
      (princ "at-landscape:ai-section-lines pline-ent 必须是线状图元"))
  res)

;; ------------------------------------------------------------
;; 3. 高程点分层设色
;; ------------------------------------------------------------
(defun at-landscape:num-str-p (s / c)
  "判断字符串是否为数字（允许正负号与小数点开头）。"
  "T 或 nil"
  "(at-landscape:num-str-p \"12.5\")"
  (setq s (at-landscape:trim s))
  (if (> (strlen s) 0)
      (setq c (substr s 1 1))
      (setq c nil))
  (if (and c (vl-string-search c "0123456789+-.")) t))

(defun at-landscape:ent-elev (ent / d ty s)
  "取图元高程：POINT 取 Z 值，TEXT 取数字内容。"
  "实数值，失败返回 nil"
  "(at-landscape:ent-elev (car (entsel)))"
  (setq d (entget ent))
  (setq ty (cdr (assoc 0 d)))
  (cond
   ((= ty "POINT") (caddr (cdr (assoc 10 d))))
   ((= ty "TEXT")
    (setq s (cdr (assoc 1 d)))
    (if (at-landscape:num-str-p s) (atof s)))
   (t nil)))

(defun at-landscape:band-color (z bands / res b hit)
  "按高程区间取颜色；bands 为 ((最小 最大 颜色) ...)。"
  "颜色号，未匹配返回 nil"
  "(at-landscape:band-color 6.0 '((0 5 1) (5 10 3)))"
  (setq res nil hit nil)
  (foreach b bands
    (if (and (null hit) (numberp z)
             (>= z (car b)) (<= z (cadr b)))
        (progn (setq res (caddr b)) (setq hit t))))
  res)

(defun at-landscape:auto-bands (elevs / mn mx step bands i lo hi)
  "按实际高程范围均分 5 段，返回 ((最小 最大 颜色) ...)。"
  "区间颜色表"
  "(at-landscape:auto-bands '(1.0 2.0 3.0))"
  (setq mn (apply 'min elevs))
  (setq mx (apply 'max elevs))
  (if (equal mn mx 1e-9) (setq mx (+ mn 1.0)))
  (setq step (/ (- mx mn) 5.0))
  (setq bands nil i 0)
  (while (< i 5)
    (setq lo (+ mn (* step i)))
    (setq hi (if (= i 4) mx (+ mn (* step (1+ i)))))
    (setq bands (append bands
                        (list (list lo hi (nth i '(1 3 2 5 6))))))
    (setq i (1+ i)))
  bands)

(defun at-landscape:ent-list (ents / res)
  "把 ename / 选择集 / ename 列表统一成 ename 列表。"
  "ename 列表，失败返回 nil"
  "(at-landscape:ent-list (ssget))"
  (cond
   ((= 'ename (type ents)) (setq res (list ents)))
   ((= 'pickset (type ents)) (setq res (pickset:to-entlist ents)))
   ((listp ents) (setq res ents)))
  res)

(defun at-landscape:do-elev-color (ents bands / lst elevs z c n)
  "核心：按高程区间给高程点/高程文字改颜色。"
  "改色图元数量"
  "(at-landscape:do-elev-color (ssget) '((0 5 1) (5 10 3)))"
  (setq lst (at-landscape:ent-list ents))
  (if lst
      (progn
        (if (null bands)
            (progn
              (setq elevs nil)
              (foreach e lst
                (if (setq z (at-landscape:ent-elev e))
                    (setq elevs (cons z elevs))))
              (if elevs (setq bands (at-landscape:auto-bands elevs)))))
        (setq n 0)
        (foreach e lst
          (if (and bands (setq z (at-landscape:ent-elev e)))
              (if (setq c (at-landscape:band-color z bands))
                  (progn
                    (entity:putdxf e 62 c)
                    (setq n (1+ n))))))
        n)
      0))

(defun at-landscape:elev-color (/ ss bands res)
  "高程点分层设色：按高程区间给 POINT/TEXT 改颜色。"
  "改色图元数量"
  "(at-landscape:elev-color)"
  (if (setq ss (ssget '((0 . "POINT,TEXT"))))
      (progn
        (sssetfirst nil nil)
        (setq bands nil)
        (command "_.UNDO" "Begin")
        (setq res (at-landscape:do-elev-color ss bands))
        (command "_.UNDO" "End")
        (princ (strcat "\n已按高程改色 " (itoa res) " 个图元。")))
      (princ "\n未选择高程点或高程文字。"))
  (princ))

(defun at-landscape:ai-elev-color (ents bands / res)
  "AI 调用入口：按高程区间给高程点/高程文字改颜色。
  ents: ename / 选择集 / ename 列表；
  bands: ((最小 最大 颜色) ...)，传 nil 时按实际高程均分 5 段
  （颜色 1 3 2 5 6）。
  示例: (at-landscape:ai-elev-color (ssget) '((0 5 1) (5 10 3)))"
  "改色图元数量"
  "(at-landscape:ai-elev-color (ssget) nil)"
  (setq res (at-landscape:do-elev-color ents bands))
  (princ (strcat "at-landscape:ai-elev-color 改色 " (itoa res) " 个图元"))
  res)
