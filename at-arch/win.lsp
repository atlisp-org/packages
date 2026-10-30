;; ============================================================
;; mini-arch / win.lsp — 门窗
;; 两点画门、两点画窗、墙上开门、门窗编号、门窗表
;; ============================================================

(defun at-arch:pt-seg (pt a b / v l f p)
  "点 pt 到线段 ab 的距离与最近点（投影限制在线段内）。
返回 (dist c)，c 为最近点。"
  "list"
  "(at-arch:pt-seg '(5 3) '(0 0) '(10 0))"
  (setq v (at-arch:vec a b))
  (setq l (distance '(0 0) v))
  (if (< l 1e-9)
      (list (distance pt a) (at-arch:2d a))
      (progn
        (setq f (/ (+ (* (- (car pt) (car a)) (car v))
                      (* (- (cadr pt) (cadr a)) (cadr v)))
                  l l))
        (setq f (max 0.0 (min 1.0 f)))
        (setq p (at-arch:vadd a (at-arch:vscale v f)))
        (list (distance pt p) p))))

(defun at-arch:door-geo (p1 p2 th side leaves layer / dir n W res
                                      A1 A2 B1 B2 P)
  "在门洞 p1→p2 画门（门框+门扇+开启弧）。
t: 墙厚。side: 1 朝方向左侧开启 / -1 右侧。leaves: 1 单扇 2 双扇。
返回图元 ename 列表。"
  "list"
  "(at-arch:door-geo '(0 0) '(900 0) 240 1 1 \"DOOR\")"
  (if (or (null p1) (null p2)) nil
      (progn
        (setq dir (at-arch:normal p1 p2))
        (setq n (list (- (cadr dir)) (car dir)))
        (setq W (distance p1 p2))
        (setq A1 (at-arch:vadd p1 (at-arch:vscale dir (/ th 2.0))))
        (setq A2 (at-arch:vadd p2 (at-arch:vscale dir (/ th 2.0))))
        (setq B1 (at-arch:vadd p1 (at-arch:vscale dir (- (/ th 2.0)))))
        (setq B2 (at-arch:vadd p2 (at-arch:vscale dir (- (/ th 2.0)))))
        (setq res (list (at-arch:pline (list A1 A2 B2 B1) 1 layer)))
        (if (= leaves 2)
            (progn
              (setq P (at-arch:vadd p1 (at-arch:vscale n
                       (* (/ W 2.0) side))))
              (setq res (append res
                       (list (at-arch:line p1 P layer 0)
                             (at-arch:arc p1 (/ W 2.0) P
                              (at-arch:vadd p1 (at-arch:vscale dir
                                       (/ W 2.0))) layer))))
              (setq P (at-arch:vadd p2 (at-arch:vscale n
                       (* (/ W 2.0) side))))
              (setq res (append res
                       (list (at-arch:line p2 P layer 0)
                             (at-arch:arc p2 (/ W 2.0) P
                              (at-arch:vadd p2 (at-arch:vscale dir
                                       (- (/ W 2.0))) layer))))))
            (progn
              (setq P (at-arch:vadd p1 (at-arch:vscale n (* W side))))
              (setq res (append res
                       (list (at-arch:line p1 P layer 0)
                             (at-arch:arc p1 W P
                              (at-arch:vadd p1 (at-arch:vscale dir W))
                              layer))))))
        res)))

(defun at-arch:ai-door (p1 p2 th side leaves / layer res)
  "AI 入口：两点画门（门框+门扇+开启弧）。
p1/p2: 门洞两端点。
t: 墙厚（默认 240）。
side: 1 朝 p1→p2 方向左侧开启 / -1 右侧（默认 1）。
leaves: 1 单扇 / 2 双扇（默认 1）。
return: 图元 ename 列表；参数非法返回 nil。
示例: (at-arch:ai-door '(0 0) '(900 0) 240 1 1)"
  (if (or (null p1) (null p2) (< (distance p1 p2) 10))
      (progn (princ "ai-door: 参数缺失或门洞过窄。") nil)
      (progn
        (if (null th) (setq th 240))
        (if (null side) (setq side 1))
        (if (null leaves) (setq leaves 1))
        (setq layer (at-arch:set-layer *at-arch:layer-door* 4))
        (setq res (at-arch:door-geo (at-arch:2d p1) (at-arch:2d p2)
                   th side leaves layer))
        (if res
            (progn
              (princ (strcat "\n画门完成，" (itoa (length res)) " 个图元。"))
              res)
            nil))))

(defun at-arch:door (/ p1 p2 th kw side leaves)
  "两点画门：输入门洞两端点、墙厚、开启方向与扇数。"
  "无"
  "示例: 运行命令后按提示输入"
  (if (setq p1 (getpoint "\n门洞起点:"))
      (if (setq p2 (getpoint p1 "\n门洞终点:"))
          (progn
            (setq th (getint "\n墙厚<240>:"))
            (if (null th) (setq th 240))
            (setq kw (getkword "\n开启方向: 左[L]/右[R]<L>:"))
            (setq side (if (eq kw "R") -1 1))
            (setq kw (getkword "\n扇数: 单[S]/双[D]<S>:"))
            (setq leaves (if (eq kw "D") 2 1))
            (startundo)
            (at-arch:ai-door p1 p2 th side leaves)
            (endundo))))
  (princ))

(defun at-arch:win-geo (p1 p2 th layer / dir res A1 A2 B1 B2 m1 m2)
  "在窗洞 p1→p2 画窗（窗框+中线，三线窗符号）。
返回图元 ename 列表。"
  "list"
  "(at-arch:win-geo '(0 0) '(1500 0) 240 \"WINDOW\")"
  (if (or (null p1) (null p2)) nil
      (progn
        (setq dir (at-arch:normal p1 p2))
        (setq A1 (at-arch:vadd p1 (at-arch:vscale dir (/ th 2.0))))
        (setq A2 (at-arch:vadd p2 (at-arch:vscale dir (/ th 2.0))))
        (setq B1 (at-arch:vadd p1 (at-arch:vscale dir (- (/ th 2.0)))))
        (setq B2 (at-arch:vadd p2 (at-arch:vscale dir (- (/ th 2.0)))))
        (setq m1 (list (list (/ (+ (car A1) (car A2)) 2.0)
                           (/ (+ (cadr A1) (cadr A2)) 2.0))))
        (setq m2 (list (list (/ (+ (car B1) (car B2)) 2.0)
                           (/ (+ (cadr B1) (cadr B2)) 2.0))))
        (setq res (list (at-arch:pline (list A1 A2 B2 B1) 1 layer)
                        (at-arch:line (car m1) (car m2) layer 0)))
        res)))

(defun at-arch:ai-win (p1 p2 th / layer res)
  "AI 入口：两点画窗（窗框+中线，三线窗符号）。
p1/p2: 窗洞两端点。t: 墙厚（默认 240）。
return: 图元 ename 列表；参数非法返回 nil。
示例: (at-arch:ai-win '(0 0) '(1500 0) 240)"
  (if (or (null p1) (null p2) (< (distance p1 p2) 10))
      (progn (princ "ai-win: 参数缺失或窗洞过窄。") nil)
      (progn
        (if (null th) (setq th 240))
        (setq layer (at-arch:set-layer *at-arch:layer-window* 5))
        (setq res (at-arch:win-geo (at-arch:2d p1) (at-arch:2d p2)
                   th layer))
        (if res
            (progn
              (princ (strcat "\n画窗完成，" (itoa (length res)) " 个图元。"))
              res)
            nil))))

(defun at-arch:win (/ p1 p2 th)
  "两点画窗：输入窗洞两端点与墙厚。"
  "无"
  "示例: 运行命令后按提示输入"
  (if (setq p1 (getpoint "\n窗洞起点:"))
      (if (setq p2 (getpoint p1 "\n窗洞终点:"))
          (progn
            (setq th (getint "\n墙厚<240>:"))
            (if (null th) (setq th 240))
            (startundo)
            (at-arch:ai-win p1 p2 th)
            (endundo))))
  (princ))

(defun at-arch:ai-hole-door (ents pt width th side leaves / layer data
                                       d i n a b cc c best cs c-mid dir
                                       p1 p2 ext j1a j1b j2a j2b pieces
                                       res e l r)
  "AI 入口：墙上开门（在双线墙上切出洞口并画门）。
ents: 墙线（LINE/LWPOLYLINE，双线墙）ename 列表或选择集。
pt: 墙上靠近开门中心的一点。
width: 门宽（默认 900）。t: 墙厚（默认 240）。
side: 开启方向 1/-1（默认 1）。leaves: 1 单扇 2 双扇（默认 1）。
return: 图元 ename 列表（新墙线+门）；无有效墙线返回 nil。
示例: (at-arch:ai-hole-door (ssget) '(1800 1200) 900 240 1 1)"
  (if (or (null ents) (null pt))
      (progn (princ "ai-hole-door: 参数缺失。") nil)
      (progn
        (if (null th) (setq th 240))
        (if (null width) (setq width 900))
        (if (null side) (setq side 1))
        (if (null leaves) (setq leaves 1))
        (if (null entity:getdxf) (require 'entity:*))
        (setq layer (at-arch:ensure-layer *at-arch:layer-door* 4))
        (setq data nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (at-arch:ent-pts e))
              (setq data (cons (list e (at-arch:ent-pts e)) data))))
        (if (null data)
            (progn (princ "ai-hole-door: 没有有效的墙线。") nil)
            (progn
              ;; 每条墙线上离 pt 最近的点（及其线段方向）
              (setq best nil)
              (setq cs nil)
              (foreach d data
                (setq i 0)
                (setq n (1- (length (cadr d))))
                (setq c nil)
                (while (< i n)
                  (setq a (nth i (cadr d)))
                  (setq b (nth (1+ i) (cadr d)))
                  (setq cc (at-arch:pt-seg pt a b))
                  (if (or (null c) (< (car cc) (car c)))
                      (setq c (list a b cc)))
                  (setq i (1+ i)))
                (if c
                    (progn
                      (setq cs (cons (caddr c) cs))
                      (if (null best)
                          (setq best (list (car d) (car c) (cadr c)))))))
              (if (null best)
                  (progn (princ "ai-hole-door: 无法定位墙线。") nil)
                  (progn
                    (setq c-mid nil)
                    (foreach p cs
                      (if c-mid
                          (setq c-mid (at-arch:vadd c-mid p))
                          (setq c-mid (list (car p) (cadr p)))))
                    (setq c-mid (list (/ (car c-mid) (length cs))
                                       (/ (cadr c-mid) (length cs))))
                    (setq dir (at-arch:normal (cadr best) (caddr best)))
                    (setq p1 (at-arch:vadd c-mid
                            (at-arch:vscale dir (/ width -2.0))))
                    (setq p2 (at-arch:vadd c-mid
                            (at-arch:vscale dir (/ width 2.0))))
                    ;; 两条门垛切割线（垂直于墙线）
                    (setq ext (+ (/ th 2.0) 500.0))
                    (setq j1a (at-arch:vadd p1 (at-arch:vscale
                             (at-arch:normal (cadr best) (caddr best))
                             (- ext))))
                    (setq j1b (at-arch:vadd p1 (at-arch:vscale
                             (at-arch:normal (cadr best) (caddr best))
                             ext)))
                    (setq j2a (at-arch:vadd p2 (at-arch:vscale
                             (at-arch:normal (cadr best) (caddr best))
                             (- ext))))
                    (setq j2b (at-arch:vadd p2 (at-arch:vscale
                             (at-arch:normal (cadr best) (caddr best))
                             ext)))
                    (setq res nil)
                    (foreach d data
                      (if (setq pieces (at-arch:split-poly
                              (cadr d) j1a j1b j2a j2b))
                          (progn
                            (if (at-arch:alive-p (car d))
                                (entdel (car d)))
                            (if (at-arch:alive-p (car d))
                                (setq l (entity:getdxf (car d) 8))
                                (setq l nil))
                            (if l
                                (setq l (at-arch:ensure-layer l nil))
                                (setq l "0"))
                            (foreach r pieces
                              (setq res (append res
                                       (list (at-arch:pline r 0 l))))))))
                    (if (setq res (append res (at-arch:door-geo
                              (at-arch:2d p1) (at-arch:2d p2)
                              th side leaves layer)))
                        (progn
                          (princ (strcat "\n墙上开门完成，"
                                (itoa (length res)) " 个图元。"))
                          res)
                        nil))))))))

(defun at-arch:hole-door (/ ss pt w th kw side)
  "墙上开门：框选双线墙，指定开门中心点，按门宽切墙并画门。"
  "无"
  "示例: 运行命令后按提示输入"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (if (setq pt (getpoint "\n开门中心(在墙上取点):"))
          (progn
            (setq w (getint "\n门宽<900>:"))
            (if (null w) (setq w 900))
            (setq th (getint "\n墙厚<240>:"))
            (if (null th) (setq th 240))
            (setq kw (getkword "\n开启方向: 左[L]/右[R]<L>:"))
            (setq side (if (eq kw "R") -1 1))
            (startundo)
            (at-arch:ai-hole-door ss pt w th side 1)
            (endundo))))
  (princ))

(defun at-arch:ai-win-text (ents label / tt res i e bb center layer s
                                       prefix cnt)
  "AI 入口：门窗编号。在每个选中门窗实体的中心放置编号文字。
ents: ename 列表或选择集（门窗实体）。
label: 编号字符串（如 \"M1\"）；nil 时按图层自动编号：
       DOOR 层=>M1...，WINDOW 层=>C1...，其他=>N1...。
return: 文字 ename 列表；未选择实体返回 nil。
示例: (at-arch:ai-win-text (ssget) nil)"
  (if (null ents)
      (progn (princ "ai-win-text: 未选择实体。") nil)
      (progn
        (if (null entity:getdxf) (require 'entity:*))
        (setq res nil)
        (setq cnt nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (setq bb (at-arch:bbox (list e))))
              (progn
                (setq center (list (/ (+ (car (car bb)) (car (cadr bb)))
                                      2.0)
                                   (/ (+ (cadr (car bb))
                                         (cadr (cadr bb))) 2.0)))
                (setq layer (entity:getdxf e 8))
                (if (null layer) (setq layer "0"))
                (at-arch:del-near center (at-arch:ts 500) layer)
                (if (null label)
                    (progn
                      (setq s (strcase layer))
                      (if (or (wcmatch s "*DOOR*")
                              (wcmatch s "*门*"))
                          (setq prefix "M")
                          (if (or (wcmatch s "*WINDOW*")
                                  (wcmatch s "*窗*"))
                              (setq prefix "C")
                              (setq prefix "N")))
                      (setq cnt (if (member (list prefix 0) cnt)
                                    cnt
                                    (cons (list prefix 0) cnt)))
                      (setq n (assoc prefix cnt))
                      (setq n (list (car n) (1+ (cadr n))))
                      (setq cnt (at-arch:setn cnt
                                 (position n cnt) n))
                      (setq label (strcat prefix (itoa (cadr n)))))
                (if (setq tt (at-arch:text label center
                          (at-arch:ts 250) 0 "M" layer))
                    (setq res (cons tt res))))))
        (if res
            (progn
              (princ (strcat "\n门窗编号完成，" (itoa (length res))
                    " 个编号。"))
              (reverse res))
            (progn
              (princ "ai-win-text: 没有有效的门窗实体。")
              nil))))))

(defun at-arch:win-text (/ sel e s)
  "门窗编号：逐个点选门窗实体并输入编号（回车自动编号）。"
  "无"
  "示例: 运行命令后点选实体"
  (startundo)
  (while (setq sel (entsel "\n选择门窗实体(回车结束):"))
    (setq s (getstring T "\n输入编号(如 M1,回车自动):"))
    (if (eq s "")
        (at-arch:ai-win-text (list (car sel)) nil)
        (at-arch:ai-win-text (list (car sel)) s)))
  (endundo)
  (princ))

(defun at-arch:parse-win-code (s / s2 n)
  "从门窗编号解析宽/高（mm）：\"M0921\"=>(900 2100)，
  \"M0921-1\" 同理；C1524=>(1500 2400)。无法解析返回 nil。"
  "list/nil"
  "(at-arch:parse-win-code \"M0921\")"
  (if (and s (wcmatch (strcase s) "[MC]####*"))
      (progn
        (setq s2 (substr (strcase s) 2 4))
        (if (vl-every
             (function (lambda (c)
                (and (>= c 48) (<= c 57))))
             (vl-string->list s2))
          (progn
            (setq n (read s2))
            (list (* 100 (fix (/ n 100))) (* 100 (rem n 100))))
          nil))
      nil))

(defun at-arch:ai-win-list (ents / texts res e s bb c ss2 w h code r
                                       rows i)
  "AI 入口：生成门窗表数据（统计门窗编号与尺寸）。
ents: TEXT 实体 ename 列表或选择集；nil 时自动扫描全图 M*/C* 编号。
每个编号在 2500 范围内找 DOOR/WINDOW 层门窗实体取其包围盒尺寸；
编号自带尺寸（如 M0921）时优先采用编号值。
return: ((编号 宽 高 数量) ...)；没有门窗编号返回 nil。
示例: (at-arch:ai-win-list nil)"
  (progn
    (if (null entity:getdxf) (require 'entity:*))
    (if ents
        (setq texts (at-arch:ent-list ents))
        (progn
          (setq texts nil)
          (if (setq ss (ssget "x" '((0 . "TEXT"))))
              (progn
                (setq i 0)
                (while (< i (sslength ss))
                  (setq texts (cons (ssname ss i) texts))
                  (setq i (1+ i)))))))
    (setq res nil)
    (foreach e texts
      (if (and (at-arch:alive-p e) (setq s (entity:getdxf e 1))
               (wcmatch (strcase s) "[MC]*"))
          (progn
            (setq bb (at-arch:bbox (list e)))
            (if bb
                (progn
                  (setq c (list (/ (+ (car (car bb)) (car (cadr bb)))
                                   2.0)
                                 (/ (+ (cadr (car bb)) (cadr (cadr bb)))
                                   2.0)))
                  (setq w nil)
                  (setq h nil)
                  (if (setq ss2 (ssget "C" c 2500
                            (list (cons 0 "LWPOLYLINE LINE CIRCLE")
                                  (cons 8 "DOOR,WINDOW,门*,窗*"))))
                      (progn
                        (setq bb (at-arch:bbox
                                  (at-arch:ent-list ss2)))
                        (if bb
                            (progn
                              (setq w (max (- (car (cadr bb))
                                       (car (car bb)))
                                (- (cadr (cadr bb)) (cadr (car bb)))))
                              (setq h (min (- (car (cadr bb))
                                       (car (car bb)))
                                (- (cadr (cadr bb)) (cadr (car bb)))))))))
                  (if (setq code (at-arch:parse-win-code s))
                      (progn
                        (setq w (car code))
                        (setq h (cadr code))))
                  (setq r (assoc (strcase s) res))
                  (if r
                      (setq res (at-arch:setn res
                                 (position r res)
                                 (list (car r) w h
                                       (1+ (cadddr r)))))
                      (setq res (cons (list (strcase s) w h 1) res))))))))
    (if res
        (progn
          (setq res (vl-sort res
                 (function (lambda (a b) (wcmatch (car a) (car b))))))
          (princ (strcat "\n门窗表统计完成，" (itoa (length res)) " 种。"))
          res)
        (progn
          (princ "ai-win-list: 没有找到门窗编号。")
          nil))))

(defun at-arch:win-list (/ ss rows p)
  "门窗表：扫描门窗编号文字，统计尺寸与数量，生成表格。"
  "无"
  "示例: 运行命令后框选编号文字（或不选扫描全图）"
  (setq ss (ssget '((0 . "TEXT"))))
  (if (setq rows (at-arch:ai-win-list ss))
      (if (setq p (getpoint "\n门窗表放置位置(左下角):"))
          (progn
            (startundo)
            (at-arch:draw-table
             p "门窗表"
             (list "编号" "宽" "高" "数量")
             (mapcar
              (function (lambda (r)
                 (list (car r)
                       (if (cadr r) (at-arch:str (cadr r)) "-")
                       (if (caddr r) (at-arch:str (caddr r)) "-")
                       (cadddr r))))
              rows)))
            (endundo)))
  (princ))
