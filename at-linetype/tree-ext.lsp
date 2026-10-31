;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-linetype —— tree-ext.lsp
;; 炸线型为块 / 变线为树 / 成片布树
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------- 1. 炸线型为块 ----------

(defun at-linetype:place-shape (pt angle scale blk / en en2)
  "放置一个形元素：插入指定块，或画默认树（圆+点）。
  参数: pt:放置点; angle:旋转角（弧度）; scale:缩放系数; blk:块名（nil=默认树）"
  "创建的 ename 列表"
  "(at-linetype:place-shape '(0 0 0) 0.0 1.0 nil)"
  (if (and blk (tblsearch "BLOCK" blk))
      (progn
        (setq en (entmakex (list (cons 0 "INSERT")
                                 (cons 100 "AcDbEntity")
                                 (cons 100 "AcDbBlockReference")
                                 (cons 2 blk)
                                 (cons 10 pt)
                                 (cons 41 scale)
                                 (cons 42 scale)
                                 (cons 50 (/ (* 180.0 angle) pi)))))
        (if en (list en)))
      (progn
        (setq en (entity:make-circle pt (* 10.0 scale)))
        (setq en2 (entity:make-point pt))
        (if en2
            (list en en2)
            (list en)))))

(defun at-linetype:shp2blk-core (ent blk / rec elems total shape-offs s period obj L scale k off pos pt pt2 angle lst rr)
  "把实体线型中的形元素替换为块/树，并将线型改为 CONTINUOUS。
  参数: ent:实体; blk:块名（nil=默认树，圆半径=10×缩放）"
  "创建的 ename 列表；线型无形元素时 nil"
  "(at-linetype:shp2blk-core (car (entsel)) nil)"
  (if (null (setq rec (at-linetype:ltype-record (entity:get-linetype ent))))
      (progn
        (princ "\n无法解析实体的线型")
        nil)
      (progn
        (setq elems (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 49)) rec)))
        (setq total (car elems))
        (setq elems (cdr elems))
        (if (vl-some '(lambda (e) (equal e 0.0 1e-9)) elems)
            (progn
              (setq shape-offs nil)
              (setq s 0.0)
              (foreach e elems
                (if (equal e 0.0 1e-9)
                    (setq shape-offs (append shape-offs (list s)))
                    (setq s (+ s (abs e)))))
              (if (> total 0.0)
                  (setq period total)
                  (setq period s))
              (if (> period 1e-9)
                  (progn
                    (setq obj (e2o ent))
                    (setq L (vlax-curve-getdistancet obj (vlax-curve-getendparam obj)))
                    (setq scale (* (getvar "ltscale")
                                   (if (cdr (assoc 48 rec)) (cdr (assoc 48 rec)) 1.0)))
                    (setq lst nil)
                    (setq k 0)
                    (while (< (* k period) L)
                      (foreach off shape-offs
                        (setq pos (+ (* k period) off))
                        (if (< pos L)
                            (progn
                              (setq pt (vlax-curve-getpointatdistance obj pos))
                              (setq pt2 (vlax-curve-getpointatdistance obj
                                                       (min (+ pos 1.0) L)))
                              (if (equal pt pt2 1e-9)
                                  (progn
                                    (setq pt2 (vlax-curve-getpointatdistance
                                                     obj (max (- pos 1.0) 0.0)))
                                    (if (equal pt pt2 1e-9)
                                        (setq angle 0.0)
                                        (setq angle (angle pt pt2))))
                                  (setq angle (angle pt pt2)))
                              (if (setq rr (at-linetype:place-shape pt angle scale blk))
                                  (setq lst (append lst rr))))))
                      (setq k (1+ k)))
                    (at-linetype:set-ltype ent "CONTINUOUS")
                    lst)
                  (progn
                    (princ "\n线型周期无效")
                    nil)))
            nil))))

(defun at-linetype:shp2blk (/ ss blk r n en)
  "炸线型为块：把所选实体线型中的形元素替换为块/树，线型改为 CONTINUOUS。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:shp2blk)"
  (setq ss (ssget))
  (if ss
      (progn
        (setq blk (getstring "\n输入块名（留空=用默认树）: "))
        (command "_.UNDO" "Begin")
        (setq n 0)
        (foreach en (pickset:to-list ss)
          (if (setq r (at-linetype:shp2blk-core en
                                        (if (null blk) nil blk)))
              (setq n (+ n (length r)))))
        (command "_.UNDO" "End")
        (princ (strcat "\n共替换形元素 " (itoa n) " 处"))))
  (princ))

(defun at-linetype:ai-shp2blk (ents blk / lst res rr en)
  "AI 调用入口：炸线型为块（形元素替换为块/树，线型改 CONTINUOUS）。
  参数: ents:实体列表或选择集; blk:块名（nil=默认树 圆+点）"
  "创建的 ename 列表，失败 nil"
  "(at-linetype:ai-shp2blk (ssget) nil)"
  (setq lst (at-linetype:collect-ents ents nil))
  (if lst
      (progn
        (command "_.UNDO" "Begin")
        (setq res nil)
        (foreach en lst
          (if (setq rr (at-linetype:shp2blk-core en blk))
              (setq res (append res rr))))
        (command "_.UNDO" "End")
        (if res res nil))
      nil))

;; ---------- 2. 变线为树 ----------

(defun at-linetype:tree-style-core (ent dist r / obj L i pos pt tree lst)
  "沿多段线/直线按固定间距放树并删除原线。
  参数: ent:LINE/*POLYLINE/ARC 图元; dist:间距（nil=1000）; r:树半径（nil=200）"
  "创建的树 ename 列表，失败 nil"
  "(at-linetype:tree-style-core (car (entsel)) 500 100)"
  (if (null dist)
      (setq dist 1000.0))
  (if (null r)
      (setq r 200.0))
  (if (or (< dist 1e-9) (< r 1e-9))
      (progn
        (princ "\n间距或半径无效")
        nil)
      (progn
        (setq obj (e2o ent))
        (if (vlax-curve-p obj)
            (progn
              (setq L (vlax-curve-getdistancet obj (vlax-curve-getendparam obj)))
              (command "_.UNDO" "Begin")
              (setq lst nil)
              (setq i 0)
              (while (<= (* i dist) L)
                (setq pt (vlax-curve-getpointatdistance obj (* i dist)))
                (setq tree (at-linetype:tree-at pt r))
                (if tree
                    (setq lst (append lst tree)))
                (setq i (1+ i)))
              (entdel ent)
              (command "_.UNDO" "End")
              (if lst lst nil))
            (progn
              (princ "\n不是线性图元")
              nil)))))

(defun at-linetype:tree-style (/ ent dist r)
  "变线为树：沿多段线按固定间距放树（圆+点）并删除原线。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:tree-style)"
  (setq ent (car (entsel '((0 . "LINE,*POLYLINE,ARC")))))
  (if ent
      (progn
        (setq dist (getdist "\n树木间距 <1000>: "))
        (if (= dist 0.0)
            (setq dist 1000.0))
        (setq r (getdist "\n树木半径 <200>: "))
        (if (= r 0.0)
            (setq r 200.0))
        (if (at-linetype:tree-style-core ent dist r)
            (princ "\n完成"))))
  (princ))

(defun at-linetype:ai-tree-style (pline-ent dist r)
  "AI 调用入口：变线为树（按间距放树并删除原线）。
  参数: pline-ent:多段线/直线图元; dist:间距（nil=1000）; r:树半径（nil=200）"
  "创建的树 ename 列表，失败 nil"
  "(at-linetype:ai-tree-style (car (entsel)) 500 100)"
  (if (null pline-ent)
      (progn
        (princ "\n缺少参数 pline-ent")
        nil)
      (at-linetype:tree-style-core pline-ent dist r)))

;; ---------- 3. 成片布树 ----------

(defun at-linetype:no-overlap (pt r centers / ok)
  "判断候选点与已有树心不重叠（距离 >= 2r）。
  参数: pt:候选点; r:树半径; centers:已有树心点列表"
  "t 或 nil"
  "(at-linetype:no-overlap '(5 5 0) 20 '...)"
  (setq ok t)
  (foreach c centers
    (if (< (distance pt c) (* 2.0 r))
        (setq ok nil)))
  ok)

(defun at-linetype:tree-group-core (ent density r / pts area n x0 y0 x1 y1 i attempts pt tree centers lst)
  "在闭合边界内随机布树（拒绝采样，互不重叠）。
  参数: ent:闭合多段线; density:每单位面积树数（nil=0.001）; r:树半径（nil=200）"
  "创建的树 ename 列表，失败 nil"
  "(at-linetype:tree-group-core (car (entsel)) nil 100)"
  (if (null density)
      (setq density 0.001))
  (if (null r)
      (setq r 200.0))
  (if (or (< density 1e-12) (< r 1e-9))
      (progn
        (princ "\n密度或半径无效")
        nil)
      (progn
        (if (not (at-linetype:pline-closed-p ent))
            (progn
              (princ "\n边界必须是闭合多段线")
              nil)
            (progn
              (setq pts (mapcar 'car (at-linetype:pline-verts ent)))
              (setq area (at-linetype:poly-area pts))
              (if (< area 1e-9)
                  (progn
                    (princ "\n边界面积为 0")
                    nil)
                  (progn
                    (setq n (fix (* area density)))
                    (if (< n 1)
                        (progn
                          (princ "\n边界面积过小，布不了树")
                          nil)
                        (progn
                          (setq x0 1e300)
                          (setq y0 1e300)
                          (setq x1 -1e300)
                          (setq y1 -1e300)
                          (foreach p pts
                            (setq x0 (min x0 (car p)))
                            (setq y0 (min y0 (cadr p)))
                            (setq x1 (max x1 (car p)))
                            (setq y1 (max y1 (cadr p))))
                          (command "_.UNDO" "Begin")
                          (setq centers nil)
                          (setq lst nil)
                          (setq i 0)
                          (setq attempts 0)
                          (while (and (< i n) (< attempts 50000))
                            (setq pt (list (+ x0 (* (- x1 x0) (rand)))
                                           (+ y0 (* (- y1 y0) (rand)))
                                           0.0))
                            (setq attempts (1+ attempts))
                            (if (and (at-linetype:pt-in-poly pt pts)
                                     (at-linetype:no-overlap pt r centers))
                                (progn
                                  (setq tree (at-linetype:tree-at pt r))
                                  (if tree
                                      (progn
                                        (setq centers (append centers (list pt)))
                                        (setq lst (append lst tree))
                                        (setq i (1+ i)))))))
                          (command "_.UNDO" "End")
                          (princ (strcat "\n目标 " (itoa n) " 棵，实际布 "
                                    (itoa i) " 棵"))
                          (if lst lst nil))))))))))

(defun at-linetype:tree-group (/ ent density r)
  "成片布树：在闭合边界内随机布树（互不重叠，圆+点）。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:tree-group)"
  (setq ent (car (entsel '((0 . "*POLYLINE")))))
  (if ent
      (progn
        (setq density (getreal "\n树木密度（每单位面积棵数）<0.001>: "))
        (if (= density 0.0)
            (setq density 0.001))
        (setq r (getreal "\n树木半径 <200>: "))
        (if (= r 0.0)
            (setq r 200.0))
        (at-linetype:tree-group-core ent density r)))
  (princ))

(defun at-linetype:ai-tree-group (boundary-ent density r)
  "AI 调用入口：成片布树（闭合边界内随机不重叠布树）。
  参数: boundary-ent:闭合多段线; density:每单位面积树数（nil=0.001）; r:树半径（nil=200）"
  "创建的树 ename 列表，失败 nil"
  "(at-linetype:ai-tree-group (car (entsel)) nil 100)"
  (if (null boundary-ent)
      (progn
        (princ "\n缺少参数 boundary-ent")
        nil)
      (at-linetype:tree-group-core boundary-ent density r)))
