;; ============================================================
;; mini-arch / wall.lsp — 墙体
;; 画墙、单线变墙、批量修改墙宽、墙体完成面线、删除墙柱门窗
;; ============================================================

(defun at-arch:ai-wall (pts th / layer res)
  "AI 入口：由中心线点表与墙厚画双线墙（两端封口）。
  pts: 墙中心线点表（二维或三维点），至少 2 个。
  t: 墙厚（数字，如 240）。
  return: 墙图元 ename 列表；参数非法返回 nil。
  示例: (at-arch:ai-wall '((0 0)(3600 0)(3600 2400)) 240)"
  (if (or (null pts) (< (length pts) 2) (null th))
      (progn (princ "ai-wall: 参数缺失（pts/t）。") nil)
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-wall* 7))
        (setq res (at-arch:draw-wall pts th layer))
        (princ (strcat "\n画墙完成，" (itoa (length res)) " 个图元。"))
        res)))

(defun at-arch:wall (/ pts p th res)
  "画墙：逐点输入墙路径回车结束，按墙厚生成双线墙（端头闭合）。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq p (getpoint "\n指定墙起点:"))
  (if p
      (progn
        (setq pts (list p))
        (while (setq p (getpoint (car pts) "\n下一点(回车结束):"))
          (setq pts (append pts (list p))))
        (if (> (length pts) 1)
            (progn
              (setq th (getint "\n墙厚<240>:"))
              (if (null th) (setq th 240))
              (startundo)
              (at-arch:ai-wall pts th)
              (endundo))
            (princ "\n至少需要 2 个点。")))
  (princ)))

(defun at-arch:ai-line2wall (ents th both-side del / layer res e pts)
  "AI 入口：单线变墙。选中折线/多段线，按墙厚偏移生成双线墙。
  ents: ename 列表或选择集（LINE/LWPOLYLINE）。
  t: 墙厚（数字）。
  both-side: th 向两侧偏移（默认）；nil 只向一侧偏移 t/2。
  del: th 删除原线（默认 nil 保留）。
  return: 新墙 ename 列表；无有效线返回 nil。
  示例: (at-arch:ai-line2wall (ssget) 240 th nil)"
  (if (or (null ents) (null th))
      (progn (princ "ai-line2wall: 参数缺失。") nil)
      (progn
        (if (null entity:putdxf) (require 'entity:*))
        (setq layer (at-arch:set-layer *at-arch:layer-wall* 7))
        (setq res nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (setq pts (at-arch:ent-pts e)))
              (progn
                (if both-side
                    (setq res (append res
                             (at-arch:draw-wall pts th layer)))
                    (setq res (append res
                             (list (at-arch:pline
                                   (at-arch:offset-poly pts (/ th 2.0))
                                   0 layer)))))
                (if (and del (at-arch:alive-p e)) (entdel e)))))
        (if res
            (progn
              (princ (strcat "\n单线变墙完成，" (itoa (length res)) " 个图元。"))
              res)
            (progn
              (princ "ai-line2wall: 没有有效的线实体。")
              nil)))))

(defun at-arch:line2wall (/ ss th both del)
  "单线变墙：选中折线/多段线，按墙厚向两侧偏移生成双线墙。"
  "无"
  "示例: 运行命令后框选线条"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (progn
        (setq th (getint "\n墙厚<240>:"))
        (if (null th) (setq th 240))
        (setq both (getkword "\n两侧偏移[B]/单侧[S]<B>:"))
        (if (eq both "S") (setq both nil))
        (setq del (getkword "\n删除原线[Y/N]<N>:"))
        (if (eq del "Y") (setq del t))
        (startundo)
        (at-arch:ai-line2wall ss th both del)
        (endundo))
  (princ)))

(defun at-arch:ai-wall-width (ents tnew / layer data pairs res)
  "AI 入口：批量修改墙宽。选中墙双线，按新墙厚重新生成。
  ents: ename 列表或选择集（墙的两侧线，可多组）。
  tnew: 新墙厚（数字）。
  配对规则：同顶点数且逐点距离近似相等的两条线视为一对墙。
  return: 新墙 ename 列表；没有可配对的线返回 nil。
  示例: (at-arch:ai-wall-width (ssget) 200)"
  (if (or (null ents) (null tnew))
      (progn (princ "ai-wall-width: 参数缺失。") nil)
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-wall* 7))
        (setq data nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (at-arch:ent-pts e))
              (setq data (cons (list e (at-arch:ent-pts e)) data))))
        (setq pairs (at-arch:pair-lines data))
        (if pairs
            (progn
              (setq res (at-arch:rebuild-pairs pairs tnew layer))
              (princ (strcat "\n已修改 " (itoa (length pairs))
                    " 组墙，墙厚 " (at-arch:str tnew) "。"))
              res)
            (progn
              (princ "ai-wall-width: 没有可配对的墙双线。")
              nil)))))

(defun at-arch:wall-width (/ ss th)
  "批量修改墙宽：选中墙的两侧线，输入新墙厚重新生成。"
  "无"
  "示例: 运行命令后框选墙线"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (progn
        (setq th (getint "\n新墙厚<240>:"))
        (if (null th) (setq th 240))
        (startundo)
        (at-arch:ai-wall-width ss th)
        (endundo))
  (princ)))

(defun at-arch:ai-wall-cover (ents d / layer data pairs res pr c s1 s2
                                       un)
  "AI 入口：墙体完成面线。选中墙双线，向外偏移生成完成面线。
  ents: ename 列表或选择集（墙的两侧线，单线亦可）。
  d: 偏距（数字，如保温 20、抹灰 10）。
  成对线各向外偏移 d；单线向两侧各偏移 d。
  return: 完成面线 ename 列表；无有效线返回 nil。
  示例: (at-arch:ai-wall-cover (ssget) 20)"
  (if (or (null ents) (null d))
      (progn (princ "ai-wall-cover: 参数缺失。") nil)
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-cover* 8))
        (setq data nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (at-arch:ent-pts e))
              (setq data (cons (list e (at-arch:ent-pts e)) data))))
        (setq pairs (at-arch:pair-lines data))
        (setq res nil)
        (foreach pr pairs
          (setq c (at-arch:center-of (cadr (car pr)) (cadr (cadr pr))))
          (setq s1 (at-arch:pline
                    (at-arch:offset-side c (cadr (car pr)) d) 0 layer))
          (setq s2 (at-arch:pline
                    (at-arch:offset-side c (cadr (cadr pr)) d) 0 layer))
          (setq res (append res (list s1 s2))))
        ;; 未配对的单线：两侧偏移
        (setq un (vl-remove-if
                  (function (lambda (x) (member x (mapcar 'cadr pairs))))
                  data))
        (foreach pr un
          (setq res (append res
                   (list (at-arch:pline
                         (at-arch:offset-poly (cadr pr) d) 0 layer)
                         (at-arch:pline
                          (at-arch:offset-poly (cadr pr) (- d)) 0 layer)))))
        (if res
            (progn
              (princ (strcat "\n完成面线生成，" (itoa (length res)) " 条。"))
              res)
            (progn
              (princ "ai-wall-cover: 没有有效的线实体。")
              nil)))))

(defun at-arch:wall-cover (/ ss d)
  "墙体完成面线：选中墙，向外偏移生成完成面线（偏距参数）。"
  "无"
  "示例: 运行命令后框选墙线"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (progn
        (setq d (getint "\n偏距<20>:"))
        (if (null d) (setq d 20))
        (startundo)
        (at-arch:ai-wall-cover ss d)
        (endundo))
  (princ)))

(defun at-arch:type-pats (types / res)
  "类型表转图层名通配模式表。类型: wall/column/door/window/all。"
  "list"
  (setq res nil)
  (foreach x types
    (cond
      ((eq x "all") (setq res (list "*")))
      ((eq x "wall") (setq res (append res (list "WALL,墙*,*墙*"))))
      ((eq x "column") (setq res (append res (list "COLUMN,柱*,*柱*"))))
      ((eq x "door") (setq res (append res (list "DOOR,门*,*门*"))))
      ((eq x "window") (setq res (append res (list "WINDOW,窗*,*窗*"))))))
  res)

(defun at-arch:ai-erase-wall (ents types / pats n lay)
  "AI 入口：按图层关键字批量删除墙柱门窗实体。
  ents: ename 列表或选择集。
  types: 类型表，元素 \"wall\"/\"column\"/\"door\"/\"window\"/\"all\"。
  图层匹配: wall=>WALL/墙*，column=>COLUMN/柱*，door=>DOOR/门*，
  window=>WINDOW/窗*（不区分大小写）。
  return: 被删除的 ename 列表；没有可删实体返回 nil。
  示例: (at-arch:ai-erase-wall (ssget) '(\"all\"))"
  (if (null ents)
      (progn (princ "ai-erase-wall: 未选择实体。") nil)
      (progn
        (if (null types) (setq types (list "all")))
        (setq pats (at-arch:type-pats types))
        (setq n nil)
        (foreach e (at-arch:ent-list ents)
          (if (at-arch:alive-p e)
              (progn
                (setq lay (strcase (entity:getdxf e 8)))
                (if (vl-some (function (lambda (p) (wcmatch lay p))) pats)
                    (progn
                      (entdel e)
                      (setq n (cons e n)))))))
        (if n
            (progn
              (princ (strcat "\n已删除 " (itoa (length n)) " 个实体。"))
              (reverse n))
            (progn
              (princ "ai-erase-wall: 没有匹配的实体。")
              nil)))))

(defun at-arch:erase-wall (/ ss kw)
  "删除墙柱门窗：选中实体，按类型（墙/柱/门/窗，按图层名匹配）批量删除。"
  "无"
  "示例: 运行命令后框选实体"
  (if (setq ss (ssget))
      (progn
        (setq kw (getkword "\n类型: 全部[A]/墙[W]/柱[C]/门[D]/窗[N]<A>:"))
        (if (null kw) (setq kw "A"))
        (startundo)
        (at-arch:ai-erase-wall ss
          (list
           (cond ((= kw "W") "wall")
                 ((= kw "C") "column")
                 ((= kw "D") "door")
                 ((= kw "N") "window")
                 (t "all"))))
        (endundo))
  (princ)))
