;; ============================================================
;; mini-arch / beam.lsp — 梁
;; 画梁、单线变梁、批量修改梁宽
;; ============================================================

(defun at-arch:beam-path (pts w / layer n dir res i s1 s2 o1 o2 e1 e2
                                       cl)
  "由路径点表与梁宽画梁（两侧线+中线+两端封口）。
pts: 路径点表（≥2）。w: 梁宽。
return: 图元 ename 列表。"
  "list"
  "(at-arch:beam-path '((0 0)(3600 0)) 300)"
  (if (< (length pts) 2) nil
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-beam* 3))
        (setq o1 (at-arch:offset-poly pts (/ w 2.0)))
        (setq o2 (at-arch:offset-poly pts (- (/ w 2.0))))
        (setq s1 (at-arch:pline o1 0 layer))
        (setq s2 (at-arch:pline o2 0 layer))
        (setq e1 (at-arch:line (car o1) (car o2) layer 0))
        (setq e2 (at-arch:line (cadr (reverse o1)) (cadr (reverse o2))
                  layer 0))
        ;; 中线（CENTER 线型）
        (setq cl (at-arch:pline (mapcar (function at-arch:2d) pts) 0
                  layer))
        (if (null entity:putdxf) (require 'entity:*))
        (if cl (entity:putdxf cl 6 "CENTER"))
        (setq res (list s1 s2 e1 e2))
        (if cl (setq res (append res (list cl))))
        res)))

(defun at-arch:ai-beam (p1 p2 w / res)
  "AI 入口：两点画梁（双线+中线+封口）。
p1/p2: 梁两端点（三维点自动取 XY）。
w: 梁宽（数字，如 300）。
return: 梁图元 ename 列表；参数非法返回 nil。
示例: (at-arch:ai-beam '(0 0) '(3600 0) 300)"
  (if (or (null p1) (null p2) (null w) (<= w 0))
      (progn (princ "ai-beam: 参数缺失（p1/p2/w）。") nil)
      (progn
        (setq res (at-arch:beam-path (list (at-arch:2d p1)
                    (at-arch:2d p2)) w))
        (princ (strcat "\n画梁完成，" (itoa (length res)) " 个图元。"))
        res)))

(defun at-arch:beam (/ pts p w)
  "画梁：逐点输入梁路径回车结束，按梁宽生成双线+中线。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq p (getpoint "\n梁起点:"))
  (if p
      (progn
        (setq pts (list p))
        (while (setq p (getpoint (car pts) "\n下一点(回车结束):"))
          (setq pts (append pts (list p))))
        (if (> (length pts) 1)
            (progn
              (setq w (getint "\n梁宽<300>:"))
              (if (null w) (setq w 300))
              (startundo)
              (at-arch:beam-path pts w)
              (endundo))
            (princ "\n至少需要 2 个点。"))))
  (princ))

(defun at-arch:ai-line2beam (ents w del / layer res e pts)
  "AI 入口：单线变梁。选中折线/多段线，按梁宽生成双线+中线。
ents: ename 列表或选择集（LINE/LWPOLYLINE）。
w: 梁宽（数字）。
del: t 删除原线（默认 nil 保留）。
return: 新梁图元 ename 列表；无有效线返回 nil。
示例: (at-arch:ai-line2beam (ssget) 300 nil)"
  (if (or (null ents) (null w))
      (progn (princ "ai-line2beam: 参数缺失。") nil)
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-beam* 3))
        (setq res nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (setq pts (at-arch:ent-pts e)))
              (progn
                (setq res (append res
                         (at-arch:beam-path pts w)))
                (if (and del (at-arch:alive-p e)) (entdel e)))))
        (if res
            (progn
              (princ (strcat "\n单线变梁完成，" (itoa (length res))
                    " 个图元。"))
              res)
            (progn
              (princ "ai-line2beam: 没有有效的线实体。")
              nil)))))

(defun at-arch:line2beam (/ ss w del)
  "单线变梁：选中折线/多段线，按梁宽生成双线+中线。"
  "无"
  "示例: 运行命令后框选线条"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (progn
        (setq w (getint "\n梁宽<300>:"))
        (if (null w) (setq w 300))
        (setq del (getkword "\n删除原线[Y/N]<N>:"))
        (if (eq del "Y") (setq del t))
        (startundo)
        (at-arch:ai-line2beam ss w del)
        (endundo)))
  (princ))

(defun at-arch:ai-beam-width (ents wnew / layer data pairs res)
  "AI 入口：批量修改梁宽。选中梁双线，按新梁宽重新生成。
ents: ename 列表或选择集（梁的两侧线，可多组）。
wnew: 新梁宽（数字）。
配对规则：同顶点数且逐点距离近似相等的两条线视为一对梁。
return: 新梁图元 ename 列表；没有可配对的线返回 nil。
示例: (at-arch:ai-beam-width (ssget) 400)"
  (if (or (null ents) (null wnew))
      (progn (princ "ai-beam-width: 参数缺失。") nil)
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-beam* 3))
        (setq data nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e) (at-arch:ent-pts e))
              (setq data (cons (list e (at-arch:ent-pts e)) data))))
        (setq pairs (at-arch:pair-lines data))
        (if pairs
            (progn
              (setq res (at-arch:rebuild-pairs pairs wnew layer))
              (princ (strcat "\n已修改 " (itoa (length pairs))
                    " 组梁，梁宽 " (at-arch:str wnew) "。"))
              res)
            (progn
              (princ "ai-beam-width: 没有可配对的梁双线。")
              nil)))))

(defun at-arch:beam-width (/ ss w)
  "批量修改梁宽：选中梁的两侧线，输入新梁宽重新生成。"
  "无"
  "示例: 运行命令后框选梁线"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (progn
        (setq w (getint "\n新梁宽<300>:"))
        (if (null w) (setq w 300))
        (startundo)
        (at-arch:ai-beam-width ss w)
        (endundo)))
  (princ))
