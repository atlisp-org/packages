;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 填充 (at-hatch) — 图案预览
;; 内置/标准图案样块表 + 任意 .pat 图案按真实尺寸预览，并支持一键清除。
;; 不依赖 DCL，可作为 AI 入口调用（无交互、不弹窗）。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(entity:*))

;; 菜单注册（与 at-hatch.lsp 的「填充工具」组并列）
(@:add-menus
 '("图案预览"
   ("图案预览表" "(at-hatch:pattern-table)")
   ("PAT 文件预览" "(at-hatch:pat-preview)")
   ("清除预览" "(at-hatch:pat-erase)")))

(defun at-hatch:preview-layer ()
  "预览图层名，不存在时自动创建（颜色 8）。"
  "图层名"
  "(at-hatch:preview-layer)"
  (if (null (tblsearch "LAYER" "PAT-PREVIEW"))
      (layer:make "PAT-PREVIEW" 8 nil nil))
  "PAT-PREVIEW")

(defun at-hatch:preview-put (ename lyr color)
  "把预览图元放到指定图层，并设置颜色。"
  "图元名"
  "(at-hatch:preview-put (entlast) \"PAT-PREVIEW\" 8)"
  (if ename
      (entity:putdxf ename (list 8 62) (list lyr color)))
  ename)

(defun at-hatch:ai-pat-preview (file pattern p1 p2 scale ang /
                                entries defs pts recs lyr hatch f)
  "AI 调用入口：把任意 .pat 中的图案按真实尺寸画到两点矩形内。
file: .pat 文件；pattern: 图案名（不区分大小写），nil 取文件中第一个图案；
p1/p2: 矩形两角点；scale/ang: 填充比例/角度，缺省 1.0/0.0。
图层固定 PAT-PREVIEW，颜色随层；图案定义直接写入填充，不依赖支持路径。
返回填充 ename，失败返回 nil。"
  "HATCH 图元名或 nil"
  "(at-hatch:ai-pat-preview \"/tmp/t.pat\" \"P1\" '(0 0) '(100 100) 1.0 0.0)"
  (setq f (if file (findfile file)))
  (if f (setq file f))
  (if (null file)
    (progn (princ "\n未指定图案文件。") nil)
    (progn
      (setq entries (at-hatch:parse-pat-all file))
      (if (null entries)
        (progn (princ (strcat "\n无法从 " file " 读取图案定义。")) nil)
        (progn
          (setq pattern (if pattern pattern (car (car entries))))
          (setq defs (at-hatch:pat-all-defs entries pattern))
          (if (null defs)
            (progn (princ (strcat "\n图案 " pattern " 在该文件中没有定义行。")) nil)
            (progn
              (setq p1 (at-hatch:3d p1) p2 (at-hatch:3d p2))
              (if (or (< (abs (- (car p1) (car p2))) 1e-8)
                      (< (abs (- (cadr p1) (cadr p2))) 1e-8))
                (progn (princ "\n两角点重合（矩形宽或高为 0）。") nil)
                (progn
                  (setq pts (list (list (car p1) (cadr p1))
                                  (list (car p2) (cadr p1))
                                  (list (car p2) (cadr p2))
                                  (list (car p1) (cadr p2))))
                  (setq recs (list (list 1 pts nil)))
                  (setq lyr (at-hatch:preview-layer))
                  (setq scale (if (and (numberp scale) (> scale 0.0)) scale 1.0))
                  (setq ang (if (numberp ang) ang 0.0))
                  (setq hatch (at-hatch:make-hatch
                               recs pattern scale ang lyr nil defs))
                  (if hatch
                    hatch
                    (progn (princ "\n创建预览填充失败。") nil)))))))))))

(defun at-hatch:pat-preview (/ file entries n i e idx name p1 p2 scale ang
                             hatch)
  "PAT 文件预览：选 .pat → 列出全部图案并选序号 → 两角点/比例/角度 → 按真实尺寸绘制。"
  "填充图元名或 nil"
  "(at-hatch:pat-preview)"
  (setq file (getfiled "选择填充图案文件" "" "pat" 0))
  (if (null file)
    (progn (princ "\n已取消。") nil)
    (progn
      (setq entries (at-hatch:parse-pat-all file))
      (if (null entries)
        (progn (princ (strcat "\n" file " 中没有可用的图案定义。")) nil)
        (progn
          (setq n (length entries) i 0)
          (princ (strcat "\n图案文件: " file "（共 " (itoa n) " 个图案）"))
          (foreach e entries
            (setq i (1+ i))
            (princ (strcat "\n  " (itoa i) ". " (car e))))
          (setq idx (getint (strcat "\n选择图案序号 <1>: ")))
          (if (or (null idx) (< idx 1) (> idx n)) (setq idx 1))
          (setq name (car (nth (1- idx) entries)))
          (setq p1 (getpoint "\n第一角点: "))
          (if (null p1)
            (progn (princ "\n已取消。") nil)
            (progn
              (setq p2 (getcorner p1 "\n另一角点: "))
              (if (null p2)
                (progn (princ "\n已取消。") nil)
                (progn
                  (setq scale (getreal "\n填充比例 <1.0>: "))
                  (if (or (null scale) (<= scale 0.0)) (setq scale 1.0))
                  (setq ang (getreal "\n填充角度 <0>: "))
                  (if (null ang) (setq ang 0.0))
                  (at-hatch:undo-start)
                  (setq hatch
                        (at-hatch:ai-pat-preview file name p1 p2 scale ang))
                  (at-hatch:undo-end)
                  (if hatch
                    (progn
                      (princ (strcat "\n已绘制图案 " name "（比例 "
                                     (rtos scale 2 2) "，角度 "
                                     (rtos ang 2 2) "）。"))
                      hatch)
                    (progn (princ "\n绘制失败。") nil)))))))))))

(defun at-hatch:pat-erase (/ ss i e n)
  "清除 PAT-PREVIEW 图层上的全部预览图元（含图案预览表与 PAT 文件预览）。"
  "删除的图元数量"
  "(at-hatch:pat-erase)"
  (setq n 0)
  (if (tblsearch "LAYER" "PAT-PREVIEW")
    (progn
      (setq ss (ssget "_X" '((8 . "PAT-PREVIEW"))))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq e (ssname ss i))
            (if (entdel e) (setq n (1+ n)))
            (setq i (1+ i)))))))
  (princ (strcat "\n已删除 " (itoa n) " 个预览图元。"))
  n)

(defun at-hatch:preview-cell (name x0 y1 w h scale lyr /
                              recs pts border hatch txt th x1 y0)
  "内部：画一个图案样块（边框 + 填充 + 名称），返回图元列表。"
  "图元 ename 列表，失败返回 nil"
  "(at-hatch:preview-cell \"ANSI31\" 0.0 60.0 90.0 60.0 20.0 \"PAT-PREVIEW\")"
  (setq x1 (+ x0 w))
  (setq y0 (- y1 h))
  (setq pts (list (list x0 y0) (list x1 y0) (list x1 y1) (list x0 y1)))
  (setq th (max 2.0 (* 0.22 h)))
  (setq recs (list (list 1 pts '(0.0 0.0 0.0 0.0))))
  (setq border (entity:make-lwpolyline pts nil 0 1 0.0))
  (if border (at-hatch:preview-put border lyr 8))
  (setq hatch (at-hatch:hatch-from-loops recs name scale 0.0 lyr nil))
  (if hatch
      (at-hatch:preview-put hatch lyr 256)
      ;; 图案不可用时画一条红色对角线，避免“空白块”看不出是失败
      (if (setq txt (entity:make-line (list x0 y0) (list x1 y1)))
          (at-hatch:preview-put txt lyr 1)))
  (setq txt (entity:make-text name
                              (list x0 (- y0 th 3.0))
                              th 0.0 0.75 0 "LB"))
  (if txt (at-hatch:preview-put txt lyr 7))
  (vl-remove nil (list border hatch txt)))

(defun at-hatch:ai-pattern-table
  (names pt cell-w cell-h cols scale / lyr w h n i name res row col
   x0 y1 row-pitch col-pitch)
  "AI 调用入口：把图案画成样块表（边框 + 填充 + 名称），不弹对话框。
  names: 图案名列表，nil = 内置图案 + CAD 标准图案；
  pt: 表格左上角点，nil = (0 0)；
  cell-w/cell-h: 单元格宽高，缺省 90/60；
  cols: 列数，缺省 6；scale: 填充比例，缺省 20。
  示例: (at-hatch:ai-pattern-table '(\"ANSI31\" \"BRICK\" \"PARQUET\") '(0 0) 90 60 3 20)"
  "生成的图元 ename 列表，失败返回 nil"
  "(at-hatch:ai-pattern-table nil '(0 0) 90 60 6 20)"
  (if (null names)
      (setq names (append (mapcar 'car (at-hatch:builtin-patterns))
                          (at-hatch:std-patterns))))
  (if (null names)
      (progn (princ "at-hatch:ai-pattern-table names 为空") nil)
      (progn
        (setq pt (if pt pt '(0 0)))
        (setq w (max 10.0 (if (numberp cell-w) (float cell-w) 90.0)))
        (setq h (max 10.0 (if (numberp cell-h) (float cell-h) 60.0)))
        (setq cols (max 1 (fix (if (numberp cols) cols 6))))
        (setq scale (if (and (numberp scale) (> scale 0.0)) scale 20.0))
        (setq lyr (at-hatch:preview-layer))
        (setq row-pitch (+ h (* 0.22 h) 11.0))
        (setq col-pitch (+ w 10.0))
        (setq n (length names))
        (setq i 0 res nil)
        (while (< i n)
          (setq name (nth i names))
          (setq row (fix (/ i cols)))
          (setq col (rem i cols))
          (setq x0 (+ (car pt) (* col col-pitch)))
          (setq y1 (- (cadr pt) (* row row-pitch)))
          (setq res (append res
                            (at-hatch:preview-cell
                             name x0 y1 w h scale lyr)))
          (setq i (1+ i)))
        res)))

(defun at-hatch:pattern-table (/ pt cols w h scale res)
  "图案预览表：在当前图中画出图案样块并标注名称。"
  "生成的图元 ename 列表"
  "(at-hatch:pattern-table)"
  (setq pt (getpoint "\n表格左上角 <0,0>:"))
  (if (null pt) (setq pt '(0 0 0)))
  (setq cols (getint "\n列数 <6>:"))
  (if (null cols) (setq cols 6))
  (setq w (getdist "\n单元格宽 <90>:"))
  (if (null w) (setq w 90.0))
  (setq h (getdist "\n单元格高 <60>:"))
  (if (null h) (setq h 60.0))
  (setq scale (getreal "\n填充比例 <20>:"))
  (if (null scale) (setq scale 20.0))
  (command "_.UNDO" "Begin")
  (setq res (at-hatch:ai-pattern-table nil pt w h cols scale))
  (command "_.UNDO" "End")
  (if res
      (princ (strcat "\n已生成图案预览表，共 "
                     (itoa (length res)) " 个图元。"))
      (princ "\n生成图案预览表失败。"))
  (princ))
