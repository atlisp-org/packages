;; ============================================================
;; mini-arch / axis.lsp — 轴网
;; 直线轴网、轴线编号、修改轴线编号
;; ============================================================

(defun at-arch:ai-axis-line (origin x-spacings y-spacings / layer ents
                                      xs ys x0 y0 xe ye ext i j yv xh)
  "AI 入口：生成直线轴网（轴线+轴号圆圈+编号）。
  origin: 起点 (x y) 或 (x y z)。
  x-spacings: X 向轴线间距表（竖轴线之间的间距，数字表）；nil 不画竖轴。
  y-spacings: Y 向轴线间距表（横轴线之间的间距，数字表）；nil 不画横轴。
  竖轴编号 A,B,C...（顶部）；横轴编号 1,2,3...（左侧）。
  return: ename 列表（轴线+圆圈+文字）；两方向均为空返回 nil。
  示例: (at-arch:ai-axis-line '(0 0) '(3600 3600 4200) '(3300 3900 3300))"
  (if (or (null origin) (and (null x-spacings) (null y-spacings)))
      (progn
        (princ "ai-axis-line: 参数错误（origin 缺失或两方向间距均为空）。")
        nil)
      (progn
        (setq layer (at-arch:set-layer *at-arch:layer-axis* 1))
        (setq x0 (car origin))
        (setq y0 (cadr origin))
        (setq xe (if x-spacings (+ x0 (apply (function +) x-spacings)) x0))
        (setq ye (if y-spacings (+ y0 (apply (function +) y-spacings)) y0))
        (setq ext 1000)
        (setq ents nil)
        ;; 竖轴线（Y 方向，编号 A,B,C...）
        (if x-spacings
            (progn
              (setq xh x0)
              (setq i 0)
              (while (<= i (length x-spacings))
                (if (= i 0) (setq xh x0)
                    (setq xh (+ xh (nth (1- i) x-spacings))))
                (setq ents (append ents
                         (list (at-arch:line (list xh (- y0 ext))
                               (list xh (+ ye ext)) layer 0.18))
                         (at-arch:axis-number
                          (list xh (+ ye ext))
                          (at-arch:letter (1+ i)) layer)))
                (setq i (1+ i)))))
        ;; 横轴线（X 方向，编号 1,2,3...）
        (if y-spacings
            (progn
              (setq yv y0)
              (setq j 0)
              (while (<= j (length y-spacings))
                (if (= j 0) (setq yv y0)
                    (setq yv (+ yv (nth (1- j) y-spacings))))
                (setq ents (append ents
                         (list (at-arch:line (list (- x0 ext) yv)
                               (list (+ xe ext) yv) layer 0.18))
                         (at-arch:axis-number
                          (list (- x0 ext) yv)
                          (itoa (1+ j)) layer)))
                (setq j (1+ j)))))
        (princ (strcat "\n轴网生成完毕，共 " (itoa (length ents)) " 个图元。"))
        ents)))

(defun at-arch:axis-line (/ origin xs ys)
  "直线轴网：输入起点、X 向（竖轴）间距、Y 向（横轴）间距，
生成轴线与轴号圆圈编号。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq origin (getpoint "\n指定轴网起点:"))
  (if origin
      (progn
        (setq xs (at-arch:parse-numbers
                  (getstring T
                    "\nX 向轴线间距表(竖轴间距,如 3600,3600,4200): ")))
        (setq ys (at-arch:parse-numbers
                  (getstring T
                    "\nY 向轴线间距表(横轴间距,如 3300,3900,3300): ")))
        (if (or xs ys)
            (progn
              (command "_.UNDO" "Begin")
              (at-arch:ai-axis-line origin xs ys)
              (command "_.UNDO" "End"))
            (princ "\n至少需要输入一个方向的间距。")))
  (princ)))

(defun at-arch:ai-axis-text (ents / layer hs vs res i e pts
                                       all-same x0 y0)
  "AI 入口：对已有轴线重新生成轴号圆圈+编号。
  ents: 轴线实体（直线/多段线）ename 列表或选择集。
  横轴按 Y 从下到上编号 1,2,3...（圆圈在左端）；
  竖轴按 X 从左到右编号 A,B,C...（圆圈在上端）。
  return: 新生成的 ename 列表；没有可用轴线返回 nil。
  示例: (at-arch:ai-axis-text (ssget '((0 . \"LINE LWPOLYLINE\"))))"
  (if (null ents)
      (progn (princ "ai-axis-text: 未选择实体。") nil)
      (progn
        (if (null entity:getdxf) (require 'entity:*))
        (setq ents (at-arch:ent-list ents))
        (setq hs nil)
        (setq vs nil)
        (foreach e ents
          (if (setq pts (at-arch:ent-pts e))
              (progn
                (setq x0 (car (car pts)))
                (setq y0 (cadr (car pts)))
                (setq all-same t)
                (foreach q pts
                  (if (> (distance (list (car q) y0) q) 1.0)
                      (setq all-same nil)))
                (if all-same
                    (setq hs (cons
                             (list e y0
                              (list (apply (function min)
                                       (mapcar (function car) pts))
                                    y0))
                             hs)))
                (setq all-same t)
                (foreach q pts
                  (if (> (distance (list x0 (cadr q)) q) 1.0)
                      (setq all-same nil)))
                (if all-same
                    (setq vs (cons (list e x0
                             (list x0
                                   (apply (function max)
                                          (mapcar (function cadr) pts))))
                        vs)))))
        (setq hs (vl-sort hs
               (function (lambda (a b) (< (cadr a) (cadr b))))))
        (setq vs (vl-sort vs
               (function (lambda (a b) (< (cadr a) (cadr b))))))
        (if (or hs vs)
            (progn
              (setq res nil)
              (setq i 0)
              (foreach h hs
                (setq layer (at-arch:ensure-layer
                             (entity:getdxf (car h) 8) nil))
                (at-arch:del-near (caddr h) (at-arch:ts 400) layer)
                (setq res (append res
                         (at-arch:axis-number (caddr h) (itoa (1+ i))
                           layer)))
                (setq i (1+ i)))
              (setq i 0)
              (foreach v vs
                (setq layer (at-arch:ensure-layer
                             (entity:getdxf (car v) 8) nil))
                (at-arch:del-near (caddr v) (at-arch:ts 400) layer)
                (setq res (append res
                         (at-arch:axis-number (caddr v)
                           (at-arch:letter (1+ i)) layer)))
                (setq i (1+ i)))
              (princ (strcat "\n已重新编号：横轴 " (itoa (length hs))
                    " 条，竖轴 " (itoa (length vs)) " 条。"))
              (reverse res))
            (progn
              (princ "ai-axis-text: 未找到水平/竖直轴线。")
              nil))))))

(defun at-arch:axis-text (/ ss)
  "轴线编号：选中已有轴线，重新生成轴号圆圈+编号。"
  "无"
  "示例: 运行命令后框选轴线"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE"))))
      (progn
        (command "_.UNDO" "Begin")
        (at-arch:ai-axis-text ss)
        (command "_.UNDO" "End"))
  (princ)))

(defun at-arch:ai-axis-update (texts start / is-letter n i e s
                                       num-list let-list)
  "AI 入口：修改已有轴号文字的编号（从 start 开始连续编号）。
  texts: 轴号文字 ename 列表或选择集。
  start: 起始编号字符串，数字(如 \"3\")或字母(如 \"C\")；
         数字只重编数字轴号，字母只重编字母轴号。
  数字轴号按 Y 从下到上排序；字母轴号按 X 从左到右排序。
  return: 被修改的文字 ename 列表；没有可修改的文字返回 nil。
  示例: (at-arch:ai-axis-update (ssget '((0 . \"TEXT\"))) \"3\")"
  (if (null texts)
      (progn (princ "ai-axis-update: 未选择文字。") nil)
      (progn
        (if (null entity:getdxf) (require 'entity:*))
        (setq texts (at-arch:ent-list texts))
        (if (null start) (setq start "1"))
        (setq is-letter (wcmatch (strcase start) "[A-Z]*"))
        (setq num-list nil)
        (setq let-list nil)
        (foreach e texts
          (if (at-arch:alive-p e)
              (progn
                (setq s (entity:getdxf e 1))
                (if (and s (wcmatch (strcase s) "[A-Z]*"))
                    (setq let-list (cons (list e (at-arch:2d
                                          (entity:getdxf e 10))) let-list))
                    (setq num-list (cons (list e (at-arch:2d
                                        (entity:getdxf e 10))) num-list))))))
        (if is-letter
            (progn
              (setq let-list (vl-sort let-list
                      (function (lambda (a b)
                         (if (/= (car (cadr a)) (car (cadr b)))
                             (< (car (cadr a)) (car (cadr b)))
                             (< (cadr (cadr a)) (cadr (cadr b))))))))
              (setq n (at-arch:letter-index start))
              (setq i 0)
              (foreach pair let-list
                (entity:putdxf (car pair) 1 (at-arch:letter (+ n i)))
                (setq i (1+ i)))
              (princ (strcat "\n已重编 " (itoa (length let-list))
                    " 个字母轴号。"))
              (mapcar (function car) let-list))
            (progn
              (setq num-list (vl-sort num-list
                      (function (lambda (a b)
                         (if (/= (cadr (cadr a)) (cadr (cadr b)))
                             (< (cadr (cadr a)) (cadr (cadr b)))
                             (< (car (cadr a)) (car (cadr b))))))))
              (if (vl-catch-all-error-p (vl-catch-all-apply 'read (list start)))
                  (progn (princ "ai-axis-update: 起始号不是数字。") nil)
                  (progn
                    (setq n (fix (read start)))
                    (setq i 0)
                    (foreach pair num-list
                      (entity:putdxf (car pair) 1 (itoa (+ n i)))
                      (setq i (1+ i)))
                    (princ (strcat "\n已重编 " (itoa (length num-list))
                          " 个数字轴号。"))
                    (mapcar (function car) num-list))))))))

(defun at-arch:axis-update (/ ss start)
  "修改轴线编号：选中已有轴号文字，从指定编号开始重新编号。"
  "无"
  "示例: 运行命令后框选轴号文字"
  (if (setq ss (ssget '((0 . "TEXT"))))
      (progn
        (setq start (getstring T "\n输入起始编号(数字或字母,如 3 或 C):"))
        (if (null start) (setq start "1"))
        (command "_.UNDO" "Begin")
        (at-arch:ai-axis-update ss start)
        (command "_.UNDO" "End"))
  (princ)))
