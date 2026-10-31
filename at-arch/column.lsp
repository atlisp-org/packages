;; ============================================================
;; mini-arch / column.lsp — 柱
;; 画柱（方/圆/十字/L/T）、多段线变柱
;; ============================================================

(defun at-arch:col-shape-pts (pt shape w h / a res)
  "生成柱截面点表（以 pt 为中心）。
shape: \"r\"矩形 / \"o\"圆 / \"c\"十字 / \"l\"L型 / \"t\"T型。
圆截面返回 nil（用圆实体表示）。"
  "list/nil"
  "(at-arch:col-shape-pts '(0 0) \"r\" 400 600)"
  (setq a (max 50.0 (min (/ w 3.0) (/ h 3.0))))
  (cond
    ((eq shape "r")
     (list (list (- (car pt) (/ w 2.0)) (- (cadr pt) (/ h 2.0)))
           (list (+ (car pt) (/ w 2.0)) (- (cadr pt) (/ h 2.0)))
           (list (+ (car pt) (/ w 2.0)) (+ (cadr pt) (/ h 2.0)))
           (list (- (car pt) (/ w 2.0)) (+ (cadr pt) (/ h 2.0)))))
    ((eq shape "c")
     (list (list (- (car pt) a) (+ (cadr pt) (/ h 2.0)))
           (list (+ (car pt) a) (+ (cadr pt) (/ h 2.0)))
           (list (+ (car pt) a) (+ (cadr pt) a))
           (list (+ (car pt) (/ w 2.0)) (+ (cadr pt) a))
           (list (+ (car pt) (/ w 2.0)) (- (cadr pt) a))
           (list (+ (car pt) a) (- (cadr pt) a))
           (list (- (car pt) a) (- (cadr pt) a))
           (list (- (car pt) (/ w 2.0)) (- (cadr pt) a))
           (list (- (car pt) (/ w 2.0)) (+ (cadr pt) a))
           (list (- (car pt) a) (+ (cadr pt) a))))
    ((eq shape "l")
     (list (list (- (car pt) (/ w 2.0)) (- (cadr pt) (/ h 2.0)))
           (list (+ (car pt) (/ w 2.0)) (- (cadr pt) (/ h 2.0)))
           (list (+ (car pt) (/ w 2.0)) (+ (cadr pt) (- a (/ h 2.0))))
           (list (- (car pt) (- a (/ w 2.0))) (+ (cadr pt) (- a (/ h 2.0))))
           (list (- (car pt) (- a (/ w 2.0))) (+ (cadr pt) (/ h 2.0)))
           (list (- (car pt) (/ w 2.0)) (+ (cadr pt) (/ h 2.0)))))
    ((eq shape "t")
     (list (list (- (car pt) (/ w 2.0)) (- (cadr pt) (/ h 2.0)))
           (list (+ (car pt) (/ w 2.0)) (- (cadr pt) (/ h 2.0)))
           (list (+ (car pt) (/ w 2.0)) (+ (cadr pt) (- a (/ h 2.0))))
           (list (+ (car pt) (- (/ w 4.0) (/ a 2.0)))
                 (+ (cadr pt) (- a (/ h 2.0))))
           (list (+ (car pt) (- (/ w 4.0) (/ a 2.0))) (+ (cadr pt) (/ h 2.0)))
           (list (- (car pt) (- (/ w 4.0) (/ a 2.0))) (+ (cadr pt) (/ h 2.0)))
           (list (- (car pt) (- (/ w 4.0) (/ a 2.0)))
                 (+ (cadr pt) (- a (/ h 2.0))))
           (list (- (car pt) (/ w 2.0)) (+ (cadr pt) (- a (/ h 2.0)))))))
  (if (eq shape "o") nil res))

(defun at-arch:ai-column (pt shape w h label / layer res e c tt)
  "AI 入口：在 pt 画柱截面。
pt: 柱中心 (x y)。
shape: \"r\"方柱 / \"o\"圆柱 / \"c\"十字柱 / \"l\"L型柱 / \"t\"T型柱。
w: 宽（圆柱为直径）。h: 高（圆柱忽略）。
label: 柱编号文字（如 \"KZ1\"），nil 不画文字。
return: 图元 ename 列表（截面+文字）；参数非法返回 nil。
示例: (at-arch:ai-column '(0 0) \"r\" 400 600 \"KZ1\")"
  (if (or (null pt) (null shape) (null w) (<= w 0))
      (progn (princ "ai-column: 参数缺失（pt/shape/w）。") nil)
      (progn
        (if (null h) (setq h w))
        (setq layer (at-arch:set-layer *at-arch:layer-column* 2))
        (setq res nil)
        (cond
          ((eq shape "o")
           (setq e (at-arch:circle pt (/ w 2.0) layer))
           (setq res (list e)))
          ((member shape '("r" "c" "l" "t"))
           (setq c (at-arch:col-shape-pts pt shape w h))
           (if c
               (setq e (at-arch:pline c 1 layer))
               (progn
                 (princ (strcat "ai-column: 未知柱形 " shape "。"))
                 (setq e nil)))
           (if e (setq res (list e))))
          (t
           (princ (strcat "ai-column: 未知柱形 " shape "。"))
           (setq res nil)))
        (if (and label res)
            (progn
              (setq tt (at-arch:text label pt (at-arch:ts 250) 0 "M" layer))
              (if tt (setq res (append res (list tt))))))
        (if res
            (progn
              (princ (strcat "\n画柱完成（" shape "）。"))
              res)
            nil))))

(defun at-arch:column (/ shape w h pt label)
  "画柱：选择柱形（方/圆/十字/L/T），输入宽高与中心点，
  可选输入柱编号。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq shape (getkword "\n柱形: 方[R]/圆(O)/十字(C)/L[L]/T[T]<R>:"))
  (if (null shape) (setq shape "R"))
  (setq w (getint (strcat "\n柱宽<400>:")))
  (if (null w) (setq w 400))
  (setq h (getint (strcat "\n柱高<400>:")))
  (if (null h) (setq h 400))
  (if (setq pt (getpoint "\n柱中心:"))
      (progn
        (setq label (getstring T "\n柱编号(如 KZ1,回车跳过):"))
        (command "_.UNDO" "Begin")
        (at-arch:ai-column pt (strcase shape T) w h
          (if (eq label "") nil label))
        (command "_.UNDO" "End")))
  (princ))

(defun at-arch:ai-pline2column (ents hatch / layer res e n)
  "AI 入口：多段线变柱。选中线/多段线，改到柱图层，可选填充。
ents: ename 列表或选择集（LINE/LWPOLYLINE/CIRCLE）。
hatch: t 用 ANSI31 图案填充（默认 nil 不填充）。
return: 被处理的 ename 列表；无有效实体返回 nil。
示例: (at-arch:ai-pline2column (ssget) t)"
  (if (null ents)
      (progn (princ "ai-pline2column: 未选择实体。") nil)
      (progn
        (if (null entity:putdxf) (require 'entity:*))
        (if (null entity:addhatch) (require 'entity:*))
        (setq layer (at-arch:ensure-layer *at-arch:layer-column* 2))
        (setq res nil)
        (foreach e (at-arch:ent-list ents)
          (if (and (at-arch:alive-p e)
                   (member (cdr (assoc 0 (entget e)))
                           '("LINE" "LWPOLYLINE" "CIRCLE")))
              (progn
                (entity:putdxf e 8 layer)
                (if hatch
                    (progn
                      (vl-catch-all-apply
                       'entity:addhatch (list (list e) nil "ANSI31")))
                    nil)
                (setq res (cons e res)))))
        (if res
            (progn
              (princ (strcat "\n多段线变柱完成，" (itoa (length res)) " 个实体。"))
              (reverse res))
            (progn
              (princ "ai-pline2column: 没有有效的线/圆实体。")
              nil)))))

(defun at-arch:pline2column (/ ss hatch)
  "多段线变柱：选中线/多段线/圆，改到柱图层并可选填充。"
  "无"
  "示例: 运行命令后框选线条"
  (if (setq ss (ssget '((0 . "LINE LWPOLYLINE CIRCLE"))))
      (progn
        (setq hatch (getkword "\n填充[Y/N]<N>:"))
        (if (eq hatch "Y") (setq hatch t) (setq hatch nil))
        (command "_.UNDO" "Begin")
        (at-arch:ai-pline2column ss hatch)
        (command "_.UNDO" "End")))
  (princ))
