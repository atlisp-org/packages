;; ============================================================
;; at-arch/symbol.lsp — 标高/坐标/折断线/剖面符号/指北针
;; （iMini 复现：mini_bg / zb / mini_break_line / index_sec* /
;;   index_north）
;; 交互入口：at-arch:elev / at-arch:arch-coord / at-arch:break-line
;;           at-arch:sec-symbol / at-arch:north
;; AI 入口：at-arch:ai-elev / at-arch:ai-arch-coord
;;          at-arch:ai-break-line / at-arch:ai-sec-symbol
;;          at-arch:ai-north
;; ============================================================

(defun at-arch:ai-elev (pt value / layer res h w c1 c2)
  "AI 入口：建筑标高。在 pt 画倒三角标高符号与标高值文字。
pt: 标高基准点（符号最低点，list x y [z]）。
value: 标高值字符串（如 \"±0.000\"）；nil 取 0.000。
return: 创建实体 ename 列表；缺参返回 nil。
示例: (at-arch:ai-elev '(0 0) \"±0.000\")"
  (if (null pt)
      (progn (princ "ai-elev: pt 缺失。") nil)
      (progn
        (if (null value) (setq value "0.000"))
        (setq layer (at-arch:ensure-layer *at-arch:layer-elev* 7))
        (setq h (at-arch:ts 250))
        (setq w (at-arch:ts 250))
        (setq pt (at-arch:2d pt))
        (setq c1 (list (- (car pt) (/ w 2.0)) (+ (cadr pt) h)))
        (setq c2 (list (+ (car pt) (/ w 2.0)) (+ (cadr pt) h)))
        (setq res nil)
        (setq res (cons (at-arch:pline (list pt c1 c2) 1 layer) res))
        (setq res (cons
                     (at-arch:text
                      value
                      (list (+ (car pt) (* w 0.9)) (+ (cadr pt) (* h 0.5)))
                      (at-arch:ts 200) 0 "L" layer)
                res))
        (setq res (vl-remove nil res))
        (if (null res)
            (progn (princ "ai-elev: 实体创建失败。") nil)
            (progn
              (princ (strcat "\n标高完成: " value))
              (reverse res))))))

(defun at-arch:elev (/ pt value)
  "建筑标高：点选基准点并输入标高值，
生成倒三角标高符号与标高值文字。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq pt (getpoint "\n指定标高基准点:"))
  (if pt
      (progn
        (setq value (getstring T "\n标高值(回车 0.000):"))
        (if (eq value "") (setq value "0.000"))
        (startundo)
        (at-arch:ai-elev pt value)
        (endundo)))
  (princ))

(defun at-arch:ai-arch-coord (pt x y / layer res)
  "AI 入口：建筑坐标。在 pt 画十字坐标符号与 X=/Y= 坐标文字。
pt: 坐标点（list x y [z]）。
x: X 坐标值（数字）；nil 取 pt 的 X 坐标。
y: Y 坐标值（数字）；nil 取 pt 的 Y 坐标。
return: 创建实体 ename 列表；缺参返回 nil。
示例: (at-arch:ai-arch-coord '(1000 2000) nil nil)"
  (if (null pt)
      (progn (princ "ai-arch-coord: pt 缺失。") nil)
      (progn
        (if (null x) (setq x (car pt)))
        (if (null y) (setq y (cadr pt)))
        (setq layer (at-arch:ensure-layer *at-arch:layer-coord* 7))
        (setq pt (at-arch:2d pt))
        (setq res nil)
        (setq res (cons
                     (at-arch:line
                      (list (- (car pt) (at-arch:ts 200)) (cadr pt))
                      (list (+ (car pt) (at-arch:ts 200)) (cadr pt))
                      layer 0.18)
                res))
        (setq res (cons
                     (at-arch:line
                      (list (car pt) (- (cadr pt) (at-arch:ts 200)))
                      (list (car pt) (+ (cadr pt) (at-arch:ts 200)))
                      layer 0.18)
                res))
        (setq res (cons
                     (at-arch:text
                      (strcat "X=" (rtos x 2 3))
                      (list (+ (car pt) (at-arch:ts 250))
                            (- (cadr pt) (at-arch:ts 250)))
                      (at-arch:ts 200) 0 "L" layer)
                res))
        (setq res (cons
                     (at-arch:text
                      (strcat "Y=" (rtos y 2 3))
                      (list (+ (car pt) (at-arch:ts 250))
                            (- (cadr pt) (at-arch:ts 550)))
                      (at-arch:ts 200) 0 "L" layer)
                res))
        (setq res (vl-remove nil res))
        (if (null res)
            (progn (princ "ai-arch-coord: 实体创建失败。") nil)
            (progn
              (princ "\n建筑坐标完成。")
              (reverse res))))))

(defun at-arch:arch-coord (/ pt s x y)
  "建筑坐标：点选坐标点并输入坐标值（回车取点坐标），
生成十字坐标符号与 X=/Y= 文字。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq pt (getpoint "\n指定坐标点:"))
  (if pt
      (progn
        (setq s (getstring T "\nX 坐标(回车取点 X 坐标):"))
        (if (eq s "")
            (setq x nil)
            (progn
              (setq x (vl-catch-all-apply 'read (list s)))
              (if (or (vl-catch-all-error-p x) (not (numberp x)))
                  (progn
                    (princ "\nX 坐标无效，按点坐标处理。")
                    (setq x nil)))))
        (setq s (getstring T "\nY 坐标(回车取点 Y 坐标):"))
        (if (eq s "")
            (setq y nil)
            (progn
              (setq y (vl-catch-all-apply 'read (list s)))
              (if (or (vl-catch-all-error-p y) (not (numberp y)))
                  (progn
                    (princ "\nY 坐标无效，按点坐标处理。")
                    (setq y nil)))))
        (startundo)
        (at-arch:ai-arch-coord pt x y)
        (endundo)))
  (princ))

(defun at-arch:ai-break-line (p1 p2 teeth amp gap / layer len vec
                                               perp off pts res i f base sgn)
  "AI 入口：折断线。在 p1 与 p2 之间生成两条平行锯齿折断线
（平面折断表示）。
p1: 折断线起点（list x y [z]）。
p2: 折断线终点（list x y [z]）。
teeth: 锯齿数（整数，nil 取 10）。
amp: 振幅（mm）；nil 取总长 1/12。
gap: 两线间距（mm）；nil 取 (at-arch:ts 100)。
return: 创建实体 ename 列表；缺参或两点重合返回 nil。
示例: (at-arch:ai-break-line '(0 0) '(4000 0) nil nil nil)"
  (if (or (null p1) (null p2))
      (progn (princ "ai-break-line: p1/p2 缺失。") nil)
      (progn
        (setq len (distance p1 p2))
        (if (< len 1.0)
            (progn (princ "ai-break-line: 两点距离过小。") nil)
            (progn
              (if (null teeth) (setq teeth 10))
              (if (null amp) (setq amp (/ len 12.0)))
              (if (null gap) (setq gap (at-arch:ts 100)))
              (setq layer (at-arch:ensure-layer *at-arch:layer-symbol* 7))
              (setq vec (at-arch:vec p1 p2))
              (setq perp (at-arch:normal p1 p2))
              (setq off (at-arch:vscale perp (/ gap 2.0)))
              ;; 锯齿折点（含端点）
              (setq pts (list (at-arch:2d p1)))
              (setq i 1)
              (while (< i teeth)
                (setq f (/ i teeth))
                (setq base (at-arch:vadd p1 (at-arch:vscale vec f)))
                (setq sgn (if (= (rem i 2) 0) 1 -1))
                (setq pts (append pts
                               (list (at-arch:vadd
                                      base
                                      (at-arch:vscale perp (* sgn amp))))))
                (setq i (1+ i)))
              (setq pts (append pts (list (at-arch:2d p2))))
              ;; 两条平行锯齿线（±gap/2 偏移）
              (setq res nil)
              (setq res (cons
                           (at-arch:pline
                            (mapcar (function (lambda (p)
                                        (at-arch:vadd p off))) pts)
                            0 layer)
                       res))
              (setq res (cons
                           (at-arch:pline
                            (mapcar (function (lambda (p)
                                        (at-arch:vadd p (at-arch:vscale off -1))))
                             pts)
                            0 layer)
                       res))
              (setq res (vl-remove nil res))
              (if (null res)
                  (progn (princ "ai-break-line: 实体创建失败。") nil)
                  (progn
                    (princ "\n折断线完成。")
                    (reverse res))))))))

(defun at-arch:break-line (/ p1 p2 teeth amp gap s)
  "折断线：输入折断线两端点与参数，
生成两条平行锯齿折断线（平面折断表示）。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq p1 (getpoint "\n指定折断线起点:"))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n指定折断线终点:"))
        (if p2
            (progn
              (setq teeth (getint "\n锯齿数<10>:"))
              (if (null teeth) (setq teeth 10))
              (setq s (getstring T "\n振幅(回车取总长 1/12):"))
              (if (eq s "")
                  (setq amp nil)
                  (progn
                    (setq amp (vl-catch-all-apply 'read (list s)))
                    (if (or (vl-catch-all-error-p amp)
                            (not (numberp amp)))
                        (setq amp nil))))
              (setq s (getstring T "\n两线间距(回车默认):"))
              (if (eq s "")
                  (setq gap nil)
                  (progn
                    (setq gap (vl-catch-all-apply 'read (list s)))
                    (if (or (vl-catch-all-error-p gap)
                            (not (numberp gap)))
                        (setq gap nil))))
              (startundo)
              (at-arch:ai-break-line p1 p2 teeth amp gap)
              (endundo)))))
  (princ))

(defun at-arch:ai-sec-symbol (p1 p2 num / layer perp e ang res)
  "AI 入口：剖面符号。生成粗实线剖切线、垂直视线箭头与剖面编号。
p1: 剖切线起点（视线箭头一侧，list x y [z]）。
p2: 剖切线终点（list x y [z]）。
num: 剖面编号字符串（如 \"1-1\"）；nil 取 1-1。
return: 创建实体 ename 列表；缺参或两点重合返回 nil。
示例: (at-arch:ai-sec-symbol '(0 0) '(6000 0) \"1-1\")"
  (if (or (null p1) (null p2))
      (progn (princ "ai-sec-symbol: p1/p2 缺失。") nil)
      (progn
        (if (null num) (setq num "1-1"))
        (if (< (distance p1 p2) 1.0)
            (progn (princ "ai-sec-symbol: 两点距离过小。") nil)
            (progn
              (setq layer (at-arch:ensure-layer *at-arch:layer-symbol* 7))
              (setq perp (at-arch:normal p1 p2))
              (setq res nil)
              ;; 剖切线（粗实线）
              (setq res (cons (at-arch:line p1 p2 layer 0.5) res))
              ;; 视线箭头（p1 处垂直短线 + 箭头）
              (setq e (at-arch:vadd (at-arch:2d p1)
                       (at-arch:vscale perp (at-arch:ts 400))))
              (setq res (cons (at-arch:line (at-arch:2d p1) e layer 0.5)
                    res))
              (setq ang (angle '(0 0) perp))
              (setq res (cons
                           (at-arch:line
                            e
                            (list (+ (car e)
                                     (* (at-arch:ts 150) (cos (+ ang 2.7))))
                                  (+ (cadr e)
                                     (* (at-arch:ts 150) (sin (+ ang 2.7)))))
                            layer 0.18)
                       res))
              (setq res (cons
                           (at-arch:line
                            e
                            (list (+ (car e)
                                     (* (at-arch:ts 150) (cos (- ang 2.7))))
                                  (+ (cadr e)
                                     (* (at-arch:ts 150) (sin (- ang 2.7)))))
                            layer 0.18)
                       res))
              ;; 剖面编号
              (setq res (cons
                           (at-arch:text
                            num
                            (at-arch:vadd e (at-arch:vscale perp
                                                (at-arch:ts 250)))
                            (at-arch:ts 250) 0 "M" layer)
                       res))
              (setq res (vl-remove nil res))
              (if (null res)
                  (progn (princ "ai-sec-symbol: 实体创建失败。") nil)
                  (progn
                    (princ (strcat "\n剖面符号完成: " num))
                    (reverse res))))))))

(defun at-arch:sec-symbol (/ p1 p2 num)
  "剖面符号：输入剖切线两端点与剖面编号，
生成粗实线剖切线、视线箭头与编号文字。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq p1 (getpoint "\n指定剖切线起点(视线箭头一侧):"))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n指定剖切线终点:"))
        (if p2
            (progn
              (setq num (getstring T "\n剖面编号(回车 1-1):"))
              (if (eq num "") (setq num "1-1"))
              (startundo)
              (at-arch:ai-sec-symbol p1 p2 num)
              (endundo)))))
  (princ))

(defun at-arch:ai-north (pt / layer r res)
  "AI 入口：指北针。在 pt 画圆形外框、指向北的针与 N 字。
pt: 指北针中心（list x y [z]）。
return: 创建实体 ename 列表；缺参返回 nil。
示例: (at-arch:ai-north '(1000 1000))"
  (if (null pt)
      (progn (princ "ai-north: pt 缺失。") nil)
      (progn
        (setq layer (at-arch:ensure-layer *at-arch:layer-symbol* 7))
        (setq r (at-arch:ts 300))
        (setq pt (at-arch:2d pt))
        (setq res nil)
        (setq res (cons (at-arch:circle pt r layer) res))
        ;; 指北针（菱形）
        (setq res (cons
                     (at-arch:pline
                      (list pt
                            (list (- (car pt) (* r 0.2))
                                  (+ (cadr pt) (* r 0.5)))
                            (list (car pt) (+ (cadr pt) r))
                            (list (+ (car pt) (* r 0.2))
                                  (+ (cadr pt) (* r 0.5))))
                      1 layer)
                res))
        (setq res (cons
                     (at-arch:text
                      "N"
                      (list (car pt) (- (cadr pt) (* r 0.5)))
                      (at-arch:ts 200) 0 "M" layer)
                res))
        (setq res (vl-remove nil res))
        (if (null res)
            (progn (princ "ai-north: 实体创建失败。") nil)
            (progn
              (princ "\n指北针完成。")
              (reverse res))))))

(defun at-arch:north (/ pt)
  "指北针：点选中心，生成圆形外框、指北针与 N 字。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq pt (getpoint "\n指定指北针中心:"))
  (if pt
      (progn
        (startundo)
        (at-arch:ai-north pt)
        (endundo)))
  (princ))
