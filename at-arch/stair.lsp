;; ============================================================
;; at-arch/stair.lsp — 楼梯/电梯（iMini 复现：mini_stair_plan / elevator）
;; 交互入口：at-arch:stair-plan / at-arch:elevator
;; AI 入口：at-arch:ai-stair-plan / at-arch:ai-elevator
;; ============================================================

(defun at-arch:ai-stair-plan (p1 p2 treads width / layer len vec
                                               perp off res i mid a2 ang)
  "AI 入口：普通楼梯平面。按中心线 p1→p2 生成梯段双线轮廓、
踏步分格线与上行箭头。
p1: 梯段中心线起点（list x y [z]）。
p2: 梯段中心线终点（list x y [z]）。
treads: 踏步数（整数，nil 取 10）。
width: 梯段宽度（mm，nil 取 1000）。
return: 创建实体 ename 列表；缺参或两点重合返回 nil。
示例: (at-arch:ai-stair-plan '(0 0) '(6000 0) 10 1000)"
  (if (or (null p1) (null p2))
      (progn (princ "ai-stair-plan: p1/p2 缺失。") nil)
      (progn
        (if (null treads) (setq treads 10))
        (if (null width) (setq width 1000))
        (setq len (distance p1 p2))
        (if (< len 1.0)
            (progn (princ "ai-stair-plan: 两点距离过小。") nil)
            (progn
              (setq layer (at-arch:ensure-layer *at-arch:layer-stair* 7))
              (setq vec (at-arch:vec p1 p2))
              (setq perp (at-arch:normal p1 p2))
              (setq off (at-arch:vscale perp (/ width 2.0)))
              (setq res nil)
              ;; 梯段轮廓（矩形）
              (setq res (cons
                           (at-arch:pline
                            (list (at-arch:vadd p1 off)
                                  (at-arch:vadd p2 off)
                                  (at-arch:vadd p2 (at-arch:vscale off -1))
                                  (at-arch:vadd p1 (at-arch:vscale off -1)))
                            1 layer)
                       res))
              ;; 踏步分格线（treads-1 条）
              (setq i 1)
              (while (< i treads)
                (setq mid (at-arch:vadd p1 (at-arch:vscale vec (/ i treads))))
                (setq res (cons
                             (at-arch:line
                              (at-arch:vadd mid (at-arch:vscale off -1))
                              (at-arch:vadd mid off)
                              layer 0.18)
                         res))
                (setq i (1+ i)))
              ;; 上行箭头（中心线中点→终点 + 箭头）
              (setq a2 (at-arch:2d p2))
              (setq res (cons
                           (at-arch:line
                            (at-arch:vadd p1 (at-arch:vscale vec 0.5))
                            a2 layer 0.18)
                       res))
              (setq ang (angle '(0 0) vec))
              (setq res (cons
                           (at-arch:line
                            a2
                            (list (+ (car a2)
                                     (* (at-arch:ts 200) (cos (+ ang 2.6))))
                                  (+ (cadr a2)
                                     (* (at-arch:ts 200) (sin (+ ang 2.6)))))
                            layer 0.18)
                       res))
              (setq res (cons
                           (at-arch:line
                            a2
                            (list (+ (car a2)
                                     (* (at-arch:ts 200) (cos (- ang 2.6))))
                                  (+ (cadr a2)
                                     (* (at-arch:ts 200) (sin (- ang 2.6)))))
                            layer 0.18)
                       res))
              ;; “上”字
              (setq res (cons
                           (at-arch:text
                            "上"
                            (at-arch:vadd a2
                             (at-arch:vscale (list (cos ang) (sin ang))
                              (at-arch:ts 250)))
                            (at-arch:ts 250) 0 "M" layer)
                       res))
              (setq res (vl-remove nil res))
              (if (null res)
                  (progn (princ "ai-stair-plan: 实体创建失败。") nil)
                  (progn
                    (princ (strcat "\n楼梯平面完成，" (itoa (length res))
                           " 个实体。"))
                    (reverse res))))))))

(defun at-arch:stair-plan (/ p1 p2 treads width)
  "楼梯平面：输入梯段中心线两端点、踏步数与宽度，
生成双线梯段轮廓、踏步分格线与上行箭头。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq p1 (getpoint "\n指定梯段中心线起点:"))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n指定梯段中心线终点:"))
        (if p2
            (progn
              (setq treads (getint "\n踏步数<10>:"))
              (if (null treads) (setq treads 10))
              (setq width (getreal "\n梯段宽度<1000>:"))
              (if (null width) (setq width 1000))
              (command "_.UNDO" "Begin")
              (at-arch:ai-stair-plan p1 p2 treads width)
              (command "_.UNDO" "End")))))
  (princ))

(defun at-arch:ai-elevator (p1 p2 label / layer c1 c2 c3 c4
                                               yb x1 x2 res)
  "AI 入口：电梯轿厢平面。按对角角点 p1/p2 生成轿厢轮廓、
对角 X、门线与编号文字。
p1: 轿厢一个对角角点（list x y [z]）。
p2: 轿厢另一个对角角点（list x y [z]）。
label: 编号字符串（如 \"D1\"）；nil 不画编号文字。
return: 创建实体 ename 列表；缺参或两点重合返回 nil。
示例: (at-arch:ai-elevator '(0 0) '(1600 1600) \"D1\")"
  (if (or (null p1) (null p2))
      (progn (princ "ai-elevator: p1/p2 缺失。") nil)
      (if (< (distance p1 p2) 1.0)
          (progn (princ "ai-elevator: 两点距离过小。") nil)
          (progn
            (setq layer (at-arch:ensure-layer *at-arch:layer-elevator* 7))
            (setq c1 (at-arch:2d p1))
            (setq c2 (at-arch:2d p2))
            (setq c3 (list (car c2) (cadr c1)))
            (setq c4 (list (car c1) (cadr c2)))
            (setq res nil)
            ;; 轿厢轮廓 + 对角 X
            (setq res (cons (at-arch:pline (list c1 c2 c3 c4) 1 layer)
                  res))
            (setq res (cons (at-arch:line c1 c2 layer 0.18) res))
            (setq res (cons (at-arch:line c3 c4 layer 0.18) res))
            ;; 门线（下侧边，居中 60% 宽度，向内缩进）
            (setq yb (min (cadr c1) (cadr c2)))
            (setq x1 (min (car c1) (car c2)))
            (setq x2 (max (car c1) (car c2)))
            (setq res (cons
                         (at-arch:line
                          (list (+ x1 (* (- x2 x1) 0.2))
                                (+ yb (at-arch:ts 80)))
                          (list (+ x1 (* (- x2 x1) 0.8))
                                (+ yb (at-arch:ts 80)))
                          layer 0.18)
                     res))
            ;; 编号文字
            (if label
                (setq res (cons
                             (at-arch:text
                              label
                              (list (/ (+ (car c1) (car c2)) 2.0)
                                    (/ (+ (cadr c1) (cadr c2)) 2.0))
                              (at-arch:ts 250) 0 "M" layer)
                         res)))
            (setq res (vl-remove nil res))
            (if (null res)
                (progn (princ "ai-elevator: 实体创建失败。") nil)
                (progn
                  (princ (strcat "\n电梯轿厢平面完成，" (itoa (length res))
                         " 个实体。"))
                  (reverse res)))))))

(defun at-arch:elevator (/ p1 p2 label)
  "电梯轿厢平面：输入两个对角角点与编号，
生成轿厢轮廓、对角 X、门线与编号文字。"
  "无"
  "示例: 运行命令后按提示输入"
  (setq p1 (getpoint "\n指定轿厢第一角点:"))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n指定另一对角角点:"))
        (if p2
            (progn
              (setq label (getstring T "\n编号(回车不画):"))
              (if (eq label "") (setq label nil))
              (command "_.UNDO" "Begin")
              (at-arch:ai-elevator p1 p2 label)
              (command "_.UNDO" "End")))))
  (princ))
