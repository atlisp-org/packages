;; dim-tools-ext.lsp — 标注增强命令：交互入口 at-dim:xxx + AI 入口 at-dim:ai-xxx
;; 依赖 dim-core.lsp 的公共工具函数（at-dim:dxf / make-rotated-dim / proj-line 等）
(vl-load-com)
(require '(entity:* point:* curve:* pickset:* string:* layer:*))

;;; ---------------------------------------------------------------
;;; 命令 1：标注文字移动
;;; ---------------------------------------------------------------
(defun at-dim:project-on-dim (en pt / a base)
  "把点垂直投影到标注的尺寸线上(过 10 组码定义点、方向为标注测量方向)。"
  "三维点"
  "(at-dim:project-on-dim (car (entsel)) '(0 0 0))"
  (setq a (at-dim:dim-dir en)
    base (at-dim:dxf en 10))
  (if (null base) (setq base (at-dim:dxf en 13)))
  (if (and a base)
    (at-dim:proj-line pt base (polar base a 1.0))
    (at-dim:pt3 pt)))

(defun at-dim:ai-dim-move (dims newpt align / en cnt tp)
  "AI 入口：把标注文字移到 newpt；align 非 nil 时投影到各自尺寸线，返回成功个数。"
  "整数"
  "(at-dim:ai-dim-move (list en) '(100 50 0) t)"
  (setq cnt 0)
  (foreach en dims
    (if (and newpt (= (at-dim:dxf en 0) "DIMENSION"))
      (progn
        (setq tp (if align
            (at-dim:project-on-dim en newpt)
            (at-dim:pt3 newpt)))
        (if (at-dim:set-dim-text-pt en tp)
          (setq cnt (1+ cnt))))))
  cnt)

(defun at-dim:ai-dim-move-by (dims delta / en cnt tp)
  "AI 入口：把标注文字整体平移 delta(三维向量)，返回成功个数。"
  "整数"
  "(at-dim:ai-dim-move-by (list en) '(0 100 0))"
  (setq cnt 0)
  (foreach en dims
    (if (= (at-dim:dxf en 0) "DIMENSION")
      (progn
        (setq tp (at-dim:dim-text-pt en))
        (if (null tp) (setq tp (at-dim:dxf en 11)))
        (if tp
          (if (at-dim:set-dim-text-pt en (mapcar '+ (at-dim:pt3 tp) delta))
            (setq cnt (1+ cnt)))))))
  cnt)

(defun at-dim:dim-move (/ dims base p)
  "命令入口：选择标注后指定新位置，所选标注文字整体平移。"
  "成功个数 或 nil"
  "(at-dim:dim-move)"
  (if (setq dims (at-dim:select-dims "\n选择要移动文字的标注: "))
    (if (setq base (getpoint "\n指定文字新位置的基准点: "))
      (if (setq p (getpoint base "\n指定文字移动到的位置: "))
        (progn
          (at-dim:undo-begin)
          (setq dims (at-dim:ai-dim-move-by dims
              (mapcar '- (at-dim:pt3 p)
                (at-dim:pt3 base))))
          (at-dim:undo-end)
          (princ (strcat "\n已移动 " (itoa dims) " 个标注文字。"))
          dims)))))

;;; ---------------------------------------------------------------
;;; 命令 2：标注文字归位
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-home (dims / en cnt tp)
  "AI 入口：把标注文字复位到默认位置(尺寸线中点)，返回成功个数。"
  "整数"
  "(at-dim:ai-dim-home (list en))"
  (setq cnt 0)
  (foreach en dims
    (if (= (at-dim:dxf en 0) "DIMENSION")
      (progn
        (setq tp (at-dim:dim-default-pt en))
        (if (if tp (at-dim:set-dim-text-pt en tp))
          (setq cnt (1+ cnt))))))
  cnt)

(defun at-dim:dim-home (/ dims n)
  "命令入口：选择标注，清除文字手工位置使其归位。"
  "成功个数 或 nil"
  "(at-dim:dim-home)"
  (if (setq dims (at-dim:select-dims "\n选择要归位的标注: "))
    (progn
      (at-dim:undo-begin)
      (setq n (at-dim:ai-dim-home dims))
      (at-dim:undo-end)
      (princ (strcat "\n已归位 " (itoa n) " 个标注文字。"))
      n)))

;;; ---------------------------------------------------------------
;;; 命令 3：标注对齐
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-align (dims p1 p2 / en cnt tp)
  "AI 入口：把多个标注文字沿 p1→p2 直线垂直投影对齐，返回成功个数。"
  "整数"
  "(at-dim:ai-dim-align (list en) '(0 0 0) '(100 0 0))"
  (setq cnt 0)
  (foreach en dims
    (if (and p1 p2 (= (at-dim:dxf en 0) "DIMENSION"))
      (progn
        (setq tp (at-dim:dim-text-pt en))
        (if (null tp) (setq tp (at-dim:dxf en 11)))
        (if (null tp) (setq tp (at-dim:dxf en 10)))
        (if (if tp (at-dim:set-dim-text-pt en (at-dim:proj-line tp p1 p2)))
          (setq cnt (1+ cnt))))))
  cnt)

(defun at-dim:dim-align (/ dims p1 p2 n)
  "命令入口：选择标注，再取两点确定对齐基线，文字对齐到基线上。"
  "成功个数 或 nil"
  "(at-dim:dim-align)"
  (if (setq dims (at-dim:select-dims "\n选择要对齐的标注: "))
    (if (setq p1 (getpoint "\n指定对齐基线第一点: "))
      (if (setq p2 (getpoint p1 "\n指定对齐基线第二点: "))
        (progn
          (at-dim:undo-begin)
          (setq n (at-dim:ai-dim-align dims p1 p2))
          (at-dim:undo-end)
          (princ (strcat "\n已对齐 " (itoa n) " 个标注文字。"))
          n)))))

;;; ---------------------------------------------------------------
;;; 命令 4：合并标注
;;; ---------------------------------------------------------------
(defun at-dim:dim-merge-key (en / d a)
  "计算标注合并分组键(测量方向 + 尺寸线偏移)，非对齐/旋转标注返回 nil。"
  "(角度 偏移) 或 nil"
  "(at-dim:dim-merge-key (car (entsel)))"
  (setq d (entget en))
  (if (and (= (cdr (assoc 0 d)) "DIMENSION")
      (cdr (assoc 13 d))
      (cdr (assoc 14 d))
      (cdr (assoc 10 d))
      (setq a (at-dim:dim-dir en)))
    (progn
      (setq a (at-dim:norm-ang a))
      (list a (at-dim:dot2 (at-dim:pt2d (cdr (assoc 10 d)))
          (list (- (sin a)) (cos a)))))))

(defun at-dim:ai-dim-merge (dims / groups res g a base en d p13 p14
    t1 t2 tmin tmax off pmin pmax new)
  "AI 入口：把同一直线上连续的多个标注合并为一个总标注，返回新标注列表。"
  "新标注 ename 列表"
  "(at-dim:ai-dim-merge (pickset:to-list (ssget '((0 . \"DIMENSION\")))))"
  (setq groups '() res '())
  (foreach en dims
    (if (setq a (at-dim:dim-merge-key en))
      (if (setq g (vl-some (function (lambda (x) (if (equal (car x) a 1e-3) x))) groups))
        (setq groups (subst (append g (list en)) g groups))
        (setq groups (append groups (list (list a en)))))))
  (foreach g groups
    (if (> (length (cdr g)) 1)
      (progn
        (setq a (caar g)
              off  (cadr (car g))
              base (at-dim:pt2d (at-dim:dxf (cadr g) 13))
              tmin nil
              tmax nil)
        (foreach en (cdr g)
          (setq d (entget en)
                p13 (at-dim:pt2d (cdr (assoc 13 d)))
                p14 (at-dim:pt2d (cdr (assoc 14 d)))
                t1  (at-dim:dot2 (at-dim:sub2 p13 base) (list (cos a) (sin a)))
                t2  (at-dim:dot2 (at-dim:sub2 p14 base) (list (cos a) (sin a))))
          (if (or (null tmin) (< t1 tmin)) (setq tmin t1))
          (if (or (null tmin) (< t2 tmin)) (setq tmin t2))
          (if (or (null tmax) (> t1 tmax)) (setq tmax t1))
          (if (or (null tmax) (> t2 tmax)) (setq tmax t2)))
        (setq pmin (list (+ (car base) (* tmin (cos a)))
            (+ (cadr base) (* tmin (sin a))) 0.0)
          pmax (list (+ (car base) (* tmax (cos a)))
            (+ (cadr base) (* tmax (sin a))) 0.0))
        (setq new (at-dim:make-rotated-dim
            pmin pmax
            (list (+ (/ (+ (car pmin) (car pmax)) 2.0)
                (* off (- (sin a))))
              (+ (/ (+ (cadr pmin) (cadr pmax)) 2.0)
                (* off (cos a)))
              0.0)
            a))
        (if new
          (progn
            (foreach en (cdr g) (entdel en))
            (setq res (append res (list new))))))))
  res)

(defun at-dim:dim-merge (/ dims res)
  "命令入口：选择多个共线标注，合并为一个总标注(删除原标注)。"
  "新标注列表 或 nil"
  "(at-dim:dim-merge)"
  (if (setq dims (at-dim:select-dims "\n选择要合并的共线标注: "))
    (progn
      (at-dim:undo-begin)
      (setq res (at-dim:ai-dim-merge dims))
      (at-dim:undo-end)
      (princ (strcat "\n合并完成，生成 " (itoa (length res)) " 个总标注。"))
      res)))

;;; ---------------------------------------------------------------
;;; 命令 5：标注总尺寸
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-total (pts basept / xs ys xmin xmax ymin ymax h off en)
  "AI 入口：按点表包围盒沿较长方向创建总标注，basept 定位置(nil 自动)，返回 ename。"
  "ename 或 nil"
  "(at-dim:ai-dim-total '((0 0) (100 0) (150 0)) nil)"
  (if (and pts (> (length pts) 1))
    (progn
      (setq xs (mapcar 'car pts)
            ys   (mapcar 'cadr pts)
            xmin (apply 'min xs)
            xmax (apply 'max xs)
            ymin (apply 'min ys)
            ymax (apply 'max ys)
            h    (at-dim:txt-h)
            off  (* 8.0 h))
      (if (>= (- xmax xmin) (- ymax ymin))
        (setq en (at-dim:make-rotated-dim
            (list xmin ymin 0.0) (list xmax ymin 0.0)
            (list (/ (+ xmin xmax) 2.0)
              (if basept (cadr basept) (- ymin off))
              0.0)
            0))
        (setq en (at-dim:make-rotated-dim
            (list xmin ymin 0.0) (list xmin ymax 0.0)
            (list (if basept (car basept) (- xmin off))
              (/ (+ ymin ymax) 2.0)
              0.0)
            (/ pi 2))))
      en)))

(defun at-dim:dim-total (/ dims pts p en)
  "命令入口：选择标注取其测量点，再指定放置点，生成总尺寸标注。"
  "ename 或 nil"
  "(at-dim:dim-total)"
  (if (setq dims (at-dim:select-dims "\n选择构成总尺寸的标注: "))
    (progn
      (setq pts '())
      (foreach en dims
        (if (and (at-dim:dxf en 13) (at-dim:dxf en 14))
          (setq pts (append pts (list (at-dim:pt2d (at-dim:dxf en 13))
                (at-dim:pt2d (at-dim:dxf en 14)))))))
      (if (> (length pts) 1)
        (progn
          (setq pts (at-dim:sort-pts pts (car pts) '(1 0)))
          (setq p (getpoint "\n指定总标注位置(回车自动): "))
          (at-dim:undo-begin)
          (setq en (at-dim:ai-dim-total pts p))
          (at-dim:undo-end)
          (if en (princ "\n已生成总标注。") (princ "\n生成总标注失败。"))
          en)))))

;;; ---------------------------------------------------------------
;;; 命令 6：标注原值检查
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-check (dims / res en txt m nums ok)
  "AI 入口：检查文字值与真实测量距离不符的标注，返回 (ename 文字 实测) 列表。"
  "列表，每项 (ename 文字 实测)"
  "(at-dim:ai-dim-check (pickset:to-list (ssget '((0 . \"DIMENSION\")))))"
  (setq res '())
  (foreach en dims
    (setq txt (at-dim:dim-text en)
      m (at-dim:dim-measure en))
    (if (and m (/= txt ""))
      (progn
        (setq nums (at-dim:numbers-in txt)
          ok (vl-some (function (lambda (x)
                (<= (abs (- x m)) (* 1e-4 (max 1.0 (abs m))))))
            nums))
        (if (and nums (null ok))
          (setq res (append res (list (list en txt m))))))))
  res)

(defun at-dim:dim-check (/ dims res it)
  "命令入口：选择标注(或用全图)检查文字值与实测距离是否相符，列出异常项。"
  "异常列表 或 nil"
  "(at-dim:dim-check)"
  (setq dims (at-dim:select-dims "\n选择要检查的标注(回车检查全图): "))
  (if (null dims)
    (setq dims (pickset:to-list (ssget "_X" '((0 . "DIMENSION"))))))
  (setq res (at-dim:ai-dim-check dims))
  (if res
    (progn
      (foreach it res
        (princ (strcat "\n" (at-dim:dxf (car it) 5)
            " 文字:" (cadr it)
            " 实测:" (at-dim:fmt-num (caddr it)))))
      (princ (strcat "\n共 " (itoa (length res)) " 个标注与原值不符。")))
    (princ "\n所有标注与原值一致。"))
  res)

;;; ---------------------------------------------------------------
;;; 命令 7：恢复标注原值
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-reassoc (dims / cnt en m)
  "AI 入口：按 13/14 点真实距离重建标注文字值，返回成功个数。"
  "整数"
  "(at-dim:ai-dim-reassoc (list en))"
  (setq cnt 0)
  (foreach en dims
    (if (setq m (at-dim:dim-measure en))
      (progn
        (at-dim:set-dxf en 1 (at-dim:fmt-num m))
        (setq cnt (1+ cnt)))))
  cnt)

(defun at-dim:dim-reassoc (/ dims n)
  "命令入口：选择标注，按两点真实距离重建文字值。"
  "成功个数 或 nil"
  "(at-dim:dim-reassoc)"
  (if (setq dims (at-dim:select-dims "\n选择要恢复原值的标注: "))
    (progn
      (at-dim:undo-begin)
      (setq n (at-dim:ai-dim-reassoc dims))
      (at-dim:undo-end)
      (princ (strcat "\n已恢复 " (itoa n) " 个标注的文字值。"))
      n)))

;;; ---------------------------------------------------------------
;;; 命令 8：全能标注（点表沿 X/Y 逐段 + 总标）
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-devide (pts dir total / d sorted n i h off ybase xbase
    p1 p2 en res pmin pmax)
  "AI 入口：点表沿 X(\"x\")或 Y(\"y\")逐段标注，total 非 nil 补总标注。"
  "标注 ename 列表"
  "(at-dim:ai-dim-devide '((0 0) (100 0) (150 0)) \"x\" t)"
  (if (and pts (> (length pts) 1))
    (progn
      (setq d (strcase (if (stringp dir) dir (vl-princ-to-string dir))))
      (if (/= d "Y") (setq d "X"))
      (setq sorted (vl-sort pts
                            (function
                             (lambda (a b)
                               (if (= d "Y")
                                 (< (cadr a) (cadr b))
                                 (< (car a) (car b))))))
            n      (length sorted)
            h      (at-dim:txt-h)
            off    (* 8.0 h)
            res    '())
      (if (= d "Y")
        (setq xbase (+ (apply 'max (mapcar 'car sorted)) off))
        (setq ybase (- (apply 'min (mapcar 'cadr sorted)) off)))
      (setq i 0)
      (while (< i (1- n))
        (setq p1 (nth i sorted)
          p2 (nth (1+ i) sorted))
        (if (> (distance (at-dim:pt3 p1) (at-dim:pt3 p2)) 0.0)
          (progn
            (if (= d "Y")
              (setq en (at-dim:make-rotated-dim
                  (list (car p1) (cadr p1) 0.0)
                  (list (car p2) (cadr p2) 0.0)
                  (list xbase (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0)
                  (/ pi 2)))
              (setq en (at-dim:make-rotated-dim
                  (list (car p1) (cadr p1) 0.0)
                  (list (car p2) (cadr p2) 0.0)
                  (list (/ (+ (car p1) (car p2)) 2.0) ybase 0.0)
                  0)))
            (if en (setq res (append res (list en))))))
        (setq i (1+ i)))
      (if (and total (> (length res) 1))
        (progn
          (setq pmin (car sorted)
            pmax (car (reverse sorted)))
          (if (= d "Y")
            (setq en (at-dim:make-rotated-dim
                (list (car pmin) (cadr pmin) 0.0)
                (list (car pmax) (cadr pmax) 0.0)
                (list (+ xbase off)
                  (/ (+ (cadr pmin) (cadr pmax)) 2.0) 0.0)
                (/ pi 2)))
            (setq en (at-dim:make-rotated-dim
                (list (car pmin) (cadr pmin) 0.0)
                (list (car pmax) (cadr pmax) 0.0)
                (list (/ (+ (car pmin) (car pmax)) 2.0)
                  (- ybase off) 0.0)
                0)))
          (if en (setq res (append res (list en))))))
      res)))

(defun at-dim:dim-devide (/ pts p dir total res)
  "命令入口：连续取点后选择 X/Y 方向，逐段标注并可选总标注。"
  "标注列表 或 nil"
  "(at-dim:dim-devide)"
  (setq pts '())
  (setq p (getpoint "\n第一点: "))
  (while p
    (setq pts (append pts (list (at-dim:pt2d p))))
    (setq p (getpoint p "\n下一点(回车结束): ")))
  (if (> (length pts) 1)
    (progn
      (initget "X Y")
      (setq dir (getkword "\n标注方向 [X 水平/Y 垂直] <X>: "))
      (if (null dir) (setq dir "X"))
      (initget "Y N")
      (setq total (getkword "\n是否标注总尺寸 [Y 是/N 否] <Y>: "))
      (if (null total) (setq total "Y"))
      (at-dim:undo-begin)
      (setq res (at-dim:ai-dim-devide pts dir (= total "Y")))
      (at-dim:undo-end)
      (princ (strcat "\n已生成 " (itoa (length res)) " 个标注。"))
      res)))

;;; ---------------------------------------------------------------
;;; 命令 9：沿任意线逐段标注
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-line (pline-ent total / pts n i off en res lastp)
  "AI 入口：沿直线/多段线逐段生成对齐标注，total 非 nil 补首尾总标注。"
  "标注 ename 列表"
  "(at-dim:ai-dim-line (car (entsel)) t)"
  (if (and pline-ent (setq pts (at-dim:pts-from-ent pline-ent)))
    (progn
      (setq pts (mapcar 'at-dim:pt3 pts)
            off (* 6.0 (at-dim:txt-h))
            res '())
      (if (and (> (length pts) 2)
          (= 1 (logand 1 (at-dim:dxf0 pline-ent 70)))
          (> (distance (car pts) (car (reverse pts))) 0.0))
        (setq pts (append pts (list (car pts)))))
      (setq n (length pts)
        i 0)
      (while (< i (1- n))
        (if (> (distance (nth i pts) (nth (1+ i) pts)) 0.0)
          (progn
            (setq en (at-dim:make-aligned
                (nth i pts) (nth (1+ i) pts)
                (at-dim:txt-pt (nth i pts) (nth (1+ i) pts) off)))
            (if en (setq res (append res (list en))))))
        (setq i (1+ i)))
      (if (and total (> (length res) 1))
        (progn
          (setq lastp (car (reverse pts)))
          (setq en (at-dim:make-aligned
              (car pts) lastp
              (at-dim:txt-pt (car pts) lastp (* 2.0 off))))
          (if en (setq res (append res (list en))))))
      res)))

(defun at-dim:dim-line (/ ent total res)
  "命令入口：选择直线或多段线，逐段生成对齐标注(可选总标注)。"
  "标注列表 或 nil"
  "(at-dim:dim-line)"
  (setq ent (car (at-dim:select "\n选择要标注的直线或多段线: "
        '((0 . "LINE,LWPOLYLINE,POLYLINE")))))
  (if ent
    (progn
      (initget "Y N")
      (setq total (getkword "\n是否标注总长 [Y 是/N 否] <N>: "))
      (at-dim:undo-begin)
      (setq res (at-dim:ai-dim-line ent (= total "Y")))
      (at-dim:undo-end)
      (if res
        (princ (strcat "\n已生成 " (itoa (length res)) " 个标注。"))
        (princ "\n未能生成标注。"))
      res)))

;;; ---------------------------------------------------------------
;;; 命令 10：点标房间（周长 + 面积）
;;; ---------------------------------------------------------------
(defun at-dim:ai-dim-room (pt area-only / ss i en ar barea bperi best txth txt mt)
  "AI 入口：在 pt 处标注所在房间的周长与面积；area-only 非 nil 只标面积。"
  "MTEXT ename 或 nil"
  "(at-dim:ai-dim-room '(5000 3000) nil)"
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE")))
    i 0)
  (while (and ss (< i (sslength ss)))
    (setq en (ssname ss i)
      i (1+ i))
    (if (and (= 1 (logand 1 (at-dim:dxf0 en 70)))
        (at-dim:pt-in-poly pt (curve:pline-2dpoints en)))
      (progn
        (setq ar (vl-catch-all-apply
            'vla-get-Area (list (vlax-ename->vla-object en))))
        (if (not (vl-catch-all-error-p ar))
          (if (or (null barea) (< ar barea))
            (setq barea ar
                  bperi (curve:length en)
                  best  en)
  )))))
  (if best
    (progn
      (setq txth (* 1.6 (at-dim:txt-h))
            txt  (if area-only
                  (strcat "面积=" (rtos barea 2 2))
                  (strcat "周长=" (rtos bperi 2 2)
                          "\\P面积=" (rtos barea 2 2)))
            mt   (entity:make-mtext txt (at-dim:pt3 pt) txth
                                  (* txth 10.0) txth))
      (if mt
        (progn
          (at-dim:set-dxf mt 62 7)
          mt)))))

(defun at-dim:dim-room (/ p ao mt)
  "命令入口：在房间内点取一点，标注该房间周长与面积。"
  "MTEXT ename 或 nil"
  "(at-dim:dim-room)"
  (if (setq p (getpoint "\n在房间内部点取一点: "))
    (progn
      (initget "Y N")
      (setq ao (getkword "\n只标面积 [Y 是/N 否] <N>: "))
      (at-dim:undo-begin)
      (setq mt (at-dim:ai-dim-room p (= ao "Y")))
      (at-dim:undo-end)
      (if mt
        (princ "\n已标注房间周长与面积。")
        (princ "\n该点不在任何闭合多段线内。"))
      mt)))

;;; ---------------------------------------------------------------
;;; 命令 11：多重引线对齐
;;; ---------------------------------------------------------------
(defun at-dim:ai-mleader-align (mleaders p1 p2 / en cnt tp d)
  "AI 入口：把多重引线文字位置垂直投影对齐到 p1→p2 直线，返回成功个数。"
  "整数"
  "(at-dim:ai-mleader-align (list en) '(0 0 0) '(100 0 0))"
  (setq cnt 0)
  (foreach en mleaders
    (if (and p1 p2 (= (at-dim:dxf en 0) "MULTILEADER"))
      (progn
        (setq d (entget en)
          tp (cdr (assoc 10 d)))
        (if tp
          (progn
            (setq tp (at-dim:proj-line tp p1 p2))
            (if (entmod (subst (cons 10 tp) (assoc 10 d) d))
              (progn
                (entupd en)
                (setq cnt (1+ cnt)))))))))
  cnt)

(defun at-dim:mleader-align (/ ml p1 p2 n)
  "命令入口：选择多重引线，再取两点确定基线，文字对齐到基线。"
  "成功个数 或 nil"
  "(at-dim:mleader-align)"
  (if (setq ml (at-dim:select "\n选择要对齐的多重引线: "
        '((0 . "MULTILEADER"))))
    (if (setq p1 (getpoint "\n指定对齐基线第一点: "))
      (if (setq p2 (getpoint p1 "\n指定对齐基线第二点: "))
        (progn
          (at-dim:undo-begin)
          (setq n (at-dim:ai-mleader-align ml p1 p2))
          (at-dim:undo-end)
          (princ (strcat "\n已对齐 " (itoa n) " 个多重引线。"))
          n)))))

;;; ---------------------------------------------------------------
;;; 命令 12：引线转多重引线
;;; ---------------------------------------------------------------
(defun at-dim:leader-text (en / d ann pts tol ss txt p best bd te dp)
  "取引线的标注文字：先看 340 组码关联实体，否则取末端点附近最近的 TEXT/MTEXT。"
  "字符串 或 nil"
  "(at-dim:leader-text (car (entsel)))"
  (setq d (entget en)
        ann (cdr (assoc 340 d))
        pts (mapcar 'cdr (vl-remove-if-not (function (lambda (x) (= (car x) 10))) d))
        p   (car (reverse pts))
        tol (* 10.0 (at-dim:txt-h)))
  (cond
    ((and ann (not (vl-catch-all-error-p (entget ann)))
        (setq txt (at-dim:dxf ann 1)))
      (if (= (at-dim:dxf ann 0) "MTEXT")
        (at-dim:clean-mtext txt)
        (at-dim:strip-mtext-codes txt)))
    (p
      (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))
      (foreach te (pickset:to-list ss)
        (setq dp (at-dim:pt3 (at-dim:dxf te 10))
          dp (distance p dp))
        (if (and (< dp tol) (or (null bd) (< dp bd)))
          (setq bd dp
            best te)))
      (if best
        (progn
          (setq txt (at-dim:dxf best 1))
          (if (= (at-dim:dxf best 0) "MTEXT")
            (at-dim:clean-mtext txt)
            (at-dim:strip-mtext-codes txt)))))))

(defun at-dim:ai-leader2mleader (leaders / res en pts txt ml)
  "AI 入口：把普通引线转为多重引线(沿用引线标注文字)，返回新多重引线列表。"
  "多重引线 ename 列表"
  "(at-dim:ai-leader2mleader (list en))"
  (setq res '())
  (foreach en leaders
    (if (= (at-dim:dxf en 0) "LEADER")
      (progn
        (setq pts (mapcar 'cdr
            (vl-remove-if-not (function (lambda (x) (= (car x) 10)))
              (entget en)))
          txt (at-dim:leader-text en))
        (if (null txt) (setq txt ""))
        (if (> (length pts) 1)
          (progn
            (setq ml (vl-catch-all-apply
                'entity:make-multileader
                (list (mapcar 'at-dim:pt2d pts) txt)))
            (if (not (vl-catch-all-error-p ml))
              (if ml
                (progn
                  (entdel en)
                  (setq res (append res (list ml)))))))))))
  res)

(defun at-dim:leader2mleader (/ ls res)
  "命令入口：选择普通引线，转换为多重引线。"
  "多重引线列表 或 nil"
  "(at-dim:leader2mleader)"
  (if (setq ls (at-dim:select "\n选择要转换的引线: " '((0 . "LEADER"))))
    (progn
      (at-dim:undo-begin)
      (setq res (at-dim:ai-leader2mleader ls))
      (at-dim:undo-end)
      (princ (strcat "\n已转换 " (itoa (length res)) " 条引线。"))
      res)))

;;; ---------------------------------------------------------------
;;; 命令 13：标注块名/填充名
;;; ---------------------------------------------------------------
(defun at-dim:ai-bzmc (ents / res en typ nm box ctr tx h)
  "AI 入口：在块参照或填充的中心标注其块名/图案名，返回生成的文字列表。"
  "TEXT ename 列表"
  "(at-dim:ai-bzmc (pickset:to-list (ssget '((0 . \"INSERT,HATCH\")))))"
  (setq res '()
    h (at-dim:txt-h))
  (foreach en ents
    (setq typ (at-dim:dxf en 0)
      nm (at-dim:dxf en 2))
    (if (and nm (member typ '("INSERT" "HATCH")))
      (progn
        (setq box (vl-catch-all-apply 'entity:getbox (list en 0.0)))
        (if (vl-catch-all-error-p box) (setq box nil))
        (setq ctr (if (and box (car box) (cadr box))
            (list (/ (+ (caar box) (car (cadr box))) 2.0)
              (/ (+ (cadr (car box)) (cadr (cadr box))) 2.0)
              0.0)
            (if (and (= typ "INSERT") (listp (at-dim:dxf en 10)))
              (at-dim:pt3 (at-dim:dxf en 10)))))
        (if ctr
          (progn
            (setq tx (entity:make-text nm ctr h 0 0.8 0 "M"))
            (if tx (setq res (append res (list tx)))))))))
  res)

(defun at-dim:bzmc (/ ents res)
  "命令入口：选择块参照或填充，在其中心标注块名/图案名。"
  "TEXT 列表 或 nil"
  "(at-dim:bzmc)"
  (if (setq ents (at-dim:select "\n选择块参照或填充: "
        '((0 . "INSERT,HATCH"))))
    (progn
      (at-dim:undo-begin)
      (setq res (at-dim:ai-bzmc ents))
      (at-dim:undo-end)
      (princ (strcat "\n已标注 " (itoa (length res)) " 个名称。"))
      res)))

;;; ---------------------------------------------------------------
;;; 命令 14：标注图名
;;; ---------------------------------------------------------------
(defun at-dim:ai-index-name (str pt box / tex bx xs ys xmin xmax ymin ymax
    m h en res)
  "AI 入口：在 pt(左下角)生成图名文字，box 非 nil 加矩形框，否则加下划线。"
  "(文字 ename) 或 nil"
  "(at-dim:ai-index-name \"一层平面图\" '(0 0 0) t)"
  (setq res nil)
  (if (and str pt)
    (progn
      (setq h (* 2.0 (at-dim:txt-h))
        tex (entity:make-text str (at-dim:pt3 pt) h 0 0.8 0 "LB"))
      (if tex (setq res (list tex)))))
  (setq bx (if res (vl-catch-all-apply 'text:box (list (car res)))))
  (if bx
    (if (not (vl-catch-all-error-p bx))
      (progn
        (setq xs (mapcar 'car bx)
              ys   (mapcar 'cadr bx)
              xmin (apply 'min xs)
              xmax (apply 'max xs)
              ymin (apply 'min ys)
              ymax (apply 'max ys)
              m    (* 0.3 h))
        (setq en (if box
            (entmakex
              (list '(0 . "LWPOLYLINE")
                '(100 . "AcDbEntity")
                '(100 . "AcDbPolyline")
                '(90 . 4)
                '(70 . 1)
                (cons 10 (list (- xmin m) (- ymin m)))
                (cons 10 (list (+ xmax m) (- ymin m)))
                (cons 10 (list (+ xmax m) (+ ymax m)))
                (cons 10 (list (- xmin m) (+ ymax m)))))
            (entmakex
              (list '(0 . "LINE")
                (cons 10 (list (- xmin m) (- ymin (* 0.5 m)) 0.0))
                (cons 11 (list (+ xmax m) (- ymin (* 0.5 m)) 0.0))))))
        (if en (setq res (append res (list en)))))))
  res)

(defun at-dim:index-name (/ str p box res)
  "命令入口：输入图名文字并指定位置，可选加矩形框。"
  "(文字 ename) 或 nil"
  "(at-dim:index-name)"
  (if (setq str (getstring T "\n输入图名文字: "))
    (if (setq p (getpoint "\n指定图名文字左下角: "))
      (progn
        (initget "Y N")
        (setq box (getkword "\n加矩形框 [Y 是/N 否] <N>: "))
        (at-dim:undo-begin)
        (setq res (at-dim:ai-index-name str p (= box "Y")))
        (at-dim:undo-end)
        (if res (princ "\n已生成图名文字。") (princ "\n生成图名文字失败。"))
        res))))
