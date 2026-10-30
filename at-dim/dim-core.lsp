;; dim-core.lsp — 标注增强核心：菜单注册与公共几何/标注工具
;; 由原 mini-dim 包并入 at-dim 包，命令入口见 dim-tools-ext.lsp
(vl-load-com)
(require '(entity:* point:* curve:* pickset:* string:* layer:*))

(@:add-menus
  '("标注增强"
    ("标注移动" (at-dim:dim-move))
    ("文字归位" (at-dim:dim-home))
    ("标注对齐" (at-dim:dim-align))
    ("合并标注" (at-dim:dim-merge))
    ("总标注" (at-dim:dim-total))
    ("检查标注" (at-dim:dim-check))
    ("恢复原值" (at-dim:dim-reassoc))
    ("分段标注" (at-dim:dim-devide))
    ("沿线标注" (at-dim:dim-line))
    ("房间标注" (at-dim:dim-room))
    ("引线对齐" (at-dim:mleader-align))
    ("引线转多引线" (at-dim:leader2mleader))
    ("块填充名" (at-dim:bzmc))
    ("图名标注" (at-dim:index-name))))

;;; ---------------------------------------------------------------
;;; 基础读写
;;; ---------------------------------------------------------------
(defun at-dim:dxf (en code)
  "读取图元指定组码的首个值。"
  "组码值 或 nil"
  "(at-dim:dxf (car (entsel)) 8)"
  (if en (cdr (assoc code (entget en)))))

(defun at-dim:dxf0 (en code)
  "读取图元组码值，缺失时返回 0(位运算用)。"
  "整数"
  "(at-dim:dxf0 (car (entsel)) 70)"
  (if (at-dim:dxf en code) (at-dim:dxf en code) 0))

(defun at-dim:get-dim-data (en / d p13 p14 tpt)
  "读取标注数据：测量点 13/14、文字位置(10 优先 11 次之)、文字内容。"
  "列表 (p13 p14 txtpt text) 或 nil"
  "(at-dim:get-dim-data (car (entsel)))"
  (if (and en (= (at-dim:dxf en 0) "DIMENSION"))
    (progn
      (setq d (entget en)
            p13 (cdr (assoc 13 d))
            p14 (cdr (assoc 14 d))
            tpt (cdr (assoc 10 d)))
      (if (null tpt)
        (setq tpt (cdr (assoc 11 d))))
      (list p13 p14 tpt (cdr (assoc 1 d))))))

(defun at-dim:dim-dir (en / d a)
  "读取标注的测量方向：旋转标注(含 AcDbRotatedDimension)取 50 组码，否则取 13→14 方向。"
  "角度(弧度) 或 nil"
  "(at-dim:dim-dir (car (entsel)))"
  (setq d (entget en))
  (cond
    ((member '(100 . "AcDbRotatedDimension") d)
      (cdr (assoc 50 d)))
    ((and (cdr (assoc 13 d)) (cdr (assoc 14 d)))
      (angle (cdr (assoc 13 d)) (cdr (assoc 14 d))))))

(defun at-dim:dim-measure (en / d p13 p14 a)
  "按 13/14 点与测量方向计算标注的真实距离；非标注或数据不全返回 nil。"
  "数值 或 nil"
  "(at-dim:dim-measure (car (entsel)))"
  (setq d (entget en))
  (if (= (cdr (assoc 0 d)) "DIMENSION")
    (progn
      (setq p13 (cdr (assoc 13 d))
            p14 (cdr (assoc 14 d))
            a   (at-dim:dim-dir en))
      (if (and p13 p14)
        (if a
          (abs (+ (* (cos a) (- (car p14) (car p13)))
              (* (sin a) (- (cadr p14) (cadr p13)))))
          (distance p13 p14))))))

(defun at-dim:dim-text (en / s)
  "读取标注的显示文字内容(1 组码覆盖值)，未覆盖返回空字符串。"
  "字符串"
  "(at-dim:dim-text (car (entsel)))"
  (setq s (at-dim:dxf en 1))
  (if s s ""))

(defun at-dim:txt-h ()
  "当前标注文字高度，DIMTXT 与 2.5 取大。"
  "数值"
  "(at-dim:txt-h)"
  (max 2.5 (getvar "dimtxt")))

(defun at-dim:pt3 (pt)
  "二维点补 z=0 转三维点。"
  "三维点"
  "(at-dim:pt3 '(1 2))"
  (list (car pt) (cadr pt) 0.0))

(defun at-dim:pt2d (pt)
  "三维点取 xy 转二维点。"
  "二维点"
  "(at-dim:pt2d '(1 2 3))"
  (list (car pt) (cadr pt)))

(defun at-dim:set-dxf (en code val / lst)
  "设置图元组码值：已存在则替换，否则追加到实体数据末尾。"
  "图元名"
  "(at-dim:set-dxf en 8 \"A\")"
  (setq lst (entget en))
  (if (assoc code lst)
    (entmod (subst (cons code val) (assoc code lst) lst))
    (entmod (append lst (list (cons code val)))))
  (entupd en)
  en)

(defun at-dim:del-dxf (en code / lst)
  "删除图元指定组码。"
  "图元名"
  "(at-dim:del-dxf en 72)"
  (setq lst (vl-remove-if (function (lambda (x) (= (car x) code))) (entget en)))
  (entmod lst)
  (entupd en)
  en)

;;; ---------------------------------------------------------------
;;; 建标注
;;; ---------------------------------------------------------------
(defun at-dim:make-rotated-dim (p1 p2 txtpt ang / dxf)
  "创建旋转标注(AcDbRotatedDimension)，ang 为标注线角度(弧度)。"
  "ename 或 nil"
  "(at-dim:make-rotated-dim '(0 0 0) '(10 0 0) '(5 10 0) 0)"
  (setq dxf (list '(0 . "DIMENSION")
      '(100 . "AcDbEntity")
      '(100 . "AcDbDimension")
      (cons 10 (at-dim:pt3 txtpt))
      '(70 . 32)
      '(1 . "")
      '(100 . "AcDbAlignedDimension")
      (cons 13 (at-dim:pt3 p1))
      (cons 14 (at-dim:pt3 p2))
      (cons 50 ang)
      '(100 . "AcDbRotatedDimension")))
  (if (entmakex (vl-remove nil dxf))
    (entlast)))

(defun at-dim:make-aligned (p1 p2 txtpt)
  "创建对齐标注，返回标注图元名。"
  "ename 或 nil"
  "(at-dim:make-aligned '(0 0 0) '(10 0 0) '(5 5 0))"
  (entity:dimaligned (at-dim:pt3 p1) (at-dim:pt3 p2) (at-dim:pt3 txtpt)))

(defun at-dim:seg-angle (p1 p2 / a)
  "线段方向判断：近水平返回 0，近垂直返回 90，否则返回 nil。"
  "数值或 nil"
  "(at-dim:seg-angle '(0 0 0) '(10 1 0))"
  (if (and p1 p2)
    (progn
      (setq a (abs (atan (- (cadr p2) (cadr p1))
            (- (car p2) (car p1)))))
      (cond
        ((or (< a 0.15) (> a (- pi 0.15))) 0)
        ((< (abs (- a (/ pi 2))) 0.15) 90)
        (t nil)))))

(defun at-dim:txt-pt (p1 p2 offset / ang)
  "标注文字位置：两点中点沿法线偏移 offset。"
  "三维点"
  "(at-dim:txt-pt '(0 0 0) '(10 0 0) 5)"
  (setq ang (angle p1 p2))
  (polar (list (/ (+ (car p1) (car p2)) 2.0)
      (/ (+ (cadr p1) (cadr p2)) 2.0)
      0.0)
    (+ ang (/ pi 2))
    offset))

(defun at-dim:make-seg-dim (p1 p2 offset / ang)
  "按方向自动创建标注：近水平/近垂直用旋转标注，否则用对齐标注。"
  "ename 或 nil"
  "(at-dim:make-seg-dim '(0 0 0) '(10 10 0) 5)"
  (setq ang (at-dim:seg-angle p1 p2))
  (cond
    ((= ang 0)
      (at-dim:make-rotated-dim p1 p2
        (list (/ (+ (car p1) (car p2)) 2.0)
          (+ (cadr p1) offset) 0.0)
        0))
    ((= ang 90)
      (at-dim:make-rotated-dim p1 p2
        (list (+ (car p1) offset)
          (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0)
        (/ pi 2)))
    (t (at-dim:make-aligned p1 p2 (at-dim:txt-pt p1 p2 offset)))))

(defun at-dim:make-total-dim (p1 p2 offset)
  "两点间创建总标注，offset 为标注线沿法线偏移量。"
  "ename 或 nil"
  "(at-dim:make-total-dim '(0 0 0) '(100 0 0) 10)"
  (at-dim:make-seg-dim p1 p2 offset))

(defun at-dim:vla-pt (val)
  "把 VLA 返回的点(variant/safearray/列表)规范为三维点，其他返回 nil。"
  "三维点 或 nil"
  "(at-dim:vla-pt (vlax-get obj 'TextPosition))"
  (cond
    ((null val) nil)
    ((= (type val) 'variant) (at-dim:vla-pt (vlax-variant-value val)))
    ((= (type val) 'safearray) (at-dim:vla-pt (vlax-safearray->list val)))
    ((listp val) (at-dim:pt3 val))))

(defun at-dim:set-dim-text-pt (en pt / obj)
  "设置标注文字位置(ActiveX TextPosition，entmod 对 11 组码无效时用)；成功返回 t。"
  "t 或 nil"
  "(at-dim:set-dim-text-pt (car (entsel)) '(0 0 0))"
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list en)))
  (if (not (vl-catch-all-error-p obj))
    (if (not (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-put-TextPosition
            (list obj (vlax-3d-point (at-dim:pt3 pt))))))
      t)))

(defun at-dim:dim-text-pt (en / obj p)
  "读取标注当前文字位置(ActiveX TextPosition)，失败返回 nil。"
  "三维点 或 nil"
  "(at-dim:dim-text-pt (car (entsel)))"
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list en)))
  (if (not (vl-catch-all-error-p obj))
    (at-dim:vla-pt (vl-catch-all-apply 'vlax-get (list obj 'TextPosition)))))

(defun at-dim:dim-default-pt (en / d a base p13 p14 dir nrm t1 t2 tm s gap)
  "计算标注文字默认位置(尺寸线中点、朝文字侧偏移 gap+字高/2)；非标注返回 nil。"
  "三维点 或 nil"
  "(at-dim:dim-default-pt (car (entsel)))"
  (setq d (entget en)
        a    (at-dim:dim-dir en)
        base (cdr (assoc 10 d))
        p13  (cdr (assoc 13 d))
        p14  (cdr (assoc 14 d)))
  (if (and a base p13 p14)
    (progn
      (setq dir (list (cos a) (sin a))
            nrm (list (- (sin a)) (cos a))
            t1  (at-dim:dot2 (at-dim:sub2 p13 base) dir)
            t2  (at-dim:dot2 (at-dim:sub2 p14 base) dir)
            tm  (/ (+ t1 t2) 2.0)
            s   (at-dim:dot2 (at-dim:sub2 base p13) nrm))
      (if (< s 0) (setq nrm (list (sin a) (- (cos a)))))
      (setq gap (+ (getvar "dimgap") (/ (at-dim:txt-h) 2.0)))
      (list (+ (car base) (* tm (car dir)) (* gap (car nrm)))
        (+ (cadr base) (* tm (cadr dir)) (* gap (cadr nrm)))
        (if (caddr base) (caddr base) 0.0)))))

;;; ---------------------------------------------------------------
;;; 二维向量与线段运算
;;; ---------------------------------------------------------------
(defun at-dim:sub2 (a b)
  "二维向量差 a-b。"
  "二维点"
  "(at-dim:sub2 '(3 4) '(1 1))"
  (list (- (car a) (car b)) (- (cadr a) (cadr b))))

(defun at-dim:dot2 (a b)
  "二维向量点积。"
  "数值"
  "(at-dim:dot2 '(1 0) '(0 1))"
  (+ (* (car a) (car b)) (* (cadr a) (cadr b))))

(defun at-dim:cross2 (a b)
  "二维向量叉积标量。"
  "数值"
  "(at-dim:cross2 '(1 0) '(0 1))"
  (- (* (car a) (cadr b)) (* (cadr a) (car b))))

(defun at-dim:normal (v)
  "二维向量左法向量。"
  "二维点"
  "(at-dim:normal '(1 0))"
  (list (- (cadr v)) (car v)))

(defun at-dim:unit (v / l)
  "二维向量单位化，零向量返回 nil。"
  "二维点 或 nil"
  "(at-dim:unit '(3 4))"
  (if (setq l (distance '(0 0 0) (list (car v) (cadr v) 0.0)))
    (if (> l 0.0)
      (list (/ (car v) l) (/ (cadr v) l)))))

(defun at-dim:ray-line-t (p dir a b / den)
  "射线(p 沿 dir)与直线 ab 交点参数 t，平行返回 nil。"
  "数值 或 nil"
  "(at-dim:ray-line-t '(0 0) '(1 0) '(3 -2) '(3 2))"
  (setq den (at-dim:cross2 dir (at-dim:sub2 b a)))
  (if (and den (/= den 0.0))
    (/ (at-dim:cross2 (at-dim:sub2 a p) (at-dim:sub2 b a)) den)))

(defun at-dim:sort-pts (pts p0 dir)
  "按沿 dir 方向从 p0 起的投影距离排序点列。"
  "点列表"
  "(at-dim:sort-pts '((5 0) (1 0) (3 0)) '(0 0) '(1 0))"
  (vl-sort pts
    (function
      (lambda (x y)
        (< (at-dim:dot2 (at-dim:sub2 x p0) dir)
          (at-dim:dot2 (at-dim:sub2 y p0) dir))))))

(defun at-dim:proj-line (pt p1 p2 / u t0 z)
  "点 pt 到直线 p1→p2 的垂直投影点(保留 p1 的 z)。"
  "三维点"
  "(at-dim:proj-line '(3 5) '(0 0) '(10 0))"
  (setq u (at-dim:sub2 p2 p1)
    z (if (caddr p1) (caddr p1) 0.0))
  (if (and u (/= (at-dim:dot2 u u) 0.0))
    (setq t0 (/ (at-dim:dot2 (at-dim:sub2 pt p1) u) (at-dim:dot2 u u))
      pt (list (+ (car p1) (* t0 (car u)))
        (+ (cadr p1) (* t0 (cadr u)))
        z)))
  (at-dim:pt3 pt))

(defun at-dim:norm-ang (a)
  "把角度归一化到 [0, pi) 区间(用于判断标注方向)。"
  "弧度"
  "(at-dim:norm-ang (* 1.5 pi))"
  (setq a (rem a pi))
  (if (< a 0) (+ a pi) a))

(defun at-dim:pt-in-poly (pt pts / x y i j n xi yi xj yj inside)
  "射线法判断点是否在多边形内(pts 为二维点列表)。"
  "t 或 nil"
  "(at-dim:pt-in-poly '(1 1) '((0 0) (2 0) (2 2) (0 2)))"
  (setq x (car pt)
        y      (cadr pt)
        n      (length pts)
        i      0
        inside nil)
  (while (< i n)
    (setq j (if (= i 0) (1- n) (1- i)))
    (setq xi (car (nth i pts))
          yi (cadr (nth i pts))
          xj (car (nth j pts))
          yj (cadr (nth j pts)))
    (if (and (or (and (> yi y) (<= yj y)) (and (> yj y) (<= yi y)))
        (< x (+ xi (/ (* 1.0 (- xj xi) (- y yi)) (- yj yi)))))
      (setq inside (not inside)))
    (setq i (1+ i)))
  inside)

;;; ---------------------------------------------------------------
;;; 图元提取 / 选择 / 文字处理
;;; ---------------------------------------------------------------
(defun at-dim:pts-from-ent (en / d etype)
  "从图元提取折点(仅 LINE/POLYLINE/LWPOLYLINE)，否则返回 nil。"
  "二维点列表 或 nil"
  "(at-dim:pts-from-ent (car (entsel)))"
  (if en
    (progn
      (setq d (entget en)
        etype (cdr (assoc 0 d)))
      (cond
        ((= etype "LINE")
          (list (at-dim:pt2d (cdr (assoc 10 d)))
            (at-dim:pt2d (cdr (assoc 11 d)))))
        ((member etype '("LWPOLYLINE" "POLYLINE"))
          (mapcar 'at-dim:pt2d (curve:pline-2dpoints en)))
        (t nil)))))

(defun at-dim:ensure-layer (name)
  "确保图层存在(不存在则创建)，返回图层名。"
  "字符串"
  "(at-dim:ensure-layer \"A-DIM\")"
  (if (and name (/= name "") (null (tblsearch "LAYER" name)))
    (layer:make name 7 nil nil))
  name)

(defun at-dim:select (msg filter / ss)
  "提示并选择指定过滤条件的对象，无选择返回 nil。"
  "图元名列表 或 nil"
  "(at-dim:select \"选择直线\" '((0 . \"LINE\")))"
  (princ msg)
  (if (setq ss (ssget filter))
    (pickset:to-list ss)))

(defun at-dim:select-dims (msg)
  "提示并选择标注对象，无选择返回 nil。"
  "图元名列表 或 nil"
  "(at-dim:select-dims \"选择标注\")"
  (at-dim:select msg '((0 . "DIMENSION"))))

(defun at-dim:extract-num (str / i c s dot done)
  "从字符串中提取第一个数值(支持小数/负号)，无则返回 nil。"
  "数值 或 nil"
  "(at-dim:extract-num \"L=123.45\")"
  (setq i 1
        s    ""
        dot  nil
        done nil)
  (while (and (null done) (<= i (strlen str)))
    (setq c (substr str i 1))
    (cond
      ((vl-string-search c "0123456789")
        (setq s (strcat s c)
          i (1+ i)))
      ((and (= c ".") (/= s "") (null dot) (< i (strlen str))
          (vl-string-search (substr str (1+ i) 1) "0123456789"))
        (setq dot t
              s   (strcat s c)
              i   (1+ i))
      )
      ((and (= s "") (= c "-") (< i (strlen str))
          (vl-string-search (substr str (1+ i) 1) "0123456789"))
        (setq s "-"
          i (1+ i)))
      ((/= s "") (setq done t))
      (t (setq i (1+ i)))))
  (if (/= s "") (atof s)))

(defun at-dim:numbers-in (s / i c tok lst)
  "提取字符串中所有数值，按出现顺序返回数值列表。"
  "数值列表"
  "(at-dim:numbers-in \"2x500\")"
  (setq i 1
        tok ""
        lst '())
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (or (vl-string-search c "0123456789") (= c "."))
      (setq tok (strcat tok c))
      (progn
        (if (/= tok "")
          (setq lst (append lst (list (atof tok)))))
        (setq tok "")))
    (setq i (1+ i)))
  (if (/= tok "")
    (setq lst (append lst (list (atof tok)))))
  lst)

(defun at-dim:fmt-num (n / s)
  "数值转字符串，去掉尾部多余的 0 与小数点。"
  "字符串"
  "(at-dim:fmt-num 123.0)"
  (setq s (rtos n 2 4))
  (if (vl-string-search "." s)
    (progn
      (while (and (> (strlen s) 1)
          (= (substr s (strlen s) 1) "0"))
        (setq s (substr s 1 (1- (strlen s)))))
      (if (= (substr s (strlen s) 1) ".")
        (setq s (substr s 1 (1- (strlen s)))))))
  s)

(defun at-dim:strip-mtext-codes (s / i j)
  "删除字符串中的 \\xxx; 格式码。"
  "字符串"
  "(at-dim:strip-mtext-codes \"\\C1;red\")"
  (setq i 0)
  (while (setq i (vl-string-search "\\" s i))
    (setq j (vl-string-search ";" s i))
    (if (and j (< (- j i) 6))
      (setq s (strcat (substr s 1 i) (substr s (+ j 2)))
        i 0)
      (setq i (1+ i))))
  s)

(defun at-dim:clean-mtext (str / s)
  "清理 MTEXT 格式码：换行转空格，去花括号与 \\xxx; 码，返回纯文本。"
  "字符串"
  "(at-dim:clean-mtext \"{\\C1;red}\\Pline\")"
  (setq s (string:subst-all " " "\\P" str)
        s (string:subst-all "" "{" s)
        s (string:subst-all "" "}" s))
  (at-dim:strip-mtext-codes s))

(defun at-dim:count-list (item lst / n)
  "统计列表中某元素出现次数。"
  "整数"
  "(at-dim:count-list \"A\" '(\"A\" \"B\" \"A\"))"
  (setq n 0)
  (while lst
    (if (equal (car lst) item)
      (setq n (1+ n)))
    (setq lst (cdr lst)))
  n)

(defun at-dim:align-code (k)
  "对齐方式转 71 组码：1 左 2 中 3 右 4 沿 5 最佳。"
  "数值"
  "(at-dim:align-code 2)"
  (cond
    ((= k 1) 1)
    ((= k 2) 0)
    ((= k 3) 2)
    ((= k 4) 4)
    (t 4)))

(defun at-dim:same-line-p (p1 p2 p3 / a1 a2 d)
  "判断 p1p2 与 p1p3 是否共线(角度差小于 0.05 弧度或互补)。"
  "t 或 nil"
  "(at-dim:same-line-p '(0 0 0) '(1 0 0) '(2 0 0))"
  (setq a1 (angle p1 p2)
        a2 (angle p1 p3)
        d  (abs (- a1 a2)))
  (or (< d 0.05)
    (< (abs (- d pi)) 0.05)))

(defun at-dim:dim-param (dd p0 dir)
  "标注 13 点沿 dir 方向从 p0 的投影参数。"
  "数值"
  "(at-dim:dim-param (list '(0 0) '(10 0) '(5 0) \"5.0\") '(0 0) '(1 0))"
  (at-dim:dot2 (at-dim:sub2 (at-dim:pt2d (car dd)) p0) dir))

;;; ---------------------------------------------------------------
;;; 撤销标记（交互命令用）
;;; ---------------------------------------------------------------
(defun at-dim:undo-begin ()
  "开始撤销标记：优先用内核 startundo，退化用 vla-StartUndoMark。"
  "t 或 nil"
  "(at-dim:undo-begin)"
  (if (member 'startundo (atoms-family 0))
    (progn (startundo) t)
    (if *DOC*
      (not (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-StartUndoMark (list *DOC*)))))))

(defun at-dim:undo-end ()
  "结束撤销标记：优先用内核 endundo，退化用 vla-EndUndoMark。"
  "t 或 nil"
  "(at-dim:undo-end)"
  (if (member 'endundo (atoms-family 0))
    (progn (endundo) t)
    (if *DOC*
      (not (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-EndUndoMark (list *DOC*)))))))
