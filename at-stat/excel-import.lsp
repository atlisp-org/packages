;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-stat Excel → CAD 表格导入（excel-import）
;; 需求来源：明经论坛 thread-101879（ExCel表格→AutoCAD表格）
;; 读取 Excel UsedRange（单次 Value2 数组）/CSV → 画表格线 + 单元格文字，
;; 支持合并单元格；无 Excel 时回退同名 .csv。与「文字→Excel」互补。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:add-menus
 '(("Excel表格"
    ("Excel表格导入" "(at-stat:excel2table)"))))

;; ================= 数据读取 =================

(defun at-stat:cell-str (x)
  "单元格值转字符串（nil 变空串，整数不带小数）。"
  "字符串"
  "(at-stat:cell-str 3.0) => \"3\""
  (cond
   ((null x) "")
   ((= 'str (type x)) x)
   ((numberp x)
    (if (= x (fix x)) (itoa (fix x)) (rtos x 2 4)))
   (t (vl-prin1-to-string x))))

(defun at-stat:last1 (lst)
  "取列表最后一个元素（空表返回 nil）。"
  "元素"
  "(at-stat:last1 '(1 2 3)) => 3"
  (if lst (car (reverse lst)) nil))

(defun at-stat:norm-matrix (vals)
  "把 Excel Value2 的 1x1 / 一维 / 二维结果统一成二维列表。"
  "二维列表"
  "(at-stat:norm-matrix '(1 2)) => ((1 2))"
  (cond
   ((null vals) (list (list nil)))
   ((listp vals)
    (if (listp (car vals)) vals (list vals)))
   (t (list (list vals)))))

(defun at-stat:csv-split (line / res cur inq i n c nxt)
  "按逗号拆分一行 CSV，支持双引号字段与 \"\" 转义。"
  "字符串列表"
  "(at-stat:csv-split \"a,\\\"b,c\\\",3\") => (\"a\" \"b,c\" \"3\")"
  (setq res nil cur "" inq nil i 1 n (strlen line))
  (while (<= i n)
    (setq c (substr line i 1)
          nxt (if (< i n) (substr line (1+ i) 1) ""))
    (cond
     (inq
      (if (= c "\"")
          (if (= nxt "\"")
              (progn (setq cur (strcat cur "\"")) (setq i (1+ i)))
              (setq inq nil))
          (setq cur (strcat cur c))))
     ((= c "\"") (setq inq t))
     ((= c ",") (setq res (cons (at-stat:trim cur) res) cur ""))
     (t (setq cur (strcat cur c))))
    (setq i (1+ i)))
  (reverse (cons (at-stat:trim cur) res)))

(defun at-stat:csv-read (file / fp line first res)
  "读取 CSV 为二维列表（跳过空行，去掉 BOM）。"
  "二维列表；失败返回 nil"
  "(at-stat:csv-read \"d:/t.csv\")"
  (setq fp (if file (vl-catch-all-apply 'open (list file "r")) nil))
  (if (or (null fp) (vl-catch-all-error-p fp))
      (progn (princ (strcat "at-stat: 无法打开文件: "
                            (if file file ""))) nil)
      (progn
        (setq first t res nil)
        (while (setq line (read-line fp))
          (if (and first (>= (strlen line) 3)
                   (= 239 (ascii (substr line 1 1))))
              (setq line (substr line 4)))
          (setq first nil)
          (if (/= "" (at-stat:trim line))
              (setq res (cons (at-stat:csv-split line) res))))
        (close fp)
        (if res (reverse res) (progn (princ "at-stat: CSV 无数据。") nil)))))

(defun at-stat:excel-merges (ur / cells rows cols res r c cell ma)
  "读取已用区域的合并单元格：((行 列 行数 列数) ...)，0 基索引。
  遍历单元格的 MergeCells/MergeArea；合并检测失败时返回 nil（不影响表格导入）。"
  "列表；无合并返回 nil"
  "(at-stat:excel-merges usedrange)"
  (if (or (null ur) (vl-catch-all-error-p ur))
      nil
      (progn
        (setq res
              (vl-catch-all-apply
               (function
                 (lambda ()
                   (setq rows (vlax-get-property
                               (vlax-get-property ur 'Rows) 'Count)
                         cols (vlax-get-property
                               (vlax-get-property ur 'Columns) 'Count)
                         cells (vlax-get-property ur 'Cells))
                   (setq r 1)
                   (while (<= r rows)
                     (setq c 1)
                     (while (<= c cols)
                       (setq cell (vlax-invoke cells 'Item r c))
                       (if (vlax-get-property cell 'MergeCells)
                           (progn
                             (setq ma (vlax-get-property cell 'MergeArea))
                             ;; 只记录合并区左上角，避免重复
                             (if (and (= r (vlax-get-property ma 'Row))
                                      (= c (vlax-get-property ma 'Column)))
                                 (setq res
                                       (cons
                                        (list
                                         (1- r) (1- c)
                                         (vlax-get-property
                                          (vlax-get-property ma 'Rows) 'Count)
                                         (vlax-get-property
                                          (vlax-get-property ma 'Columns) 'Count))
                                        res)))))
                       (setq c (1+ c)))
                     (setq r (1+ r)))
                   (reverse res)))
               nil))
        (if (vl-catch-all-error-p res) nil res))))

(defun at-stat:excel2table-data (file sheet /
                                 ext xl ur vals matrix merges base csv)
  "读取 Excel/CSV 表格数据，返回 (matrix merges)；失败 nil。
  file: .xlsx/.xls/.csv；sheet: 工作表名（nil=第一张）。"
  "(matrix merges)"
  "(at-stat:excel2table-data \"d:/t.xlsx\" nil)"
  (setq ext (at-stat:ext file))
  (cond
   ((= "CSV" ext) (list (at-stat:csv-read file) nil))
   ((or (= "XLS" ext) (= "XLSX" ext))
    (setq xl (vl-catch-all-apply 'excel:open (list file nil)))
    (if (or (null xl) (vl-catch-all-error-p xl))
        (progn
          (setq base (vl-filename-base file))
          (setq base (strcat (vl-filename-directory file) "/" base ".csv"))
          (setq csv (findfile base))
          (if csv
              (progn
                (princ (strcat "at-stat: Excel 不可用，读取同名 CSV: " csv))
                (list (at-stat:csv-read csv) nil))
              (progn
                (princ "at-stat: Excel 不可用且无同名 CSV，无法导入。")
                nil)))
        (progn
          (if (null sheet) (setq sheet (car (excel:sheets xl))))
          (setq ur (vl-catch-all-apply 'excel:get-usedrange
                                       (list xl sheet)))
          (setq matrix
                (if (or (null ur) (vl-catch-all-error-p ur))
                    nil
                    (progn
                      (setq vals (vlax-get-property ur 'Value2))
                      (at-stat:norm-matrix
                       (excel:utils-getvalue vals)))))
          (setq merges (if ur (at-stat:excel-merges ur) nil))
          (vl-catch-all-apply 'excel:quit (list xl nil))
          (if matrix (list matrix merges) nil))))
   (t
    (princ (strcat "at-stat: 不支持的文件类型: "
                   (at-stat:to-str ext)))
    nil)))

;; ================= 绘制 =================

(defun at-stat:scaled (v / r)
  "按当前绘图比例缩放（@::scale）；函数不可用时返回原值。"
  "数值"
  "(at-stat:scaled 3.5)"
  (setq r (vl-catch-all-apply '@::scale (list v)))
  (if (or (vl-catch-all-error-p r) (null r)) v r))

(defun at-stat:bool-row (n / row)
  "生成长度 n 的 nil 列表。"
  "列表"
  "(at-stat:bool-row 3) => (nil nil nil)"
  (setq row nil)
  (repeat n (setq row (cons nil row)))
  row)

(defun at-stat:true-row (n / row)
  "生成长度 n 的 t 列表。"
  "列表"
  "(at-stat:true-row 3) => (T T T)"
  (setq row nil)
  (repeat n (setq row (cons t row)))
  row)

(defun at-stat:set-nth (lst n val / i res)
  "替换列表第 n 项（0 基）。"
  "列表"
  "(at-stat:set-nth '(1 2 3) 1 9) => (1 9 3)"
  (setq i 0 res nil)
  (foreach x lst
    (setq res (cons (if (= i n) val x) res))
    (setq i (1+ i)))
  (reverse res))

(defun at-stat:set-2d (m i j val / row newrow k)
  "设置二维列表第 i 行第 j 列（函数式，返回新列表；本方言无 setcar）。"
  "二维列表"
  "(at-stat:set-2d '((1 2) (3 4)) 0 1 9) => ((1 9) (3 4))"
  (setq row (nth i m) newrow nil k 0)
  (foreach x row
    (setq newrow (cons (if (= k j) val x) newrow)
          k (1+ k)))
  (at-stat:set-nth m i (reverse newrow)))

(defun at-stat:draw-line (x1 y1 x2 y2 layer / ent)
  "画表格线（2 点 LWPOLYLINE）。"
  "ename 或 nil"
  "(at-stat:draw-line 0 0 10 0 \"TABLE\")"
  (setq ent (entmakex
             (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                   (cons 8 layer) '(100 . "AcDbPolyline")
                   '(90 . 2) '(70 . 0)
                   (cons 10 (list x1 y1))
                   (cons 10 (list x2 y2)))))
  ent)

(defun at-stat:draw-text (s x y h layer / ent)
  "画居中单元格文字。"
  "ename 或 nil"
  "(at-stat:draw-text \"A\" 5 5 3.5 \"TABLE\")"
  (setq ent (entmakex
             (list '(0 . "TEXT") '(100 . "AcDbEntity")
                   (cons 8 layer) '(100 . "AcDbText")
                   (cons 10 (list x y 0.0)) (cons 40 h)
                   (cons 1 s) '(72 . 1) '(73 . 2)
                   (cons 11 (list x y 0.0)))))
  ent)

(defun at-stat:excel-draw (matrix merges pt col-widths row-height
                           text-height layer /
                           rows cols h rh wlist xlist ylist vis hflag vflag
                           res i j k s w mx len row v ent)
  "在插入点（左上角）绘制表格线 + 单元格文字，返回 ename 列表。
  matrix: 二维列表；merges: ((行 列 行数 列数) ...)；
  pt: 左上角；col-widths: 列宽表（nil=自动）；row-height/text-height/layer 可 nil。"
  "ename 列表；无数据 nil"
  "(at-stat:excel-draw '((\"A\" \"B\") (\"1\" \"2\")) nil '(0 0) nil nil nil nil)"
  (if (or (null matrix) (null (car matrix)))
      (progn (princ "at-stat: 无表格数据。") nil)
      (progn
        (setq rows (length matrix)
              cols (apply 'max (mapcar 'length matrix)))
        ;; 行补齐
        (setq i 0)
        (while (< i rows)
          (while (< (length (nth i matrix)) cols)
            (setq matrix (at-stat:set-nth
                          matrix i
                          (append (nth i matrix) (list "")))))
          (setq i (1+ i)))
        (setq h (if (and (numberp text-height) (> text-height 0))
                    text-height
                    (at-stat:scaled 3.5)))
        (setq rh (if (and (numberp row-height) (> row-height 0))
                     row-height
                     (* 2.0 h)))
        ;; 列宽
        (setq wlist nil i 0)
        (while (< i cols)
          (setq w (if (and (listp col-widths) (> (length col-widths) 0))
                      (if (nth i col-widths)
                          (nth i col-widths)
                          (at-stat:last1 col-widths))
                      nil))
          (if (and (numberp w) (> w 0))
              (setq wlist (append wlist (list w)))
              (progn
                (setq mx 0)
                (foreach row matrix
                  (setq len (strlen (at-stat:cell-str (nth i row))))
                  (if (> len mx) (setq mx len)))
                (setq wlist (append wlist
                                    (list (max (* 2.0 h)
                                               (* 0.7 h (+ mx 2))))))))
          (setq i (1+ i)))
        ;; 坐标累加
        (setq xlist (list (car pt)) ylist (list (cadr pt)))
        (foreach w wlist
          (setq xlist (append xlist
                              (list (+ (at-stat:last1 xlist) w)))))
        (setq i 0)
        (while (< i rows)
          (setq ylist (append ylist
                              (list (+ (at-stat:last1 ylist) rh)))
                i (1+ i)))
        ;; 可见性：t=普通格；(行数 列数)=合并左上；nil=被覆盖
        (setq vis nil i 0)
        (while (< i rows)
          (setq vis (append vis (list (at-stat:true-row cols))) i (1+ i)))
        (foreach m merges
          (setq i (car m) j (cadr m) k (caddr m) w (cadddr m))
          (if (and (>= i 0) (< i rows) (>= j 0) (< j cols))
              (progn
                (setq k (min k (- rows i)) w (min w (- cols j)))
                (setq s i)
                (while (< s (+ i k))
                  (setq v j)
                  (while (< v (+ j w))
                    (setq vis
                          (at-stat:set-2d vis s v
                                          (if (and (= s i) (= v j))
                                              (list k w)
                                              nil)))
                    (setq v (1+ v)))
                  (setq s (1+ s))))))
        ;; 边线标志
        (setq hflag nil i 0)
        (while (<= i rows)
          (setq hflag (append hflag (list (at-stat:bool-row cols)))
                i (1+ i)))
        (setq vflag nil i 0)
        (while (< i rows)
          (setq vflag (append vflag (list (at-stat:bool-row (1+ cols))))
                i (1+ i)))
        (setq i 0)
        (while (< i rows)
          (setq j 0)
          (while (< j cols)
            (setq v (nth j (nth i vis)))
            (if v
                (progn
                  (setq s (if (listp v) (car v) 1)
                        w (if (listp v) (cadr v) 1)
                        k 0)
                  (while (< k w)
                    (setq hflag (at-stat:set-2d hflag i (+ j k) t))
                    (setq hflag (at-stat:set-2d hflag (+ i s) (+ j k) t))
                    (setq k (1+ k)))
                  (setq k 0)
                  (while (< k s)
                    (setq vflag (at-stat:set-2d vflag (+ i k) j t))
                    (setq vflag (at-stat:set-2d vflag (+ i k) (+ j w) t))
                    (setq k (1+ k)))))
            (setq j (1+ j)))
          (setq i (1+ i)))
        ;; 图层
        (if (null layer) (setq layer (getvar "clayer")))
        (if (null (tblsearch "LAYER" layer))
            (progn (if (null layer:make) (require 'layer:*))
                   (layer:make layer nil nil nil)))
        (setq layer (if (tblsearch "LAYER" layer)
                        layer (getvar "clayer")))
        ;; 横线（合并连续段）
        (setq res nil i 0)
        (while (<= i rows)
          (setq j 0)
          (while (< j cols)
            (if (nth j (nth i hflag))
                (progn
                  (setq k j)
                  (while (and (< (1+ k) cols)
                              (nth (1+ k) (nth i hflag)))
                    (setq k (1+ k)))
                  (if (setq ent (at-stat:draw-line
                                 (nth j xlist) (nth i ylist)
                                 (nth (1+ k) xlist) (nth i ylist) layer))
                      (setq res (cons ent res)))
                  (setq j (1+ k)))
                (setq j (1+ j))))
          (setq i (1+ i)))
        ;; 竖线
        (setq j 0)
        (while (<= j cols)
          (setq i 0)
          (while (< i rows)
            (if (nth j (nth i vflag))
                (progn
                  (setq k i)
                  (while (and (< (1+ k) rows)
                              (nth j (nth (1+ k) vflag)))
                    (setq k (1+ k)))
                  (if (setq ent (at-stat:draw-line
                                 (nth j xlist) (nth i ylist)
                                 (nth j xlist) (nth (1+ k) ylist) layer))
                      (setq res (cons ent res)))
                  (setq i (1+ k)))
                (setq i (1+ i))))
          (setq j (1+ j)))
        ;; 文字（空格不建实体）
        (setq i 0)
        (while (< i rows)
          (setq j 0)
          (while (< j cols)
            (setq v (nth j (nth i vis)))
            (if v
                (progn
                  (setq s (if (listp v) (car v) 1)
                        w (if (listp v) (cadr v) 1)
                        k (nth j (nth i matrix)))
                  (if (and k (/= (at-stat:cell-str k) ""))
                      (if (setq ent (at-stat:draw-text
                                     (at-stat:cell-str k)
                                     (/ (+ (nth j xlist) (nth (+ j w) xlist)) 2.0)
                                     (/ (+ (nth i ylist) (nth (+ i s) ylist)) 2.0)
                                     h layer))
                          (setq res (cons ent res))))))
            (setq j (1+ j)))
          (setq i (1+ i)))
        (reverse res))))

;; ================= 入口 =================

(defun at-stat:ai-excel2table (file pt sheet col-widths row-height
                               text-height layer / data)
  "AI 调用入口：Excel/CSV → CAD 表格，无交互、不弹窗。
  file: .xlsx/.xls/.csv；pt: 插入点（左上角）；sheet: 工作表名（nil=第一张）；
  col-widths: 列宽表（nil=自动）；row-height/text-height/layer 可 nil。
  返回表格图元 ename 列表，失败 nil。
  示例: (at-stat:ai-excel2table \"d:/t.xlsx\" '(0 0) nil nil nil nil nil)"
  "ename 列表或 nil"
  "(at-stat:ai-excel2table \"d:/t.xlsx\" '(0 0) nil nil nil nil \"TABLE\")"
  (if (or (null file) (null pt) (null (findfile file)))
      (progn
        (princ (strcat "at-stat:ai-excel2table 文件不存在: "
                       (at-stat:to-str file)))
        nil)
      (progn
        (setq data (at-stat:excel2table-data file sheet))
        (if (null data)
            nil
            (at-stat:excel-draw (car data) (cadr data) pt
                                col-widths row-height text-height layer)))))

(defun at-stat:excel2table (/ file pt sheet th rh layer cw res)
  "Excel表格导入：选择 Excel/CSV，指定插入点后绘制表格。"
  "ename 列表或 nil"
  "(at-stat:excel2table)"
  (setq file (getfiled
              "选择 Excel/CSV 文件:"
              (getvar "dwgprefix")
              "Excel (*.xlsx;*.xls)|*.xlsx;*.xls|CSV (*.csv)|*.csv|所有文件 (*.*)|*.*||"))
  (if file
      (progn
        (setq pt (getpoint "\n插入点(左上角): "))
        (if pt
            (progn
              (setq th (getreal "\n文字高度<0=自动>: "))
              (setq rh (getreal "\n行高<0=自动>: "))
              (setq layer (at-stat:trim (getstring "\n图层<当前>: ")))
              (setq sheet (at-stat:trim (getstring "\n工作表名<第一张>: ")))
              (setq cw (vl-catch-all-apply
                        'read
                        (list (strcat "("
                                      (at-stat:trim
                                       (getstring "\n列宽(空格分隔,空=自动): "))
                                      ")"))))
              (if (vl-catch-all-error-p cw) (setq cw nil))
              (if (or (null cw) (null (car cw))) (setq cw nil))
              (setq res (at-stat:ai-excel2table
                         file pt
                         (if (= "" sheet) nil sheet)
                         cw
                         (if (and (numberp rh) (> rh 0)) rh nil)
                         (if (and (numberp th) (> th 0)) th nil)
                         (if (= "" layer) nil layer)))
              (if res
                  (princ (strcat "\nat-stat: 完成，共 "
                                 (itoa (length res)) " 个图元。"))
                  (princ "\nat-stat: 导入失败。"))))))
  (princ))
