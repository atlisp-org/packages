;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-stat 杂项（misc）
;; 标注线段长度 / 文字输出Excel / 文字计算
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------- 标注线段长度 ----------

(defun at-stat:mark-lengths (ents prefix suffix layer / res ent l pt te)
  "标注线段长度：为每条线型实体在中点绘制 前缀+长度+后缀 文字。
  ents: 线型图元列表；prefix/suffix: 前后缀（nil = 无）；
  layer: 文字图层（nil = 当前图层）。"
  "((实体 长度 文字图元) ...) 列表"
  "示例: (at-stat:mark-lengths (at-stat:pick-ents \"选: \" '((0 . \"LINE\"))) \"L=\" \"\" nil)"
  (if (null entity:make-text) (require 'entity:*))
  (if (null curve:midpoint) (require 'curve:*))
  (setq res nil)
  (foreach ent ents
    (if (setq l (at-stat:ent-length ent))
        (progn
          (setq pt (curve:midpoint ent))
          (setq te (entity:make-text
                    (strcat (if prefix prefix "")
                           (at-stat:fmt l 2)
                           (if suffix suffix ""))
                    pt (@:scale 2.5) 0 1.0 0 "M"))
          (if te
              (progn
                (if layer (at-stat:ent-layer te layer))
                (setq res (cons (list ent l te) res)))))))
  (reverse res))

(defun at-stat:text-length (/ ents prefix suffix layer)
  "标注线段长度：选择线型实体后，在中点绘制长度文字（可加前后缀）。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择线/曲线: "
              '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,SPLINE,ELLIPSE"))))
  (if ents
      (progn
        (setq prefix (getstring t "\n前缀<无>: "))
        (setq suffix (getstring t "\n后缀<无>: "))
        (setq layer (getstring t "\n文字图层<当前>: "))
        (if (null std:startundo) (require 'std:*))
        (std:startundo *doc*)
        (at-stat:mark-lengths ents
                                (if (> (strlen prefix) 0) prefix nil)
                                (if (> (strlen suffix) 0) suffix nil)
                                (if (> (strlen layer) 0) layer nil))
        (std:endundo *doc*)))
  (princ))

(defun at-stat:ai-text-length (ents prefix suffix layer / res)
  "AI 调用入口：标注线段长度（中点长度文字），无交互、不弹窗。
  ents: 线型图元列表（选择集或 ename 列表）；prefix: 前缀（nil = 无）；
  suffix: 后缀（nil = 无）；layer: 文字图层（nil = 当前图层）。
  return: ((实体 长度 文字图元) ...) 列表；无线型实体返回 nil。
  示例: (at-stat:ai-text-length (ssget \"x\" '((0 . \"LINE\"))) \"L=\" \"\" nil)"
  (setq res (at-stat:mark-lengths (at-stat:to-ents ents) prefix suffix layer))
  (if res res nil))

;; ---------- 文字输出Excel ----------

(defun at-stat:transpose (m / n res i r row)
  "二维矩阵转置（缺项补空串）。"
  "二维矩阵"
  "示例: (at-stat:transpose '((1 2) (3 4) (5))) => ((1 3 5) (2 4 \"\"))"
  (if (null m)
      nil
      (progn
        (setq n (apply 'max (mapcar 'length m))
              res nil i 0)
        (while (< i n)
          (setq row nil)
          (foreach r m
            (if (nth i r)
                (setq row (cons (nth i r) row))
                (setq row (cons "" row))))
          (setq res (cons (reverse row) res))
          (setq i (1+ i)))
        (reverse res))))

(defun at-stat:text2excel-data (ents rows cols / lst res ent content)
  "文字输出Excel数据：提取 TEXT/MTEXT 内容，按 Y 降序 → X 升序排列。
  ents: 文字图元列表；cols: 列数（rows 为 nil 时按每行 cols 个分行）；
  rows: 行数（cols 为 nil 时按列填充，列数 = ceil(n/rows)）；
  两者皆 nil → 单行。
  返回: 文字内容二维矩阵。"
  "二维矩阵"
  "示例: (at-stat:text2excel-data (at-stat:pick-ents \"选: \" '((0 . \"TEXT\"))) nil 5)"
  (if (null text:get-mtext) (require 'text:*))
  (setq lst nil)
  (foreach ent ents
    (cond
     ((null ent) nil)
     ((= "TEXT" (entity:getdxf ent 0))
      (setq content (at-stat:clean-text (entity:getdxf ent 1)))
      (if content
          (setq lst (cons (cons (entity:getdxf ent 10) content) lst))))
     ((= "MTEXT" (entity:getdxf ent 0))
      (setq content (at-stat:clean-text (text:get-mtext ent)))
      (if content
          (setq lst (cons (cons (entity:getdxf ent 10) content) lst))))
     (t nil)))
  (if lst
      (progn
        (setq lst
             (sort lst
                   (function (lambda (a b)
                               (if (/= (cadr (car a)) (cadr (car b)))
                                   (> (cadr (car a)) (cadr (car b)))
                                   (< (car (car a)) (car (car b))))))))
        (setq res (mapcar 'cdr lst))
        (cond
         ((and (numberp cols) (> cols 0) (null rows))
          (at-stat:chunk res cols))
         ((and (numberp rows) (> rows 0) (null cols))
          (at-stat:transpose (at-stat:chunk res rows)))
         (t (list res))))
      nil))

(defun at-stat:pick-file (msg / file)
  "交互选择保存文件（csv/xlsx），返回路径；取消返回 nil。"
  "字符串"
  "示例: (at-stat:pick-file \"另存为:\")"
  (setq file (getfiled msg (strcat (getvar "dwgprefix") "stat.")
                       "CSV 文件 (*.csv)|*.csv|Excel 文件 (*.xlsx)|*.xlsx|所有文件 (*.*)|*.*||"))
  (if file file nil))

(defun at-stat:text2excel (/ ents cols rows file data)
  "文字输出Excel：选择文字后按位置排列，导出为 Excel/CSV 文件。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择文字(TEXT/MTEXT): "
                                  '((0 . "TEXT,MTEXT"))))
  (if ents
      (progn
        (setq cols (getint "\n列数<0=按行数自动>: "))
        (setq rows (getint "\n行数<0=自动>: "))
        (if (setq file (at-stat:pick-file "另存为:"))
            (progn
              (setq data (at-stat:text2excel-data ents
                                                     (if (and (numberp rows) (> rows 0))
                                                         rows nil)
                                                     (if (and (numberp cols) (> cols 0))
                                                         cols nil)))
              (if (null data)
                  (princ "\nat-stat: 无有效文字。")
                  (at-stat:write-table-file data file))))))
  (princ))

(defun at-stat:ai-text2excel (ents out-file rows cols / data)
  "AI 调用入口：文字输出Excel（按 Y 降序 → X 升序排列），无交互、不弹窗。
  ents: 文字图元列表（选择集或 ename 列表）；out-file: 输出路径（.csv/.xlsx）；
  rows: 行数（cols 为 nil 时按列填充）；cols: 列数（rows 为 nil 时按行分行）；
  两者皆 nil → 单行。
  return: 文字内容二维矩阵；无有效文字返回 nil。
  示例: (at-stat:ai-text2excel (ssget \"x\" '((0 . \"TEXT\"))) \"d:/t.csv\" nil 5)"
  (setq data (at-stat:text2excel-data (at-stat:to-ents ents) rows cols))
  (if data
      (progn
        (if out-file (at-stat:write-table-file data out-file))
        data)
      nil))

;; ---------- 文字计算 ----------

(defun at-stat:eval-txt-num (str / s i c ok r)
  "安全计算文字数值表达式：仅允许 0-9 . + - * / ( ) 空格。
  str: 文字内容。"
  "数值；含其他字符或表达式非法返回 nil"
  "示例: (at-stat:eval-txt-num \"1+2*3\") => 7.0"
  (if (null str)
      nil
      (progn
        (setq s (at-stat:trim str))
        (if (= 0 (strlen s))
            nil
            (progn
              (setq ok t i 1)
              (while (and ok (<= i (strlen s)))
                (setq c (substr s i 1))
                (if (not (member c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                                    "." "+" "-" "*" "/" "(" ")" " ")))
                    (setq ok nil))
                (setq i (1+ i)))
              (if ok
                  (progn
                    (setq s (at-stat:str-replace " " "" s))
                    (if (= 0 (strlen s))
                        nil
                        (progn
                          (setq r (vl-catch-all-apply 'read (list s)))
                          (if (and (not (vl-catch-all-error-p r))
                                   (or (numberp r) (listp r)))
                              (progn
                                (setq r (vl-catch-all-apply 'eval (list r)))
                                (if (and (not (vl-catch-all-error-p r))
                                         (numberp r))
                                    r
                                    nil))
                              nil))))
        nil))))))

(defun at-stat:cale-data (ents op / res n first ent str val)
  "文字计算：提取每个文字中的数值表达式值（安全 eval）并求和/连乘。
  ents: 文字图元列表；op: \"sum\" 求和 / \"prod\" 连乘（其他 = 求和）。
  返回: (结果值 有效个数)；无有效数值文字返回 nil。"
  "(数值 整数)"
  "示例: (at-stat:cale-data (at-stat:pick-ents \"选: \" '((0 . \"TEXT\"))) \"sum\")"
  (if (null text:get-mtext) (require 'text:*))
  (setq res 0.0 n 0 first t)
  (foreach ent ents
    (cond
     ((null ent) nil)
     ((= "TEXT" (entity:getdxf ent 0))
      (setq str (entity:getdxf ent 1)))
     ((= "MTEXT" (entity:getdxf ent 0))
      (setq str (text:get-mtext ent)))
     (t nil))
    (if (setq val (at-stat:eval-txt-num str))
        (progn
          (setq n (1+ n))
          (if first
              (progn
                (setq res val)
                (setq first nil))
              (if (= "prod" op)
                  (setq res (* res val))
                  (setq res (+ res val)))))))
  (if (> n 0)
      (list res n)
      nil))

(defun at-stat:cale-mark (ents op result-pt layer / d res te)
  "文字计算：计算文字数值之和/积，result-pt 给定时在图中绘制结果文字。
  ents: 文字图元列表；op: \"sum\"/\"prod\"；result-pt: 结果点（nil = 不绘制）；
  layer: 结果文字图层（nil = 当前图层）。"
  "(结果值 有效个数 结果文字图元或nil)；无有效数值文字返回 nil"
  "示例: (at-stat:cale-mark (at-stat:pick-ents \"选: \" '((0 . \"TEXT\"))) \"sum\" nil nil)"
  (if (null (setq d (at-stat:cale-data ents op)))
      (progn
        (princ "\nat-stat: 未找到有效数值文字。")
        nil)
      (progn
        (setq te nil)
        (if result-pt
            (progn
              (if (null entity:make-text) (require 'entity:*))
              (setq te (entity:make-text (at-stat:fmt (car d) 4)
                                          result-pt (@:scale 2.5) 0 1.0 0 "M"))
              (if (and te layer) (at-stat:ent-layer te layer))))
        (princ (strcat "\nat-stat: 计算结果: " (at-stat:fmt (car d) 4)
                       " (共 " (itoa (cadr d)) " 项)"))
        (list (car d) (cadr d) te))))

(defun at-stat:cale-txt (/ ents op pt)
  "文字计算：选择含数值表达式的文字，求和/连乘并标注结果。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择文字: " '((0 . "TEXT,MTEXT"))))
  (if ents
      (progn
        (setq op (getkword "\n计算方式 [求和(S)/连乘(P)]<S>: " "S P"))
        (if (= "P" op) (setq op "prod"))
        (setq pt (getpoint "\n指定结果点(Esc跳过): "))
        (if (null std:startundo) (require 'std:*))
        (std:startundo *doc*)
        (at-stat:cale-mark ents (if op op "sum") pt nil)
        (std:endundo *doc*)))
  (princ))

(defun at-stat:ai-cale-txt (ents op result-pt / d)
  "AI 调用入口：文字计算（文字中数值表达式安全 eval），无交互、不弹窗。
  ents: 文字图元列表（选择集或 ename 列表）；op: \"sum\" 求和 / \"prod\" 连乘；
  result-pt: 结果点（绘制结果文字，nil = 不绘制）。
  return: (结果值 有效个数 结果文字图元或nil)；无有效数值文字返回 nil。
  示例: (at-stat:ai-cale-txt (ssget \"x\" '((0 . \"TEXT\"))) \"sum\" '(0 0 0))"
  (setq d (at-stat:cale-mark (at-stat:to-ents ents) op result-pt nil))
  (if d d nil))
