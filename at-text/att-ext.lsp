;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mini-text 块属性类命令：块属性转文字 att2text、文字变块属性 text2att、
;; 刷块属性 match-att、块属性统计表 att-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @text:att-align (h v)
  "把 TEXT 的 72/73 对齐码转换为 make-text 的对齐字符串。
  返回 string（如 \"MM\"）。"
  "string"
  (strcat
   (cond
     ((= 2 h) "R")
     ((= 1 h) "M")
     (t "L"))
   (cond
     ((= 3 v) "T")
     ((= 2 v) "M")
     (t "B"))))

(defun @text:att2text-1 (att / dx ent)
  "把一个属性(ATTRIB)转为独立 TEXT（保留位置/样式/字高/角度/图层/对齐）。
  返回新文字 ename。"
  "ename"
  (setq dx (entget att))
  (setq ent (entity:make-text
             (cdr (assoc 1 dx))
             (if (and (cdr (assoc 11 dx))
                      (not (equal (cdr (assoc 10 dx)) (cdr (assoc 11 dx)))))
                 (cdr (assoc 11 dx))
                 (cdr (assoc 10 dx)))
             (cdr (assoc 40 dx))
             (if (cdr (assoc 50 dx)) (cdr (assoc 50 dx)) 0.0)
             (if (cdr (assoc 41 dx)) (cdr (assoc 41 dx)) 0.8)
             0
             (@text:att-align (or (cdr (assoc 72 dx)) 0)
                                  (if (cdr (assoc 73 dx)) (cdr (assoc 73 dx)) 1))))
  (entmod (append (entget ent)
                  (list (cons 8 (cdr (assoc 8 dx)))
                        (cons 7 (cdr (assoc 7 dx))))))
  (entupd ent)
  ent)

(defun @text:ai-att2text (blocks del-block / res e dx te)
  "块属性转文字（AI 入口，无交互）。
  把每个块的属性转为独立 TEXT，原块属性被删除，可选删除原块。
  blocks: 块 ename 列表；del-block: t=同时删除原块。
  返回创建的文字 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (null blocks)
    (progn (princ "@text:ai-att2text: blocks 为空") nil)
    (progn
      (command "_.UNDO" "Begin")
      (setq res nil)
      (foreach e blocks
        (if (and (setq dx (entget e))
                 (= "INSERT" (cdr (assoc 0 dx))))
            (progn
              (foreach att (@text:block-attrs e)
                (if (setq te (@text:att2text-1 att))
                    (progn
                      (setq res (cons te res))
                      (vlax-invoke (vlax-ename->vla-object att) 'delete))))
              (if del-block
                  (entdel e)))))
      (command "_.UNDO" "End")
      (if res
          (progn
            (princ (strcat "\n已转换 " (itoa (length res)) " 个属性"))
            (reverse res))
          (progn
            (princ "\n未找到属性。")
            nil)))))

(defun @text:att2text (/ ss del)
  "块属性转文字：把块的属性转成独立文字。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (setq del (wcmatch (getkword "\n删除原块? [Y删除/N保留] <N>: ") "*Y*"))
      (if (@text:ai-att2text (pickset:to-list ss) del)
          (princ "\n完成。")
          (princ "\n未找到属性。"))
    )
    (princ "\n未选择块，命令取消。"))
  (princ))

(defun @text:make-tag (str / res c)
  "从字符串生成属性标签名（大写字母数字，最长 25 字符）。
  返回 string，无有效字符时返回 \"ATT\"。"
  "string"
  (if (= ""  (@text:str str)) (setq str "ATT"))
  (setq res nil)
  (foreach c (vl-string->list (strcase  (@text:str str)))
    (if (or (and (>= c 65) (<= c 90))
            (and (>= c 48) (<= c 57)))
        (if (< (length res) 25)
            (setq res (cons (chr c) res)))))
  (if res
      (vl-list->string (reverse res))
      "ATT"))

(defun @text:ai-text2att (ents blk-name / res e dx tag)
  "文字变块属性（AI 入口，无交互，简化实现）。
  给文字加 xdata 标记 (1001 MINI-TEXT)(1002 ATT)(1 块名)(2 标签)，
  供后续识别并转为块属性。
  ents: 文字 ename 列表；blk-name: 目标块名。
  返回标记过的 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (or (null ents) (null blk-name))
    (progn (princ "@text:ai-text2att: 缺少参数") nil)
    (progn
      (command "_.UNDO" "Begin")
      (setq res nil)
      (foreach e ents
        (if (and (setq dx (entget e))
                 (wcmatch (cdr (assoc 0 dx)) "TEXT,MTEXT,ATTRIB")
                 (not (member (cons 1001 "MINI-TEXT") dx)))
            (progn
              (setq tag (@text:make-tag (cdr (assoc 1 dx))))
              (entmod (append dx
                              (list (cons 1001 "MINI-TEXT")
                                    (cons 1002 "ATT")
                                    (cons 1  (@text:str blk-name))
                                    (cons 2  (@text:str tag)))))
              (entupd e)
              (setq res (cons e res)))))
      (command "_.UNDO" "End")
      (if res
          (progn
            (princ (strcat "\n已标记 " (itoa (length res)) " 个文字"))
            (reverse res))
          (progn
            (princ "\n没有可标记的文字。")
            nil)))))

(defun @text:text2att (/ ss blk)
  "文字变块属性：给选中文字打 xdata 标记（简化实现）。
  返回 nil。"
  "nil"
  (if (setq blk (getstring "\n目标块名: "))
    (if (= "" blk)
        (princ "\n命令取消。")
      (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
          (if (@text:ai-text2att (pickset:to-list ss) blk)
              (princ "\n完成。")
              (princ "\n没有可标记的文字。"))
          (princ "\n未选择文字，命令取消。")))
    (princ "\n命令取消。"))
  (princ))

(defun @text:att-values (block / res att)
  "取块属性的 (标签 . 值) 列表。
  返回 assoc 列表。"
  "list"
  (foreach att (@text:block-attrs block)
    (setq res (cons (cons (cdr (assoc 2 (entget att)))
                          (cdr (assoc 1 (entget att))))
                    res)))
  (reverse res))

(defun @text:att-match-1 (block src-atts attrs / n att tag val)
  "刷单个块属性（内部函数）：按标签把 src-atts 的值刷到 block。
  返回有修改时 t，否则 nil。"
  "t 或 nil"
  (setq n 0)
  (foreach att (@text:block-attrs block)
    (setq tag (cdr (assoc 2 (entget att))))
    (if (and (or (null attrs)
                 (vl-some (function (lambda (x) (= x tag))) attrs))
             (setq val (assoc tag src-atts))
             (/= (cdr val) (cdr (assoc 1 (entget att)))))
        (progn
          (entmod (list-subst (cons 1 (cdr val)) (entget att)))
          (entupd att)
          (setq n (1+ n)))))
  (if (> n 0)
      t
      nil))

(defun @text:ai-match-att (src targets attrs / res src-atts e)
  "刷块属性（AI 入口，无交互）。
  把源块的属性值刷到同块名的其他块（按属性标签匹配）。
  src: 源块 ename；targets: 目标块 ename 列表；
  attrs: 属性标签列表 (nil=源块全部属性)。
  返回修改过的目标块 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (or (null src) (null targets))
    (progn (princ "@text:ai-match-att: 缺少参数") nil)
    (progn
      (setq src-atts (@text:att-values src))
      (if (null src-atts)
          (progn (princ "@text:ai-match-att: 源块没有属性") nil)
        (progn
          (command "_.UNDO" "Begin")
          (setq res nil)
          (foreach e targets
            (if (@text:att-match-1 e src-atts attrs)
                (setq res (cons e res))))
          (command "_.UNDO" "End")
          (if res
              (progn
                (princ (strcat "\n已刷新 " (itoa (length res)) " 个块"))
                (reverse res))
              (progn
                (princ "\n没有匹配的块属性。")
                nil)))))))

(defun @text:match-att (/ src ss attrstr)
  "刷块属性：把一个块的属性值刷到同名的其他块。
  返回 nil。"
  "nil"
  (if (setq src (car (entsel "\n选择源块(带属性): ")))
    (if (null (@text:att-values src))
        (princ "\n源块没有属性。")
      (if (setq ss (ssget "\n选择目标块(可多选): "))
          (progn
            (setq attrstr (getstring "\n属性标签列表,逗号分隔 (回车=全部): "))
            (if (@text:ai-match-att
                 src
                 (pickset:to-list ss)
                 (if (= "" attrstr)
                     nil
                     (mapcar (function (lambda (x) (@text:trim x)))
                             (string:to-list attrstr ","))))
                (princ "\n刷取完成。")
                (princ "\n没有匹配的块属性。"))
          )
          (princ "\n未选择目标块，命令取消。")))
    (princ "\n取消。"))
  (princ))

(defun @text:stat-add (stats tag val / tag-item val-item)
  "给统计表加一条计数（结构: (tag . (val . count)...)）。
  返回新统计表。"
  "list"
  (if (setq tag-item (assoc tag stats))
    (if (setq val-item (assoc val (cdr tag-item)))
        (setq stats (subst (cons tag
                                  (subst (cons val (1+ (cdr val-item)))
                                         val-item
                                         (cdr tag-item)))
                           tag-item stats))
        (setq stats (subst (cons tag (cons (cons val 1) (cdr tag-item)))
                           tag-item stats)))
    (setq stats (cons (cons tag (list (cons val 1))) stats)))
  stats)

(defun @text:att-list-csv (stats file / fp tag-item val-item val)
  "把统计表写入 CSV 文件（默认编码，中文可直接用 Excel 打开）。
  返回文件路径，失败 nil。"
  "string 或 nil"
  (if (null (setq fp (vl-catch-all-apply 'open (list file "w"))))
    (progn
      (princ (strcat "\n无法写入文件 " file))
      nil)
    (progn
      (write-line "标签,值,数量" fp)
      (foreach tag-item stats
        (foreach val-item (cdr tag-item)
          (setq val (string:subst-all "，" "," (car val-item)))
          (write-line (strcat (car tag-item) "," val ","
                              (itoa (cdr val-item))) fp)))
      (close fp)
      (princ (strcat "\n已写入 " file))
      file)))

(defun @text:att-list-draw (stats / res tag-item val-item y)
  "把统计表在 (0,0) 处画成文字列表。
  返回文字 ename 列表。"
  "list"
  (setq res nil)
  (setq y 0.0)
  (foreach tag-item stats
    (foreach val-item (cdr tag-item)
      (setq res (cons
                 (entity:addtext
                  (strcat (car tag-item) ": " (car val-item)
                          " x" (itoa (cdr val-item)))
                  (list 0.0 (- y) 0.0)
                  2.5 0 "LT")
                 res))
      (setq y (+ y 4.0))))
  (reverse res))

(defun @text:ai-att-list (blk-name out-file / ss res stats i n e att tag val)
  "块属性统计表（AI 入口，无交互）。
  统计图中某类块的属性值分布。
  blk-name: 块名 (nil=全部块)；
  out-file: CSV 输出文件路径 (nil=在图中画文字表格)。
  返回 CSV 时为文件路径，画图为文字 ename 列表，失败返回 nil。"
  "string 或 list 或 nil"
  (progn
    (setq ss (ssget "X"
                    (if blk-name
                        (list '(0 . "INSERT") (cons 2  (@text:str blk-name)))
                        (list '(0 . "INSERT")))))
    (if (null ss)
        (progn (princ "@text:ai-att-list: 未找到块") nil)
      (progn
        (setq stats nil)
        (setq i 0)
        (setq n (sslength ss))
        (while (< i n)
          (setq e (ssname ss i))
          (foreach att (@text:block-attrs e)
            (setq tag (cdr (assoc 2 (entget att))))
            (setq val (cdr (assoc 1 (entget att))))
            (setq stats (@text:stat-add stats tag val)))
          (setq i (1+ i)))
        (if (null stats)
            (progn (princ "@text:ai-att-list: 没有属性数据") nil)
          (if out-file
              (@text:att-list-csv stats out-file)
              (progn
                (command "_.UNDO" "Begin")
                (setq res (@text:att-list-draw stats))
                (command "_.UNDO" "End")
                res)))))))

(defun @text:att-list (/ blk out)
  "块属性统计表：统计某类块的属性值分布。
  返回 nil。"
  "nil"
  (setq blk (getstring "\n块名 (回车统计全部块): "))
  (setq out (getstring "\nCSV输出文件路径 (回车在图中绘制): "))
  (if (@text:ai-att-list (if (= "" blk) nil blk)
                             (if (= "" out) nil out))
    (princ "\n完成。")
    (princ "\n没有统计数据。"))
  (princ))
