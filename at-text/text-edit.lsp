;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mini-text 编辑类命令：全能文字编辑 sedit、批量更改 cht、重排版 text-order、
;; 连接 join-text / text2mtext、打断 exp-text、外框 tbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ============ 全能文字编辑 sedit ============

(defun @text:cut-str (str n last-p / len)
  "删除字符串开头 n 个字符（last-p 为 t 时删除末尾 n 个字符）。
  返回结果字符串。"
  "string"
  (setq n (fix (float  (@text:str n))))
  (setq len (string:length str))
  (cond
    ((<= n 0) str)
    ((= last-p t)
     (if (>= len n)
         (if (> len n) (string:substr str 1 (- len n)) "")
         ""))
    ((>= len n)
     (if (> len n) (string:substr str (1+ n) (- len n)) ""))
    (t "")))

(defun @text:sedit-apply (infos opt arg1 arg2 / res info new o)
  "对 text-info 列表应用编辑操作（内部函数）。
  opt: prefix/suffix/replace/uc/lc/del/del-last。
  返回修改过的 ename 列表。"
  "list"
  (if (null infos)
    nil
    (progn
      (setq o (strcase  (@text:str opt)))
      (setq res nil)
      (foreach info infos
        (setq new nil)
        (cond
          ((= "PREFIX" o)
           (if (and arg1 (/= ""  (@text:str arg1)))
               (setq new (strcat  (@text:str arg1) (cdr (assoc 'content info))))))
          ((= "SUFFIX" o)
           (if (and arg1 (/= ""  (@text:str arg1)))
               (setq new (strcat (cdr (assoc 'content info))  (@text:str arg1)))))
          ((= "REPLACE" o)
           (if (and arg1 (/= ""  (@text:str arg1)))
               (setq new (string:subst-all  (@text:str arg2)  (@text:str arg1)
                                           (cdr (assoc 'content info))))))
          ((= "UC" o)
           (setq new (string:case (cdr (assoc 'content info)) 1)))
          ((= "LC" o)
           (setq new (string:case (cdr (assoc 'content info)) 2)))
          ((= "DEL" o)
           (if arg1
               (setq new (@text:cut-str (cdr (assoc 'content info)) arg1 nil))))
          ((= "DEL-LAST" o)
           (if arg1
               (setq new (@text:cut-str (cdr (assoc 'content info)) arg1 t))))
          (t
           (princ (strcat "@text: 未知操作 " o))))
        (if (and new (/= new (cdr (assoc 'content info))))
            (progn
              (@text:set-content (cdr (assoc 'ent info)) new)
              (setq res (cons (cdr (assoc 'ent info)) res)))))
      (if res
          (princ (strcat "\n已修改 " (itoa (length res)) " 个文字"))
          (princ "\n无修改（内容未变化或未匹配操作）"))
      (reverse res))))

(defun @text:ai-sedit (ents opt arg1 arg2 / o)
  "全能文字编辑（AI 入口，无交互）。
  ents: 文字图元 ename 列表；opt: prefix/suffix/replace/uc/lc/del/del-last；
  arg1: 前缀/后缀串、替换旧串或删除字符数；arg2: replace 的新串。
  返回修改过的 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (null ents)
    (progn (princ "@text:ai-sedit: ents 为空") nil)
    (progn
      (setq o (strcase  (@text:str opt)))
      (cond
        ((not (wcmatch o "PREFIX,SUFFIX,REPLACE,UC,LC,DEL,DEL-LAST"))
         (princ (strcat "@text:ai-sedit: 未知操作 " o))
         nil)
        ((or (= o "PREFIX") (= o "SUFFIX"))
         (if (and arg1 (/= ""  (@text:str arg1)))
             (@text:sedit-apply (@text:ents-to-infos ents) o arg1 arg2)
             (progn (princ "@text:ai-sedit: 前缀/后缀不能为空") nil)))
        ((= o "REPLACE")
         (if (and arg1 (/= ""  (@text:str arg1)))
             (@text:sedit-apply (@text:ents-to-infos ents) o arg1 arg2)
             (progn (princ "@text:ai-sedit: 替换旧串不能为空") nil)))
        ((wcmatch o "DEL,DEL-LAST")
         (if arg1
             (@text:sedit-apply (@text:ents-to-infos ents) o arg1 nil)
             (progn (princ "@text:ai-sedit: 缺少删除字符数") nil)))
        (t
         (@text:sedit-apply (@text:ents-to-infos ents) o nil nil))))))

(defun @text:sedit (/ ss infos opt arg1 arg2)
  "全能文字编辑：对选中文字批量加前缀/后缀/替换子串/删字符/转大小写。
  返回 nil。"
  "nil"
  (@::prompt "全能文字编辑：前缀/后缀/替换/删字符/大小写")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (setq infos (@text:get-text-ents ss))
      (if (null infos)
          (princ "\n选择集中没有可编辑文字。")
        (progn
          (princ (strcat "\n已选中 " (itoa (length infos)) " 个文字。"))
          (setq opt (getkword "\n操作 [P前缀/S后缀/R替换/D删字符/U大写/L小写] <P>: "))
          (cond
            ((= "S" (strcase opt))
             (setq arg1 (getstring "\n输入后缀 (回车取消): "))
             (if (/= "" arg1)
                 (@text:sedit-apply infos "suffix" arg1 nil)))
            ((= "R" (strcase opt))
             (setq arg1 (getstring "\n输入要替换的子串 (回车取消): "))
             (if (/= "" arg1)
                 (progn
                   (setq arg2 (getstring "\n输入替换后的字符串 (回车替换为空): "))
                   (@text:sedit-apply infos "replace" arg1 arg2))))
            ((= "D" (strcase opt))
             (setq arg1 (if (getint "\n删除开头 N 个字符 <1>: ") (getint "\n删除开头 N 个字符 <1>: ") 1))
             (@text:sedit-apply infos "del" arg1 nil))
            ((= "U" (strcase opt))
             (@text:sedit-apply infos "uc" nil nil))
            ((= "L" (strcase opt))
             (@text:sedit-apply infos "lc" nil nil))
            (t
             (setq arg1 (getstring "\n输入前缀 (回车取消): "))
             (if (/= "" arg1)
                 (@text:sedit-apply infos "prefix" arg1 nil)))))
    )
    (princ "\n未选择图元，命令取消。"))
  (princ)))

;; ============ 文字批量更改 cht ============

(defun @text:ai-cht (ents findstr repl style height match / res infos info content hit new m)
  "文字批量更改（AI 入口，无交互）。
  ents: 文字 ename 列表；findstr: 查找串 (nil=匹配全部)；
  repl: 替换串 (nil=不改内容)；style: 目标样式 (nil=不改)；
  height: 目标字高 (nil=不改)；
  match: exact(整词)/like(包含,默认)/regex(正则)。
  返回修改过的 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (null ents)
    (progn (princ "@text:ai-cht: ents 为空") nil)
    (progn
      (setq m  (@text:str match))
      (setq infos (@text:ents-to-infos ents))
      (if (null infos)
          (progn (princ "@text:ai-cht: 无有效文字") nil)
        (progn
          (startundo)
          (setq res nil)
          (foreach info infos
            (setq content (cdr (assoc 'content info)))
            (cond
              ((null findstr) (setq hit t))
              ((= "EXACT" (strcase m)) (setq hit (= findstr content)))
              ((= "REGEX" (strcase m))
               (setq hit (string:regexp-search content findstr "g")))
              (t (setq hit (vl-string-search findstr content))))
            (if hit
                (progn
                  (if (and repl (/= "" repl))
                      (progn
                        (cond
                          ((= "REGEX" (strcase m))
                           (setq new (string:regexp-replace content repl findstr "g")))
                          (t
                           (setq new (string:subst-all repl findstr content))))
                        (if (/= new content)
                            (@text:set-content (cdr (assoc 'ent info)) new))))
                  (if style
                      (@text:set-style-ext (cdr (assoc 'ent info)) style))
                  (if height
                      (@text:set-height (cdr (assoc 'ent info)) height))
                  (setq res (cons (cdr (assoc 'ent info)) res)))))
          (endundo)
          (princ (strcat "\n已处理 " (itoa (length res)) " 个文字"))
          (reverse res))))))

(defun @text:cht (/ ss infos findstr repl style height use-regex)
  "文字批量更改：按内容查找替换（可正则），批量改样式/字高。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (setq infos (@text:get-text-ents ss))
      (if (null infos)
          (princ "\n选择集中没有可编辑文字。")
        (progn
          (setq findstr (getstring "\n查找串 (回车匹配全部): "))
          (setq repl (getstring "\n替换串 (回车不改内容): "))
          (setq use-regex (wcmatch (getkword "\n使用正则表达式? [N否/Y是] <N>: ") "*Y*"))
          (setq style (getstring "\n新样式名 (回车保持): "))
          (setq height (getreal "\n新字高 (回车保持): "))
          (@text:ai-cht
           (@text:infos-to-ents infos)
           (if (= "" findstr) nil findstr)
           (if (= "" repl) nil repl)
           (if (= "" style) nil style)
           (if (null height) nil height)
           (if use-regex "regex" "like"))
          (princ "\n完成。")))
    )
    (princ "\n未选择图元，命令取消。"))
  (princ))

;; ============ 文字重排版 text-order ============

(defun @text:ai-text-order (ents cols row-gap x-gap max-width / infos rows row xw hmax x0 y0 i n w res)
  "文字重排版（AI 入口，无交互）。
  ents: 文字 ename 列表；cols: 每行个数 (nil=按宽度断行)；
  row-gap: 行距 (nil=最大字高1.5倍)；x-gap: 水平间距 (nil=最大字高0.5倍)；
  max-width: 每行最大宽度 (cols 为 nil 时生效，两者皆 nil 则排成一行)。
  返回移动过的 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (null ents)
    (progn (princ "@text:ai-text-order: ents 为空") nil)
    (progn
      (setq infos (@text:sort-by-position (@text:ents-to-infos ents)))
      (if (null infos)
          (progn (princ "@text:ai-text-order: 无有效文字") nil)
        (progn
          (setq hmax (apply 'max
                            (mapcar (function (lambda (x)
                                                (cdr (assoc 'height x)))) infos)))
          (if (null row-gap) (setq row-gap (* 1.5 hmax)))
          (if (null x-gap) (setq x-gap (* 0.5 hmax)))
          (setq x0 (apply 'min
                          (mapcar (function (lambda (x)
                                              (car (assoc 'pt x)))) infos)))
          (setq y0 (apply 'max
                          (mapcar (function (lambda (x)
                                              (cadr (assoc 'pt x)))) infos)))
          (setq rows nil)
          (setq row nil)
          (setq xw 0.0)
          (foreach info infos
            (setq w (@text:width (cdr (assoc 'ent info))))
            (cond
              ((null row)
               (setq row (list info) xw w))
              ((and cols (>= (length row) cols))
               (setq rows (cons (reverse row) rows) row (list info) xw w))
              ((and max-width (> (+ xw x-gap w) max-width))
               (setq rows (cons (reverse row) rows) row (list info) xw w))
              (t
               (setq row (cons info row) xw (+ xw x-gap w)))))
          (if row (setq rows (cons (reverse row) rows)))
          (setq rows (reverse rows))
          (startundo)
          (setq res nil)
          (setq i 0)
          (foreach row rows
            (setq xw 0.0)
            (foreach info row
              (@text:move-ent (cdr (assoc 'ent info))
                                  (list (+ x0 xw)
                                        (- y0 (* i row-gap))
                                        0.0))
              (setq xw (+ xw x-gap (@text:width (cdr (assoc 'ent info)))))
              (setq res (cons (cdr (assoc 'ent info)) res)))
            (setq i (1+ i)))
          (endundo)
          (princ (strcat "\n已重排 " (itoa (length res)) " 个文字，共 "
                         (itoa (length rows)) " 行"))
          (reverse res))))))

(defun @text:text-order (/ ss infos cols max-width row-gap x-gap)
  "文字重排版：选中文字按每行 N 个或按宽度断行重新排列。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (setq infos (@text:get-text-ents ss))
      (if (null infos)
          (princ "\n选择集中没有可编辑文字。")
        (progn
          (setq cols (getint "\n每行个数 (回车按宽度断行): "))
          (if (null cols)
              (setq max-width (getreal "\n每行最大宽度 (回车排成一行): ")))
          (setq row-gap (getreal "\n行距 (回车默认): "))
          (setq x-gap (getreal "\n水平间距 (回车默认): "))
          (@text:ai-text-order (@text:infos-to-ents infos)
                                   cols row-gap x-gap max-width)
          (princ "\n完成。")))
    )
    (princ "\n未选择图元，命令取消。"))
  (princ))

;; ============ 智能连接多行文字 join-text / 一组文字变多重 text2mtext ============

(defun @text:join-impl (ents style height wrap / infos content width pt newent)
  "文字连接内部实现。
  wrap: t=按原始最大宽度自动换行，nil=逐行不换行。
  返回新 MTEXT 的 ename，失败返回 nil。"
  "ename 或 nil"
  (if (null ents)
    nil
    (progn
      (setq infos (@text:sort-by-position (@text:ents-to-infos ents)))
      (if (null infos)
          nil
        (progn
          (setq content "")
          (foreach info infos
            (setq content
                  (if (= "" content)
                      (cdr (assoc 'content info))
                      (strcat content "\\P" (cdr (assoc 'content info))))))
          (setq width (if wrap
                        (apply 'max
                               (mapcar (function (lambda (x)
                                                   (@text:width
                                                    (cdr (assoc 'ent x)))))
                                     infos))
                        0.0))
          (setq pt (list (apply 'min
                                (mapcar (function (lambda (x)
                                                    (car (assoc 'pt x)))) infos))
                         (apply 'max
                                (mapcar (function (lambda (x)
                                                    (cadr (assoc 'pt x)))) infos))
                         0.0))
          (startundo)
          (foreach info infos (entdel (cdr (assoc 'ent info))))
          (setq newent (@text:make-mtext content pt
                                             (if style style (cdr (assoc 'style (car infos))))
                                             (if height height (cdr (assoc 'height (car infos))))
                                             width))
          (endundo)
          (princ (strcat "\n已生成 MTEXT"
                         (if wrap (strcat "，换行宽度 " (rtos width 2 2)) "")))
          newent)))))

(defun @text:ai-join-text (ents style height)
  "智能连接多行文字（AI 入口，无交互）。
  将多个文字（含 MTEXT 多行）连接为一个 MTEXT，按位置排序，
  按原始文字最大宽度自动换行。
  ents: 文字 ename 列表；style: 目标样式 (nil=用第一个文字的样式)；
  height: 目标字高 (nil=用第一个文字的字高)。
  返回新 MTEXT 的 ename，失败返回 nil。"
  "ename 或 nil"
  (@text:join-impl ents style height t))

(defun @text:join-text (/ ss infos style height)
  "智能连接多行文字：把选中的多个文字连接为一个 MTEXT。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (setq infos (@text:get-text-ents ss))
      (if (null infos)
          (princ "\n选择集中没有可编辑文字。")
        (progn
          (setq style (getstring "\n目标样式 (回车用原样式): "))
          (setq height (getreal "\n目标字高 (回车用原字高): "))
          (if (@text:ai-join-text (@text:infos-to-ents infos)
                                      (if (= "" style) nil style)
                                      (if (null height) nil height))
              (princ "\n完成。")
              (princ "\n生成失败。"))))
    )
    (princ "\n未选择图元，命令取消。"))
  (princ))

(defun @text:ai-text2mtext (ents style height)
  "智能一组文字变多重文字（AI 入口，无交互）。
  将多个文字转为一个 MTEXT，每个文字独占一行，不自动换行。
  ents: 文字 ename 列表；style: 目标样式 (nil=用第一个文字的样式)；
  height: 目标字高 (nil=用第一个文字的字高)。
  返回新 MTEXT 的 ename，失败返回 nil。"
  "ename 或 nil"
  (@text:join-impl ents style height nil))

(defun @text:text2mtext (/ ss infos style height)
  "智能一组文字变多重文字：多个单行文字按位置变成一个 MTEXT。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (setq infos (@text:get-text-ents ss))
      (if (null infos)
          (princ "\n选择集中没有可编辑文字。")
        (progn
          (setq style (getstring "\n目标样式 (回车用原样式): "))
          (setq height (getreal "\n目标字高 (回车用原字高): "))
          (if (@text:ai-text2mtext (@text:infos-to-ents infos)
                                       (if (= "" style) nil style)
                                       (if (null height) nil height))
              (princ "\n完成。")
              (princ "\n生成失败。"))))
    )
    (princ "\n未选择图元，命令取消。"))
  (princ))

;; ============ 文字打断 exp-text ============

(defun @text:ai-exp-text (ent pt / dx content len base ang w ratio dxvec dyvec idx part1 part2 newent lines i l)
  "文字打断（AI 入口，无交互）。
  在 pt 处把一个 TEXT/MTEXT 打断成两个。
  ent: 文字 ename；pt: 打断点 (x y)。
  返回两个文字 ename 列表（第一个为原图元），失败返回 nil。"
  "list 或 nil"
  (if (or (null ent) (null pt))
    (progn (princ "@text:ai-exp-text: 缺少参数") nil)
    (if (and (setq dx (entget ent))
             (wcmatch (cdr (assoc 0 dx)) "TEXT,MTEXT"))
      (progn
        (startundo)
        (cond
          ((= "TEXT" (cdr (assoc 0 dx)))
           (setq content (cdr (assoc 1 dx)))
           (setq len (string:length content))
           (if (< len 2)
               (progn
                 (endundo)
                 (princ "\n文字太短，无法打断。")
                 nil)
             (progn
               (setq base (cdr (assoc 10 dx)))
               (setq ang (or (cdr (assoc 50 dx)) 0.0))
               (setq w (@text:width ent))
               (if (> w 0.0)
                 (progn
                   (setq dxvec (- (car pt) (car base))
                         dyvec (- (cadr pt) (cadr base)))
                   (setq ratio (/ (+ (* dxvec (cos ang)) (* dyvec (sin ang))) w))
                   (setq ratio (min 0.95 (max 0.05 ratio)))
                   (setq idx (fix (* ratio len)))
                   (setq idx (min (1- len) (max 1 idx)))
                   (setq part1 (string:substr content 1 idx))
                   (setq part2 (string:substr content (1+ idx) (- len idx)))
                   (@text:set-content ent part1)
                   (setq newent (entity:make-text part2
                                                  (polar base (* ratio w) ang)
                                                  (cdr (assoc 40 dx))
                                                  ang
                                                  (or (cdr (assoc 41 dx)) 0.8)
                                                  0
                                                  "LB"))
                   (entmod (append (entget newent)
                                   (list (cons 8 (cdr (assoc 8 dx)))
                                         (cons 7 (cdr (assoc 7 dx))))))
                   (entupd newent)
                   (endundo)
                   (princ "\n打断完成。")
                   (list ent newent))
                 (progn
                   (endundo)
                   (princ "\n无法计算文字宽度。")
                   nil))))))
          ((= "MTEXT" (cdr (assoc 0 dx)))
           (setq content (text:get-mtext ent))
           (setq lines (string:to-list content "\\P"))
           (if (< (length lines) 2)
               (progn
                 (endundo)
                 (princ "\nMTEXT 只有一行，无法打断。")
                 nil)
             (progn
               (setq base (cdr (assoc 10 dx)))
               (setq ratio (fix (/ (max 0.0 (- (cadr base) (cadr pt)))
                                   (cdr (assoc 40 dx)))))
               (setq idx (max 1 (min (1- (length lines)) ratio)))
               (setq part1 "")
               (setq part2 "")
               (setq i 0)
               (foreach l lines
                 (if (< i idx)
                     (setq part1 (strcat part1
                                         (if (= "" part1) l (strcat "\\P" l))))
                     (setq part2 (strcat part2
                                         (if (= "" part2) l (strcat "\\P" l)))))
                 (setq i (1+ i)))
               (@text:set-content ent part1)
               (setq newent (@text:make-mtext part2
                                                  (list (car base)
                                                        (- (cadr base)
                                                           (* idx (cdr (assoc 40 dx))))
                                                        0.0)
                                                  (cdr (assoc 7 dx))
                                                  (cdr (assoc 40 dx))
                                                  (@text:width ent)))
               (entmod (append (entget newent)
                               (list (cons 8 (cdr (assoc 8 dx))))))
               (endundo)
               (princ "\n打断完成。")
               (list ent newent)))))
      (progn
        (princ "\n请选择 TEXT 或 MTEXT 图元。")
        nil))))

(defun @text:exp-text (/ e pt)
  "文字打断：在指定点把一个文字打断成两个。
  返回 nil。"
  "nil"
  (if (setq e (car (entsel "\n选择要打断的文字: ")))
    (if (wcmatch (cdr (assoc 0 (entget e))) "TEXT,MTEXT")
        (if (setq pt (getpoint "\n打断点: "))
            (@text:ai-exp-text e pt)
            (princ "\n取消。"))
        (princ "\n请选择 TEXT 或 MTEXT。"))
    (princ "\n取消。"))
  (princ))

;; ============ 文字加外框并关联 tbox ============

(defun @text:make-tbox-rect (box pad layer / x1 x2 y1 y2 ent)
  "由 4 点框外扩 pad 创建闭合矩形框（LWPOLYLINE）。
  返回 ename。"
  "ename"
  (setq x1 (- (apply 'min (mapcar 'car box)) pad)
        x2 (+ (apply 'max (mapcar 'car box)) pad)
        y1 (- (apply 'min (mapcar 'cadr box)) pad)
        y2 (+ (apply 'max (mapcar 'cadr box)) pad))
  (setq ent
        (entmakex
         (list '(0 . "LWPOLYLINE")
               '(100 . "AcDbEntity")
               '(100 . "AcDbPolyline")
               '(90 . 4)
               '(70 . 1)
               (cons 8 layer)
               (cons 10 (list x1 y1 0.0))
               (cons 10 (list x2 y1 0.0))
               (cons 10 (list x2 y2 0.0))
               (cons 10 (list x1 y2 0.0))
               '(210 0.0 0.0 1.0))))
  ent)

(defun @text:resize-rect (rect-ent box pad / x1 x2 y1 y2)
  "按 4 点框 box 外扩 pad 原地调整矩形框（保留 xdata）。
  返回 ename。"
  "ename"
  (setq x1 (- (apply 'min (mapcar 'car box)) pad)
        x2 (+ (apply 'max (mapcar 'car box)) pad)
        y1 (- (apply 'min (mapcar 'cadr box)) pad)
        y2 (+ (apply 'max (mapcar 'cadr box)) pad))
  (entmod (append
           (list '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbPolyline")
                 '(90 . 4)
                 '(70 . 1)
                 (cons 10 (list x1 y1 0.0))
                 (cons 10 (list x2 y1 0.0))
                 (cons 10 (list x2 y2 0.0))
                 (cons 10 (list x1 y2 0.0)))
           (@text:xdata-groups (entget rect-ent))))
  (entupd rect-ent)
  rect-ent)

(defun @text:ai-tbox (ents padding layer / res infos info e box rect)
  "文字加外框并关联（AI 入口，无交互）。
  为每个文字外围画矩形框，xdata 标记 MINI-TEXT/TBOX 并记录
  文字句柄与边距，可用 ai-tbox-refresh 刷新。
  ents: 文字 ename 列表；padding: 框边距 (nil=0.5倍字高)；
  layer: 框图层 (nil=文字所在图层)。
  返回创建的框 ename 列表，失败返回 nil。"
  "list 或 nil"
  (if (null ents)
    (progn (princ "@text:ai-tbox: ents 为空") nil)
    (progn
      (setq infos (@text:ents-to-infos ents))
      (if (null infos)
          (progn (princ "@text:ai-tbox: 无有效文字") nil)
        (progn
          (startundo)
          (setq res nil)
          (foreach info infos
            (setq e (cdr (assoc 'ent info)))
            (if (setq box (textbox (entget e)))
              (progn
                (setq rect (@text:make-tbox-rect box
                                                     (or padding
                                                         (* 0.5 (cdr (assoc 'height info))))
                                                     (if layer layer (cdr (assoc 'layer info)))))
                (entmod (append (entget rect)
                                (list (cons 1001 "MINI-TEXT")
                                      (cons 1002 "TBOX")
                                      (cons 1 (cdr (assoc 5 e)))
                                      (cons 40 (or padding
                                                    (* 0.5 (cdr (assoc 'height info))))))))
                (setq res (cons rect res)))))
          (endundo)
          (if res
              (progn
                (princ (strcat "\n已生成 " (itoa (length res)) " 个文字外框"))
                (reverse res))
              (princ "\n没有可加框的文字。")))))))

(defun @text:ai-tbox-refresh (box-ent / dx e box pad)
  "刷新单个文字外框（AI 入口，无交互）。
  box-ent: 外框 ename（需有 xdata MINI-TEXT/TBOX 标记），
  按文字当前大小重算框。
  返回成功 t，失败 nil。"
  "t 或 nil"
  (if (and (setq dx (entget box-ent))
           (wcmatch (cdr (assoc 0 dx)) "LWPOLYLINE,POLYLINE")
           (assoc 1001 dx)
           (assoc 1002 dx)
           (setq e (handent (@text:xdata-get dx "MINI-TEXT" "TBOX")))
           (setq box (textbox (entget e)))
           (setq pad (or (cdr (assoc 40 dx)) 0.0)))
    (progn
      (@text:resize-rect box-ent box pad)
      (princ "\n外框已刷新。")
      t)
    nil))

(defun @text:ai-tbox-refresh-all (/ ss res i n e)
  "刷新全部文字外框（AI 入口，无交互）。
  遍历图中所有带 xdata MINI-TEXT/TBOX 标记的框并刷新。
  返回刷新过的 ename 列表，没有则 nil。"
  "list 或 nil"
  (if (setq ss (ssget "X" '((0 . "LWPOLYLINE,POLYLINE"))))
    (progn
      (setq res nil)
      (setq i 0)
      (setq n (sslength ss))
      (while (< i n)
        (setq e (ssname ss i))
        (if (and (assoc 1001 (entget e))
                 (assoc 1002 (entget e))
                 (@text:xdata-get (entget e) "MINI-TEXT" "TBOX"))
            (if (@text:ai-tbox-refresh e)
                (setq res (cons e res))))
        (setq i (1+ i)))
      (if res
          (progn
            (princ (strcat "\n已刷新 " (itoa (length res)) " 个文字外框"))
            (reverse res))
          (progn
            (princ "\n未找到文字外框。")
            nil)))
    (progn
      (princ "\n未找到文字外框。")
      nil)))

(defun @text:tbox (/ ss padding layer)
  "文字加外框并关联：为选中文字画关联外框。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (setq padding (getreal "\n框边距 (回车默认): "))
      (setq layer (getstring "\n框图层 (回车用文字图层): "))
      (if (@text:ai-tbox (@text:infos-to-ents (@text:get-text-ents ss))
                             padding
                             (if (= "" layer) nil layer))
          (princ "\n完成。文字改动后可用「刷新文字外框」调整框大小。")
          (princ "\n没有可加框的文字。"))
    )
    (princ "\n未选择图元，命令取消。"))
  (princ))
