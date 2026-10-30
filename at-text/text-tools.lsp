;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mini-text 迷你文字
;; 复现 iMini 文字类命令：全能文字编辑、批量更改、重排版、连接/打断、
;; 文字变多重文字、外框关联、粘贴板、字体替换、块属性、词库输入、多文件查找替换。
;; Author: VitalGG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 注册屏幕菜单
(@:add-menus
 '(("文字工具"
    ("词库输入" "(@text:text-lib)")
    ("全能文字编辑" "(@text:sedit)")
    ("文字批量更改Cht" "(@text:cht)")
    ("文字重排版" "(@text:text-order)")
    ("智能连接多行文字" "(@text:join-text)")
    ("智能一组文字变多重文字" "(@text:text2mtext)")
    ("文字打断" "(@text:exp-text)")
    ("文字加外框并关联" "(@text:tbox)")
    ("刷新文字外框" "(@text:ai-tbox-refresh-all)")
    ("输出文字到粘贴板" "(@text:text2clipboard)")
    ("多文件查找替换" "(@text:find)")
    ("自动替换缺失的字库" "(@text:fix-fonts)")
    ("全部字体用我的字库" "(@text:use-myfonts)")
    ("块属性转文字" "(@text:att2text)")
    ("文字变块属性" "(@text:text2att)")
    ("刷块属性" "(@text:match-att)")
    ("块属性统计表" "(@text:att-list)")
    )))
;; 按需加载函数库
(if (null text:get-mtext) (require 'text:*))
(if (null string:subst-all) (require 'string:*))
(if (null entity:make-text) (require 'entity:*))
(if (null pickset:to-list) (require 'pickset:*))
(if (null clipboard:setdata) (require 'clipboard:*))

;; ============ 公共辅助 ============

(defun @text:str (x)
  "把任意值转成字符串（整数不带小数点，实数保留 6 位）。
  返回 string。"
  "string"
  (cond
    ((null x) "")
    ((= "STR" (type x)) x)
    ((= "INT" (type x)) (itoa x))
    ((= "REAL" (type x))
     (if (= x (fix x)) (itoa (fix x)) (rtos x 2 6)))
    (t (rtos (float x) 2 6))))

(defun @text:trim (s)
  "去除字符串首尾的空格和制表符。
  返回 string。"
  "string"
  (if (null s)
      ""
      (progn
        (while (and (> (strlen s) 0) (wcmatch (substr s 1 1) " ,\t"))
          (setq s (substr s 2)))
        (while (and (> (strlen s) 0)
                    (wcmatch (substr s (strlen s) 1) " ,\t"))
          (setq s (substr s 1 (- (strlen s) 1))))
        s)))

(defun @text:text-info (e / dx etype)
  "取单个文字图元的信息（TEXT/MTEXT/ATTRIB）。
  返回 text-info 关联表：(ent . ename)(content . 字符串)(style . 样式名)
  (height . 字高)(pt . 插入点)(angle . 角度)(layer . 图层)(type . 类型)
  ATTRIB 另含 (tag . 属性标签)。非文字图元返回 nil。"
  "text-info 或 nil"
  (if (setq dx (entget e))
    (progn
      (setq etype (cdr (assoc 0 dx)))
      (cond
        ((wcmatch etype "TEXT,ATTRIB")
         (append
          (list (cons 'ent e)
                (cons 'content (cdr (assoc 1 dx)))
                (cons 'style (cdr (assoc 7 dx)))
                (cons 'height (cdr (assoc 40 dx)))
                (cons 'pt (cdr (assoc 10 dx)))
                (cons 'angle (if (cdr (assoc 50 dx)) (cdr (assoc 50 dx)) 0.0))
                (cons 'layer (cdr (assoc 8 dx)))
                (cons 'type etype))
          (if (= "ATTRIB" etype)
              (list (cons 'tag (cdr (assoc 2 dx))))
              nil)))
        ((= "MTEXT" etype)
         (list (cons 'ent e)
               (cons 'content (text:get-mtext e))
               (cons 'style (cdr (assoc 7 dx)))
               (cons 'height (cdr (assoc 40 dx)))
               (cons 'pt (cdr (assoc 10 dx)))
               (cons 'angle (if (cdr (assoc 50 dx)) (cdr (assoc 50 dx)) 0.0))
               (cons 'layer (cdr (assoc 8 dx)))
               (cons 'type type))))))
  nil)

(defun @text:get-text-ents (ss / res i n e info)
  "从选择集收集文字信息（TEXT/MTEXT/ATTRIB，含块属性）。
  返回 text-info 列表。"
  "list"
  (setq res nil)
  (if ss
    (progn
      (setq n (sslength ss))
      (setq i 0)
      (while (< i n)
        (setq e (ssname ss i))
        (cond
          ((wcmatch (cdr (assoc 0 (entget e))) "TEXT,MTEXT,ATTRIB")
           (if (setq info (@text:text-info e))
               (setq res (cons info res))))
          ((= "INSERT" (cdr (assoc 0 (entget e))))
           (foreach e2 (@text:block-attrs e)
             (if (setq info (@text:text-info e2))
                 (setq res (cons info res))))))
        (setq i (1+ i))))
  (reverse res)))

(defun @text:ents-to-infos (ents / res e info)
  "文字 ename 列表转 text-info 列表（忽略无效图元）。
  返回 list。"
  "list"
  (foreach e ents
    (if (setq info (@text:text-info e))
        (setq res (cons info res))))
  (reverse res))

(defun @text:infos-to-ents (infos / res info)
  "从 text-info 列表提取 ename 列表。
  返回 list。"
  "list"
  (foreach info infos
    (setq res (cons (cdr (assoc 'ent info)) res)))
  (reverse res))

(defun @text:set-content (e str / dx new g)
  "设置文字图元内容（TEXT/MTEXT/ATTRIB，MTEXT 会移除旧的多段 group 1）。
  返回成功 t，失败 nil。"
  "t 或 nil"
  (if (setq dx (entget e))
    (progn
      (setq new nil)
      (foreach g dx
        (cond
          ((= 1 (car g))
           (if (null (assoc 1 new))
               (setq new (cons (cons 1 str) new))))
          (t
           (setq new (cons g new)))))
      (entmod (reverse new))
      (entupd e)
      t)
    nil))

(defun @text:set-style-ext (e style / dx)
  "设置文字图元样式。
  返回成功 t，失败 nil。"
  "t 或 nil"
  (if (and style (setq dx (entget e)))
    (progn
      (entmod (list-subst (cons 7 style) dx))
      (entupd e)
      t)
    nil))

(defun @text:set-height (e h / dx)
  "设置文字图元字高。
  返回成功 t，失败 nil。"
  "t 或 nil"
  (if (and h (setq dx (entget e)))
    (progn
      (entmod (list-subst (cons 40 h) dx))
      (entupd e)
      t)
    nil))

(defun @text:move-ent (e pt / dx)
  "移动文字图元到 pt（修改插入点 group 10）。
  返回成功 t，失败 nil。"
  "t 或 nil"
  (if (setq dx (entget e))
    (progn
      (entmod (list-subst (cons 10 pt) dx))
      (entupd e)
      t)
    nil))

(defun @text:width (e / box)
  "取文字图元显示宽度（旋转文字取近似 x 向跨度）。
  返回数字。"
  "number"
  (if (setq box (textbox (entget e)))
    (- (apply 'max (mapcar 'car box))
       (apply 'min (mapcar 'car box)))
    0.0))

(defun @text:sort-by-position (infos)
  "按位置排序 text-info 列表（y 降序、x 升序，即从上到下、从左到右）。
  返回新列表。"
  "list"
  (vl-sort infos
           (function (lambda (a b)
                       (if (/= (cadr (assoc 'pt a)) (cadr (assoc 'pt b)))
                           (> (cadr (assoc 'pt a)) (cadr (assoc 'pt b)))
                           (< (car (assoc 'pt a)) (car (assoc 'pt b))))))))

(defun @text:make-mtext (str pt style height width / ent)
  "创建 MTEXT（行距 1.0，左上对齐，附着于 (0,0,1)）。
  返回 MTEXT 的 ename。"
  "ename"
  (setq ent
        (entmakex
         (list '(0 . "MTEXT")
               '(100 . "AcDbEntity")
               (cons 8 (getvar "clayer"))
               (cons 10 pt)
               (cons 40 height)
               (cons 41 1.0)
               '(46 . 0.0)
               '(71 . 1)
               '(72 . 5)
               (cons 1 str)
               (cons 7 style)
               '(210 0.0 0.0 1.0)
               '(11 1.0 0.0 0.0)
               (cons 42 width)
               '(43 . 0.0)
               '(50 . 0.0)
               '(73 . 1)
               '(44 . 1.0)
               '(45 . 0.0))))
  (entupd ent)
  ent)

(defun @text:block-attrs (block-e / e res)
  "取插入块（INSERT）的属性实体列表。
  返回 ATTRIB ename 列表，无属性或无效块返回 nil。"
  "list 或 nil"
  (if (and (wcmatch (cdr (assoc 0 (entget block-e))) "INSERT")
           (= 1 (cdr (assoc 66 (entget block-e)))))
    (progn
      (setq e (entnext block-e))
      (setq res nil)
      (while (and e (eq "ATTRIB" (cdr (assoc 0 (entget e)))))
        (setq res (cons e res))
        (setq e (entnext e)))
      (reverse res))
    nil))

(defun @text:entity-content (e / dx)
  "取文字图元的原始内容（MTEXT 含格式码）。
  返回 string 或 nil。"
  "string 或 nil"
  (if (setq dx (entget e))
    (cond
      ((= "MTEXT" (cdr (assoc 0 dx)))
       (text:get-mtext e))
      ((wcmatch (cdr (assoc 0 dx)) "TEXT,ATTRIB")
       (cdr (assoc 1 dx)))))
  nil)

(defun @text:entity-contains (e str / c)
  "判断图元文字内容是否包含 str。
  返回 t/nil。"
  "t 或 nil"
  (if (setq c (@text:entity-content e))
    (vl-string-search str c)
    nil))

(defun @text:xdata-get (dx app tag / res found g)
  "取 xdata 标记 (1001 app)(1002 tag) 之后的 group 1 字符串值。
  返回 string 或 nil。"
  "string 或 nil"
  (if (and (assoc 1001 dx) (assoc 1002 dx))
    (progn
      (foreach g dx
        (cond
          ((and (= 1002 (car g)) (= tag (cdr g)))
           (setq found t))
          (found
           (= 1 (car g))
           (setq res (cdr g)))))
      res)
    nil))

(defun @text:xdata-groups (dx / res found g)
  "提取 xdata 组列表（从 group 1001 开始）。
  返回 list 或 nil。"
  "list 或 nil"
  (foreach g dx
    (if (and (not found) (= 1001 (car g)))
        (setq found t))
    (if found
        (setq res (cons g res))))
  (if found
      (reverse res)
      nil))
