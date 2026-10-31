;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layout.lsp — 布局管理
;; 布局改名增减排序 / 布局合并 / 布局分图 / 布局选字重命名 / 局部取图
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (null layout:list) (require 'layout:*))
(if (null string:sort-by-number) (require 'string:*))
(if (null point:mid) (require 'point:*))

;; ===================== 公共小工具 =====================

(defun @layout:sanitize-lst (str / res x)
  "把逗号/分号/空格分隔的字符串拆成清理后的名字列表"
  "字符串列表"
  "示例: (@layout:sanitize-lst \"布局1,布局2\")"
  (setq res nil)
  (foreach x (@layout:split-str str ",，;； \t")
    (setq x (vl-string-trim " \t" x))
    (if (/= "" x)
        (setq res (cons (@layout:sanitize-name x) res))))
  (reverse res))

;; ===================== 1. 布局改名增减排序 tab-sort =====================

(defun @layout:apply-pattern (pattern i)
  "把 pattern 中的 %s 或 {} 替换为序号 i，都没有则追加序号"
  "字符串"
  "示例: (@layout:apply-pattern \"图纸%s\" 3) => \"图纸3\""
  (cond
   ((vl-string-search "%s" pattern)
    (vl-string-subst (itoa i) "%s" pattern))
   ((vl-string-search "{}" pattern)
    (vl-string-subst (itoa i) "{}" pattern))
   (t
    (strcat pattern (itoa i)))))

(defun @layout:rename-1 (old new)
  "重命名单个布局 old → new"
  "t / nil"
  "示例: (@layout:rename-1 \"布局1\" \"图纸1\")"
  (if (@layout:layout-p old)
      (progn
        (vla-put-name (vla-item *layouts* old) new)
        t)
      nil))

(defun @layout:ai-tab-rename (pattern start / names i pairs ok tmp j)
  "AI 调用入口：按模式批量重命名全部布局（按标签页顺序，序号从 start 起）。
模式串中 %s 或 {} 处替换为序号，如 \"图纸%s\"、\"A-{}\"，都没有则追加序号。
成功返回新布局名列表，失败返回 nil。
示例: (@layout:ai-tab-rename \"图纸%s\" 1)"
  "新布局名列表 / nil"
  "示例: (@layout:ai-tab-rename \"图纸%s\" 1)"
  (if (or (not (stringp pattern)) (= 0 (strlen pattern)))
      (progn (princ "\n错误: 重命名模式不能为空") nil)
      (progn
        (setq names (layout:list)
              i (if (and (numberp start) (> start 0)) (fix start) 1)
              pairs nil
              ok t)
        (foreach nm names
          (setq pairs (cons (cons nm
                                  (@layout:sanitize-name
                                   (@layout:apply-pattern pattern i)))
                            pairs))
          (setq i (1+ i)))
        (setq pairs (reverse pairs))
        (if (/= (length (mapcar 'cdr pairs))
                (length (vl-remove-duplicates (mapcar 'cdr pairs))))
            (progn
              (princ "\n错误: 生成的新布局名存在重复")
              (setq ok nil)))
        (if ok
            (progn
              ;; 随机临时名前缀，两阶段改名，避免中间冲突
              (setq tmp (strcat "MLTMP" (itoa (random 100000000)))
                    j 0)
              (foreach p pairs
                (setq j (1+ j))
                (@layout:rename-1 (car p) (strcat tmp "_" (itoa j))))
              (setq j 0)
              (foreach p pairs
                (setq j (1+ j))
                (@layout:rename-1 (strcat tmp "_" (itoa j)) (cdr p)))
              (princ (strcat "\n已重命名 " (itoa (length pairs)) " 个布局"))
              (mapcar 'cdr pairs))
            nil))))

(defun @layout:ai-tab-add (count / n added nm)
  "AI 调用入口：新建 count 个布局（名称 布局/布局-1/布局-2… 自动去重）"
  "新布局名列表 / nil"
  "示例: (@layout:ai-tab-add 3)"
  (setq n (if (and (numberp count) (> count 0)) (fix count) 1)
        added nil)
  (repeat n
    (setq nm (@layout:unique-name "布局"))
    (vla-add *layouts* nm)
    (setq added (cons nm added)))
  (princ (strcat "\n已新增 " (itoa n) " 个布局"))
  (reverse added))

(defun @layout:ai-tab-del (count / n deleted lst nm)
  "AI 调用入口：从末尾删 count 个布局（当前布局永不删，至少保留 1 个）"
  "已删除布局名列表 / nil"
  "示例: (@layout:ai-tab-del 2)"
  (setq n (if (and (numberp count) (> count 0)) (fix count) 1)
        deleted nil
        lst (reverse (layout:list)))
  (if (< (length (layout:list)) 2)
      (progn (princ "\n错误: 仅剩一个布局，不能删除") nil)
      (progn
        (while (and lst (< (length deleted) n))
          (setq nm (car lst))
          (setq lst (cdr lst))
          (if (and (/= nm (getvar "ctab"))
                   (> (length (layout:list)) 1)
                   (@layout:layout-p nm))
              (progn
                (vla-delete (vla-item *layouts* nm))
                (setq deleted (cons nm deleted)))))
        (if deleted
            (progn
              (princ (strcat "\n已删除 " (itoa (length deleted)) " 个布局"))
              (reverse deleted))
            (progn (princ "\n没有可删除的布局") nil)))))

(defun @layout:ai-tab-sort (mode / names sorted i)
  "AI 调用入口：重排布局标签顺序。
mode \"name\"=按布局名排序（数字感知，图纸1<图纸2<图纸10）；
mode \"order\"=倒序。成功返回新顺序布局名列表，失败返回 nil。
示例: (@layout:ai-tab-sort \"name\")"
  "新顺序布局名列表 / nil"
  "示例: (@layout:ai-tab-sort \"name\")"
  (setq names (layout:list)
        sorted nil
        i 0)
  (cond
   ((or (not (stringp mode)) (= "" mode) (= (strcase mode) "NAME"))
    (setq sorted (string:sort-by-number names)))
   ((= (strcase mode) "ORDER")
    (setq sorted (reverse names)))
   (t
    (princ "\n错误: 排序模式应为 name 或 order")
    nil))
  (if sorted
      (progn
        (foreach nm sorted
          (setq i (1+ i))
          (layout:set-position nm i))
        (princ (strcat "\n已重排 " (itoa (length sorted)) " 个布局"))
        sorted)
      nil))

(defun @layout:tab-sort (/ opt pattern start n mode)
  "布局改名增减排序（交互）：选项菜单 重命名/增加/删除/排序"
  "t"
  "示例: (@layout:tab-sort)"
  (setq opt (getkword "\n选择操作 [重命名(R)/增加(A)/删除(D)/排序(S)]: R"))
  (cond
   ((= opt "R")
    (setq pattern (getstring 1 "\n输入重命名模式(如 图纸%s 或 A-{}, 回车取消): "))
    (if (/= "" pattern)
        (progn
          (setq start (getint 1 "\n起始序号 <1>: "))
          (command "_.UNDO" "Begin")
          (@layout:ai-tab-rename pattern (if start start 1))
          (command "_.UNDO" "End"))))
   ((= opt "A")
    (setq n (getint 1 "\n增加布局个数 <1>: "))
    (if n
        (progn
          (command "_.UNDO" "Begin")
          (@layout:ai-tab-add n)
          (command "_.UNDO" "End"))))
   ((= opt "D")
    (setq n (getint 1 "\n删除布局个数 <1>: "))
    (if n
        (progn
          (command "_.UNDO" "Begin")
          (@layout:ai-tab-del n)
          (command "_.UNDO" "End"))))
   ((= opt "S")
    (setq mode (getkword "\n排序方式 [按名(N)/倒序(O)]: N"))
    (progn
      (command "_.UNDO" "Begin")
      (@layout:ai-tab-sort (if (= mode "O") "order" "name"))
      (command "_.UNDO" "End"))))
  (princ))

;; ===================== 2. 布局合并 layout-merge =====================

(defun @layout:ai-layout-merge (layouts target / lst views srcs gap maxw
                                   maxh i cols src vps2 v vw best bestarea
                                   w h c r vps pv)
  "AI 调用入口：把多个布局合并为一个布局。
每个源布局取其主视口（图纸面积最大的）的模型视图，在目标布局中按网格排布
（自动避让不重叠），完成后删除源布局。
layouts 为布局名列表，target 为目标布局名（不存在则创建）。
成功返回目标布局名，失败返回 nil。
示例: (@layout:ai-layout-merge '(\"布局1\" \"布局2\") \"合并布局\")"
  "目标布局名 / nil"
  "示例: (@layout:ai-layout-merge '(\"布局1\" \"布局2\") \"合并布局\")"
  (setq lst (if (listp layouts) layouts (list layouts))
        views nil
        srcs nil
        gap 500.0
        maxw 0.0
        maxh 0.0
        i 0
        cols 1)
  (setq lst (vl-remove target lst))
  (if (null lst)
      (progn (princ "\n错误: 没有可合并的源布局") nil)
      (progn
        ;; 收集每个源布局主视口的模型视图
        (foreach src lst
          (setq vps2 (@layout:vp-list src)
                best nil
                bestarea 0.0)
          (foreach v vps2
            (setq vw (@layout:vp-view v))
            (if (> (* (nth 2 vw) (nth 3 vw)) bestarea)
                (progn
                  (setq best vw)
                  (setq bestarea (* (nth 2 vw) (nth 3 vw))))))
          (if best
              (progn
                (setq views (cons (list src best) views))
                (setq srcs (cons src srcs))
                (if (> (nth 2 best) maxw) (setq maxw (nth 2 best)))
                (if (> (nth 3 best) maxh) (setq maxh (nth 3 best))))
              (progn
                (princ (strcat "\n警告: 布局 [" src "] 没有视口，跳过")))))
        (if (null views)
            (progn (princ "\n错误: 源布局都没有视口，无法合并") nil)
            (progn
              (setq views (reverse views))
              ;; 目标布局（不存在则创建），并清掉其中已有视口
              (@layout:ensure-layout target)
              (setq vps (@layout:vp-list target))
              (if vps (@layout:erase-ents vps))
              ;; 网格排布
              (setq cols (fix (sqrt (float (length views)))))
              (if (< cols 1) (setq cols 1))
              (foreach pv views
                (setq vw (cadr pv)
                      w (nth 2 vw)
                      h (nth 3 vw)
                      c (mod i cols)
                      r (fix (/ i cols)))
                (@layout:make-vp target
                                      (list (* (+ 1 c) (+ maxw gap))
                                            (* (+ 1 r) (+ maxh gap))
                                            0.0)
                                      w h
                                      (car vw) (cadr vw)
                                      (nth 4 vw))
                (setq i (1+ i)))
              (princ (strcat "\n已把 " (itoa (length views))
                             " 个布局合并到 [" target "]"))
              ;; 删除源布局（先切到目标布局）
              (setvar "ctab" target)
              (foreach src srcs
                (@layout:del-layout src))
              (princ (strcat "\n合并完成: " target))
              target)))))

(defun @layout:layout-merge (/ target srcs src-str all)
  "布局合并（交互）：把多个布局合并到一个布局，每个源布局变成目标布局里一个视口"
  "t"
  "示例: (@layout:layout-merge)"
  (setq all (layout:list))
  (setq target (getstring 1 (strcat "\n目标布局名 <"
                                    (@layout:unique-name "合并布局")
                                    ">: ")))
  (setq target (if (= "" target)
                   (@layout:unique-name "合并布局")
                   (@layout:sanitize-name target)))
  (setq src-str (getstring 1
                           (strcat "\n要合并的布局名,逗号分隔,回车=除目标外全部: ")))
  (setq srcs (if (= "" src-str)
                 (vl-remove target all)
                 (@layout:sanitize-lst src-str)))
  (if srcs
      (progn
        (command "_.UNDO" "Begin")
        (@layout:ai-layout-merge srcs target)
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 3. 布局分图 layout-isolate =====================

(defun @layout:ai-layout-isolate (layout / vps newnames i nm n v vw)
  "AI 调用入口：把一个布局里的多个视口拆成多个布局（每视口一个布局）。
新布局名为 <原布局名>-1/-2…（自动去重），完成后删除原布局。
成功返回新布局名列表，失败返回 nil。
示例: (@layout:ai-layout-isolate \"布局1\")"
  "新布局名列表 / nil"
  "示例: (@layout:ai-layout-isolate \"布局1\")"
  (setq vps (if (@layout:layout-p layout)
                (@layout:vp-list layout))
        newnames nil
        i 0)
  (if (null vps)
      (progn (princ (strcat "\n错误: 布局 [" layout "] 没有视口")) nil)
      (progn
        (foreach v vps
          (setq i (1+ i)
                nm (strcat layout "-" (itoa i))
                n i
                vw (@layout:vp-view v))
          (while (@layout:layout-p nm)
            (setq n (1+ n))
            (setq nm (strcat layout "-" (itoa n))))
          (@layout:make-vp nm
                               (list 0.0 0.0 0.0)
                               (nth 2 vw) (nth 3 vw)
                               (car vw) (cadr vw)
                               (nth 4 vw))
          (setq newnames (cons nm newnames)))
        ;; 切到第一个新布局，再删原布局
        (setvar "ctab" (car (reverse newnames)))
        (@layout:del-layout layout)
        (setq newnames (reverse newnames))
        (princ (strcat "\n已把 [" layout "] 拆分为 "
                       (itoa (length newnames)) " 个布局"))
        newnames)))

(defun @layout:layout-isolate ()
  "布局分图（交互）：一个布局里的多个视口拆成多个布局"
  "t"
  "示例: (@layout:layout-isolate)"
  (setq layout (getstring 1
                          (strcat "\n要拆分的布局名 <当前 " (getvar "ctab") ">: ")))
  (setq layout (if (= "" layout)
                   (getvar "ctab")
                   (@layout:sanitize-name layout)))
  (if (@layout:layout-p layout)
      (progn
        (command "_.UNDO" "Begin")
        (@layout:ai-layout-isolate layout)
        (command "_.UNDO" "End"))
      (princ (strcat "\n错误: 布局 [" layout "] 不存在")))
  (princ))

;; ===================== 4. 布局选字重命名 layout-rename =====================

(defun @layout:ai-layout-rename (layout filter / ents texts newname e s pt)
  "AI 调用入口：用布局图纸空间内的文字内容重命名该布局。
在 layout 的图纸空间找 TEXT/MTEXT，filter 为子串过滤（nil=全部），
取 X 坐标最小的一条文字作为新布局名。成功返回新布局名，失败返回 nil。
示例: (@layout:ai-layout-rename \"布局1\" \"平面图\")"
  "新布局名 / nil"
  "示例: (@layout:ai-layout-rename \"布局1\" nil)"
  (setq ents (if (@layout:layout-p layout)
                 (@layout:layout-ents layout))
        texts nil)
  (if (null ents)
      (progn (princ "\n错误: 布局没有图纸空间实体") nil)
      (progn
        (foreach e ents
          (setq s (@layout:text-of e))
          (if (and s
                   (/= "" s)
                   (or (null filter)
                       (vl-string-search filter s)))
              (progn
                (setq pt (cdr (assoc 10 (entget e))))
                (setq texts (cons (list (car pt) s) texts)))))
        (if (null texts)
            (progn (princ "\n错误: 布局内没有匹配的文字") nil)
            (progn
              (setq texts (vl-sort texts
                                   (function (lambda (a b)
                                               (< (car a) (car b))))))
              (setq newname (@layout:sanitize-name (cadr (car texts))))
              (if (/= newname layout)
                  (progn
                    (vla-put-name (vla-item *layouts* layout) newname)
                    (princ (strcat "\n布局 [" layout "] 已重命名为 ["
                                   newname "]"))))
              newname)))))

(defun @layout:layout-rename ()
  "布局选字重命名（交互）：选一段文字，用文字内容重命名当前布局"
  "t"
  "示例: (@layout:layout-rename)"
  (setq sel (entsel "\n选择作为布局名的文字: "))
  (if sel
      (progn
        (setq s (@layout:text-of (car sel)))
        (if s
            (progn
              (command "_.UNDO" "Begin")
              (@layout:ai-layout-rename (getvar "ctab") s)
              (command "_.UNDO" "End"))
            (princ "\n所选实体不是文字"))))
  (princ))

;; ===================== 5. 局部取图 jbqt =====================

(defun @layout:ai-jbqt (corner1 corner2 scale layout-name / b n name w h
                             center)
  "AI 调用入口：框选模型空间区域生成新布局，视口对准该区域并按 1:scale 出图。
corner1/corner2 为模型空间两角点，scale 为比例 N（即 1:N），
layout-name 为布局名（nil=自动 局部图N）。
成功返回视口 ename，失败返回 nil。
示例: (@layout:ai-jbqt '(0 0 0) '(10000 8000 0) 50 \"一层平面图\")"
  "视口 ename / nil"
  "示例: (@layout:ai-jbqt '(0 0 0) '(10000 8000 0) 50 nil)"
  (setq b (@layout:box corner1 corner2)
        n (if (and (numberp scale) (> scale 0.0)) scale 1.0)
        w (- (caddr b) (car b))
        h (- (cadddr b) (cadr b)))
  (if (or (<= w 0) (<= h 0))
      (progn (princ "\n错误: 取图区域宽或高为零") nil)
      (progn
        (setq center (point:mid (list (car b) (cadr b))
                                (list (caddr b) (cadddr b)))
              name (if (and layout-name (stringp layout-name))
                       (@layout:sanitize-name layout-name)
                       (@layout:unique-name "局部图")))
        (@layout:make-vp name
                             (list 0.0 0.0 0.0)
                             (/ w n) (/ h n)
                             (car center) (cadr center)
                             (/ 1.0 n)))))

(defun @layout:jbqt ()
  "局部取图（交互）：模型空间框选区域→生成新布局（视口对准该区域+指定比例）"
  "t"
  "示例: (@layout:jbqt)"
  (setq p1 (getpoint "\n模型空间第一角点: "))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n模型空间第二角点: "))
        (if p2
            (progn
              (setq n (getint 1 "\n出图比例 1:<50>: "))
              (setq name (getstring 1 "\n布局名(回车=自动): "))
              (command "_.UNDO" "Begin")
              (@layout:ai-jbqt p1 p2
                                   (if n (float n) 50.0)
                                   (if (= "" name) nil name))
              (command "_.UNDO" "End"))))))
  (princ)

(princ)

(defun @layout:split-str (str seps / res cur i n c)
  "按分隔符集合拆分字符串（seps 中任一字符都算分隔符），忽略空段"
  "字符串列表"
  "(@layout:split-str \"a,b;c\" \",;\")"
  (setq res '() cur "" i 1 n (strlen str))
  (while (<= i n)
    (setq c (substr str i 1))
    (if (vl-string-search c seps)
        (progn
          (if (/= cur "") (setq res (append res (list cur))))
          (setq cur ""))
        (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (if (/= cur "") (setq res (append res (list cur))))
  res)
