;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @图库 (at-library) — 本地文件夹图库
;; 把一个或多个目录（及其子目录）里的 .dwg 当作图库：扫描 → 检索 → 按名称插入。
;; 与在线图库（library:*）互补：本地库不下载、不走索引服务，目录结构即分类。
;; 「库就是文件夹」：配置 at-library:local-roots 即可挂多个库，无需导入或建 SLD。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(block:* entity:* point:*))
(vl-load-com)

(@:add-menu "@图库" "本地图库：列出目录" "(at-library:local-list)")
(@:add-menu "@图库" "本地图库：按名称插入" "(at-library:local-insert)")
(@:add-menu "@图库" "本地图库：多库浏览插入" "(at-library:local-browse)")
(@:add-menu "@图库" "本地图库：设置根目录" "(at-library:local-set-roots)")

;; 最近一次使用的本地图库根目录（兼容旧调用；多根请用 at-library:local-roots）
(if (null *at-library:local-root*) (setq *at-library:local-root* ""))

(defun at-library:local-norm (dir / s)
  "把目录整理成以 / 结尾的形式；空串返回空串。"
  "目录字符串"
  "(at-library:local-norm \"D:/lib\") => \"D:/lib/\""
  (setq s (vl-string-right-trim "\\/" (vl-string-trim " \t\r\n" (if dir dir ""))))
  (if (= s "") "" (strcat s "/")))

(defun at-library:local-trim (s)
  "内部：去掉字符串首尾空白，非字符串返回空串。"
  "去空白后的字符串"
  "(at-library:local-trim \" D:/lib \") => \"D:/lib\""
  (if (= (type s) 'STR) (vl-string-trim " \t\r\n" s) ""))

(defun at-library:local-split (s / pos res)
  "内部：按 ; 拆分字符串，去掉首尾空白和空项。"
  "字符串表"
  "(at-library:local-split \"D:/a; D:/b\") => (\"D:/a\" \"D:/b\")"
  (setq res nil)
  (if (/= (type s) 'STR) (setq s ""))
  (while (setq pos (vl-string-search ";" s))
    (setq res (cons (at-library:local-trim (substr s 1 pos)) res))
    (setq s (substr s (+ pos 2))))
  (setq res (cons (at-library:local-trim s) res))
  (vl-remove "" (reverse res)))

(defun at-library:local-base (dir / s pos last-pos)
  "内部：取目录名的最后一段，用作多根库的显示别名。"
  "目录名（不含路径）"
  "(at-library:local-base \"D:/lib/门窗\") => \"门窗\""
  (setq s (vl-string-translate "\\" "/" (at-library:local-norm dir)))
  (setq s (vl-string-right-trim "/" s))
  (setq last-pos nil)
  (setq pos 0)
  (while (setq pos (vl-string-search "/" s pos))
    (setq last-pos pos)
    (setq pos (1+ pos)))
  (if last-pos (substr s (+ last-pos 2)) s))

(defun at-library:local-norm-roots (roots / raw lst out dir)
  "内部：把 roots（nil / 字符串 / 字符串表）规范成以 / 结尾的目录表。
   nil 时先读配置 at-library:local-roots，再回退旧变量 *at-library:local-root*。
   字符串支持单个目录，或多个目录用 ; 分隔。结果按出现顺序去重。"
  "目录表，未配置返回 nil"
  "(at-library:local-norm-roots \"D:/libA;D:/libB\")"
  (setq raw roots)
  (if (null raw)
      (progn
        (setq raw (@:get-config 'at-library:local-roots))
        (if (or (null raw)
                (and (= (type raw) 'STR) (= "" (at-library:local-trim raw))))
            (setq raw *at-library:local-root*))))
  (cond
   ((null raw) (setq lst nil))
   ((= (type raw) 'STR) (setq lst (at-library:local-split raw)))
   ((listp raw) (setq lst raw))
   (t (setq lst nil)))
  (foreach x lst
    (if (= (type x) 'STR)
        (progn
          (setq dir (at-library:local-norm x))
          (if (and (/= dir "") (not (member (strcase dir) (mapcar 'strcase out))))
              (setq out (append out (list dir)))))))
  out)

(defun at-library:local-roots-list nil
  "解析配置 at-library:local-roots（多个用 ; 分隔），兼容旧的 *at-library:local-root*。"
  "目录表（以 / 结尾），未配置返回 nil"
  "(at-library:local-roots-list)"
  (at-library:local-norm-roots nil))

(defun at-library:local-count-str (s lst / n)
  "内部：统计字符串在表中出现的次数（忽略大小写）。"
  "整数"
  "(at-library:local-count-str \"A\" '(\"a\" \"b\")) => 1"
  (setq n 0)
  (foreach x lst
    (if (= (strcase x) (strcase s)) (setq n (1+ n))))
  n)

(defun at-library:local-aliases (roots / res r b n)
  "内部：为根目录生成短别名（末级目录名），重名时加 (2)(3) 序号。"
  "((根目录 . 别名) ...)"
  "(at-library:local-aliases '(\"D:/libA/\" \"D:/libB/\"))"
  (foreach x roots
    (setq r (at-library:local-norm x))
    (if (/= r "")
        (progn
          (setq b (at-library:local-base r))
          (if (= b "") (setq b r))
          (setq n (1+ (at-library:local-count-str b (mapcar 'cdr res))))
          (setq res (append res
                            (list (cons r (if (= n 1)
                                              b
                                            (strcat b "(" (itoa n) ")")))))))))
  res)

(defun at-library:local-walk (dir rel depth maxd / res f d ext)
  "内部：递归收集目录下的 .dwg，返回条目 alist 列表。"
  "条目列表"
  "(at-library:local-walk \"D:/lib/\" \"\" 0 6)"
  (if (< depth maxd)
      (progn
        (foreach f (vl-directory-files dir "*" 1)
          (setq ext (strcase (if (vl-filename-extension f)
                                 (vl-filename-extension f)
                                 "")))
          (if (= ext ".DWG")
              (setq res
                    (cons (list (cons "id" (strcat rel (vl-filename-base f)))
                                (cons "name" (vl-filename-base f))
                                (cons "file" (strcat dir f))
                                (cons "dir" dir)
                                (cons "rel" (strcat rel f)))
                          res))))
        (foreach d (vl-directory-files dir "*" -1)
          (if (and (/= d ".") (/= d ".."))
              (setq res
                    (append res
                            (at-library:local-walk
                             (strcat dir d "/")
                             (strcat rel d "/")
                             (1+ depth)
                             maxd)))))))
  res)

(defun at-library:local-scan (root depth / dir rows)
  "扫描本地图库目录下的所有 .dwg，返回按相对路径排序的条目表。
   条目是 alist（字符串键）：id（相对路径去扩展名）、name、file、dir、rel。
   参数：root 根目录；depth 最大递归深度（缺省 6）。"
  "条目列表，目录不存在返回 nil"
  "(at-library:local-scan \"D:/lib\" 6)"
  (setq dir (at-library:local-norm root))
  (if (and (/= dir "") (vl-file-directory-p dir))
      (progn
        (setq rows (at-library:local-walk
                    dir "" 0 (max 1 (fix (if (numberp depth) depth 6)))))
        (vl-sort rows
                 (function
                  (lambda (a b)
                    (< (strcase (cdr (assoc "rel" a)) T)
                       (strcase (cdr (assoc "rel" b)) T))))))
      (progn
        (if (/= dir "")
            (princ (strcat "\nat-library:local-scan 目录不存在: " dir))
            (princ "\nat-library:local-scan 未指定目录"))
        nil)))

(defun at-library:local-take (lst n / res)
  "取列表前 n 项。"
  "子列表"
  "(at-library:local-take '(1 2 3) 2)"
  (while (and lst (> n 0))
    (setq res (cons (car lst) res))
    (setq lst (cdr lst))
    (setq n (1- n)))
  (reverse res))

(defun at-library:local-search (kw limit root / rows q lim pref rest idl)
  "在本地图库中按名称/相对路径检索。
   参数：kw 关键字（空 = 全部）；limit 最多返回条数（缺省 30）；root 根目录。
   返回条目 alist 列表（与 local-scan 同结构）。"
  "条目列表"
  "(at-library:local-search \"门\" 10 \"D:/lib\")"
  (setq rows (at-library:local-scan root 6))
  (setq q (strcase (vl-string-trim " " (if kw kw "")) T))
  (setq lim (max 1 (fix (if (and limit (> limit 0)) limit 30))))
  (cond
   ((= q "")
    (at-library:local-take rows lim))
   (t
    (foreach it rows
      (setq idl (strcase (cdr (assoc "id" it)) T))
      (if (= 0 (vl-string-search q idl))
          (setq pref (cons it pref))
          (if (vl-string-search q idl)
              (setq rest (cons it rest)))))
    (at-library:local-take (append (reverse pref) (reverse rest)) lim))))

(defun at-library:local-scan-multi (roots depth / dirs rel seen res)
  "扫描多个本地图库根目录，返回合并后的条目表。
   参数：roots 根目录表（也可给 ; 分隔的字符串或单个目录）；depth 递归深度（缺省 6）。
   每个条目增加 \"root\" 字段（该文件所属根目录，以 / 结尾）；rel 仍是相对该根的路径。
   按 rel 去重：同一相对路径只保留第一个根目录中的文件；结果按 rel 排序。"
  "条目列表，全部目录都不存在返回 nil"
  "(at-library:local-scan-multi (list \"D:/libA\" \"D:/libB\") 6)"
  (setq dirs (at-library:local-norm-roots roots))
  (foreach dir dirs
    (foreach it (at-library:local-scan dir depth)
      (setq rel (strcase (cdr (assoc "rel" it)) T))
      (if (not (member rel seen))
          (progn
            (setq seen (cons rel seen))
            (setq res (cons (append it (list (cons "root" dir))) res))))))
  (vl-sort res
           (function
            (lambda (a b)
              (< (strcase (cdr (assoc "rel" a)) T)
                 (strcase (cdr (assoc "rel" b)) T))))))

(defun at-library:local-search-multi (kw limit roots / rows q lim pref rest idl)
  "跨多个本地图库根目录检索，合并结果并去重。
   参数：kw 关键字（空 = 全部）；limit 最多返回条数（缺省 30）；
   roots 根目录表（也可给 ; 分隔字符串或单个目录）。
   排序：名称前缀命中优先，其次是包含匹配；条目中的 \"root\" 指出来源根目录。"
  "条目列表，无匹配返回 nil"
  "(at-library:local-search-multi \"门\" 10 (list \"D:/libA\" \"D:/libB\"))"
  (setq rows (at-library:local-scan-multi roots 6))
  (setq q (strcase (vl-string-trim " " (if kw kw "")) T))
  (setq lim (max 1 (fix (if (and limit (> limit 0)) limit 30))))
  (cond
   ((= q "")
    (at-library:local-take rows lim))
   (t
    (foreach it rows
      (setq idl (strcase (cdr (assoc "id" it)) T))
      (if (= 0 (vl-string-search q idl))
          (setq pref (cons it pref))
          (if (vl-string-search q idl)
              (setq rest (cons it rest)))))
    (at-library:local-take (append (reverse pref) (reverse rest)) lim))))

(defun at-library:local-print (rows / i)
  "按序号打印本地图库条目，返回原列表。"
  "条目列表"
  "(at-library:local-print (at-library:local-search \"\" 10 \"D:/lib\"))"
  (setq i 0)
  (foreach it rows
    (setq i (1+ i))
    (princ (strcat "\n  " (itoa i) ". " (cdr (assoc "id" it)))))
  (princ (strcat "\n共 " (itoa (length rows)) " 条"))
  rows)

(defun at-library:local-labels (rows roots / als pair alias res)
  "内部：给条目生成「根目录别名 | 相对路径」显示名，返回 (条目 . 显示名) 表。"
  "点对表"
  "(at-library:local-labels rows (at-library:local-roots-list))"
  (setq als (at-library:local-aliases roots))
  (foreach it rows
    (setq pair (assoc (at-library:local-norm (cdr (assoc "root" it))) als))
    (setq alias (if pair
                    (cdr pair)
                  (at-library:local-base (cdr (assoc "root" it)))))
    (setq res (append res
                      (list (cons it (strcat alias " | " (cdr (assoc "rel" it))))))))
  res)

(defun at-library:local-insert-file (file pt scale rot / blk dir)
  "把指定 dwg 作为块插入到 pt，返回块参照 ename。
  file: dwg 全路径；pt: 插入点；scale: 比例（缺省 1）；rot: 旋转角（弧度，缺省 0）。"
  "块参照 ename，失败返回 nil"
  "(at-library:local-insert-file \"D:/lib/乔木/银杏.dwg\" '(0 0) 1.0 0.0)"
  (if (and file (findfile file))
      (progn
        (setq blk (vl-filename-base file))
        (setq dir (vl-filename-directory file))
        (block:insert blk (strcat dir "/") pt
                      (if (numberp rot) rot 0.0)
                      (if (and (numberp scale) (> scale 0.0)) scale 1.0)))
      (progn
        (princ (strcat "at-library:local-insert-file 找不到文件: "
                       (if file file "")))
        nil)))

(defun at-library:local-move (ref dx dy / obj)
  "内部：把图元平移 (dx dy)，返回 ref；失败静默忽略。"
  "图元 ename"
  "(at-library:local-move ename 10.0 0.0)"
  (if (and ref (or (/= dx 0.0) (/= dy 0.0)))
      (progn
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ref)))
        (if (not (vl-catch-all-error-p obj))
            (vl-catch-all-apply
             'vla-move
             (list obj
                   (vlax-3d-point 0.0 0.0 0.0)
                   (vlax-3d-point dx dy 0.0))))))
  ref)

(defun at-library:local-insert-items (items pt gap / refs ref box p1 p2 prev-right
                                     row-y g dx dy w)
  "把多个本地图库条目依次插入 pt，并按包围盒从左到右排开，返回块参照 ename 表。
   items: 条目表（local-scan / local-scan-multi 结构，需 \"file\"）；
   pt: 第一个符号的插入点（其余自动排到右侧）；gap: 间距（缺省 = 首块宽度的 10%）。
   所有符号纵向居中对齐 pt；插入规则与 local-insert-file 相同（比例 1、旋转 0）。"
  "块参照 ename 表，失败返回 nil"
  "(at-library:local-insert-items rows '(0 0) 2.0)"
  (if (or (null items) (null pt))
      nil
    (progn
      (command "_.UNDO" "Begin")
      (foreach it items
        (setq ref (at-library:local-insert-file
                   (cdr (assoc "file" it)) pt 1.0 0.0))
        (if ref
            (progn
              (setq box (entity:getbox ref 0))
              (if (and box (= 2 (length box)))
                  (progn
                    (setq p1 (car box) p2 (cadr box))
                    (setq w (- (car p2) (car p1)))
                    (if (null g)
                        (setq g (max 1.0 (* 0.1 (if (> w 0.0) w 10.0)))))
                    (if (null row-y)
                        (setq row-y (/ (+ (cadr p1) (cadr p2)) 2.0)))
                    (if prev-right
                        (progn
                          (setq dx (- (+ prev-right
                                         (if (numberp gap) (max 0.0 gap) g))
                                      (car p1)))
                          (setq dy (- row-y
                                      (/ (+ (cadr p1) (cadr p2)) 2.0)))
                          (at-library:local-move ref dx dy)
                          (setq p1 (list (+ (car p1) dx) (+ (cadr p1) dy)))
                          (setq p2 (list (+ (car p2) dx) (+ (cadr p2) dy)))))
                    (setq prev-right (car p2))))
              (setq refs (append refs (list ref))))))
      (command "_.UNDO" "End")
      refs)))

(defun at-library:local-ask-root (/ s def dirs)
  "询问本地图库根目录，回车沿用配置或上次输入。
   配置了多个根目录时默认取第一个；要管多个库请用 at-library:local-browse。"
  "目录字符串（以 / 结尾），未指定返回 nil"
  "(at-library:local-ask-root)"
  (setq def (car (at-library:local-roots-list)))
  (if (or (null def) (= def ""))
      (setq def (if *at-library:local-root* *at-library:local-root* "")))
  (setq s (getstring T
                     (strcat "\n本地图库根目录"
                             (if (= def "") "" (strcat " <" def ">"))
                             ": ")))
  (cond
   ((= s "")
    (if (and def (/= def "")) def))
   (t
    (setq dirs (at-library:local-norm-roots s))
    (if dirs
        (progn
          (setq *at-library:local-root* (car dirs))
          (car dirs))))))

(defun at-library:local-set-roots (/ cur s dirs)
  "交互设置本地图库根目录（多个用 ; 分隔），写入配置 at-library:local-roots。
   保存后 at-library:local-browse / ai-local-scan / ai-local-search 都会使用这些目录。"
  "目录表，取消返回 nil"
  "(at-library:local-set-roots)"
  (setq cur (at-library:local-roots-list))
  (setq s (getstring T
                     (strcat "\n本地图库根目录（多个用 ; 分隔）"
                             (if cur
                                 (strcat " <"
                                         (at-library:local-join cur ";")
                                         ">")
                               "")
                             ": ")))
  (if (= s "")
      (setq dirs cur)
    (setq dirs (at-library:local-norm-roots s)))
  (cond
   (dirs
    (@:set-config 'at-library:local-roots (at-library:local-join dirs ";"))
    (setq *at-library:local-root* (car dirs))
    (princ (strcat "\n本地图库根目录: " (at-library:local-join dirs ";")))
    dirs)
   (t
    (princ "\n未指定目录。")
    nil)))

(defun at-library:local-join (lst sep / res)
  "内部：用 sep 把字符串表连接成一个字符串。"
  "字符串"
  "(at-library:local-join '(\"a\" \"b\") \";\") => \"a;b\""
  (foreach x lst
    (setq res (if res (strcat res sep x) x)))
  res)

(defun at-library:local-browse-insert (items pt / refs)
  "内部：local-browse 选中条目后的插入步骤（无对话框，可被程序调用）。
   items: 条目表（local-scan-multi 结构）；pt: 基点。
   返回插入的块参照 ename 表。"
  "块参照 ename 表，失败返回 nil"
  "(at-library:local-browse-insert rows '(0 0))"
  (setq refs (at-library:local-insert-items items pt nil))
  (princ (strcat "\n已插入 " (itoa (length refs)) " 个符号。"))
  refs)

(defun at-library:local-browse (/ roots rows labels sel items pt refs)
  "本地图库：跨多个根目录浏览、多选、一次插入。
   显示「根目录别名 | 相对路径」（最多 50 条），多选后给一个基点，
   选中的符号按包围盒从左到右排开插入（比例 1、旋转 0）。
   根目录来自配置 at-library:local-roots；未配置时提示输入单个目录。"
  "插入的块参照 ename 表，取消返回 nil"
  "(at-library:local-browse)"
  (setq refs nil)
  (setq roots (at-library:local-roots-list))
  (if (null roots)
      (setq roots (at-library:local-norm-roots (at-library:local-ask-root))))
  (cond
   ((null roots)
    (princ "\n未配置本地图库根目录：请先执行 (at-library:local-set-roots)"))
   ((null (setq rows (at-library:local-take
                      (at-library:local-scan-multi roots 6) 50)))
    (princ (strcat "\n本地图库目录中没有 dwg 文件: "
                   (at-library:local-join roots ";"))))
   (t
    (setq labels (at-library:local-labels rows roots))
    (setq sel (ui:select-multi
               "本地图库（多选，插入后按包围盒从左到右排列）"
               (mapcar (function (lambda (x) (cdr x))) labels)))
    (if (null sel)
        (princ "\n已取消。")
      (progn
        (foreach pair labels
          (if (member (cdr pair) sel)
              (setq items (cons (car pair) items))))
        (setq items (reverse items))
        (if (null items)
            (princ "\n没有选中任何符号。")
          (progn
            (setq pt (getpoint (strcat "\n插入基点（"
                                       (itoa (length items))
                                       " 个符号从左到右排列）: ")))
            (if (null pt)
                (princ "\n已取消。")
              (setq refs (at-library:local-browse-insert items pt)))))))))
  refs)

(defun at-library:local-list (/ root rows)
  "本地图库：列出目录中的 dwg（最多显示 50 条）。"
  "条目列表"
  "(at-library:local-list)"
  (setq root (at-library:local-ask-root))
  (if root
      (progn
        (setq rows (at-library:local-scan root 6))
        (if rows
            (at-library:local-print (at-library:local-take rows 50))
            (princ "\n目录中没有 dwg 文件。")))
      (princ "\n未指定目录。"))
  (princ))

(defun at-library:local-insert (/ root kw rows n item pt res)
  "本地图库：按关键字检索并插入符号。
   检索 → 选序号 → 点插入点，插入本地 dwg 作为块。
   默认根目录取配置 at-library:local-roots 的第一个。"
  "块参照 ename，取消返回 nil"
  "(at-library:local-insert)"
  (setq root (at-library:local-ask-root))
  (if root
      (progn
        (setq kw (getstring T "\n关键字（空=全部）: "))
        (setq rows (at-library:local-search kw 30 root))
        (cond
         ((null rows) (princ "\n没有匹配的符号。"))
         (t
          (at-library:local-print rows)
          (setq n (getint "\n选择序号 <取消>: "))
          (if (and n (> n 0) (<= n (length rows)))
              (progn
                (setq item (nth (1- n) rows))
                (setq pt (getpoint "\n插入点: "))
                (if pt
                    (progn
                      (command "_.UNDO" "Begin")
                      (setq res (at-library:local-insert-file
                                 (cdr (assoc "file" item)) pt 1.0 0.0))
                      (command "_.UNDO" "End")
                      (if res (princ "\n已插入。") (princ "\n插入失败。")))
                    (princ "\n已取消。")))))))
      (princ "\n未指定目录。"))
  (princ))

;; ------------------------------------------------------------
;; AI 入口（无交互）
;; ------------------------------------------------------------
(defun at-library:ai-local-scan (root limit / roots rows)
  "AI 调用入口：扫描本地图库目录，返回条目表（最多 limit 条，缺省 200）。
   root: 根目录，字符串（多个用 ; 分隔）、字符串表，或 nil（用配置
   at-library:local-roots）；多个根时条目多一个 \"root\" 字段。
   条目 alist 键：id / name / file / dir / rel (/ root)。
   示例: (at-library:ai-local-scan \"D:/lib\" 50)
         (at-library:ai-local-scan (list \"D:/libA\" \"D:/libB\") 50)"
  "条目列表，目录不存在返回 nil"
  "(at-library:ai-local-scan \"D:/lib\" 50)"
  (setq roots (at-library:local-norm-roots root))
  (setq rows (if (cdr roots)
                 (at-library:local-scan-multi roots 6)
               (if roots (at-library:local-scan (car roots) 6))))
  (if rows (at-library:local-take rows (max 1 (fix (if (numberp limit) limit 200))))))

(defun at-library:ai-local-search (kw limit root / roots)
  "AI 调用入口：在本地图库中检索符号（不打印、不交互）。
   kw: 关键字；limit: 最多返回条数（缺省 30）；
   root: 根目录，字符串（多个用 ; 分隔）、字符串表，或 nil（用配置）。
   多根时结果已跨库去重，条目带 \"root\" 指出来源目录。
   示例: (at-library:ai-local-search \"银杏\" 5 \"D:/lib\")
         (at-library:ai-local-search \"银杏\" 10 (list \"D:/libA\" \"D:/libB\"))"
  "条目列表，无匹配返回 nil"
  "(at-library:ai-local-search \"银杏\" 5 \"D:/lib\")"
  (setq roots (at-library:local-norm-roots root))
  (if (cdr roots)
      (at-library:local-search-multi kw limit roots)
    (if roots (at-library:local-search kw limit (car roots)))))

(defun at-library:ai-local-insert (name pt root scale rot / file roots rows)
  "AI 调用入口：按名称或 dwg 全路径插入本地图库符号。
   name: 关键字或 dwg 全路径；
   pt: 插入点；root: 根目录（字符串、; 分隔字符串、字符串表或 nil=配置）；
   scale: 比例（缺省 1）；rot: 旋转角（弧度，缺省 0）。
   多根时取检索排序后的第一条。
   示例: (at-library:ai-local-insert \"银杏\" '(0 0) \"D:/lib\" 1.0 0.0)
         (at-library:ai-local-insert \"银杏\" '(0 0)
                                     (list \"D:/libA\" \"D:/libB\") 1.0 0.0)"
  "块参照 ename，失败返回 nil"
  "(at-library:ai-local-insert \"银杏\" '(0 0) \"D:/lib\" 1.0 0.0)"
  (setq file (if (and name (findfile name)) (findfile name)))
  (if (null file)
      (progn
        (setq roots (at-library:local-norm-roots root))
        (setq rows (if (cdr roots)
                       (at-library:local-search-multi name 1 roots)
                     (if roots (at-library:local-search name 1 (car roots)))))
        (if rows (setq file (cdr (assoc "file" (car rows)))))))
  (if file
      (at-library:local-insert-file file pt scale rot)
      (progn
        (princ (strcat "at-library:ai-local-insert 没有匹配: "
                       (if name name "")))
        nil)))
