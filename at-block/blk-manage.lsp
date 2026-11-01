;; blk-manage.lsp — at-block 块管理扩展
;; 复现 iMini 工具集的图块管理类命令：块重命名(含通配符批量)、修改块插入基点、
;; 块替换(属性按名映射)、沿路径等距布块、多块基点缩放、块比例批量调整、块统计。
;; ai- 前缀函数为无交互入口：参数全走形参，失败返回 nil，提示仅用 princ。

(vl-load-com)

;; ============================================================
;; 公共辅助
;; ============================================================

(defun @block:undo-start (/ d)
  "开启撤消记录标记(失败忽略)"
  "始终返回 T"
  "(@block:undo-start)"
  (setq d (vl-catch-all-apply 'vla-get-activedocument (list (vlax-get-acad-object))))
  (if (not (vl-catch-all-error-p d))
      (vl-catch-all-apply 'vla-startundomark (list d)))
  T)

(defun @block:undo-end (/ d)
  "结束撤消记录标记(失败忽略)"
  "始终返回 T"
  "(@block:undo-end)"
  (setq d (vl-catch-all-apply 'vla-get-activedocument (list (vlax-get-acad-object))))
  (if (not (vl-catch-all-error-p d))
      (vl-catch-all-apply 'vla-endundomark (list d)))
  T)

(defun @block:blk-inputs (blks)
  "把 块名/块参照/选择集/混合列表 规范化为元素列表"
  "元素列表(块名字符串或 ename)，空返回 nil"
  "(@block:blk-inputs (ssget))"
  (cond ((null blks) nil)
        ((= (type blks) 'pickset) (pickset:to-list blks))
        ((or (p:enamep blks) (p:stringp blks) (= (type blks) 'vla-object))
         (list blks))
        ((listp blks) blks)
        (t nil)))

(defun @block:blk-name-of (blkre / n)
  "取块参照的真实块名；动态块取有效名，失败时退回组码 2"
  "块名字符串，失败返回 nil"
  "(@block:blk-name-of (car (entsel)))"
  (setq n (vl-catch-all-apply 'block:get-effectivename (list blkre)))
  (if (or (vl-catch-all-error-p n) (not (p:stringp n)) (= "" n))
      (setq n (vl-catch-all-apply 'entity:getdxf (list blkre 2))))
  (if (and (p:stringp n) (/= "" n)) n nil))

(defun @block:blk-names (blks / res n x)
  "把 块名/块参照/选择集 规范化为去重的块名列表"
  "块名列表"
  "(@block:blk-names (ssget))"
  (foreach x (@block:blk-inputs blks)
    (setq n (cond ((= (type x) 'str) x)
                  ((or (p:enamep x) (= (type x) 'vla-object))
                   (@block:blk-name-of x))
                  (t nil)))
    (if (and n (/= "" n) (not (member n res)))
        (setq res (cons n res))))
  (reverse res))

(defun @block:blk-refs (blks / names res x e)
  "把 块名/块参照/选择集 规范化为块参照 ename 列表"
  "块参照 ename 列表；块名会展开为图中全部同名参照"
  "(@block:blk-refs '(\"门\"))"
  (foreach x (@block:blk-inputs blks)
    (cond ((= (type x) 'str)
           (if (not (member x names)) (setq names (cons x names))))
          (t
           (setq e (if (p:enamep x)
                       x
                       (vl-catch-all-apply 'vlax-vla-object->ename (list x))))
           (if (and (p:enamep e) (not (member e res)))
               (setq res (cons e res))))))
  (if names
      (foreach e (pickset:to-list (ssget "_X" '((0 . "INSERT"))))
        (if (and (assoc (@block:blk-name-of e) names) (not (member e res)))
            (setq res (cons e res)))))
  (reverse res))

(defun @block:usage-alist (/ res e n)
  "统计图中各块名(动态块按有效名)的使用次数"
  "点对表 ((块名 . 次数) ...)"
  "(@block:usage-alist)"
  (foreach e (pickset:to-list (ssget "_X" '((0 . "INSERT"))))
    (if (setq n (@block:blk-name-of e))
        (setq res (if (assoc n res)
                      (subst (cons n (1+ (cdr (assoc n res)))) (assoc n res) res)
                      (cons (cons n 1) res)))))
  res)

(defun @block:shift-pt (pt delta)
  "点(2D/3D)按 delta(3D)平移"
  "平移后的点"
  "(@block:shift-pt '(1 2 0) '(10 0 0))"
  (cond ((not (and (listp pt) (numberp (car pt)))) pt)
        ((= 2 (length pt))
         (list (+ (car pt) (car delta)) (+ (cadr pt) (cadr delta))))
        (t (list (+ (car pt) (car delta))
                 (+ (cadr pt) (cadr delta))
                 (+ (caddr pt) (caddr delta))))))

(defun @block:shift-codes (typ)
  "可平移点组码支持表中该实体类型对应的组码"
  "组码列表，不支持的实体类型返回 nil"
  "(@block:shift-codes \"LINE\")"
  (cond ((member typ '("LINE")) '(10 11))
        ((member typ '("LWPOLYLINE" "POLYLINE" "VERTEX")) '(10))
        ((member typ '("CIRCLE" "ARC" "POINT" "INSERT" "MTEXT")) '(10))
        ((member typ '("TEXT" "ATTDEF" "ATTRIB")) '(10 11))
        ((member typ '("SOLID" "TRACE" "3DFACE")) '(10 11 12 13))
        ((member typ '("SPLINE" "LEADER" "TOLERANCE")) '(10 11))
        ((member typ '("DIMENSION")) '(10 11 12 13 14 15 16))
        ((member typ '("ELLIPSE" "XLINE" "RAY" "IMAGE" "WIPEOUT")) '(10))
        (t nil)))

(defun @block:shift-dxf (e delta / typ codes el res pair)
  "按 DXF 点组码平移实体(仅限常见二维/三维图元)"
  "T 平移成功，nil 表示该图元类型不支持"
  "(@block:shift-dxf (car (entsel)) '(100.0 0.0 0.0))"
  (setq typ (strcase (entity:getdxf e 0)))
  (if (setq codes (@block:shift-codes typ))
      (progn
        (setq el (entget e) res nil)
        (foreach pair el
          (cond ((and (member (car pair) codes) (listp (cdr pair)))
                 (setq res (cons (cons (car pair)
                                       (@block:shift-pt (cdr pair) delta))
                                 res)))
                ((and (member (car pair) '(30 38)) (numberp (cdr pair)))
                 (setq res (cons (cons (car pair)
                                       (+ (cdr pair) (caddr delta)))
                                 res)))
                (t (setq res (cons pair res)))))
        (entmod (reverse res))
        (entupd e)
        T)
      nil))

(defun @block:move-ent (e delta / r)
  "平移单个块定义内实体：优先按 DXF 组码，未知类型退回 ActiveX"
  "T 成功，nil 失败"
  "(@block:move-ent (car (block:ent-list \"门\")) '(10.0 0.0 0.0))"
  (if (@block:shift-dxf e delta)
      T
      (progn
        (setq r (vl-catch-all-apply
                 'vla-move
                 (list (vlax-ename->vla-object e)
                       (vlax-3d-point '(0.0 0.0 0.0))
                       (vlax-3d-point delta))))
        (not (vl-catch-all-error-p r)))))

(defun @block:blk-shift-def (blkname delta / n e)
  "把块定义内所有实体按 delta(块坐标系)平移"
  "成功平移的实体数"
  "(@block:blk-shift-def \"门\" '(100.0 0.0 0.0))"
  (setq n 0)
  (foreach e (block:ent-list blkname)
    (if (@block:move-ent e delta) (setq n (1+ n))))
  n)

(defun @block:blk-set-base (blkname pt / rec)
  "设置块定义基点(BLOCK 记录组码 10)"
  "T 成功，nil 失败"
  "(@block:blk-set-base \"门\" '(0.0 0.0 0.0))"
  (if (null (setq rec (tblobjname "block" blkname)))
      nil
      (progn
        (vl-catch-all-apply 'entity:putdxf (list rec 10 pt))
        (if (equal (vl-catch-all-apply 'entity:getdxf (list rec 10)) pt 1e-6)
            T
            (progn
              (princ (strcat "\n直接改组码失败，改用 ActiveX 设置基点: " blkname))
              (vl-catch-all-apply 'vla-put-origin
                                  (list (block:get-obj-by-name blkname)
                                        (vlax-3d-point pt)))
              T)))))

(defun @block:insert-ref (blkname pt ang sx sy sz / o ms)
  "在当前空间插入块参照(不弹属性对话框)，返回 INSERT 图元名"
  "INSERT 图元名，失败返回 nil"
  "(@block:insert-ref \"门\" '(0 0 0) 0.0 1.0 1.0 1.0)"
  (setq sx (if (and (numberp sx) (> sx 0)) (float sx) 1.0))
  (setq sy (if (and (numberp sy) (> sy 0)) (float sy) sx))
  (setq sz (if (and (numberp sz) (> sz 0)) (float sz) sx))
  (setq ms (if (and *MS* (string-equal "Model" (getvar "ctab")))
               *MS*
               (vla-get-block (vla-get-activelayout *DOC*))))
  (setq o (vl-catch-all-apply 'vla-insertblock
                              (list ms (point:to-ax pt) blkname sx sy sz
                                    (if (numberp ang) ang 0.0))))
  (if (vl-catch-all-error-p o) nil (vlax-vla-object->ename o)))

(defun @block:blk-def-box (blkname / ents box)
  "块定义内实体(不含属性定义)的包围盒，块坐标系"
  "左下/右上点对，空块返回 nil"
  "(@block:blk-def-box \"门\")"
  (setq ents (vl-remove-if '(lambda (x) (= "ATTDEF" (strcase (entity:getdxf x 0))))
                            (block:ent-list blkname)))
  (if ents
      (progn
        (setq box (vl-catch-all-apply 'pickset:getbox (list ents 0.0)))
        (if (vl-catch-all-error-p box) nil box))
      nil))

;; ============================================================
;; 1. 图块重命名(单个 / 通配符批量)
;; ============================================================

(defun @block:ai-blk-rename (old new / e)
  "AI 入口：重命名块定义(块表记录名 + 所有参照的组码 2)，图中参照原位保留"
  "成功返回新块名，参数无效或重名返回 nil"
  "(@block:ai-blk-rename \"门\" \"门-900\")"
  (cond
   ((or (null old) (null new) (= "" old) (= "" new) (= old new))
    (princ "\n块名无效或新旧同名") nil)
   ((not (tblsearch "block" old)) (princ (strcat "\n图块不存在: " old)) nil)
   ((tblsearch "block" new) (princ (strcat "\n目标块名已存在: " new)) nil)
   ((not (snvalid new)) (princ (strcat "\n块名不合法: " new)) nil)
   ((null (vl-catch-all-apply 'tbl:rename (list "block" old new)))
    (princ (strcat "\n重命名失败(块可能被参照锁定): " old)) nil)
   (t
    (foreach e (pickset:to-list (ssget "_X" '((0 . "INSERT"))))
      (if (equal (entity:getdxf e 2) old)
          (entity:putdxf e 2 new)))
    new)))

(defun @block:rename-pattern-split (pattern / pos)
  "把含一个通配符 * 的模板拆成 前缀/后缀"
  "(前缀 后缀 是否含通配符) 列表"
  "(@block:rename-pattern-split \"TG-*\")"
  (setq pos (vl-string-search "*" pattern))
  (if pos
      (list (substr pattern 1 pos)
            (substr pattern (+ pos 2))
            T)
      (list pattern "" nil)))

(defun @block:ai-blk-rename-pattern (pattern new-pattern / pf sf names
                                     res nm mid new)
  "AI 入口：按通配符批量重命名块；新名模板中的 * 替换为通配符匹配到的部分"
  "成功返回 ((旧名 . 新名) ...) 列表，无匹配返回 nil"
  "(@block:ai-blk-rename-pattern \"TG-*\" \"TG2-*\")"
  (cond
   ((or (null pattern) (null new-pattern) (= "" pattern)) nil)
   (t
    (setq pf (car (@block:rename-pattern-split pattern)))
    (setq sf (cadr (@block:rename-pattern-split pattern)))
    (setq names (tbl:list "block"))
    (foreach nm names
      (if (and (wcmatch (strcase nm) (strcase pattern))
               (or (not (= "*" (substr nm 1 1))) (= "*" (substr pattern 1 1)))
               (>= (strlen nm) (+ (strlen pf) (strlen sf))))
          (progn
            (setq mid (substr nm (1+ (strlen pf))
                              (- (strlen nm) (strlen pf) (strlen sf))))
            (setq new (if (vl-string-search "*" new-pattern)
                          (string:subst-all mid "*" new-pattern)
                          new-pattern))
            (if (and (not (= nm new)) (@block:ai-blk-rename nm new))
                (setq res (cons (cons nm new) res))))))
    (if res
        (progn (princ (strcat "\n已重命名 " (itoa (length res)) " 个图块"))
               (reverse res))
        (progn (princ "\n没有匹配的图块") nil)))))

(defun @block:blk-rename (/ ref old new res)
  "交互式重命名图块：选择一个块，输入新块名，图中所有同名块同步改名"
  "新块名，失败返回 nil"
  "(@block:blk-rename)"
  (@::prompt (strcat "块管理 -> 重命名图块\n"
                     " 改块定义名，图中所有同名块参照同步改名。"))
  (if (null (setq ref (car (entsel "\n请选择一个块: "))))
      (progn (princ "\n未选择对象") nil)
      (progn
        (setq old (@block:blk-name-of ref))
        (setq new (getstring (strcat "\n原块名: " old "\n新块名: ")))
        (if (or (null new) (= "" new))
            (progn (princ "\n未输入新块名") nil)
            (progn
              (@block:undo-start)
              (setq res (@block:ai-blk-rename old new))
              (@block:undo-end)
              (if res
                  (princ (strcat "\n已重命名为: " res))
                  (princ "\n重命名失败"))
              res)))))

(defun @block:blk-rename-batch (/ old new res)
  "交互式批量重命名图块：输入块名通配符与新名模板(* 为占位符)"
  "((旧名 . 新名) ...) 列表，失败返回 nil"
  "(@block:blk-rename-batch)"
  (@::prompt (strcat "块管理 -> 批量重命名图块\n"
                     " 例: 模板\"TG-*\" 新名\"TG2-*\"，则 TG-1 改为 TG2-1。"))
  (setq old (getstring "\n原块名通配符(可用 *): "))
  (setq new (getstring "\n新块名模板(* 替换为通配符匹配部分): "))
  (if (or (null old) (= "" old) (null new) (= "" new))
      (progn (princ "\n输入不完整") nil)
      (progn
        (@block:undo-start)
        (setq res (@block:ai-blk-rename-pattern old new))
        (@block:undo-end)
        res)))

;; ============================================================
;; 2. 修改块插入基点(平移块定义实体 + 改 BLOCK 记录组码 10)
;; ============================================================

(defun @block:ai-blk-insertion (blk-name new-basept ref / blkrec oldbase newbase
                                delta ns)
  "AI 入口：把块定义基点改到 new-basept，块定义内实体随之平移，图中参照位置不变"
  "成功返回块名，失败返回 nil"
  "(@block:ai-blk-insertion \"门\" '(0 0 0))"
  (cond
   ((or (null blk-name) (null new-basept)) (princ "\n参数不完整") nil)
   ((not (tblsearch "block" blk-name))
    (princ (strcat "\n图块不存在: " blk-name)) nil)
   (t
    (setq blkrec (tblobjname "block" blk-name))
    (setq oldbase (if (entity:getdxf blkrec 10) (entity:getdxf blkrec 10) '(0.0 0.0 0.0)))
    ;; new-basept 按世界坐标给出，用参照换算到块坐标系
    (setq ref (if ref ref (car (@block:blk-refs (list blk-name)))))
    (setq newbase
          (if (and ref (= "INSERT" (entity:getdxf ref 0)))
              (block:wcs2bcs new-basept oldbase
                             (entity:getdxf ref 10)
                             (if (entity:getdxf ref 50) (entity:getdxf ref 50) 0.0)
                             (if (entity:getdxf ref 41) (entity:getdxf ref 41) 1.0))
              (list (float (car new-basept))
                    (float (cadr new-basept))
                    (if (caddr new-basept) (float (caddr new-basept)) 0.0))))
    (setq delta (mapcar '- newbase oldbase))
    (cond
     ((equal delta '(0.0 0.0 0.0) 1e-9) blk-name)
     (t
      (setq ns (@block:blk-shift-def blk-name delta))
      (if (@block:blk-set-base blk-name newbase)
          (progn
            (princ (strcat "\n基点已改为 (" (rtos (car newbase) 2 2) ","
                           (rtos (cadr newbase) 2 2) ")，平移 " (itoa ns)
                           " 个图元"))
            blk-name)
          (progn (princ "\n设置块基点失败") nil)))))))

(defun @block:blk-insertion (/ ref name pt res)
  "交互式修改块插入基点：选择块、指定新基点，块定义实体平移而图中位置不变"
  "新基点，失败返回 nil"
  "(@block:blk-insertion)"
  (@::prompt (strcat "块管理 -> 修改块插入基点\n"
                     " 把块定义的基点移到指定点，图中已有块显示位置不变。"))
  (if (null (setq ref (car (entsel "\n请选择要改基点的块: "))))
      (progn (princ "\n未选择对象") nil)
      (progn
        (setq name (@block:blk-name-of ref))
        (setq pt (getpoint (entity:getdxf ref 10) "\n请指定新的插入基点: "))
        (if (null pt)
            (progn (princ "\n未指定基点") nil)
            (progn
              (@block:undo-start)
              (setq res (@block:ai-blk-insertion name pt ref))
              (@block:undo-end)
              (if res (princ "\n基点修改完成") (princ "\n基点修改失败"))
              (command "_.regen")
              res)))))

;; ============================================================
;; 3. 块替换增强(属性按名映射，保留旋转/比例)
;; ============================================================

(defun @block:ref-attribs (ref / e res)
  "取块参照携带的属性(ATTRIB)图元列表"
  "ATTRIB ename 列表，无属性返回 nil"
  "(@block:ref-attribs (car (entsel)))"
  (setq e (entnext ref))
  (while (and e (= "ATTRIB" (entity:getdxf e 0)))
    (setq res (cons e res))
    (setq e (entnext e)))
  (reverse res))

(defun @block:blk-attdef-tags (blkname / e res)
  "块定义内属性定义(ATTDEF)的标签列表，统一转大写"
  "标签列表，无属性返回 nil"
  "(@block:blk-attdef-tags \"门\")"
  (foreach e (block:ent-list blkname)
    (if (= "ATTDEF" (strcase (entity:getdxf e 0)))
        (setq res (cons (strcase (entity:getdxf e 2)) res))))
  (reverse res))

(defun @block:tag-value (atts tag / v)
  "在属性点对表中按标签(不区分大小写)取属性值"
  "属性值字符串，未找到返回 nil"
  "(@block:tag-value '((\"编号\" . \"1\")) \"编号\")"
  (foreach a atts
    (if (equal (strcase (car a)) (strcase tag))
        (setq v (cdr a))))
  v)

(defun @block:recreate-ref (ref dst-blk atts / pt ang sx sy sz lay ne)
  "按原插入点/旋转/比例重建为 dst-blk 的参照(用于原参照无属性而新块带属性的情况)"
  "新参照 ename，失败返回 nil"
  "(@block:recreate-ref (car (entsel)) \"门\" nil)"
  (setq pt (entity:getdxf ref 10)
        ang (if (entity:getdxf ref 50) (entity:getdxf ref 50) 0.0)
        sx (if (entity:getdxf ref 41) (entity:getdxf ref 41) 1.0)
        sy (if (entity:getdxf ref 42) (entity:getdxf ref 42) sx)
        sz (if (entity:getdxf ref 43) (entity:getdxf ref 43) sx)
        lay (entity:getdxf ref 8))
  (if (setq ne (@block:insert-ref dst-blk pt ang sx sy sz))
      (progn
        (if lay (entity:putdxf ne 8 lay))
        (if atts (block:set-attributes ne atts))
        (vl-catch-all-apply 'vla-delete (list (vlax-ename->vla-object ref)))
        ne)
      nil))

(defun @block:ai-blk-replace (src-blk dst-blk map-attrs ss
                              / tags refs res a atts tag val ae)
  "AI 入口：把 src-blk 的参照替换为 dst-blk，保留插入点/旋转/比例，属性按名映射"
  "被替换后的块参照 ename 列表，失败返回 nil"
  "(@block:ai-blk-replace \"旧门\" \"新门\" T nil)"
  (cond
   ((or (null src-blk) (null dst-blk) (= src-blk dst-blk))
    (princ "\n源块与目标块名无效或相同") nil)
   ((not (tblsearch "block" src-blk))
    (princ (strcat "\n源图块不存在: " src-blk)) nil)
   ((not (tblsearch "block" dst-blk))
    (princ (strcat "\n目标图块不存在: " dst-blk)) nil)
   (t
    (setq tags (@block:blk-attdef-tags dst-blk))
    (foreach a (if ss (@block:blk-refs ss) (@block:blk-refs (list src-blk)))
      (if (equal (@block:blk-name-of a) src-blk)
          (setq refs (cons a refs))))
    (foreach a (reverse refs)
      (setq atts (if (and map-attrs (= 1 (if (entity:getdxf a 66) (entity:getdxf a 66) 0)))
                     (block:get-attributes a)
                     nil))
      (if (and tags (not (= 1 (if (entity:getdxf a 66) (entity:getdxf a 66) 0))))
          ;; 新块带属性而原参照无属性：重建参照才能生成属性
          (setq res (cons (@block:recreate-ref a dst-blk atts) res))
          (progn
            (entity:putdxf a 2 dst-blk)
            (if map-attrs
                (foreach ae (@block:ref-attribs a)
                  (setq tag (strcase (entity:getdxf ae 2)))
                  (if (member tag tags)
                      (progn
                        (setq val (@block:tag-value atts tag))
                        (if val (entity:putdxf ae 1 val)))
                      (vl-catch-all-apply 'vla-delete
                                          (list (vlax-ename->vla-object ae))))))
            (setq res (cons a res)))))
    (setq res (vl-remove nil res))
    (if res
        (progn (princ (strcat "\n已替换 " (itoa (length res)) " 个块参照"))
               res)
        (progn (princ "\n没有可替换的块参照") nil)))))

(defun @block:blk-replace (/ src dst map ss res)
  "交互式块替换：选旧块、选新块、选范围，属性按名保留，位置/旋转/比例不变"
  "替换后的块参照 ename 列表，失败返回 nil"
  "(@block:blk-replace)"
  (@::prompt (strcat "块管理 -> 块替换(保留属性)\n"
                     " 1. 选择被替换的块(旧)；2. 选择替换成的块(新)；3. 选范围。"))
  (setq src (car (entsel "\n请选择被替换的块(旧): ")))
  (if (null src)
      (progn (princ "\n未选择源块") nil)
      (progn
        (setq src (@block:blk-name-of src))
        (setq dst (car (entsel "\n请选择替换成的块(新): ")))
        (if (null dst)
            (progn (princ "\n未选择目标块") nil)
            (progn
              (setq dst (@block:blk-name-of dst))
              (setq map (ui:confirm1 "是否按属性名保留原属性值?" "是-否"))
              (prompt "请选择要替换的范围(直接回车=全部):")
              (setq ss (ssget '((0 . "INSERT"))))
              (@block:undo-start)
              (setq res (@block:ai-blk-replace src dst map ss))
              (@block:undo-end)
              (command "_.regen")
              res)))))

;; ============================================================
;; 4. 沿路径等距布块
;; ============================================================

(defun @block:ai-blk-along-line (pline-ent blk-name dist rotate endp
                                 / obj len n i d p pts item ang der res e)
  "AI 入口：沿曲线(多段线/直线/圆弧/圆/样条/椭圆)等距布块，起点必定布一块"
  "插入的块参照 ename 列表，失败返回 nil"
  "(@block:ai-blk-along-line (car (entsel)) \"路灯\" 3000 T T)"
  (cond
   ((or (null pline-ent) (null blk-name)
        (not (numberp dist)) (<= dist 0))
    (princ "\n参数无效(需曲线、块名、正间距)") nil)
   ((not (member (strcase (entity:getdxf pline-ent 0))
                 '("LWPOLYLINE" "POLYLINE" "LINE" "ARC" "CIRCLE" "SPLINE" "ELLIPSE")))
    (princ "\n不是可布块的曲线对象") nil)
   ((not (tblsearch "block" blk-name))
    (princ (strcat "\n图块不存在: " blk-name)) nil)
   (t
    (setq obj (vlax-ename->vla-object pline-ent))
    (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
    (if (or (null len) (<= len 0))
        (progn (princ "\n曲线长度为 0") nil)
        (progn
          (setq n (fix (/ len dist)) i 0)
          (while (< i n)
            (setq d (* i dist))
            (setq p (vlax-curve-getPointAtDist obj d))
            (if p (setq pts (cons (list p d) pts)))
            (setq i (1+ i)))
          (if endp
              (progn
                (setq p (vlax-curve-getPointAtDist obj len))
                (if (and p (not (equal p (caar pts) 1e-6)))
                    (setq pts (cons (list p len) pts)))))
          (foreach item (reverse pts)
            (setq p (car item) d (cadr item) ang 0.0)
            (if rotate
                (progn
                  (setq der (vl-catch-all-apply
                             'vlax-curve-getFirstDeriv
                             (list obj (vlax-curve-getParamAtDist obj d))))
                  (if (and (not (vl-catch-all-error-p der)) (listp der)
                           (> (distance '(0.0 0.0 0.0) der) 1e-9))
                      (setq ang (angle '(0.0 0.0 0.0) der)))))
            (if (setq e (@block:insert-ref blk-name p ang 1.0 1.0 1.0))
                (setq res (cons e res))))
          (if res
              (progn (princ (strcat "\n沿线布置 " (itoa (length res)) " 个块"))
                     (reverse res))
              (progn (princ "\n布块失败") nil)))))))

(defun @block:blk-along-line (/ pline name dist rotate endp ss res e)
  "交互式沿路径布块：选曲线、输入块名(可点选图中块)、输入间距，可选随线旋转、末端补块"
  "插入的块参照 ename 列表，失败返回 nil"
  "(@block:blk-along-line)"
  (@::prompt (strcat "块管理 -> 沿路径等距布块\n"
                     " 以指定间距沿曲线布置块，可随线旋转、末端补块。"))
  (setq pline (car (entsel "\n请选择路径曲线(多段线/直线/圆弧等): ")))
  (if (null pline)
      (progn (princ "\n未选择曲线") nil)
      (progn
        (setq name (getstring "\n请输入要布置的块名 <回车点选图中块>: "))
        (if (= name "")
            (progn
              (setq e (car (entsel "\n请点选一个要布置的块: ")))
              (if e (setq name (@block:blk-name-of e)))))
        (if (or (null name) (= "" name))
            (progn (princ "\n未指定图块") nil)
            (progn
              (setq dist (getdist "\n请输入间距: "))
              (setq rotate (ui:confirm1 "块是否随路径方向旋转?" "是-否"))
              (setq endp (ui:confirm1 "末端不足一个间距时是否补一块?" "是-否"))
              (@block:undo-start)
              (setq res (@block:ai-blk-along-line pline name dist rotate endp))
              (@block:undo-end)
              res)))))

;; ============================================================
;; 5. 多块以各自插入点为基点缩放
;; ============================================================

(defun @block:ai-multi-scale (blks factor / res e sx sy sz)
  "AI 入口：多个块参照以各自插入点为基点统一缩放(按比例系数调整块自身比例)"
  "被修改的块参照 ename 列表，失败返回 nil"
  "(@block:ai-multi-scale (pickset:to-list (ssget)) 2.0)"
  (cond
   ((or (null blks) (not (numberp factor)) (<= factor 0))
    (princ "\n参数无效(需块参照与正的比例系数)") nil)
   (t
    (foreach e (@block:blk-refs blks)
      (if (= "INSERT" (entity:getdxf e 0))
          (progn
            (setq sx (if (entity:getdxf e 41) (entity:getdxf e 41) 1.0))
            (setq sy (if (entity:getdxf e 42) (entity:getdxf e 42) sx))
            (setq sz (if (entity:getdxf e 43) (entity:getdxf e 43) sx))
            (entity:putdxf e 41 (* sx factor))
            (entity:putdxf e 42 (* sy factor))
            (entity:putdxf e 43 (* sz factor))
            (setq res (cons e res)))))
    (if res
        (progn (princ (strcat "\n已缩放 " (itoa (length res)) " 个块"))
               (reverse res))
        (progn (princ "\n没有可缩放的块参照") nil)))))

(defun @block:multi-scale (/ ss factor res)
  "交互式多块缩放：选择多个块，输入比例系数，以各自插入点为基点缩放"
  "被修改的块参照 ename 列表，失败返回 nil"
  "(@block:multi-scale)"
  (@::prompt (strcat "块管理 -> 多块基点缩放\n"
                     " 每个块以自身插入点为基点按同一系数缩放。"))
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss)
      (progn (princ "\n未选择块参照") nil)
      (progn
        (setq factor (getreal "\n请输入缩放系数 <2>: "))
        (or factor (setq factor 2.0))
        (@block:undo-start)
        (setq res (@block:ai-multi-scale (pickset:to-list ss) factor))
        (@block:undo-end)
        res)))

;; ============================================================
;; 6. 块相关比例批量调整(dtbl)
;; ============================================================

(defun @block:hatch-ents (names / res e n)
  "取若干块定义内的填充(HATCH)图元列表"
  "HATCH ename 列表，无填充返回 nil"
  "(@block:hatch-ents '(\"图框\"))"
  (foreach n names
    (foreach e (block:ent-list n)
      (if (= "HATCH" (strcase (entity:getdxf e 0)))
          (setq res (cons e res)))))
  res)

(defun @block:ai-dtbl (blks op factor / names res e v)
  "AI 入口：批量调整块相关比例；op 取 ltype(块内实体线型比例48)/blk(块参照比例41-43)/hatch(块内填充比例41)"
  "实际修改的对象数(整数)，参数无效返回 nil"
  "(@block:ai-dtbl '(\"图框\") \"ltype\" 2.0)"
  (cond
   ((or (null blks) (not (p:stringp op))
        (not (numberp factor)) (<= factor 0))
    (princ "\n参数无效(需块名/块参照、op、正的系数)") nil)
   ((not (member (strcase op) '("LTYPE" "BLK" "HATCH")))
    (princ "\nop 只能为 ltype / blk / hatch") nil)
   (t
    (setq names (@block:blk-names blks) res 0)
    (cond
     ((= (strcase op) "LTYPE")
      (foreach e (apply 'append (mapcar 'block:ent-list names))
        (setq v (entity:getdxf e 48))
        (if (listp v) (setq v (car v)))
        (if (entity:putdxf e 48 (* (if (numberp v) v 1.0) factor))
            (setq res (1+ res)))))
     ((= (strcase op) "BLK")
      (foreach e (@block:blk-refs blks)
        (if (= "INSERT" (entity:getdxf e 0))
            (progn
              (entity:putdxf e 41 (* (if (entity:getdxf e 41) (entity:getdxf e 41) 1.0) factor))
              (entity:putdxf e 42 (* (if (entity:getdxf e 42) (entity:getdxf e 42) 1.0) factor))
              (entity:putdxf e 43 (* (if (entity:getdxf e 43) (entity:getdxf e 43) 1.0) factor))
              (setq res (1+ res))))))
     (t
      (foreach e (@block:hatch-ents names)
        (entity:putdxf e 41 (* (if (entity:getdxf e 41) (entity:getdxf e 41) 1.0) factor))
        (setq res (1+ res)))))
    (princ (strcat "\n已调整 " (itoa res) " 个对象的比例"))
    res)))

(defun @block:dtbl (/ blks op factor res)
  "交互式批量调整块比例：选块、选调整对象(线型/块/填充)、输入系数"
  "实际修改的对象数(整数)，失败返回 nil"
  "(@block:dtbl)"
  (@::prompt (strcat "块管理 -> 块比例批量调整\n"
                     " 可调块内实体线型比例、块参照比例、块内填充图案比例。"))
  (setq blks (pickset:to-list (ssget '((0 . "INSERT")))))
  (if (null blks)
      (progn (princ "\n未选择块参照") nil)
      (progn
        (initget "ltype blk hatch")
        (setq op (getkword "\n调整对象 [线型比例(ltype)/块比例(blk)/填充比例(hatch)] <ltype>: "))
        (or op (setq op "ltype"))
        (setq factor (getreal "\n请输入比例系数 <2>: "))
        (or factor (setq factor 2.0))
        (@block:undo-start)
        (setq res (@block:ai-dtbl blks op factor))
        (@block:undo-end)
        res)))

;; ============================================================
;; 7. 图块统计(慧眼图库)
;; ============================================================

(defun @block:csv-quote (s)
  "把值转成 CSV 字段(加引号，内部引号加倍)"
  "带引号的字符串"
  "(@block:csv-quote \"a,b\")"
  (if (null s)
      "\"\""
      (strcat "\""
              (string:subst-all "\"\"" "\"" (vl-princ-to-string s))
              "\"")))

(defun @block:block-see-csv (data out-file / f item)
  "把块统计列表写入 CSV 文件(块名,实体数,使用次数,宽,高)"
  "输出文件路径，失败返回 nil"
  "(@block:block-see-csv data \"C:/blocks.csv\")"
  (if (null (setq f (open out-file "w")))
      (progn (princ (strcat "\n无法创建文件: " out-file)) nil)
      (progn
        (princ "块名,实体数,使用次数,宽,高\n" f)
        (foreach item data
          (princ (strcat (@block:csv-quote (car item)) ","
                         (itoa (cadr item)) ","
                         (itoa (caddr item)) ","
                         (rtos (nth 3 item) 2 2) ","
                         (rtos (nth 4 item) 2 2) "\n")
                 f))
        (close f)
        (princ (strcat "\n已导出: " out-file))
        out-file)))

(defun @block:ai-block-see (out-file top-n
                            / rec name ents uses box data item w h tmp i)
  "AI 入口：统计图中所有块定义(块名/块内实体数/使用次数/包围盒宽高)，可导出 CSV"
  "列表 (块名 实体数 使用次数 宽 高)，按使用次数降序；无块返回 nil"
  "(@block:ai-block-see \"C:/blocks.csv\" 20)"
  (setq uses (@block:usage-alist))
  (while (setq rec (tblnext "block" (null rec)))
    (setq name (cdr (assoc 2 rec)))
    (if (/= "*" (substr name 1 1))
        (progn
          (setq ents (length (block:ent-list name)))
          (setq box (@block:blk-def-box name))
          (setq w (if box (- (car (cadr box)) (car (car box))) 0.0))
          (setq h (if box (- (cadr (cadr box)) (cadr (car box))) 0.0))
          (setq data (cons (list name ents (if (cdr (assoc name uses)) (cdr (assoc name uses)) 0) w h)
                           data)))))
  (setq data (vl-sort data '(lambda (a b) (> (caddr a) (caddr b)))))
  (if (and (numberp top-n) (> top-n 0) (> (length data) top-n))
      (progn
        (setq tmp nil i 0)
        (while (and data (< i top-n))
          (setq tmp (cons (car data) tmp) data (cdr data) i (1+ i)))
        (setq data (reverse tmp))))
  (if data
      (progn
        (if out-file (@block:block-see-csv data out-file))
        (princ (strcat "\n共 " (itoa (length data)) " 个块定义"))
        (foreach item data
          (princ (strcat "\n" (car item)
                         " | 实体 " (itoa (cadr item))
                         " | 使用 " (itoa (caddr item))
                         " | " (rtos (nth 3 item) 2 1)
                         " x " (rtos (nth 4 item) 2 1))))
        data)
      (progn (princ "\n图中没有可统计的块定义") nil)))

(defun @block:block-see (/ out top data)
  "交互式图块统计：输出各块的实体数、使用次数、尺寸，可导出 CSV"
  "列表 (块名 实体数 使用次数 宽 高)，失败返回 nil"
  "(@block:block-see)"
  (@::prompt (strcat "块管理 -> 图块统计\n"
                     " 统计所有块定义的实体数、使用次数、包围盒尺寸，可导出 CSV。"))
  (setq out (getstring "\nCSV 输出文件(回车=默认名，输入 - 不导出): "))
  (if (= out "")
      (setq out (strcat (getvar "dwgprefix") (getvar "dwgname") "_blocks.csv")))
  (if (= out "-") (setq out nil))
  (setq top (getint "\n只显示使用次数前 N 个 <全部>: "))
  (setq data (@block:ai-block-see out top))
  (if out (princ (strcat "\n统计结果已写入: " out)))
  data)

;; ============================================================
;; 菜单
;; ============================================================

(@:add-menus
 '((_"块管理")
   ((_"重命名图块") (@block:blk-rename))
   ((_"批量重命名图块") (@block:blk-rename-batch))
   ((_"修改块插入基点") (@block:blk-insertion))
   ((_"块替换(保留属性)") (@block:blk-replace))
   ((_"沿路径等距布块") (@block:blk-along-line))
   ((_"多块基点缩放") (@block:multi-scale))
   ((_"块比例批量调整") (@block:dtbl))
   ((_"图块统计(慧眼)") (@block:block-see))
   ))
