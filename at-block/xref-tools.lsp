;;;; xref-tools.lsp --- 外部参照与翻转工具
;;
;; 命令:
;;   block-flipm   批量翻转图块(绕各自插入点 / 指定镜像线)
;;   block-flips   单选自由翻转实体(文字/标注保持可读)
;;   xref-repath   外部参照改相对路径
;;   xref2blk      外部参照复制为块插入
;;   batch-xref    按条件批量做外部参照条件图
;;
;; 命名空间 @block:，AI 入口统一为 @block:ai-xxx(无交互)。

(vl-load-com)

;;; ===== 通用小工具 =====

(defun @block:xt:startundo (/ res)
  "开始撤销记录标记"
  "撤销标记对象"
  "(@block:xt:startundo)"
  (setq res (vl-catch-all-apply 'vla-startundomark (list *doc*)))
  (if (vl-catch-all-error-p res) nil res))

(defun @block:xt:endundo (/ res)
  "结束撤销记录标记"
  "标记结果"
  "(@block:xt:endundo)"
  (setq res (vl-catch-all-apply 'vla-endundomark (list *doc*)))
  (if (vl-catch-all-error-p res) nil res))

(defun @block:xt:str (val)
  "把字符串或符号统一转为大写字符串(便于比较参数)"
  "大写字符串,nil 等非字符串返回空串"
  "(@block:xt:str 'mirror)"
  (cond
    ((= (type val) 'str) (strcase val))
    ((= (type val) 'sym) (strcase (vl-symbol-name val)))
    (T "")))

(defun @block:xt:norm360 (ang)
  "将角度(弧度)规范化到 [0, 2pi)"
  "规范后的弧度值"
  "(@block:xt:norm360 -1.0)"
  (setq ang (rem ang (* 2 pi)))
  (if (< ang 0) (setq ang (+ ang (* 2 pi))))
  ang)

(defun @block:xt:upright-angle (ang)
  "将文字旋转角(弧度)调整为朝上(-pi/2 < ang <= pi/2)"
  "调整后的弧度值"
  "(@block:xt:upright-angle 3.5)"
  (setq ang (@block:xt:norm360 ang))
  (if (and (> ang (/ pi 2)) (< ang (* 1.5 pi)))
    (setq ang (@block:xt:norm360 (+ ang pi))))
  ang)

(defun @block:xt:text-like-p (ent)
  "判断图元是否为文字/属性/标注一类的可读性敏感图元"
  "T 或 nil"
  "(@block:xt:text-like-p (car (entsel)))"
  (and (p:enamep ent)
       (wcmatch (strcase (entity:getdxf ent 0))
                "TEXT,MTEXT,ATTRIB,ATTDEF,DIMENSION,ARC_DIMENSION,LEADER,MULTILEADER")))

(defun @block:xt:insert-p (ent)
  "判断图元是否为块参照(INSERT)"
  "T 或 nil"
  "(@block:xt:insert-p (car (entsel)))"
  (and (p:enamep ent) (= "INSERT" (strcase (entity:getdxf ent 0)))))

(defun @block:xt:block-has-text-p (blkname / e rec res)
  "判断块定义中是否含文字/属性/标注(镜像后会出现反字)"
  "T 或 nil"
  "(@block:xt:block-has-text-p \"门\")"
  (if (and blkname
           (setq rec (tblsearch "block" blkname))
           (= 0 (logand 4 (cdr (assoc 70 rec))))
           (setq e (tblobjname "block" blkname)))
    (progn
      (setq e (entnext e))
      (while (and e (not res))
        (if (@block:xt:text-like-p e) (setq res T))
        (setq e (entnext e)))))
  res)

(defun @block:xt:xref-p (ent / name rec)
  "判断图元是否为外部参照参照块(块定义带 xref 标志)"
  "T 或 nil"
  "(@block:xt:xref-p (car (entsel)))"
  (and (@block:xt:insert-p ent)
       (setq name (entity:getdxf ent 2))
       (setq rec (tblsearch "block" name))
       (/= 0 (logand 4 (cdr (assoc 70 rec))))))

(defun @block:xt:layer-locked-p (ent / name rec)
  "判断图元所在图层是否被锁定"
  "T 或 nil"
  "(@block:xt:layer-locked-p (car (entsel)))"
  (and (setq name (entity:getdxf ent 8))
       (setq rec (tblsearch "layer" name))
       (/= 0 (logand 4 (cdr (assoc 70 rec))))))

(defun @block:xt:to-ename-list (arg)
  "把 ename / 选择集 / ename 列表统一转为 ename 列表"
  "ename 列表,nil 输入返回 nil"
  "(@block:xt:to-ename-list (ssget))"
  (cond
    ((p:picksetp arg) (pickset:to-list arg))
    ((p:enamep arg) (list arg))
    ((p:ename-listp arg) arg)
    ((listp arg) (vl-remove-if-not 'p:enamep arg))))

(defun @block:xt:split (str del / pos res)
  "按分隔串拆分字符串,空段丢弃"
  "字符串列表"
  "(@block:xt:split \"a;b\" \";\")"
  (setq res nil)
  (while (setq pos (vl-string-search del str))
    (if (> pos 0) (setq res (cons (substr str 1 pos) res)))
    (setq str (substr str (+ pos 1 (strlen del)))))
  (reverse (if (= str "") res (cons str res))))

(defun @block:xt:unique-name (name / i res)
  "生成不与现有块重名的块名"
  "可用的块名字符串"
  "(@block:xt:unique-name \"门\")"
  (setq res name
        i 1)
  (while (tblsearch "block" res)
    (setq res (strcat name "-" (itoa i))
          i (1+ i)))
  res)

;;; ===== 镜像与翻转 =====

(defun @block:xt:mirror (ent p1 p2 / obj res)
  "把图元按镜像线镜像,返回新图元(源图元保留)"
  "新图元 ename,失败 nil"
  "(@block:xt:mirror (car (entsel)) '(0 0 0) '(0 1 0))"
  (if (and (p:enamep ent) (@block:xt:insert-p ent) (@block:xt:xref-p ent))
    (progn (princ "\n外部参照不能镜像,已跳过。") nil)
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq res (vl-catch-all-apply
                  'vla-mirror
                  (list obj (vlax-3d-point p1) (vlax-3d-point p2))))
      (if (vl-catch-all-error-p res)
        (progn (princ (strcat "\n镜像失败: " (vl-catch-all-error-message res)))
               nil)
        (vlax-vla-object->ename res)))))

(defun @block:xt:mirror-line (pt axis / x y)
  "以插入点为中心构造局部镜像线的两端点"
  "两端点列表 ((x1 y1 z1) (x2 y2 z2))"
  "(@block:xt:mirror-line '(0 0 0) \"x\")"
  (setq x (car pt) y (cadr pt))
  (if (= (@block:xt:str axis) "Y")
    (list (list x (- y 1.0) 0.0) (list x (+ y 1.0) 0.0))
    (list (list (- x 1.0) y 0.0) (list (+ x 1.0) y 0.0))))

(defun @block:xt:wcs-point (ent / obj p)
  "获取图元插入点的 WCS 坐标(供旋转基准点使用)"
  "三维点列表"
  "(@block:xt:wcs-point (entlast))"
  (setq obj (vlax-ename->vla-object ent))
  (setq p (vl-catch-all-apply 'vla-get-insertionpoint (list obj)))
  (if (vl-catch-all-error-p p)
    (entity:getdxf ent 10)
    (entity:to-list p)))

(defun @block:xt:rot180 (ent / obj p res)
  "把块参照绕自身插入点旋转 180 度(保持文字可读的翻转策略)"
  "图元 ename"
  "(@block:xt:rot180 (car (entsel)))"
  (setq p (@block:xt:wcs-point ent))
  (setq obj (vlax-ename->vla-object ent))
  (setq res (vl-catch-all-apply 'vla-rotate (list obj (vlax-3d-point p) pi)))
  (if (vl-catch-all-error-p res)
    (progn (princ (strcat "\n旋转失败: " (vl-catch-all-error-message res))) nil)
    ent))

(defun @block:xt:fix-rot (ent / obj r res)
  "镜像后若文字旋转角朝下,补转 180 度避免文字倒置(仅文字类图元)"
  "图元 ename"
  "(@block:xt:fix-rot (entlast))"
  (if (and (p:enamep ent)
           (wcmatch (strcase (entity:getdxf ent 0)) "TEXT,MTEXT,ATTRIB,ATTDEF")
           (setq r (entity:getdxf ent 50)))
    (if (> (abs (- (@block:xt:upright-angle r) (@block:xt:norm360 r))) 0.001)
      (progn
        (setq obj (vlax-ename->vla-object ent))
        (setq res (vl-catch-all-apply
                    'vla-rotate
                    (list obj (vlax-3d-point (@block:xt:wcs-point ent)) pi)))
        (if (vl-catch-all-error-p res)
          (princ "\n旋转角修正失败。")
          res))))
  ent)

(defun @block:xt:flip-block (ent p1 p2 strategy / st res)
  "按策略翻转单个块参照并返回结果图元"
  "翻转后图元 ename,失败 nil"
  "(@block:xt:flip-block (car (entsel)) '(0 -1 0) '(0 1 0) \"auto\")"
  (setq st (@block:xt:str strategy))
  (if (not (member st '("MIRROR" "ROT180"))) (setq st "AUTO"))
  (if (and (= st "AUTO") (@block:xt:block-has-text-p (entity:getdxf ent 2)))
    (setq st "ROT180"))
  (if (= st "ROT180")
    (setq res (@block:xt:rot180 ent))
    (progn
      ;; 镜像策略:镜像副本替换源块(块内文字会呈镜像,含文字块建议用 auto/rot180)
      (if (setq res (@block:xt:mirror ent p1 p2))
        (progn (entdel ent) res))))
  res)

;;; ===== 1. block-flipm 多组批量翻转块 =====

(defun @block:ai-block-flipm (blks axis strategy / ax lst res e p ln r)
  "AI 调用入口:批量翻转图块,每块绕自身插入点所在轴线镜像或旋转 180。
axis: \"x\"=以水平线为镜线(上下翻转),\"y\"=以竖直线为镜线(左右翻转)。
strategy: nil/\"auto\" 默认(块内含文字则改用旋转180,避免反字),\"mirror\" 强制镜像,\"rot180\" 强制旋转180。
返回翻转后的图元 ename 列表,无有效块返回 nil。
示例: (@block:ai-block-flipm (ssget) \"y\" \"auto\")"
  (setq lst (@block:xt:to-ename-list blks))
  (setq ax (@block:xt:str axis))
  (setq ax (if (member ax '("Y" "V" "VERTICAL")) "Y" "X"))
  (setq res nil)
  (foreach e lst
    (if (and (@block:xt:insert-p e)
             (not (@block:xt:xref-p e))
             (not (@block:xt:layer-locked-p e)))
      (progn
        (setq p (entity:getdxf e 10))
        (setq ln (@block:xt:mirror-line p ax))
        (if (setq r (@block:xt:flip-block e (car ln) (cadr ln) strategy))
          (setq res (cons r res))))))
  (princ (strcat "\n批量翻转图块: " (itoa (length res)) " 个(方向 "
                 ax ")"))
  (reverse res))

(defun @block:ai-block-flipm-line (blks p1 p2 strategy / lst res e r)
  "AI 调用入口:按给定镜像线批量翻转图块。
blks: 图元/选择集/列表;p1 p2: 镜像线两端点;strategy 同 @block:ai-block-flipm。
返回翻转后的图元 ename 列表,失败 nil。
示例: (@block:ai-block-flipm-line (ssget) '(0 0 0) '(0 1 0) \"auto\")"
  (setq lst (@block:xt:to-ename-list blks))
  (setq res nil)
  (if (and p1 p2 (not (equal p1 p2 1e-8)))
    (foreach e lst
      (if (and (@block:xt:insert-p e)
               (not (@block:xt:xref-p e))
               (not (@block:xt:layer-locked-p e)))
        (if (setq r (@block:xt:flip-block e p1 p2 strategy))
          (setq res (cons r res))))
      )
    (princ "\n镜像线无效。"))
  (princ (strcat "\n按线批量翻转图块: " (itoa (length res)) " 个"))
  (reverse res))

(defun @block:block-flipm (/ ss kw st p1 p2 res)
  "命令:批量翻转图块(按 X/Y 轴向或两点镜像线)"
  "翻转结果列表"
  "(@block:block-flipm)"
  (@::prompt "批量翻转图块:选择块后指定翻转方向。块内含文字时默认改用旋转180度,避免出现反字。")
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (initget "X Y L")
      (setq kw (getkword "\n翻转方向[X轴(上下)/Y轴(左右)/两点镜像线(L)] <X>: "))
      (if (null kw) (setq kw "X"))
      (initget "A M R")
      (setq st (getkword "\n翻转策略[自动(A)/镜像(M)/旋转180(R)] <A>: "))
      (setq st (cond ((= st "M") "mirror") ((= st "R") "rot180") (T "auto")))
      (if (= kw "L")
        (progn
          (setq p1 (getpoint "\n镜像线第一点: "))
          (if p1 (setq p2 (getpoint p1 "\n镜像线第二点: "))))
        (setq p1 T))
      (@block:xt:startundo)
      (setq res (if (= kw "L")
                  (@block:ai-block-flipm-line (pickset:to-list ss) p1 p2 st)
                  (@block:ai-block-flipm (pickset:to-list ss) kw st)))
      (@block:xt:endundo)))
  (princ))

;;; ===== 2. block-flips 单组自由翻转实体 =====

(defun @block:ai-block-flips (ents p1 p2 keep-src / lst old res e r srcs)
  "AI 调用入口:把实体按镜像线翻转,文字/标注翻回保持可读。
ents: 图元/选择集/列表;p1 p2: 镜像线两端点;keep-src: 保留源对象(默认 T)。
返回翻转(镜像)后的图元 ename 列表,失败 nil。
示例: (@block:ai-block-flips (ssget) '(0 0 0) '(0 1 0) T)"
  (setq lst (@block:xt:to-ename-list ents))
  (setq res nil)
  (if (and p1 p2 (not (equal p1 p2 1e-8)) lst)
    (progn
      (setq old (getvar "MIRRTEXT"))
      (setvar "MIRRTEXT" 0)          ;; 文字/属性内容不镜像,保持可读
      (foreach e lst
        (if (not (@block:xt:layer-locked-p e))
          (if (setq r (@block:xt:mirror e p1 p2))
            (progn
              (@block:xt:fix-rot r)
              (setq srcs (cons e srcs))
              (setq res (cons r res))))))
      (setvar "MIRRTEXT" old)
      (if (null keep-src)
        (foreach e srcs (entdel e)))
      (princ (strcat "\n翻转实体: " (itoa (length res)) " 个")))
    (princ "\n参数无效或未选中实体。"))
  (reverse res))

(defun @block:block-flips (/ ss p1 p2 kw res)
  "命令:单选自由翻转实体(文字/标注保持可读)"
  "翻转结果列表"
  "(@block:block-flips)"
  (@::prompt "自由翻转:选择实体,再指定镜像线。文字和标注会自动翻回,保持可读。")
  (if (setq ss (ssget))
    (progn
      (setq p1 (getpoint "\n镜像线第一点: "))
      (if p1 (setq p2 (getpoint p1 "\n镜像线第二点: ")))
      (if (and p2 (not (equal p1 p2 1e-8)))
        (progn
          (initget "Y N")
          (setq kw (getkword "\n保留源对象[是(Y)/否(N)] <Y>: "))
          (@block:xt:startundo)
          (setq res (@block:ai-block-flips (pickset:to-list ss) p1 p2
                                          (if (= kw "N") nil T)))
          (@block:xt:endundo)
          (princ (strcat "\n完成: " (itoa (length res)) " 个"))))))
  (princ))

;;; ===== 路径处理 =====

(defun @block:xt:unify-sep (path)
  "把路径中的反斜杠统一为正斜杠"
  "统一分隔符后的路径"
  "(@block:xt:unify-sep \"D:\\\\a\\\\b.dwg\")"
  (while (vl-string-search "\\" path)
    (setq path (vl-string-subst "/" "\\" path)))
  path)

(defun @block:xt:split-path (path / pos res)
  "按目录层级拆分路径(统一用正斜杠)"
  "路径段列表"
  "(@block:xt:split-path \"D:/a/b.dwg\")"
  (setq path (@block:xt:unify-sep path))
  (setq res nil)
  (while (setq pos (vl-string-position 47 path))
    (if (> pos 0) (setq res (cons (substr path 1 pos) res)))
    (setq path (substr path (+ 2 pos))))
  (reverse (if (= path "") res (cons path res))))

(defun @block:xt:join-path (segs sep)
  "用指定分隔符把路径段连接为路径字符串"
  "路径字符串"
  "(@block:xt:join-path '\"D:\" \"a\") \"/\")"
  (if segs
    (apply 'strcat
           (cons (car segs)
                 (mapcar '(lambda (x) (strcat sep x)) (cdr segs))))
    ""))

(defun @block:xt:normalize-path (path / segs res s)
  "消解路径中的 . 与 .. 段,返回规范化的绝对路径"
  "规范化后的路径(正斜杠)"
  "(@block:xt:normalize-path \"D:/a/../b/./c.dwg\")"
  (setq segs (@block:xt:split-path path))
  (setq res nil)
  (foreach s segs
    (cond
      ((= s ".") nil)
      ((= s "..")
       (if (and res (/= (car res) ".."))
         (setq res (cdr res))
         (setq res (cons s res))))
      (T (setq res (cons s res)))))
  (setq res (reverse res))
  (if (wcmatch (@block:xt:unify-sep path) "/*")
    (strcat "/" (@block:xt:join-path res "/"))
    (@block:xt:join-path res "/")))

(defun @block:xt:absolute-path (path dir / p d)
  "把可能是相对的外部参照路径解析为绝对路径"
  "绝对路径(统一正斜杠)"
  "(@block:xt:absolute-path \".\\\\a.dwg\" \"D:/proj\")"
  (setq p (@block:xt:unify-sep path))
  (setq d (vl-string-right-trim "/" (@block:xt:unify-sep dir)))
  (cond
    ((wcmatch p "/*") p)
    ((wcmatch p "?:/*") p)
    ((= d "") p)
    (T (@block:xt:normalize-path (strcat d "/" p)))))

(defun @block:xt:relative-path (dir path / d p k nd np i res sep)
  "计算 path 相对 dir 的相对路径"
  "相对路径字符串,无法计算时返回 path 原值"
  "(@block:xt:relative-path \"D:/proj\" \"D:/proj/sub/a.dwg\")"
  (setq d (@block:xt:split-path dir))
  (setq p (@block:xt:split-path path))
  (setq nd (length d) np (length p))
  (setq k 0)
  (while (and (< k nd) (< k (1- np))
              (= (strcase (nth k d)) (strcase (nth k p))))
    (setq k (1+ k)))
  (if (or (= k 0) (= np 0))
    path
    (progn
      (setq sep (if (vl-string-search ":" path) "\\" "/"))
      (setq res "")
      (setq i k)
      (while (< i nd)
        (setq res (strcat res ".." sep) i (1+ i)))
      (setq i k)
      (while (< i np)
        (setq res (strcat res (nth i p) (if (< i (1- np)) sep ""))
              i (1+ i)))
      (cond
        ((= res "") path)
        ((wcmatch res "..*") res)
        (T (strcat "." sep res))))))

;;; ===== 3. xref-repath 外部参照改相对路径 =====

(defun @block:xt:xref-list (/ ss res)
  "获取当前图形中所有外部参照的参照块图元"
  "ename 列表"
  "(@block:xt:xref-list)"
  (setq res nil)
  (if (setq ss (ssget "x" '((0 . "INSERT"))))
    (foreach e (pickset:to-list ss)
      (if (@block:xt:xref-p e) (setq res (cons e res)))))
  (reverse res))

(defun @block:xt:set-xref-path (name rel / brec rec)
  "直接改写外部参照块记录的路径组码(1),作为 vla-put-path 的后备手段"
  "T 成功,nil 失败"
  "(@block:xt:set-xref-path \"条件图\" \".\\\\a.dwg\")"
  (setq brec (tblobjname "block" name))
  (if (and brec (setq rec (entget brec)) (assoc 1 rec))
    (if (entmod (subst (cons 1 rel) (assoc 1 rec) rec)) T nil)))

(defun @block:ai-xref-repath (xrefs base-dir / dir lst res e name brec path rel obj cr ok)
  "AI 调用入口:把外部参照路径改为相对基准目录(默认当前图形目录)的相对路径。
xrefs: 外部参照图元/列表,nil 表示全部;base-dir: 基准目录,nil 用 DWGPREFIX。
返回 ((参照名 . 新路径) ...) 成功列表,失败 nil。
示例: (@block:ai-xref-repath nil nil)"
  (setq dir (if (and base-dir (/= base-dir "")) base-dir (getvar "DWGPREFIX")))
  (setq dir (vl-string-right-trim "\\/" dir))
  (setq lst (if xrefs (@block:xt:to-ename-list xrefs) (@block:xt:xref-list)))
  (setq res nil)
  (foreach e lst
    (if (@block:xt:xref-p e)
      (progn
        (setq name (entity:getdxf e 2))
        (setq brec (tblsearch "block" name))
        (setq path (@block:xt:absolute-path (cdr (assoc 1 brec)) dir))
        (cond
          ((not (findfile path))
           (princ (strcat "\n参照文件不存在: " path)))
          (T
           (setq rel (@block:xt:relative-path dir path))
           (setq obj (vl-catch-all-apply 'vla-item (list *blks* name)))
           (setq ok (and (not (vl-catch-all-error-p obj))
                         (not (vl-catch-all-error-p
                                (vl-catch-all-apply 'vla-put-path (list obj rel))))))
           (if (not ok) (setq ok (@block:xt:set-xref-path name rel)))
           (if (not ok)
             (princ (strcat "\n" name " 路径更新失败。"))
             (progn
               (setq cr (vl-catch-all-apply 'vl-cmdf (list "_.-xref" "_R" name)))
               (if (vl-catch-all-error-p cr)
                 (princ (strcat "\n" name " 路径已改,重载失败,请手动重载。"))
                 (princ (strcat "\n" name " -> " rel)))
               (setq res (cons (cons name rel) res)))))))))
  (reverse res))

(defun @block:xref-repath (/ dir res)
  "命令:把全部外部参照的路径改为相对当前图形目录的相对路径"
  "((参照名 . 新路径) ...) 列表"
  "(@block:xref-repath)"
  (@::prompt "把外部参照的绝对路径改写为相对当前图形目录的路径,便于整个目录拷贝后仍能正确引用。")
  (setq dir (getstring T (strcat "\n基准目录 <" (getvar "DWGPREFIX") ">: ")))
  (@block:xt:startundo)
  (setq res (@block:ai-xref-repath nil dir))
  (@block:xt:endundo)
  (princ (strcat "\n已改为相对路径的参照: " (itoa (length res)) " 个"))
  (princ))

;;; ===== 4. xref2blk 复制 Xref 为块 =====

(defun @block:ai-xref2blk (xref blk-name / e name brec path pt sx sy sz rot lyr new obj dir ss res)
  "AI 调用入口:把外部参照拆为同名普通块插入(插入点/比例/旋转保持不变)。
xref: 外部参照的 INSERT 图元或参照名;blk-name: 块名(同时作为图层名),nil 用原参照名。
返回新块参照 ename,失败 nil。
示例: (@block:ai-xref2blk (car (entsel)) \"门窗\")"
  (cond
    ((p:enamep xref) (setq e xref))
    ((= (type xref) 'str)
     (if (setq ss (ssget "x" (list (cons 0 "INSERT") (cons 2 xref))))
       (setq e (ssname ss 0))))
    (T (setq e nil)))
  (cond
    ((null e) (princ "\n未找到外部参照。") nil)
    ((not (@block:xt:xref-p e)) (princ "\n所选对象不是外部参照。") nil)
    (T
     (setq name (entity:getdxf e 2))
     (setq brec (tblsearch "block" name))
     (setq dir (getvar "DWGPREFIX"))
     (setq path (@block:xt:absolute-path (cdr (assoc 1 brec)) dir))
     (if (null (findfile path))
       (progn (princ (strcat "\n参照文件不存在: " path)) nil)
       (progn
         (setq pt (@block:xt:wcs-point e)
               sx (entity:getdxf e 41)
               sy (entity:getdxf e 42)
               sz (entity:getdxf e 43)
               rot (entity:getdxf e 50))
         (setq sx (if sx sx 1.0))
         (setq sy (if sy sy sx))
         (setq sz (if sz sz sx))
         (setq rot (if rot rot 0.0))
         (setq lyr (if (and blk-name (= (type blk-name) 'str) (/= blk-name ""))
                     blk-name
                     name))
         (if (not (tblsearch "layer" lyr)) (layer:make lyr nil nil nil))
         ;; 先拆离外部参照,再按原位置插入为普通块
         (setq res (vl-catch-all-apply 'vl-cmdf (list "_.-xref" "_D" name)))
         (if (vl-catch-all-error-p res) (princ "\n拆离外部参照失败。"))
         (setq obj (vl-catch-all-apply
                     'vla-insertblock
                     (list *ms* (vlax-3d-point pt) path sx sy sz rot)))
         (cond
           ((vl-catch-all-error-p obj)
            (setvar "cmdecho" 0)
            (setq res (vl-catch-all-apply
                        'vl-cmdf
                        (list "_.-insert" path "_non" pt sx sy rot)))
            (if (vl-catch-all-error-p res)
              (progn (princ "\n转块失败:插入出错。") nil)
              (progn
                (setq new (entlast))
                (entity:putdxf new 8 lyr)
                (princ (strcat "\n外部参照已转为块: " name))
                new)))
           (T
            (setq new (vlax-vla-object->ename obj))
            (entity:putdxf new 8 lyr)
            (princ (strcat "\n外部参照已转为块: " name))
            new)))))))

(defun @block:xref2blk (/ e)
  "命令:把选定的外部参照拆为普通块插入"
  "新块参照 ename,失败 nil"
  "(@block:xref2blk)"
  (@::prompt "把选定的外部参照拆离并原位插入为普通块,便于编辑或另存。")
  (if (setq e (entsel "\n选择外部参照: "))
    (progn
      (@block:xt:startundo)
      (setq e (@block:ai-xref2blk (car e) nil))
      (@block:xt:endundo))
    (princ "\n未选择对象。"))
  e)

;;; ===== 5. batch-xref 批量做外部参照条件图 =====

(defun @block:xt:parse-filter (filter / items res pos key val str)
  "把 \"layer=条件图;block=门*\" 形式的条件串解析为 ssget 过滤表"
  "ssget 过滤表,空条件返回 nil"
  "(@block:xt:parse-filter \"layer=条件图\")"
  (setq str (if (and filter (= (type filter) 'str)) filter ""))
  (setq items (@block:xt:split str ";"))
  (setq res nil)
  (foreach it items
    (if (setq pos (vl-string-search "=" it))
      (progn
        (setq key (strcase (vl-string-trim " \t" (substr it 1 pos))))
        (setq val (vl-string-trim " \t" (substr it (+ pos 2))))
        (cond
          ((member key '("LAYER" "8" "图层" "层"))
           (setq res (cons (cons 8 val) res)))
          ((member key '("BLOCK" "NAME" "2" "块" "块名"))
           (setq res (cons (cons 2 val) res)))
          ((member key '("TYPE" "0" "类型"))
           (setq res (cons (cons 0 (strcase val)) res)))))
      (if (/= it "") (setq res (cons (cons 2 it) res)))))
  (reverse res))

(defun @block:xt:ss->vlss (ss / ssets ssetobj arr n i old)
  "把选择集转为可传给 wblock 的选择集对象"
  "ActiveX 选择集对象"
  "(@block:xt:ss->vlss (ssget))"
  (setq ssets (vla-get-selectionsets *doc*))
  (setq old (vl-catch-all-apply 'vla-item (list ssets "xt_tmp")))
  (if (not (vl-catch-all-error-p old))
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list old)))
      (princ "\n旧选择集删除失败。")
      (setq old nil)))
  (setq ssetobj (vla-add ssets "xt_tmp"))
  (setq n (sslength ss))
  (setq arr (vlax-make-safearray vlax-vbobject (cons 0 (1- n))))
  (setq i 0)
  (repeat n
    (vlax-safearray-put-element arr i (vlax-ename->vla-object (ssname ss i)))
    (setq i (1+ i)))
  (vla-additems ssetobj arr)
  ssetobj)

(defun @block:ai-batch-xref (filter out-dir prefix / flt ss dir sep file ssetobj obj e lst
                             res name new i)
  "AI 调用入口:按条件把实体写成独立 dwg,并在原图以外部参照插入(原实体被移除)。
filter: 条件串,如 \"layer=条件图;block=门*\"、\"type=LINE\",nil/空表示全部实体。
out-dir: 输出目录,默认当前图形目录;prefix: 文件名前缀,默认 xref。
返回 ((xref-ename . dwg路径) ...) 列表;无匹配或写块失败返回 nil。
示例: (@block:ai-batch-xref \"layer=条件图\" \"D:/proj\" \"条件图\")"
  (setq flt (@block:xt:parse-filter filter))
  (setq ss (ssget "x" flt))
  (cond
    ((null ss) (princ "\n没有符合条件的实体。") nil)
    (T
     (setq dir (if (and out-dir (= (type out-dir) 'str) (/= out-dir ""))
                 out-dir
                 (getvar "DWGPREFIX")))
     (setq dir (vl-string-right-trim "\\/" dir))
     (if (null (findfile (strcat dir "/")))
       (if (vl-catch-all-error-p (vl-catch-all-apply 'vl-mkdir (list dir)))
         (princ (strcat "\n目录创建失败: " dir))))
     (setq prefix (if (and prefix (= (type prefix) 'str) (/= prefix ""))
                    prefix
                    "xref"))
     (setq sep (if (vl-string-search ":" dir) "\\" "/"))
     (setq file (strcat dir sep prefix ".dwg"))
     (setq i 1)
     (while (findfile file)
       (setq file (strcat dir sep prefix "-" (itoa i) ".dwg"))
       (setq i (1+ i)))
     (setq lst (pickset:to-list ss))
     (setq ssetobj (@block:xt:ss->vlss ss))
     (setq res (vl-catch-all-apply 'vla-wblock (list *doc* file ssetobj)))
     (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list ssetobj)))
       (princ "\n临时选择集清理失败。"))
     (cond
       ((vl-catch-all-error-p res)
        (princ (strcat "\n写块失败: " (vl-catch-all-error-message res)))
        nil)
       (T
        ;; wblock 一般已移除原实体,残留的再清理一次
        (foreach e lst (if (entget e) (entdel e)))
        (setq name (@block:xt:unique-name (vl-filename-base file)))
        (setq obj (vl-catch-all-apply
                    'vla-attachexternalreference
                    (list *ms* file name (vlax-3d-point '(0.0 0.0 0.0))
                          1.0 1.0 1.0 0.0 :vlax-false)))
        (cond
          ((vl-catch-all-error-p obj)
           (setq new (block:attach file '(0.0 0.0 0.0) 0.0 1.0)))
          (T (setq new (vlax-vla-object->ename obj))))
        (if new
          (progn
            (princ (strcat "\n条件图已写出并附着: " file))
            (list (cons new file)))
          (progn (princ "\n条件图已写出,但附着失败。") nil)))))))

(defun @block:batch-xref (/ filter dir prefix res)
  "命令:按条件批量做外部参照条件图"
  "((xref-ename . dwg路径) ...) 列表"
  "(@block:batch-xref)"
  (@::prompt "批量条件图:选出符合条件的实体写成独立 dwg,原图以外部参照插入。条件示例:layer=条件图;block=门*")
  (setq filter (getstring T "\n选择条件(如 layer=条件图;block=门*),回车表示全部: "))
  (setq dir (getstring T (strcat "\n输出目录 <" (getvar "DWGPREFIX") ">: ")))
  (setq prefix (getstring T "\n文件名前缀 <xref>: "))
  (@block:xt:startundo)
  (setq res (@block:ai-batch-xref filter dir prefix))
  (@block:xt:endundo)
  (princ (strcat "\n条件图处理完成: " (itoa (length res)) " 个"))
  (princ))

;;; ===== 菜单 =====

(@:add-menus
 '((_"外部参照与翻转")
   ((_"批量翻转块") (@block:block-flipm))
   ((_"自由翻转实体") (@block:block-flips))
   ((_"参照改相对路径") (@block:xref-repath))
   ((_"XRef复制为块") (@block:xref2blk))
   ((_"批量条件图") (@block:batch-xref))))

;;; 命令名(可直接在命令行输入)
(defun c:block-flipm () (@block:block-flipm) (princ))
(defun c:block-flips () (@block:block-flips) (princ))
(defun c:xref-repath () (@block:xref-repath) (princ))
(defun c:xref2blk () (@block:xref2blk) (princ))
(defun c:batch-xref () (@block:batch-xref) (princ))
