;;------------------------=={ 一键清理/短线清理/词典/合并重线 }==--------------;;
;;  at-purge 补充清理功能（对应 iMini: mini_purge / mini_line_clean / mini_dicts / mini_line_union）：
;;   - quick        一键清理：purge 未引用 图层/块/文字样式/线型 + 清理空块 + 合并重复块
;;   - clean-lines  清理短线、孤立点、空文字
;;   - del-dicts    删除图中常规词典（ACAD_GROUP 默认不删，需显式指定）
;;   - union-lines  合并重线：重叠共线线段合并 + 多段线删除冗余共线顶点
;;  AI 入口（无交互）：at-purge:ai-quick / ai-clean-lines / ai-del-dicts / ai-union-lines
;;----------------------------------------------------------------------;;

(vl-load-com)

(@:add-menu "实体" "一键清理" "(c:purge-quick)")
(@:add-menu "实体" "清理短线点空字" "(c:clean-lines)")
(@:add-menu "实体" "删除常规词典" "(c:del-dicts)")
(@:add-menu "实体" "合并重线" "(c:union-lines)")

;;--------------------------=={ 块表工具 }==---------------------------------;;

;; 遍历每个块定义（排除 模型空间/图纸空间/布局），调用 fn(块名 块对象)

(defun @purge:each-blk (fn / blk)
  "遍历每个块定义（排除模型空间/图纸空间/布局），对每个块调用 fn(块名 块对象)"
  "nil"
  "(@purge:each-blk '(lambda (n b) (princ n)))"
  (vlax-for blk (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if (not (wcmatch (vla-get-name blk) "*Model_Space,*Paper_Space,*Layout*,*布局*"))
        (funcall fn (vla-get-name blk) blk)))
  nil)

;; 表中所有名字

(defun @purge:table-names (tname / df res)
  "列出表（layer/style/linetype 等）中所有名字"
  "字符串列表"
  "(@purge:table-names \"layer\")"
  (while (setq df (tblnext tname (not df)))
    (setq res (cons (cdr (assoc 2 df)) res)))
  (reverse res))

;; 所有块定义名（含布局块）

(defun @purge:blk-names (/ res blk)
  "所有块定义名字（含布局块）"
  "字符串列表"
  "(@purge:blk-names)"
  (vlax-for blk (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq res (cons (vla-get-name blk) res)))
  (reverse res))

;; 引用计数 +1

(defun @purge:inc-ref (refs name / p)
  "refs:((名字 . 次数) ...) 中对 name 的计数加 1"
  "更新后的 refs"
  "(@purge:inc-ref '((\"A\" . 1)) \"A\") => ((\"A\" . 2))"
  (if (setq p (assoc name refs))
      (subst (cons name (1+ (cdr p))) p refs)
      (cons (cons name 1) refs)))

;; 块几何指纹（忽略句柄/属主等引用码）

(defun @purge:blk-fingerprint (blk / ent lst)
  "生成块定义几何指纹（忽略句柄/属主/扩展名码，图元序归一化）"
  "可比较列表，块无图元返回 nil"
  "(@purge:blk-fingerprint blkobj)"
  (setq lst nil)
  (vlax-for e (vla-items blk)
    (if (setq ent (vlax-vla-object->ename e))
        (setq lst (cons
                   (vl-remove-if-not
                    '(lambda (x) (not (member (car x) '(5 330 102 340 346 360))))
                    (entget ent))
                   lst))))
  (if lst
      (vl-sort (reverse lst)
               '(lambda (a b)
                  (< (vl-princ-to-string a) (vl-princ-to-string b))))
      nil))

;;--------------------------=={ 一键清理 }==---------------------------------;;

;; 删除空块定义（无图元且无引用）

(defun at-purge:clean-empty-blks (/ n refs ss i names)
  "删除空块定义（无图元且无引用）"
  "删除的块数"
  "(at-purge:clean-empty-blks)"
  (setq refs nil)
  (if (setq ss (ssget "_X" '((0 . "INSERT"))))
      (repeat (setq i (sslength ss))
        (setq i (1- i))
        (setq refs (@purge:inc-ref refs (cdr (assoc 2 (entget (ssname ss i))))))))
  (setq names nil)
  (@purge:each-blk
   '(lambda (nm blk)
      (if (and (= 0 (vla-get-count blk))
               (null (assoc nm refs)))
          (setq names (cons nm names)))))
  (foreach nm2 names
    (if (tblobjname "block" nm2)
        (progn
          (vlax-invoke (tblobjname "block" nm2) 'vla-delete)
          (setq n (1+ n)))))
  (if n n 0))

;; 识别重复块：几何相同的块两两比较，回调 fn(待删块名 保留块名)

(defun at-purge:find-dup-blks (fn / groups fp)
  "识别几何完全相同的块定义（按块表顺序保留前者），对每对调用 fn(待删块名 保留块名)"
  "nil"
  "(at-purge:find-dup-blks '(lambda (a b) nil))"
  (setq groups nil)
  (@purge:each-blk
   '(lambda (nm blk)
      (if (and (> (vla-get-count blk) 0)
               (setq fp (@purge:blk-fingerprint blk)))
          (if (vl-some '(lambda (g) (equal (car g) fp)) groups)
              (funcall fn nm
                       (cadr (vl-some '(lambda (g) (equal (car g) fp)) groups)))
              (setq groups (cons (cons fp nm) groups))))))
  nil)

;; 合并重复块（几何完全相同的块定义）

(defun at-purge:merge-dup-blks (/ plan pr ss i)
  "合并重复块：两个块定义几何完全相同时保留前者，所有引用替换为前者并删除后者"
  "合并删除的重复块数"
  "(at-purge:merge-dup-blks)"
  (setq plan nil)
  (at-purge:find-dup-blks
   '(lambda (nm keeper)
      (setq plan (cons (cons nm keeper) plan))))
  (foreach pr plan
    (if (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 (car pr)))))
        (repeat (setq i (sslength ss))
          (setq i (1- i))
          (entity:putdxf (ssname ss i) 2 (cdr pr))))
    (@purge:each-blk
     '(lambda (nm2 blk2)
        (vlax-for e2 (vla-items blk2)
          (if (and (= "AcDbBlockReference" (vlax-name e2))
                   (= (vla-get-name e2) (car pr)))
              (vla-put-name e2 (cdr pr))))))
    (if (tblobjname "block" (car pr))
        (vlax-invoke (tblobjname "block" (car pr)) 'vla-delete)
        (princ (strcat "at-purge:merge-dup-blks: 找不到块 " (car pr)))))
  (length plan))

;; 记录一个图元使用的 图层/块/线型/文字样式

(defun @purge:count-attrs (used d)
  "把图元 d 使用的 图层(8)/块(2)/线型(6)/文字样式(7) 记入 used 计数表"
  "更新后的 used"
  "(@purge:count-attrs nil (entget en))"
  (if (cdr (assoc 8 d))
      (setq used (@purge:inc-ref used (cdr (assoc 8 d)))))
  (if (wcmatch (cdr (assoc 0 d)) "INSERT")
      (setq used (@purge:inc-ref used (cdr (assoc 2 d)))))
  (if (and (cdr (assoc 6 d))
           (not (wcmatch (strcase (cdr (assoc 6 d))) "BYLAYER,BYBLOCK")))
      (setq used (@purge:inc-ref used (cdr (assoc 6 d)))))
  (if (and (wcmatch (cdr (assoc 0 d)) "TEXT,MTEXT")
           (cdr (assoc 7 d)))
      (setq used (@purge:inc-ref used (cdr (assoc 7 d)))))
  used)

;; 收集全图（含块定义内）使用的 图层/块/线型/文字样式

(defun @purge:used-attrs (/ ss i en d used blk)
  "收集全图（含块定义内）使用的 图层/块/线型/文字样式 计数表"
  "((名字 . 次数) ...)"
  "(@purge:used-attrs)"
  (setq used nil)
  (if (setq ss (ssget "_X"))
      (repeat (setq i (sslength ss))
        (setq i (1- i))
        (if (setq d (entget (ssname ss i)))
            (setq used (@purge:count-attrs used d)))))
  (@purge:each-blk
   '(lambda (nm blk2 / en2 d2)
      (vlax-for e2 (vla-items blk2)
        (if (setq en2 (vlax-vla-object->ename e2))
            (if (setq d2 (entget en2))
                (setq used (@purge:count-attrs used d2)))))))
  used)

;; 删除表中未被使用的项（keep-fn 返回 T 的名字保留）

(defun @purge:purge-tbl (tname used keep-fn / df nm n)
  "删除符号表 tname 中未被 used 引用的项（keep-fn 保护的名字不删）"
  "删除的个数"
  "(@purge:purge-tbl \"layer\" (@purge:used-attrs) '(lambda (x) (= x \"0\")))"
  (setq n 0)
  (while (setq df (tblnext tname (not df)))
    (if (and (null (assoc (cdr (assoc 2 df)) used))
             (or (null keep-fn)
                 (not (funcall keep-fn (cdr (assoc 2 df))))))
        (if (entdel (tblobjname tname (cdr (assoc 2 df))))
            (setq n (1+ n)))))
  n)

;; AI 入口：一键清理

(defun at-purge:ai-quick (what / pre post empt dups)
  "AI 入口：一键清理：purge 未引用的 图层/块/文字样式/线型 + 清理空块 + 合并重复块（几何相同）
   what：\"layer\"/\"blk\"/\"style\"/\"ltype\"，nil=全部类型
   \"((:layers n) (:blks n) (:styles n) (:ltypes n) (:empties n) (:dups n))，失败返回 nil\"
   \"(at-purge:ai-quick nil)\""
  (if (and what (not (p:stringp what)))
      (setq what (symbol-name what)))
  (if what
      (setq what (strcase (vl-string-trim " \t" what))))
  (startundo)
  (setq pre (list (@purge:table-names "layer")
                  (@purge:blk-names)
                  (@purge:table-names "style")
                  (@purge:table-names "linetype")))
  (cond
    ((null what)
     (command "purge" "a" "*" "N"))
    ((= what "LAYER")
     (@purge:purge-tbl "layer" (@purge:used-attrs)
                       '(lambda (nm)
                          (or (wcmatch nm "0") (= nm (getvar "clayer"))))))
    ((= what "BLK")
     (@purge:purge-tbl "block" (@purge:used-attrs)
                       '(lambda (nm) (wcmatch nm "*_*,ACAD_*"))))
    ((= what "STYLE")
     (@purge:purge-tbl "style" (@purge:used-attrs)
                       '(lambda (nm)
                          (wcmatch nm "Standard,*Annotation*,ACAD_*"))))
    ((= what "LTYPE")
     (@purge:purge-tbl "linetype" (@purge:used-attrs)
                       '(lambda (nm)
                          (wcmatch nm "BYLAYER,BYBLOCK,Continuous,连续"))))
    (t
     (princ (strcat "at-purge:ai-quick: 未知类型 " what))
     (endundo)
     nil))
  (if (or (null what) (= what "BLK"))
      (progn
        (setq empt (at-purge:clean-empty-blks))
        (setq dups (at-purge:merge-dup-blks))))
  (endundo)
  (setq post (list (@purge:table-names "layer")
                   (@purge:blk-names)
                   (@purge:table-names "style")
                   (@purge:table-names "linetype")))
  (list
   (cons '(:layers) (max 0 (- (length (nth 0 pre)) (length (nth 0 post)))))
   (cons '(:blks) (max 0 (- (length (nth 1 pre)) (length (nth 1 post)))))
   (cons '(:styles) (max 0 (- (length (nth 2 pre)) (length (nth 2 post)))))
   (cons '(:ltypes) (max 0 (- (length (nth 3 pre)) (length (nth 3 post)))))
   (cons '(:empties) (if empt empt 0))
   (cons '(:dups) (if dups dups 0))))

;; 交互命令：一键清理

(defun c:purge-quick (/ what res)
  "一键清理：purge 未引用的图层/块/文字样式/线型，清理空块，合并重复块"
  (setq what (getkword "\n清理类型 [全部/图层/块/样式/线型] <全部>："))
  (if what
      (setq what (cond
                   ((wcmatch (strcase what) "ALL,全部") nil)
                   ((wcmatch (strcase what) "LAYER,图层") "layer")
                   ((wcmatch (strcase what) "BLK,BLOCK,块") "blk")
                   ((wcmatch (strcase what) "STYLE,样式") "style")
                   ((wcmatch (strcase what) "LTYPE,LTYPES,线型") "ltype")
                   (t nil))))
  (if (setq res (at-purge:ai-quick what))
      (princ (strcat "\n已清理：" (princ res))))
  (princ))

;;--------------------------=={ 清理短线点空字 }==---------------------------;;

;; 字符串是否空或全空白

(defun @purge:blank-p (s)
  "判断字符串是否为空或仅空白字符"
  "T 或 nil"
  "(@purge:blank-p \"  \") => T"
  (if (null s)
      t
      (= 0 (length (vl-string-trim " \t\n" s)))))

;; 多段线总长度

(defun @purge:lwpoly-length (d / pts n i total)
  "多段线（entget 列表）的总长度"
  "实数"
  "(@purge:lwpoly-length (entget en))"
  (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) d))
        n (length pts)
        total 0.0)
  (if (< n 2)
      0.0
      (progn
        (setq i 0)
        (repeat (1- n)
          (setq total (+ total (distance (nth i pts) (nth (+ i 1) pts)))
                i (1+ i)))
        (if (and (cdr (assoc 70 d)) (= 1 (logand 1 (cdr (assoc 70 d)))))
            (+ total (distance (nth (1- n) pts) (car pts)))
            total))))

;; 删除多段线中长度小于 min-len 的边（收缩顶点）
;; 返回：nil=多段线整体过短应删除；0=无变化；正整数=删除的顶点数

(defun at-purge:clean-lwpoly-short (en min-len / d pts new i n closed)
  "删除多段线中短于 min-len 的边（短边中间的顶点被收缩）"
  "nil=整体过短应删除；0=无变化；正数=删除的顶点数"
  "(at-purge:clean-lwpoly-short en 10)"
  (setq d (entget en))
  (if (< (@purge:lwpoly-length d) min-len)
      nil
      (progn
        (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) d))
              n (length pts))
        (if (< n 2)
            0
            (progn
              (setq closed (and (cdr (assoc 70 d))
                                (= 1 (logand 1 (cdr (assoc 70 d)))))
                    new nil)
              (if (or (not closed)
                      (>= (distance (nth (1- n) pts) (car pts)) min-len))
                  (setq new (list (car pts))))
              (setq i 0)
              (repeat (1- n)
                (setq i (1+ i))
                (if (>= (distance (nth (1- i) pts) (nth i pts)) min-len)
                    (setq new (cons (nth i pts) new))))
              (if (< (length new) 2)
                  nil
                  (if (equal (reverse new) pts)
                      0
                      (progn
                        (setq d (vl-remove-if-not '(lambda (x) (not (= (car x) 10))) d))
                        (entmod (append d (mapcar '(lambda (p) (cons 10 p)) new)))
                        (entupd en)
                        (- (length pts) (length new))))))))))

;; AI 入口：清理短线、孤立点、空文字

(defun at-purge:ai-clean-lines (min-len ss / res i en d r)
  "AI 入口：清理短线、孤立点、空文字
   min-len：短线阈值（长度小于它的线/多段线边被删除）；ss：选择集或图元列表，nil=全图
   \"((:lines n) (:polylines n) (:points n) (:texts n))，失败返回 nil\"
   \"(at-purge:ai-clean-lines 10)\""
  (if (or (null min-len) (< min-len 0))
      (setq min-len 0.0))
  (if (null ss)
      (setq ss (ssget "_X"))
      (if (listp ss)
          (setq ss (pickset:from-list ss))))
  (if (null ss)
      (progn
        (princ "at-purge:ai-clean-lines: 无图元")
        nil)
      (progn
        (setq res (list (cons '(:lines) 0) (cons '(:polylines) 0)
                        (cons '(:points) 0) (cons '(:texts) 0)))
        (startundo)
        (repeat (setq i (sslength ss))
          (setq i (1- i))
          (if (and (setq en (ssname ss i))
                   (setq d (entget en)))
              (cond
                ((= "LINE" (cdr (assoc 0 d)))
                 (if (< (distance (cdr (assoc 10 d)) (cdr (assoc 11 d))) min-len)
                     (progn
                       (entdel en)
                       (setq res (subst (cons '(:lines) (1+ (cdr (assoc '(:lines) res))))
                                        (assoc '(:lines) res) res)))))
                ((= "LWPOLYLINE" (cdr (assoc 0 d)))
                 (if (null (setq r (at-purge:clean-lwpoly-short en min-len)))
                     (progn
                       (entdel en)
                       (setq res (subst (cons '(:polylines) (1+ (cdr (assoc '(:polylines) res))))
                                        (assoc '(:polylines) res) res)))
                     (if (> r 0)
                         (setq res (subst (cons '(:polylines) (1+ (cdr (assoc '(:polylines) res))))
                                          (assoc '(:polylines) res) res)))))
                ((= "POINT" (cdr (assoc 0 d)))
                 (entdel en)
                 (setq res (subst (cons '(:points) (1+ (cdr (assoc '(:points) res))))
                                  (assoc '(:points) res) res)))
                ((wcmatch (cdr (assoc 0 d)) "TEXT,MTEXT")
                 (if (@purge:blank-p (cdr (assoc 1 d)))
                     (progn
                       (entdel en)
                       (setq res (subst (cons '(:texts) (1+ (cdr (assoc '(:texts) res))))
                                        (assoc '(:texts) res) res))))))))
        (endundo)
        res)))

;; 交互命令：清理短线点空字

(defun c:clean-lines (/ min-len ss res)
  "清理短线、孤立点、空文字：删除短于阈值的线/多段线边、所有点、空文字"
  (setq min-len (getreal "\n短线阈值（回车默认 10.0）："))
  (if (or (null min-len) (< min-len 0))
      (setq min-len 10.0))
  (princ "\n选择要清理的图元（回车清理全图）：")
  (setq ss (ssget))
  (if (setq res (at-purge:ai-clean-lines min-len ss))
      (princ (strcat "\n已清理：" (princ res))))
  (princ))

;;--------------------------=={ 删除常规词典 }==-----------------------------;;

;; 从词典中删除一个词条

(defun @purge:dict-remove (dict nm / d)
  "从词典 dict 中删除词条 nm"
  "T 或 nil"
  "(@purge:dict-remove (namedobjdict) \"ACAD_MTRANS\")"
  (if (and dict (setq d (dictget dict nm)))
      (progn
        (dictremove dict nm)
        t)
      nil))

;; AI 入口：删除图中常规词典

(defun at-purge:ai-del-dicts (names / deleted nm msp)
  "AI 入口：删除图中指定的常规词典
   names：词典名列表（字符串）或逗号分隔字符串；nil=默认（ACAD_MTRANS/ACAD_LAYER_STATES/ACAD_MLEADERSTYLE）
   注意：删除 ACAD_GROUP 会丢失组，默认不删，需显式指定
   \"已删除的词典名列表，失败返回 nil\"
   \"(at-purge:ai-del-dicts nil)\""
  (if (null names)
      (setq names (list "ACAD_MTRANS" "ACAD_LAYER_STATES" "ACAD_MLEADERSTYLE")))
  (if (p:stringp names)
      (setq names (vl-string-split (vl-string-subst "，" "," names) ",")))
  (if (and (listp names) (vl-every 'p:stringp names))
      (progn
        (setq deleted nil)
        (foreach nm (mapcar 'strcase names)
          (if (or (@purge:dict-remove (namedobjdict) nm)
                  (and (setq msp (dictget (namedobjdict) "ACAD_MODEL"))
                       (@purge:dict-remove msp nm)))
              (setq deleted (cons nm deleted))))
        (reverse deleted))
      (progn
        (princ "at-purge:ai-del-dicts: names 须为字符串列表")
        nil)))

;; 交互命令：删除常规词典

(defun c:del-dicts (/ names res)
  "删除图中常规词典（默认 ACAD_MTRANS/ACAD_LAYER_STATES/ACAD_MLEADERSTYLE；输入 ACAD_GROUP 会同时删除组）"
  (princ "\n输入词典名（逗号分隔，回车用默认）：")
  (if (setq names (getstring t "词典名（逗号分隔）："))
      (setq names (vl-string-split (vl-string-subst "，" "," names) ",")))
  (if (setq res (at-purge:ai-del-dicts names))
      (if res
          (princ (strcat "\n已删除词典：" (princ res)))
          (princ "\n没有可删除的词典")))
  (princ))

;;--------------------------=={ 合并重线 }==---------------------------------;;

;; 二维点内积（忽略 z）

(defun @purge:dot (a b)
  "二维点内积（忽略 z 分量）"
  "实数"
  "(@purge:dot '(1 0 0) '(0 1 0)) => 0.0"
  (+ (* (car a) (car b)) (* (cadr a) (cadr b))))

;; 点 pt 在过 a0 方向 d（单位向量）上的投影参数

(defun @purge:proj (a0 d pt)
  "点 pt 在过 a0、方向 d（单位向量）直线上的投影参数"
  "实数"
  "(@purge:proj '(0 0 0) '(1 0) '(3 2 0)) => 3.0"
  (+ (* (- (car pt) (car a0)) (car d))
     (* (- (cadr pt) (cadr a0)) (cadr d))))

;; 单位向量

(defun @purge:unit-vec (a b / d l)
  "a 到 b 的单位向量"
  "列表 (dx dy)，退化返回 nil"
  "(@purge:unit-vec '(0 0 0) '(1 0 0)) => (0.0 1.0)"
  (setq d (list (- (cadr b) (cadr a)) (- (car b) (car a)))
        l (distance a b))
  (if (< l 1e-12)
      nil
      (list (/ (cadr d) l) (/ (car d) l))))

;; 点到过 p0 方向为 d（单位向量）的直线距离

(defun @purge:pt-line-dist (pt d p0 / t)
  "点到过 p0、方向为 d（单位向量）的直线的距离"
  "实数"
  "(@purge:pt-line-dist '(0.5 1 0) '(1 0) '(0 0 0)) => 1.0"
  (setq t (- (* (car d) (- (car pt) (car p0)))
             (* (cadr d) (- (cadr pt) (cadr p0)))))
  (distance pt (list (+ (car p0) (* t (car d)))
                     (+ (cadr p0) (* t (cadr d)))
                     0.0)))

;; 两线段是否在同一直线上（共线且偏移 < tol）

(defun @purge:same-line-p (a1 b1 a2 b2 tol / d1 d2)
  "判断两线段是否在同一直线上（平行且距离 < tol）"
  "T 或 nil"
  "(@purge:same-line-p '(0 0 0) '(1 0 0) '(0.5 0 0) '(1.5 0 0) 0.001) => T"
  (setq d1 (@purge:unit-vec a1 b1)
        d2 (@purge:unit-vec a2 b2))
  (and d1 d2
       (< (abs (- (* (car d1) (cadr d2)) (* (cadr d1) (car d2)))) 1e-9)
       (< (@purge:pt-line-dist a2 d1 a1) tol)))

;; 共线三顶点判断

(defun at-purge:collinear-p (p1 p2 p3 tol / d1 d2 len cross)
  "判断三点是否共线（容差内）"
  "T 或 nil"
  "(at-purge:collinear-p '(0 0 0) '(1 1 0) '(2 2 0) 0.001) => T"
  (setq d1 (list (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))
        len (distance p1 p2))
  (if (< len 1e-12)
      (equal p2 p3 tol)
      (progn
        (setq d2 (list (- (cadr p3) (cadr p2)) (- (car p3) (car p2)))
              cross (- (* (car d1) (cadr d2)) (* (cadr d1) (car d2))))
        (< (abs (/ cross len)) tol))))

;; 删除多段线冗余共线顶点

(defun at-purge:rm-collinear-verts (en tol / d pts new i n closed)
  "删除多段线冗余共线顶点（连续三点共线时删除中间点）"
  "删除的顶点数；nil=图元已退化被删除；0=无变化"
  "(at-purge:rm-collinear-verts en 0.001)"
  (setq d (entget en))
  (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) d))
        n (length pts))
  (if (< n 3)
      0
      (progn
        (setq closed (and (cdr (assoc 70 d)) (= 1 (logand 1 (cdr (assoc 70 d))))))
        (setq new (list (car pts)))
        (setq i 0)
        (repeat (1- n)
          (setq i (1+ i))
          (if (or (= i (1- n))
                  (not (at-purge:collinear-p
                        (car (reverse new))
                        (nth i pts)
                        (if (and closed (= i (1- n)))
                            (car pts)
                            (nth (1+ i) pts))
                        tol)))
              (setq new (cons (nth i pts) new))))
        (if (< (length new) 2)
            (progn
              (entdel en)
              nil)
            (if (equal (reverse new) pts)
                0
                (progn
                  (setq d (vl-remove-if-not '(lambda (x) (not (= (car x) 10))) d))
                  (entmod (append d (mapcar '(lambda (p) (cons 10 p)) new)))
                  (entupd en)
                  (- (length pts) (length new))))))))

;; 合并同一直线上重叠/相接的线段
;; lines:((start end ent) ...)，返回 ((p1 p2 保留ent|nil 删除ent列表) ...)

(defun @purge:union-line-group (lines tol / lgroups g s a0 d ivs merged m segs n s2 res)
  "合并同一直线上重叠/相接的线段（同一属性组内）"
  "((p1 p2 保留ent|nil 删除ent列表) ...)，保留ent 为 nil 表示需新建"
  "(@purge:union-line-group '((0 0 0) (1 0 0) en) 0.001)"
  (setq lgroups nil)
  (foreach s lines
    (setq g (vl-some
             '(lambda (x)
                (@purge:same-line-p (car x) (cadr x) (car s) (cadr s) tol))
             lgroups))
    (if g
        (setq lgroups (subst (cons s g) g lgroups))
        (setq lgroups (cons (list s) lgroups))))
  (setq res nil)
  (foreach g lgroups
    (setq a0 (caar g)
          d (@purge:unit-vec (caar g) (cadar g)))
    (if d
        (progn
          (setq ivs nil)
          (foreach s g
            (if (> (@purge:proj a0 d (car s)) (@purge:proj a0 d (cadr s)))
                (setq ivs (cons (list (@purge:proj a0 d (cadr s))
                                       (@purge:proj a0 d (car s))
                                       (caddr s))
                                ivs))
                (setq ivs (cons (list (@purge:proj a0 d (car s))
                                       (@purge:proj a0 d (cadr s))
                                       (caddr s))
                                ivs))))
          (setq ivs (vl-sort ivs '(lambda (x y) (< (car x) (car y)))))
          (setq merged nil)
          (foreach m ivs
            (if (and merged
                     (<= (car m) (+ (cadr (car merged)) tol)))
                (setq merged (cons (list (caar merged)
                                         (max (cadr (car merged)) (cadr m))
                                         (caddr (car merged)))
                                    (cdr merged)))
                (setq merged (cons (list (car m) (cadr m) (caddr m)) merged))))
          (foreach m (reverse merged)
            (setq segs (vl-remove-if-not
                        '(lambda (iv)
                           (and (>= (car iv) (- (car m) tol))
                                (<= (cadr iv) (+ (cadr m) tol))))
                        ivs))
            (setq n (length segs))
            (if (and (= n 1)
                     (= (car m) (car (car segs)))
                     (= (cadr m) (cadr (car segs))))
                (progn
                  (setq s2 (vl-some
                            '(lambda (x2) (eq (caddr x2) (caddr (car segs))))
                            g))
                  (setq res (cons (list (car s2) (cadr s2) (caddr s2) nil) res)))
                (setq res (cons
                           (list
                            (list (+ (car a0) (* (car m) (car d)))
                                  (+ (cadr a0) (* (car m) (cadr d)))
                                  0.0)
                            (list (+ (car a0) (* (cadr m) (car d)))
                                  (+ (cadr a0) (* (cadr m) (cadr d)))
                                  0.0)
                            nil
                            (mapcar 'caddr segs))
                           res)))))
        (princ (strcat "at-purge:union-lines: 退化线段已忽略 "
                       (princ (car (car g)))))))
  (reverse res))

;; AI 入口：合并重线

(defun at-purge:ai-union-lines (ss / tol i en d r lines key group rv first-ent fd del n-merged n-verts n-del)
  "AI 入口：合并重线：重叠共线的线段合并为一条 + 多段线删除冗余共线顶点
   ss：选择集或图元列表，nil=全图；tol：容差（nil 用 0.001）
   \"((:merged n) (:verts n) (:deleted n))，失败返回 nil\"
   \"(at-purge:ai-union-lines nil 0.001)\""
  (if (or (null tol) (< tol 0))
      (setq tol 0.001))
  (if (null ss)
      (setq ss (ssget "_X"))
      (if (listp ss)
          (setq ss (pickset:from-list ss))))
  (if (null ss)
      (progn
        (princ "at-purge:ai-union-lines: 无图元")
        nil)
      (progn
        (startundo)
        (setq n-merged 0 n-verts 0 n-del 0)
        ;; 1. 多段线删除冗余共线顶点
        (repeat (setq i (sslength ss))
          (setq i (1- i))
          (if (and (setq en (ssname ss i))
                   (setq d (entget en))
                   (wcmatch (cdr (assoc 0 d)) "LWPOLYLINE,POLYLINE"))
              (if (null (setq r (at-purge:rm-collinear-verts en tol)))
                  (setq n-del (1+ n-del))
                  (if (and r (> r 0))
                      (setq n-verts (1+ n-verts))))))
        ;; 2. 合并重叠共线的 LINE（同层同色同线型）
        (setq lines nil)
        (repeat (setq i (sslength ss))
          (setq i (1- i))
          (if (and (setq en (ssname ss i))
                   (setq d (entget en))
                   (wcmatch (cdr (assoc 0 d)) "LINE"))
              (setq lines (cons
                           (cons (list (cdr (assoc 8 d))
                                       (cdr (assoc 62 d))
                                       (cdr (assoc 6 d)))
                                 (list (cdr (assoc 10 d))
                                       (cdr (assoc 11 d))
                                       en))
                           lines))))
        (foreach key (vl-remove-duplicates (mapcar 'car lines))
          (setq group (vl-remove-if-not '(lambda (x) (equal (car x) key)) lines))
          (foreach rv (@purge:union-line-group (mapcar 'cdr group) tol)
            (if (caddr rv)
                nil
                (progn
                  (setq first-ent (car (cadddr rv))
                        fd (entget first-ent))
                  (foreach del (cadddr rv)
                    (entdel del))
                  (setq n-del (+ n-del (length (cadddr rv))))
                  (entmakex (vl-remove nil
                                       (list '(0 . "LINE")
                                             '(100 . "AcDbEntity")
                                             '(100 . "AcDbLine")
                                             (cons 10 (car rv))
                                             (cons 11 (cadr rv))
                                             (assoc 8 fd)
                                             (assoc 62 fd)
                                             (assoc 6 fd)
                                             (assoc 48 fd)
                                             (assoc 49 fd))))
                  (setq n-merged (1+ n-merged))))))
        (endundo)
        (list (cons '(:merged) n-merged)
              (cons '(:verts) n-verts)
              (cons '(:deleted) n-del)))))
