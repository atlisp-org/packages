;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 填充 (at-hatch)
;; 复现 iMini 填充类命令：快捷填充、超级填充、选现有图案填充、填充移动对齐、
;; 由填充生成边界、批量分离填充、全能裁刀裁填充、SOLID 变填充、
;; SOLID 满铺多段线、填充图案提取输出、自定义填充、修复填充显示。
;; 本文件：菜单注册 + 公共辅助函数（图案解析、填充 DXF 构建与读取、几何）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:add-menus
 '("填充工具"
   ("快捷填充" "(at-hatch:quick-hatch)")
   ("超级填充" "(at-hatch:superhatch)")
   ("选现有图案填充" "(at-hatch:hatch-select)")
   ("填充移动对齐" "(at-hatch:hatch-move)")
   ("由填充生成边界" "(at-hatch:hatch-boundary)")
   ("批量分离填充" "(at-hatch:hatch-isolate)")
   ("全能裁刀裁填充" "(at-hatch:clip)")
   ("SOLID 变填充" "(at-hatch:solid2hatch)")
   ("SOLID 满铺多段线" "(at-hatch:pline2solid)")
   ("填充图案提取输出" "(at-hatch:hatch-out)")
   ("自定义填充" "(at-hatch:custom-pattern)")
   ("修复填充显示" "(at-hatch:fix-pattern)")
   ))

;; ---------------------------------------------------------------------------
;; 基础工具
;; ---------------------------------------------------------------------------

(defun at-hatch:tan (x)
  "正切函数（本方言无全局 tan，自备）"
  "实数"
  "(at-hatch:tan 0.7853982) => 1.0"
  (/ (sin x) (cos x)))

(defun at-hatch:d2r (a)
  "角度转弧度"
  "实数（弧度）"
  "(at-hatch:d2r 180) => 3.14159"
  (* (if (numberp a) a 0.0) (/ pi 180.0)))

(defun at-hatch:r2d (a)
  "弧度转角度"
  "实数（度）"
  "(at-hatch:r2d pi) => 180.0"
  (* (if (numberp a) a 0.0) (/ 180.0 pi)))

(defun at-hatch:tol ()
  "链式连接线段的容差"
  "实数"
  "(at-hatch:tol) => 0.001"
  0.001)

(defun at-hatch:2d (pt)
  "三维点转二维点（填充多段线路径要求二维点）"
  "二维点表"
  "(at-hatch:2d '(1 2 3)) => (1 2)"
  (list (car pt) (cadr pt)))

(defun at-hatch:3d (pt)
  "二维点补 z=0，三维点原样返回"
  "三维点"
  "(at-hatch:3d '(1 2)) => (1 2 0.0)"
  (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0)))

(defun at-hatch:dist (p1 p2)
  "两点距离"
  "实数"
  "(at-hatch:dist '(0 0) '(3 4)) => 5.0"
  (distance p1 p2))

(defun at-hatch:valid-ent (ent / d)
  "图元是否仍有效（未被删除）"
  "T 或 nil"
  "(at-hatch:valid-ent (entlast))"
  (if (eq (type ent) 'ename)
    (progn
      (setq d (vl-catch-all-apply 'entget (list ent)))
      (if (or (null d) (vl-catch-all-error-p d)) nil t))
    nil))

(defun at-hatch:norm-ents (ents / res e)
  "把选择集/单个图元名/嵌套图元名表统一成有效图元名表"
  "图元名列表"
  "(at-hatch:norm-ents (ssget))"
  (if ents
    (cond
      ((eq (type ents) 'pickset) (pickset:to-list ents))
      ((eq (type ents) 'ename) (if (at-hatch:valid-ent ents) (list ents)))
      ((listp ents)
       (setq res nil)
       (foreach e ents
         (if (at-hatch:valid-ent e)
           (setq res (append res (list e)))))
       res)
      (t nil))))

(defun at-hatch:del-ent (ent / o)
  "删除图元，成功返回 T"
  "T 或 nil"
  "(at-hatch:del-ent (entlast))"
  (if (at-hatch:valid-ent ent)
    (progn
      (setq o (vlax-ename->vla-object ent))
      (if o
        (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list o))))
        nil))
    nil))

(defun at-hatch:undo-start (/ r)
  "打开撤消记录标记（失败忽略）"
  "T 或 nil"
  "(at-hatch:undo-start)"
  (setq r (vl-catch-all-apply
           'vla-startundomark
           (list (vla-get-activedocument (vlax-get-acad-object)))))
  (if (vl-catch-all-error-p r) nil t))

(defun at-hatch:undo-end (/ r)
  "关闭撤消记录标记（失败忽略）"
  "T 或 nil"
  "(at-hatch:undo-end)"
  (setq r (vl-catch-all-apply
           'vla-endundomark
           (list (vla-get-activedocument (vlax-get-acad-object)))))
  (if (vl-catch-all-error-p r) nil t))

(defun at-hatch:remove-dxf (d code / res it)
  "从组码对表中删除指定组码"
  "组码对表"
  "(at-hatch:remove-dxf (entget (entlast)) 62)"
  (setq res nil)
  (foreach it d
    (if (/= (car it) code) (setq res (append res (list it)))))
  res)

(defun at-hatch:set-color (ent color / d)
  "设置图元颜色号"
  "T 或 nil"
  "(at-hatch:set-color (car (entsel)) 7)"
  (if (and (at-hatch:valid-ent ent) (numberp color))
    (progn
      (setq d (at-hatch:remove-dxf (entget ent) 62))
      (if (entmod (append d (list (cons 62 color))))
        (progn (entupd ent) t)
        nil))
    nil))

;; ---------------------------------------------------------------------------
;; 图案资源与 .pat 解析
;; ---------------------------------------------------------------------------

(defun at-hatch:pkg-dir (/ d)
  "包资源目录（找不到返回 packages/at-hatch/）"
  "目录字符串，末尾带 /"
  "(at-hatch:pkg-dir)"
  (setq d (vl-catch-all-apply '@::package-path (list "at-hatch")))
  (if (or (vl-catch-all-error-p d) (null d))
    "packages/at-hatch/"
    d))

(defun at-hatch:pkg-file (name / d res)
  "查找包内资源文件，不存在返回 nil"
  "文件全路径或 nil"
  "(at-hatch:pkg-file \"PARQUET.PAT\")"
  (setq res (findfile (strcat (at-hatch:pkg-dir) name)))
  (if (null res) (setq res (findfile (strcat "packages/at-hatch/" name))))
  res)

(defun at-hatch:builtin-patterns ()
  "返回包内置 .pat 资源表（图案名 . 文件名）
资源文件名用 ASCII，避免中文文件名在不同代码页下找不到"
  "点对表"
  "(at-hatch:builtin-patterns)"
  (list (cons "大理石1" "marble1.pat")
        (cons "石纹2" "stone2.pat")
        (cons "木地板条1" "woodfloor1.pat")
        (cons "木纹面1" "woodgrain1.pat")
        (cons "人字砖面1" "herringbone1.pat")
        (cons "有缝标准砖" "brickjoint.pat")
        (cons "PARQUET" "PARQUET.PAT")
        (cons "ENDSHAKE" "ENDSHAKE.PAT")))

(defun at-hatch:std-patterns ()
  "返回常用标准图案名表（CAD 自带，无需 .pat 文件）"
  "字符串表"
  "(at-hatch:std-patterns)"
  (list "SOLID" "ANSI31" "ANSI32" "ANSI33" "ANSI34" "ANSI35" "ANSI36"
        "ANSI37" "ANSI38" "ANSI39" "ANSI40" "ANSI50" "ANSI51" "ANSI52"
        "ANSI53" "ANSI54" "ANSI61" "ANSI62" "ANSI63" "BRICK" "CONCRETE"
        "GRID" "LINES" "DOTS" "GRASS" "CROSS" "DENSE" "DASHED"))

(defun at-hatch:std-pattern-p (name)
  "图案名是否为 CAD 自带标准图案"
  "T 或 nil"
  "(at-hatch:std-pattern-p \"ANSI31\")"
  (if name (if (member (strcase name) (at-hatch:std-patterns)) t nil) nil))

(defun at-hatch:split-str (str delim / pos res)
  "按分隔符拆分字符串"
  "字符串表"
  "(at-hatch:split-str \"a,b\" \",\") => (\"a\" \"b\")"
  (setq res nil)
  (while (setq pos (vl-string-search delim str))
    (setq res (append res (list (substr str 1 pos))))
    (setq str (substr str (+ pos 1 (strlen delim)))))
  (append res (list str)))

(defun at-hatch:parse-pat-line (line / lst res s)
  "把一行 .pat 定义解析成数值表（角度 基点x 基点y 偏移x 偏移y 线型...）"
  "数值表；非法行返回 nil"
  "(at-hatch:parse-pat-line \"0,0,0,0,10,1,-1\")"
  (setq lst (at-hatch:split-str line ",") res nil)
  (foreach s lst
    (setq s (vl-string-trim " \t" s))
    (if (> (strlen s) 0)
      (setq res (append res (list (atof s))))))
  (if (> (length res) 4) res nil))

(defun at-hatch:parse-pat (file / f line defs name nm pos)
  "读取 .pat 文件，返回 (图案名 定义行表)"
  "两元素表或 nil"
  "(at-hatch:parse-pat \"PARQUET.PAT\")"
  (setq f (open file "r") defs nil name nil)
  (if f
    (progn
      (while (setq line (read-line f))
        (setq line (vl-string-trim " \t\r" line))
        (if (and (> (strlen line) 0) (/= (substr line 1 1) ";"))
          (if (= (substr line 1 1) "*")
            (progn
              (setq nm (substr line 2))
              (setq pos (vl-string-search "," nm))
              (if pos (setq nm (substr nm 1 pos)))
              (setq nm (vl-string-trim " \t" nm))
              (if (and (null name) (> (strlen nm) 0)) (setq name nm)))
            (setq defs (append defs (list (at-hatch:parse-pat-line line)))))))
      (close f)
      (if (or name defs) (list name defs)))
    nil))

(defun at-hatch:pattern-file (name / res f files)
  "按图案名查找内置 .pat 文件，找不到返回 nil"
  "文件全路径或 nil"
  "(at-hatch:pattern-file \"PARQUET\")"
  (if name
    (progn
      (foreach f (at-hatch:builtin-patterns)
        (if (and (null res) (= (strcase (car f) t) (strcase name t)))
          (setq res (at-hatch:pkg-file (cdr f)))))
      ;; 也允许直接按图案名对应的 .pat 文件查找
      (foreach f (list (strcat name ".pat") (strcat name ".PAT"))
        (if (and (null res) (at-hatch:pkg-file f))
          (setq res (at-hatch:pkg-file f))))
      res)
    nil))

(defun at-hatch:pattern-defs (name / f r)
  "取图案的定义行（查内置 .pat 资源）"
  "定义行表或 nil"
  "(at-hatch:pattern-defs \"PARQUET\")"
  (setq f (at-hatch:pattern-file name))
  (if f
    (progn
      (setq r (at-hatch:parse-pat f))
      (if r (cadr r) nil))
    nil))

(defun at-hatch:load-pat (file / dir prefs sp)
  "把 .pat 所在目录加入支持搜索路径（便于 CAD 直接识别图案名）"
  "T 或 nil"
  "(at-hatch:load-pat (at-hatch:pattern-file \"PARQUET\"))"
  (if (and file (findfile file))
    (progn
      (setq dir (vl-filename-directory (findfile file)))
      (setq prefs (vla-get-files (vla-get-preferences (vlax-get-acad-object))))
      (setq sp (vla-get-supportpath prefs))
      (if (vl-string-search (strcase dir) (strcase sp))
        t
        (if (vl-catch-all-error-p
              (vl-catch-all-apply 'vla-put-supportpath
                                  (list prefs (strcat sp ";" dir))))
          nil
          t)))
    nil))

(defun at-hatch:ensure-pattern (name)
  "确保图案可用：标准图案或包内 .pat 资源"
  "T 或 nil"
  "(at-hatch:ensure-pattern \"ANSI31\")"
  (if (at-hatch:std-pattern-p name)
    t
    (if (at-hatch:pattern-file name)
      (progn (at-hatch:load-pat (at-hatch:pattern-file name)) t)
      nil)))

(defun at-hatch:num->str (x / s)
  "数值转 .pat 用的紧凑字符串（去掉多余的 0）"
  "字符串"
  "(at-hatch:num->str 1.5) => \"1.5\""
  (if (not (numberp x))
    "0"
    (progn
      (setq s (rtos x 2 8))
      (while (and (> (strlen s) 1) (= (substr s (strlen s)) "0"))
        (setq s (substr s 1 (1- (strlen s)))))
      (if (and (> (strlen s) 0) (= (substr s (strlen s)) "."))
        (setq s (substr s 1 (1- (strlen s)))))
      (if (= s "") "0" s))))

(defun at-hatch:pat-line->str (d / s x)
  "把一条图案定义行转成 .pat 文本行"
  "字符串"
  "(at-hatch:pat-line->str '(0.0 0.0 0.0 0.0 10.0 1.0 -1.0))"
  (setq s (at-hatch:num->str (car d)))
  (foreach x (cdr d)
    (setq s (strcat s "," (at-hatch:num->str x))))
  s)

(defun at-hatch:write-pat (file name defs / f d)
  "把图案定义写成 .pat 文件（纯文本）"
  "写出的文件全路径或 nil"
  "(at-hatch:write-pat \"/tmp/a.pat\" \"A\" '((0 0 0 0 10 1 -1)))"
  (setq f (open file "w"))
  (if f
    (progn
      (write-line (strcat "*" name "," name) f)
      (foreach d defs
        (if d (write-line (at-hatch:pat-line->str d) f)))
      (close f)
      file)
    nil))

;; ---------------------------------------------------------------------------
;; 填充 DXF 构建（entmake，无需临时边界图元）
;; ---------------------------------------------------------------------------

(defun at-hatch:nth-real (lst n)
  "取表中第 n 个元素并转实数，缺省 0.0"
  "实数"
  "(at-hatch:nth-real '(1 2 3) 1) => 2.0"
  (if (and lst (> (length lst) n) (numberp (nth n lst)))
    (float (nth n lst))
    0.0))

(defun at-hatch:pat-line-dxf (d / dashes)
  "把一条图案定义行转成填充 DXF 组码对表"
  "组码对表"
  "(at-hatch:pat-line-dxf '(0 0 0 0 10 1 -1))"
  (if (< (length d) 5)
    nil
    (progn
      (setq dashes (cddddr d))
      (append (list (cons 53 (at-hatch:d2r (car d)))
                    (cons 43 (at-hatch:nth-real d 1))
                    (cons 44 (at-hatch:nth-real d 2))
                    (cons 45 (at-hatch:nth-real d 3))
                    (cons 46 (at-hatch:nth-real d 4))
                    (cons 79 (length dashes)))
              (mapcar (function (lambda (x) (cons 49 x))) dashes)))))

(defun at-hatch:pat-head-dxf (name solid)
  "填充图案头部组码（图案名 + 实体填充标志）"
  "组码对表"
  "(at-hatch:pat-head-dxf \"ANSI31\" nil)"
  (list (cons 2 (if name name "ANSI31")) (cons 70 (if solid 1 0))))

(defun at-hatch:pat-tail-dxf (scale angle pdefs solid)
  "填充图案尾部组码（类型/角度/比例/定义行）
实体填充只写图案类型，写比例/角度会被 CAD 判为非法"
  "组码对表"
  "(at-hatch:pat-tail-dxf 1.0 45.0 nil nil)"
  (if solid
    (list '(76 . 1))
    (append
      (list (cons 76 (if pdefs 2 1))
            (cons 52 (at-hatch:d2r angle))
            (cons 41 (if (and (numberp scale) (> scale 0.0)) scale 1.0))
            '(77 . 0)
            (cons 78 (length pdefs)))
      (apply 'append (mapcar 'at-hatch:pat-line-dxf pdefs)))))

(defun at-hatch:loop-dxf (rec / flag pts bulges n i res b)
  "把环记录（flag 点表 凸度表）转成填充边界 DXF（多段线路径）"
  "组码对表；顶点不足 3 个返回 nil"
  "(at-hatch:loop-dxf (list 1 '((0 0) (10 0) (10 10)) '(0 0 0)))"
  (setq flag (car rec) pts (cadr rec) bulges (caddr rec) n (length pts))
  (if (< n 3)
    nil
    (progn
      (setq res (list (cons 92 (if (= flag 1) 3 2)) '(72 . 1) '(73 . 1) (cons 93 n))
            i 0)
      (while (< i n)
        (setq b (if (and bulges (> (length bulges) i)) (nth i bulges) 0.0))
        (setq res (append res (list (cons 10 (at-hatch:2d (nth i pts)))
                                    (cons 42 (if (numberp b) b 0.0)))))
        (setq i (1+ i)))
      (append res (list '(97 . 0))))))

(defun at-hatch:bbox-center (pts / mnx mny mxx mxy p)
  "点表的包围盒中心"
  "三维点"
  "(at-hatch:bbox-center '((0 0) (10 10))) => (5.0 5.0 0.0)"
  (if pts
    (progn
      (setq mnx (car (car pts)) mxx mnx mny (cadr (car pts)) mxy mny)
      (foreach p pts
        (if (< (car p) mnx) (setq mnx (car p)))
        (if (> (car p) mxx) (setq mxx (car p)))
        (if (< (cadr p) mny) (setq mny (cadr p)))
        (if (> (cadr p) mxy) (setq mxy (cadr p))))
      (list (/ (+ mnx mxx) 2.0) (/ (+ mny mxy) 2.0) 0.0))
    nil))

(defun at-hatch:make-hatch (loops pattern scale angle layer color pdefs /
                            dxf ldxf lp n seed nm solid lay)
  "由环记录表创建填充（entmake，不产生临时边界图元）
参数: loops 环记录表，pattern 图案名，scale 比例，angle 角度(度)，layer/color 可 nil"
  "填充图元名或 nil"
  "(at-hatch:make-hatch (list (list 1 '((0 0) (10 0) (10 10)) nil)) \"ANSI31\" 1.0 0.0)"
  (setq ldxf nil n 0)
  (foreach lp loops
    (setq dxf (at-hatch:loop-dxf lp))
    (if dxf
      (progn
        (setq ldxf (append ldxf dxf) n (1+ n)))))
  (if (< n 1)
    nil
    (progn
      (setq nm (if pattern pattern "ANSI31"))
      (setq solid (= (strcase nm) "SOLID"))
      (setq lay (if (and layer (tblsearch "LAYER" layer)) layer (getvar "CLAYER")))
      (setq seed (if (car loops) (at-hatch:bbox-center (cadr (car loops))) '(0.0 0.0 0.0)))
      (setq dxf (append
                  (list '(0 . "HATCH") '(100 . "AcDbEntity") (cons 8 lay)
                        (if (and (numberp color) (> color 0)) (cons 62 color))
                        '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0))
                  (at-hatch:pat-head-dxf nm solid)
                  (list '(71 . 0) (cons 91 n))
                  ldxf
                  (list '(75 . 0))
                  (at-hatch:pat-tail-dxf scale angle pdefs solid)
                  (list '(98 . 1) (cons 10 seed))))
      (if (entmake (vl-remove nil dxf))
        (entlast)
        nil))))

(defun at-hatch:hatch-from-loops (recs pattern scale angle layer color /
                                  defs nm)
  "由环记录表创建填充（为包内 .pat 图案嵌入定义行；未知图案退回 ANSI31）"
  "填充图元名或 nil"
  "(at-hatch:hatch-from-loops recs \"PARQUET\" 10.0 0.0 nil nil)"
  (setq nm (if pattern pattern "ANSI31"))
  (if (at-hatch:std-pattern-p nm)
    (setq defs nil)
    (progn
      (setq defs (at-hatch:pattern-defs nm))
      (if (null defs)
        (progn
          (princ (strcat "\n图案 " nm " 缺少 .pat 定义，已改用 ANSI31。"))
          (setq nm "ANSI31")))))
  (at-hatch:make-hatch recs nm scale angle layer color defs))

;; ---------------------------------------------------------------------------
;; 填充 DXF 读取（边界环、图案定义）
;; ---------------------------------------------------------------------------

(defun at-hatch:dxf-loops (ent / d cur res)
  "从填充 DXF 数据中拆出各环的组码对表"
  "组码对表列表"
  "(at-hatch:dxf-loops (car (entsel)))"
  (setq d (entget ent) cur nil res nil)
  (foreach it d
    (cond
      ((= (car it) 92)
       (if cur (setq res (cons (reverse cur) res)))
       (setq cur (list it)))
      (cur (setq cur (cons it cur)))))
  (if cur (setq res (cons (reverse cur) res)))
  (reverse res))

(defun at-hatch:edge->rec (edge / et c r a1 a2 ccw sweep mx my ratio n i pts res mj)
  "把一条边界边（组码对表）转成 (点表 凸度表)"
  "两元素表或 nil"
  "(at-hatch:edge->rec '((72 . 1) (10 0.0 0.0 0.0) (11 1.0 0.0 0.0)))"
  (setq et (cdr (assoc 72 edge)))
  (cond
    ((= et 1)
     (list (list (at-hatch:3d (cdr (assoc 10 edge)))) (list 0.0)))
    ((= et 2)
     (setq c (at-hatch:3d (cdr (assoc 10 edge)))
           r (cdr (assoc 40 edge))
           a1 (cdr (assoc 50 edge))
           a2 (cdr (assoc 51 edge))
           ccw (cdr (assoc 73 edge))
           sweep (- a2 a1))
     (if (= ccw 0)
       (if (> sweep 0.0) (setq sweep (- sweep (* 2.0 pi))))
       (if (< sweep 0.0) (setq sweep (+ sweep (* 2.0 pi)))))
     (if (>= (abs sweep) (- (* 2.0 pi) 1e-6)) (setq sweep (* 2.0 pi)))
     (list (list (list (+ (car c) (* r (cos a1)))
                        (+ (cadr c) (* r (sin a1))) 0.0))
           (list (at-hatch:tan (/ sweep 4.0)))))
    ((= et 3)
     (setq c (at-hatch:3d (cdr (assoc 10 edge)))
           mj (at-hatch:3d (cdr (assoc 11 edge)))
           ratio (cdr (assoc 40 edge))
           a1 (cdr (assoc 50 edge))
           a2 (cdr (assoc 51 edge))
           ccw (cdr (assoc 73 edge))
           sweep (- a2 a1))
     (if (and (= ccw 0) (> sweep 0.0)) (setq sweep (- sweep (* 2.0 pi))))
     (if (and (/= ccw 0) (< sweep 0.0)) (setq sweep (+ sweep (* 2.0 pi))))
     (setq mx (- (car mj) (car c)) my (- (cadr mj) (cadr c)))
     (setq n 12 i 0 pts nil)
     (while (<= i n)
       (setq res (+ a1 (* sweep (/ (float i) n))))
       (setq pts (append pts
                         (list (list (+ (car c) (* mx (cos res)) (* (- my) ratio (sin res)))
                                     (+ (cadr c) (* my (cos res)) (* mx ratio (sin res)))
                                     0.0))))
       (setq i (1+ i)))
     (list pts nil))
    ((= et 4)
     (setq pts nil)
     (foreach it edge
       (if (or (= (car it) 10) (= (car it) 11))
         (setq pts (append pts (list (at-hatch:3d (cdr it)))))))
     (list pts nil))
    (t nil)))

(defun at-hatch:loop-rec (L / flag done pts bulges edges cur et sub p b)
  "解析填充的一个边界环为 (flag 点表 凸度表)"
  "环记录或 nil"
  "(at-hatch:loop-rec (car (at-hatch:dxf-loops (car (entsel)))))"
  (setq flag (cdr (car L)) pts nil bulges nil)
  (if (and (numberp flag) (/= 0 (logand flag 2)))
    ;; 多段线路径
    (progn
      (setq done nil)
      (foreach it L
        (if (not done)
          (cond
            ((= (car it) 97) (setq done t))
            ((= (car it) 10) (setq pts (append pts (list (at-hatch:3d (cdr it))))))
            ((= (car it) 42) (setq bulges (append bulges (list (cdr it))))))))
      (if (< (length bulges) (length pts))
        (progn
          (setq p (- (length pts) (length bulges)))
          (while (> p 0) (setq bulges (append bulges (list 0.0)) p (1- p)))))
      (list (if (or (= 1 (logand flag 1)) (= 16 (logand flag 16))) 1 0) pts bulges))
    ;; 边路径
    (progn
      (setq edges nil cur nil)
      (foreach it L
        (cond
          ((= (car it) 72)
           (if cur (setq edges (append edges (list (reverse cur)))))
           (setq cur (list it)))
          (cur (setq cur (cons it cur)))))
      (if cur (setq edges (append edges (list (reverse cur)))))
      (foreach e edges
        (setq sub (at-hatch:edge->rec e))
        (if sub
          (progn
            (setq pts (append pts (car sub)))
            (setq bulges (append bulges (cadr sub))))))
      (if (< (length pts) 3)
        nil
        (list (if (or (= 1 (logand flag 1)) (= 16 (logand flag 16))) 1 0) pts bulges)))))

(defun at-hatch:hatch-recs (ent / res L)
  "提取填充的边界环记录表（外部/内部）"
  "环记录表或 nil"
  "(at-hatch:hatch-recs (car (entsel)))"
  (if (and (at-hatch:valid-ent ent)
           (= (cdr (assoc 0 (entget ent))) "HATCH"))
    (progn
      (foreach L (at-hatch:dxf-loops ent)
        (setq res (append res (list (at-hatch:loop-rec L)))))
      (vl-remove nil res))
    nil))

(defun at-hatch:hatch-loops (ent / recs out in)
  "提取填充的外/内边界点表"
  "((外边界点表...) (内边界点表...)) 或 nil"
  "(at-hatch:hatch-loops (car (entsel)))"
  (setq recs (at-hatch:hatch-recs ent))
  (if recs
    (progn
      (foreach r recs
        (if (= (car r) 1)
          (setq out (append out (list (cadr r))))
          (setq in (append in (list (cadr r))))))
      (list out in))
    nil))

(defun at-hatch:hatch-pattern-info (ent / d nm solid sc ang pdefs)
  "读取填充的图案信息（名称/实体标志/比例/角度/定义行）"
  "五元素表"
  "(at-hatch:hatch-pattern-info (car (entsel)))"
  (if (at-hatch:valid-ent ent)
    (progn
      (setq d (entget ent))
      (setq nm (cdr (assoc 2 d))
            solid (= 1 (cdr (assoc 70 d)))
            sc (cdr (assoc 41 d))
            ang (at-hatch:r2d (cdr (assoc 52 d)))
            pdefs (at-hatch:dxf-pattern-lines d))
      (list nm solid sc ang pdefs))
    nil))

(defun at-hatch:dxf-pattern-lines (d / lst res cur)
  "从填充 DXF 中取图案定义行（角度 基点 偏移 线型）"
  "定义行表"
  "(at-hatch:dxf-pattern-lines (entget (car (entsel))))"
  (setq lst (vl-remove-if-not
              (function (lambda (x) (member (car x) '(53 43 44 45 46 79 49))))
              d)
        res nil
        cur nil)
  (foreach it lst
    (if (= (car it) 53)
      (progn
        (if cur (setq res (append res (list (at-hatch:dxf-pat-line cur)))))
        (setq cur (list it)))
      (if cur (setq cur (append cur (list it))))))
  (if cur (setq res (append res (list (at-hatch:dxf-pat-line cur)))))
  (vl-remove nil res))

(defun at-hatch:dxf-pat-line (pairs / ang ox oy dx dy dashes it)
  "把一组图案线组码转成 (角度 基点 偏移 线型)"
  "定义行或 nil"
  "(at-hatch:dxf-pat-line '((53 . 0.0) (43 . 0.0) (44 . 0.0) (45 . 0.0) (46 . 0.5)))"
  (setq ang (at-hatch:r2d (cdr (assoc 53 pairs)))
        ox (at-hatch:nth-real (mapcar 'cdr pairs) 0)
        oy 0.0 dx 0.0 dy 0.0 dashes nil)
  (setq ox (if (cdr (assoc 43 pairs)) (cdr (assoc 43 pairs)) 0.0))
  (setq oy (if (cdr (assoc 44 pairs)) (cdr (assoc 44 pairs)) 0.0))
  (setq dx (if (cdr (assoc 45 pairs)) (cdr (assoc 45 pairs)) 0.0))
  (setq dy (if (cdr (assoc 46 pairs)) (cdr (assoc 46 pairs)) 0.0))
  (foreach it pairs
    (if (= (car it) 49) (setq dashes (append dashes (list (cdr it))))))
  (append (list ang ox oy dx dy) dashes))

;; ---------------------------------------------------------------------------
;; 几何辅助
;; ---------------------------------------------------------------------------

(defun at-hatch:point-in-loop (pt loop / inside py px n i a b x1 y1 x2 y2 xint)
  "射线法判断点是否在简单多边形内"
  "T 或 nil"
  "(at-hatch:point-in-loop '(5 5) '((0 0) (10 0) (10 10) (0 10)))"
  (if (and pt loop (> (length loop) 2))
    (progn
      (setq inside nil py (cadr pt) px (car pt) n (length loop) i 0)
      (while (< i n)
        (setq a (nth i loop)
              b (nth (rem (1+ i) n) loop)
              x1 (car a) y1 (cadr a)
              x2 (car b) y2 (cadr b))
        (if (and (/= y1 y2) (<= (min y1 y2) py) (< py (max y1 y2)))
          (progn
            (setq xint (+ x1 (* (- x2 x1) (/ (- py y1) (- y2 y1)))))
            (if (> xint px) (setq inside (not inside)))))
        (setq i (1+ i)))
      inside)
    nil))

(defun at-hatch:cross (a b c)
  "向量叉积 (b-a) x (c-a) 的 z 分量"
  "实数"
  "(at-hatch:cross '(0 0) '(1 0) '(0 1)) => 1.0"
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a)))))

(defun at-hatch:point-in-tri (p a b c)
  "点是否在三角形内（含边界）"
  "T 或 nil"
  "(at-hatch:point-in-tri '(0.2 0.2) '(0 0) '(1 0) '(0 1))"
  (and
    (>= (at-hatch:cross a b p) -1e-9)
    (>= (at-hatch:cross b c p) -1e-9)
    (>= (at-hatch:cross c a p) -1e-9)))

(defun at-hatch:list-remove-at (n lst / i res)
  "删除表中第 n 个元素"
  "列表"
  "(at-hatch:list-remove-at 1 '(1 2 3)) => (1 3)"
  (setq i 0 res nil)
  (foreach x lst
    (if (/= i n) (setq res (append res (list x))))
    (setq i (1+ i)))
  res)

(defun at-hatch:ear-contained (i1 i2 i3 pts idx / j p found)
  "三角形（索引 i1 i2 i3）内是否还有其它顶点"
  "T 或 nil"
  "(at-hatch:ear-contained 0 1 2 pts idx)"
  (setq j 0 found nil)
  (while (and (< j (length idx)) (not found))
    (setq p (nth j idx))
    (if (and (/= p i1) (/= p i2) (/= p i3)
             (at-hatch:point-in-tri (nth p pts) (nth i1 pts) (nth i2 pts) (nth i3 pts)))
      (setq found t))
    (setq j (1+ j)))
  found)

(defun at-hatch:ccw (pts / area i n p1 p2)
  "把多边形点表统一为逆时针"
  "点表"
  "(at-hatch:ccw '((0 0) (0 10) (10 10)))"
  (setq n (length pts) i 0 area 0.0)
  (while (< i n)
    (setq p1 (nth i pts) p2 (nth (rem (1+ i) n) pts))
    (setq area (+ area (- (* (car p1) (cadr p2)) (* (cadr p1) (car p2)))))
    (setq i (1+ i)))
  (if (< area 0.0) (reverse pts) pts))

(defun at-hatch:triangulate (pts / idx res n i p1 p2 p3 done)
  "耳切法把多边形三角化，返回顶点索引三元组表"
  "索引三元组表，如 ((0 1 2) (0 2 3))"
  "(at-hatch:triangulate '((0 0) (10 0) (10 10) (0 10)))"
  (if (and pts (> (length pts) 2))
    (progn
      (setq pts (at-hatch:ccw pts))
      (setq idx nil i 0)
      (while (< i (length pts))
        (setq idx (append idx (list i)))
        (setq i (1+ i)))
      (setq res nil)
      (while (> (length idx) 3)
        (setq n (length idx) i 0 done nil)
        (while (and (not done) (< i n))
          (setq p1 (nth (rem (+ i (1- n)) n) idx)
                p2 (nth i idx)
                p3 (nth (rem (1+ i) n) idx))
          (if (and (> (at-hatch:cross (nth p1 pts) (nth p2 pts) (nth p3 pts)) 0.0)
                   (not (at-hatch:ear-contained p1 p2 p3 pts idx)))
            (progn
              (setq res (append res (list (list p1 p2 p3))))
              (setq idx (at-hatch:list-remove-at i idx))
              (setq done t)))
          (setq i (1+ i)))
        (if (not done)
          (progn
            (setq res (append res (list (list (car idx) (cadr idx) (caddr idx)))))
            (setq idx (cddr idx)))))
      (if (= (length idx) 3)
        (setq res (append res (list (list (car idx) (cadr idx) (caddr idx))))))
      res)
    nil))

(defun at-hatch:circle-pts (ent / c r i pts ang n)
  "把圆近似为 64 段折线的点表"
  "点表"
  "(at-hatch:circle-pts (car (entsel)))"
  (setq c (cdr (assoc 10 (entget ent)))
        r (cdr (assoc 40 (entget ent)))
        n 64 i 0 pts nil)
  (while (< i n)
    (setq ang (* i (/ (* 2.0 pi) (float n))))
    (setq pts (append pts (list (list (+ (car c) (* r (cos ang)))
                                      (+ (cadr c) (* r (sin ang))) 0.0))))
    (setq i (1+ i)))
  pts)

(defun at-hatch:ellipse-pts (ent / d c mj ratio i n pts ang mx my)
  "把椭圆按 64 段折线近似，返回点表"
  "点表"
  "(at-hatch:ellipse-pts (car (entsel)))"
  (setq d (entget ent)
        c (at-hatch:3d (cdr (assoc 10 d)))
        mj (at-hatch:3d (cdr (assoc 11 d)))
        ratio (cdr (assoc 40 d))
        n 64 i 0 pts nil)
  (setq mx (- (car mj) (car c)) my (- (cadr mj) (cadr c)))
  (while (< i n)
    (setq ang (* 2.0 pi (/ (float i) n)))
    (setq pts (append pts (list (list (+ (car c) (* mx (cos ang))
                                          (* (- my) ratio (sin ang)))
                                      (+ (cadr c) (* my (cos ang))
                                          (* mx ratio (sin ang)))
                                      0.0))))
    (setq i (1+ i)))
  pts)

(defun at-hatch:sample-curve (ent n / obj p0 p1 i p pts)
  "按参数均匀采样曲线，返回点表"
  "点表"
  "(at-hatch:sample-curve (car (entsel)) 64)"
  (setq obj (vlax-ename->vla-object ent) pts nil)
  (setq p0 (vl-catch-all-apply 'vlax-curve-getstartparam (list obj)))
  (setq p1 (vl-catch-all-apply 'vlax-curve-getendparam (list obj)))
  (if (or (vl-catch-all-error-p p0) (vl-catch-all-error-p p1) (null p0) (null p1))
    nil
    (progn
      (setq i 0)
      (while (< i n)
        (setq p (vl-catch-all-apply
                  'vlax-curve-getpointatparam
                  (list obj (+ p0 (* (- p1 p0) (/ (float i) n))))))
        (if (not (vl-catch-all-error-p p))
          (if p (setq pts (append pts (list (at-hatch:3d p))))))
        (setq i (1+ i)))
      pts)))

(defun at-hatch:ent-loop (ent / d ty pts bulges n k)
  "提取闭合图元的点表与凸度表（多段线/圆/椭圆/样条/面域）"
  "(点表 凸度表) 或 nil"
  "(at-hatch:ent-loop (car (entsel)))"
  (if (at-hatch:valid-ent ent)
    (progn
      (setq d (entget ent) ty (cdr (assoc 0 d)) pts nil bulges nil)
      (cond
        ((= ty "LWPOLYLINE")
         (foreach it d
           (cond
             ((= (car it) 10) (setq pts (append pts (list (at-hatch:3d (cdr it))))))
             ((= (car it) 42) (setq bulges (append bulges (list (cdr it)))))))
         (if (< (length pts) 3)
           nil
           (progn
             (setq n (length pts) k (length bulges))
             (while (< k n)
               (setq bulges (append bulges (list 0.0)) k (1+ k)))
             (list pts bulges))))
        ((= ty "CIRCLE")
         (list (at-hatch:circle-pts ent) nil))
        ((= ty "ELLIPSE")
         (list (at-hatch:ellipse-pts ent) nil))
        ((wcmatch ty "POLYLINE,SPLINE,REGION")
         (setq pts (at-hatch:sample-curve ent 64))
         (if (> (length pts) 2) (list pts nil) nil))
        (t nil)))
    nil))

(defun at-hatch:closed-ent-pts (ent / lp)
  "提取闭合图元的顶点表（不闭合返回 nil）"
  "点表或 nil"
  "(at-hatch:closed-ent-pts (car (entsel)))"
  (setq lp (at-hatch:ent-loop ent))
  (if lp (car lp) nil))

(defun at-hatch:ent-seg (ent / d c r a1 a2 sweep)
  "取直线/圆弧的段记录 (起点 终点 凸度)"
  "段记录或 nil"
  "(at-hatch:ent-seg (car (entsel)))"
  (if (at-hatch:valid-ent ent)
    (progn
      (setq d (entget ent))
      (cond
        ((= (cdr (assoc 0 d)) "LINE")
         (list (at-hatch:3d (cdr (assoc 10 d))) (at-hatch:3d (cdr (assoc 11 d))) 0.0))
        ((= (cdr (assoc 0 d)) "ARC")
         (setq c (at-hatch:3d (cdr (assoc 10 d)))
               r (cdr (assoc 40 d))
               a1 (cdr (assoc 50 d))
               a2 (cdr (assoc 51 d))
               sweep (- a2 a1))
         (if (<= sweep 0.0) (setq sweep (+ sweep (* 2.0 pi))))
         (if (> sweep (- (* 2.0 pi) 1e-6)) (setq sweep (* 2.0 pi)))
         (list (list (+ (car c) (* r (cos a1))) (+ (cadr c) (* r (sin a1))) 0.0)
               (list (+ (car c) (* r (cos a2))) (+ (cadr c) (* r (sin a2))) 0.0)
               (at-hatch:tan (/ sweep 4.0))))
        (t nil)))
    nil))

(defun at-hatch:take-seg (segs pt tol / i found sg)
  "在段表中找到与点相接的段，返回 (段 . 剩余段表)"
  "cons 或 nil"
  "(at-hatch:take-seg segs '(0 0) 1e-3)"
  (setq i 0 found nil)
  (while (and (< i (length segs)) (not found))
    (setq sg (nth i segs))
    (if (or (< (at-hatch:dist (car sg) pt) tol)
            (< (at-hatch:dist (cadr sg) pt) tol))
      (setq found i))
    (setq i (1+ i)))
  (if found
    (cons (nth found segs) (at-hatch:list-remove-at found segs))
    nil))

(defun at-hatch:chain-loops (segs tol / res pts bulges sp ep bu lst sg closed found)
  "把首尾相接的直线/圆弧链成闭合环"
  "环记录表 ((1 点表 凸度表) ...)"
  "(at-hatch:chain-loops (list (list p1 p2 0.0) (list p2 p3 0.0) (list p3 p1 0.0)) 1e-3)"
  (setq res nil)
  (while segs
    (setq sg (car segs) segs (cdr segs))
    (setq sp (car sg) ep (cadr sg) bu (caddr sg))
    (setq pts (list sp) bulges (list bu) closed nil found t)
    (while (and (not closed) found)
      (if (< (at-hatch:dist ep sp) tol)
        (setq closed t)
        (progn
          (setq lst (at-hatch:take-seg segs ep tol))
          (if lst
            (progn
              (setq sg (car lst) segs (cdr lst))
              (if (< (at-hatch:dist (car sg) ep) tol)
                (setq ep (cadr sg) bu (caddr sg))
                (setq ep (car sg) bu (- (caddr sg))))
              (setq pts (append pts (list ep)))
              (setq bulges (append bulges (list bu))))
            (setq found nil)))))
    (if (and closed (> (length pts) 3))
      (setq res (append res (list (list 1 (reverse (cdr (reverse pts)))
                                         (reverse (cdr (reverse bulges))))))))
  res))

(defun at-hatch:ents->loops (ents / recs segs e lp sg)
  "把图元表转成环记录表（闭合图元直接取、散线圆弧自动链接）"
  "环记录表"
  "(at-hatch:ents->loops (pickset:to-list (ssget)))"
  (setq recs nil segs nil)
  (foreach e ents
    (setq lp (at-hatch:ent-loop e))
    (if lp
      (setq recs (append recs (list (list 1 (car lp) (cadr lp)))))
      (progn
        (setq sg (at-hatch:ent-seg e))
        (if sg (setq segs (append segs (list sg)))))))
  (if segs
    (setq recs (append recs (at-hatch:chain-loops segs (at-hatch:tol)))))
  recs)

(defun at-hatch:classify (recs / res i j depth p r)
  "按包含关系标记环为外边界(1)/内孔(0)"
  "环记录表"
  "(at-hatch:classify recs)"
  (setq res nil i 0)
  (while (< i (length recs))
    (setq r (nth i recs) depth 0 j 0)
    (setq p (car (cadr r)))
    (while (< j (length recs))
      (if (and (/= i j) (at-hatch:point-in-loop p (cadr (nth j recs))))
        (setq depth (1+ depth)))
      (setq j (1+ j)))
    (setq res (append res (list (list (if (= 0 (rem depth 2)) 1 0) (cadr r) (caddr r)))))
    (setq i (1+ i)))
  res)

(defun at-hatch:region-from-pt (pt / ent)
  "取点所在闭合围区的边界多段线（bpoly，必要时先缩放到该点）"
  "边界图元名或 nil"
  "(at-hatch:region-from-pt '(5 5))"
  (if pt
    (progn
      (setq ent (vl-catch-all-apply 'bpoly (list pt)))
      (if (or (vl-catch-all-error-p ent) (null ent))
        (progn
          ;; bpoly 要求区域在屏幕可见范围内，先缩放再重试
          (vl-catch-all-apply 'command (list "_.ZOOM" "_C" pt 200.0))
          (setq ent (vl-catch-all-apply 'bpoly (list pt)))
          (vl-catch-all-apply 'command (list "_.ZOOM" "_P"))
          (if (or (vl-catch-all-error-p ent) (null ent)) nil ent))
        ent))
    nil))

(defun at-hatch:make-pl (pts bulges layer closed / dxf i b)
  "由点表/凸度表创建多段线（entmake）"
  "多段线图元名或 nil"
  "(at-hatch:make-pl '((0 0) (10 0) (10 10)) '(0 0 0) nil 1)"
  (if (< (length pts) 3)
    nil
    (progn
      (setq dxf (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                      (cons 8 (if (and layer (tblsearch "LAYER" layer))
                                layer (getvar "CLAYER")))
                      '(100 . "AcDbPolyline")
                      (cons 90 (length pts))
                      (cons 70 (if (= closed 1) 1 0))))
      (setq i 0)
      (while (< i (length pts))
        (setq b (if (and bulges (> (length bulges) i)) (nth i bulges) 0.0))
        (setq dxf (append dxf (list (cons 10 (at-hatch:2d (nth i pts)))
                                    (cons 42 (if (numberp b) b 0.0)))))
        (setq i (1+ i)))
      (if (entmake (vl-remove nil dxf)) (entlast) nil))))

(defun at-hatch:loop->pl (rec layer)
  "把环记录转成闭合多段线"
  "多段线图元名或 nil"
  "(at-hatch:loop->pl (car (at-hatch:hatch-recs (car (entsel)))) nil)"
  (at-hatch:make-pl (cadr rec) (caddr rec) layer 1))

(defun at-hatch:rebuild-hatch (ent pattern scale angle color / recs en lay col)
  "按新图案重建填充（可嵌入 .pat 定义行，图案未载入也能显示）"
  "新填充图元名或 nil"
  "(at-hatch:rebuild-hatch (car (entsel)) \"PARQUET\" 10.0 0.0 nil)"
  (setq recs (at-hatch:hatch-recs ent))
  (if recs
    (progn
      (setq lay (cdr (assoc 8 (entget ent)))
            col (if (numberp color) color (cdr (assoc 62 (entget ent)))))
      (setq en (at-hatch:hatch-from-loops recs pattern scale angle lay col))
      (if en
        (progn (at-hatch:del-ent ent) en)
        nil))
    nil))

(defun at-hatch:set-hatch-pattern (ent pattern scale angle color /
                                   obj res defs)
  "设置填充的图案/比例/角度/颜色，返回结果填充图元名（图案未载入时重建填充）"
  "图元名或 nil"
  "(at-hatch:set-hatch-pattern (car (entsel)) \"ANSI31\" 1.0 45.0 7)"
  (if (not (at-hatch:valid-ent ent))
    nil
    (progn
      (setq obj (vlax-ename->vla-object ent) res ent)
      (if pattern
        (cond
          ((= (strcase pattern) "SOLID")
           (if (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-setpattern (list obj 1 "SOLID")))
             (setq res (at-hatch:rebuild-hatch ent pattern scale angle color))))
          ((at-hatch:std-pattern-p pattern)
           (if (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-setpattern (list obj 1 pattern)))
             (setq res (at-hatch:rebuild-hatch ent pattern scale angle color))))
          ((setq defs (at-hatch:pattern-defs pattern))
           (setq res (at-hatch:rebuild-hatch ent pattern scale angle color)))
          (t
           (if (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-setpattern (list obj 2 pattern)))
             (setq res (at-hatch:rebuild-hatch ent pattern scale angle color))))))
      (if (and res (at-hatch:valid-ent res))
        (progn
          (setq obj (vlax-ename->vla-object res))
          (if (and (numberp scale) (> scale 0.0))
            (vl-catch-all-apply 'vla-put-patternscale (list obj scale)))
          (if (numberp angle)
            (vl-catch-all-apply 'vla-put-patternangle (list obj (at-hatch:d2r angle))))
          (if (numberp color) (at-hatch:set-color res color))
          (vl-catch-all-apply 'vla-update (list obj))
          (entupd res)
          res)
        nil))))
