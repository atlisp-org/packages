;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 填充 (at-hatch) — 创建类命令
;; 快捷填充、超级填充、SOLID 变填充、SOLID 满铺多段线
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------------------------------------------------------
;; 交互输入辅助
;; ---------------------------------------------------------------------------

(defun at-hatch:get-pattern (default / s)
  "交互输入图案名，回车用默认值"
  "图案名字符串"
  "(at-hatch:get-pattern \"ANSI31\")"
  (setq s (getstring (strcat "\n填充图案名 <" default ">: ")))
  (if (= s "") default s))

(defun at-hatch:get-scale (default / r)
  "交互输入填充比例，回车用默认值"
  "实数"
  "(at-hatch:get-scale 1.0)"
  (setq r (getreal (strcat "\n填充比例 <" (at-hatch:num->str default) ">: ")))
  (if (numberp r) r default))

(defun at-hatch:get-angle (default / r)
  "交互输入填充角度（度），回车用默认值"
  "实数"
  "(at-hatch:get-angle 0.0)"
  (setq r (getreal (strcat "\n填充角度 <" (at-hatch:num->str default) ">: ")))
  (if (numberp r) r default))

(defun at-hatch:uniq-pts (pts / res p)
  "删除点表中相邻重复点及首尾重复点"
  "点表"
  "(at-hatch:uniq-pts '((0 0) (10 0) (10 10) (10 10)))"
  (setq res nil)
  (foreach p pts
    (if (or (null res) (> (at-hatch:dist (car (reverse res)) p) (at-hatch:tol)))
      (setq res (append res (list p)))))
  (if (and (> (length res) 2)
           (< (at-hatch:dist (car res) (car (reverse res))) (at-hatch:tol)))
    (setq res (reverse (cdr (reverse res)))))
  res)

;; ---------------------------------------------------------------------------
;; 边界输入归一
;; ---------------------------------------------------------------------------

(defun at-hatch:boundary->loops (boundary / recs be)
  "把边界输入（图元/图元表/选择集/点表/点表列表/内部点）归一成环记录表"
  "环记录表或 nil"
  "(at-hatch:boundary->loops (car (entsel)))"
  (setq recs nil)
  (cond
    ((eq (type boundary) 'pickset)
     (at-hatch:ents->loops (pickset:to-list boundary)))
    ((eq (type boundary) 'ename)
     (at-hatch:ents->loops (list boundary)))
    ((and (listp boundary) (numberp (car boundary)))
     (setq be (at-hatch:region-from-pt boundary))
     (if be
       (progn
         (setq recs (at-hatch:ents->loops (list be)))
         (at-hatch:del-ent be)
         recs)
       nil))
    ((and (listp boundary) (listp (car boundary)) (numberp (car (car boundary))))
     (list (list 1 (at-hatch:uniq-pts boundary) nil)))
    ((and (listp boundary) (listp (car boundary)) (listp (car (car boundary))))
     (foreach pl boundary
       (setq recs (append recs (list (list 1 (at-hatch:uniq-pts pl) nil)))))
     recs)
    ((listp boundary)
     (at-hatch:ents->loops (at-hatch:norm-ents boundary)))
    (t nil)))

;; ---------------------------------------------------------------------------
;; 1. 快捷填充
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-hatch (boundary pattern scale angle layer color / recs)
  "AI 入口：由边界创建填充，返回填充图元名。
boundary: 闭合图元/图元表/选择集/点表/点表列表/区域内部点；pattern 图案名；
scale/angle 可 nil；layer/color 可 nil（随当前图层/随层）"
  "填充图元名或 nil"
  "(at-hatch:ai-hatch (car (entsel)) \"ANSI31\" 1.0 45.0 nil 7)"
  (setq recs (at-hatch:classify (at-hatch:boundary->loops boundary)))
  (if recs
    (at-hatch:hatch-from-loops
      recs
      (if pattern pattern "ANSI31")
      (if (numberp scale) scale 1.0)
      (if (numberp angle) angle 0.0)
      layer color)
    (progn (princ "\n未能取得有效闭合边界。") nil)))

(defun at-hatch:quick-hatch (/ ss ents pt be pattern scale angle res)
  "快捷填充：选边界（实体或内部点）后指定图案/比例/角度生成填充"
  "填充图元名或 nil"
  "(at-hatch:quick-hatch)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE,SPLINE,LINE,ARC"))))
  (cond
    (ss (setq ents (pickset:to-list ss)))
    (t (setq pt (getpoint "\n点取封闭区域内部一点: "))))
  (if pt
    (progn
      (setq be (at-hatch:region-from-pt pt))
      (if be
        (progn
          (setq ents (list be))
          (setq res (at-hatch:ai-hatch ents (at-hatch:get-pattern "ANSI31") nil nil nil nil))
          (at-hatch:del-ent be))
        (setq res nil)))
    (if ents
      (progn
        (setq pattern (at-hatch:get-pattern "ANSI31"))
        (setq scale (at-hatch:get-scale 1.0))
        (setq angle (at-hatch:get-angle 0.0))
        (setq res (at-hatch:ai-hatch ents pattern scale angle nil nil)))
      (setq res nil)))
  (at-hatch:undo-end)
  (if res
    (progn (princ "\n已生成填充。") res)
    (progn (princ "\n未生成填充。") nil)))

;; ---------------------------------------------------------------------------
;; 2. 超级填充
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-superhatch (boundary pattern scale / recs res i j r holes h)
  "AI 入口：每个封闭区域各生成一个填充，区域内孔洞自动作内边界。
boundary: 图元表/选择集/闭合图元；pattern 图案名；scale 比例（可 nil）"
  "填充图元名表或 nil"
  "(at-hatch:ai-superhatch (pickset:to-list (ssget)) \"ANSI31\" 1.0)"
  (setq recs (at-hatch:classify (at-hatch:boundary->loops boundary))
        res nil i 0)
  (while (< i (length recs))
    (setq r (nth i recs))
    (if (= (car r) 1)
      (progn
        (setq holes nil j 0)
        (while (< j (length recs))
          (if (and (/= i j)
                   (= (car (nth j recs)) 0)
                   (at-hatch:point-in-loop (car (cadr (nth j recs))) (cadr r)))
            (setq holes (append holes (list (nth j recs)))))
          (setq j (1+ j)))
        (setq h (at-hatch:hatch-from-loops
                  (append (list r) holes)
                  (if pattern pattern "ANSI31")
                  (if (numberp scale) scale 1.0)
                  0.0 nil nil))
        (if h (setq res (append res (list h))))))
    (setq i (1+ i)))
  (if res
    res
    (progn (princ "\n未能取得有效闭合区域。") nil)))

(defun at-hatch:superhatch (/ ss pattern scale res)
  "超级填充：封闭区域按图案平铺充满，区域内孔洞作内边界处理"
  "填充图元名表或 nil"
  "(at-hatch:superhatch)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE,SPLINE,REGION,LINE,ARC"))))
  (if (null ss)
    (progn (princ "\n未选择边界。") (setq res nil))
    (progn
      (setq pattern (at-hatch:get-pattern "ANSI31"))
      (setq scale (at-hatch:get-scale 1.0))
      (setq res (at-hatch:ai-superhatch (pickset:to-list ss) pattern scale))))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已生成 " (itoa (length res)) " 个填充。")) res)
    (progn (princ "\n未生成填充。") nil)))

;; ---------------------------------------------------------------------------
;; 8. SOLID 变填充
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-solid2hatch (solids pattern scale / ents e d pts h res)
  "AI 入口：把 SOLID 实体转成图案填充（保留图层与颜色）"
  "填充图元名表或 nil"
  "(at-hatch:ai-solid2hatch (pickset:to-list (ssget '((0 . \"SOLID\")))) \"ANSI31\" 1.0)"
  (setq ents (at-hatch:norm-ents solids) res nil)
  (foreach e ents
    (setq d (entget e))
    (if (= (cdr (assoc 0 d)) "SOLID")
      (progn
        (setq pts (at-hatch:uniq-pts
                    (list (cdr (assoc 10 d)) (cdr (assoc 11 d))
                          (cdr (assoc 13 d)) (cdr (assoc 12 d)))))
        (setq h (at-hatch:make-hatch
                  (list (list 1 pts nil))
                  (if pattern pattern "ANSI31")
                  (if (numberp scale) scale 1.0)
                  0.0
                  (cdr (assoc 8 d))
                  (cdr (assoc 62 d))
                  (at-hatch:pattern-defs pattern)))
        (if h
          (progn
            (setq res (append res (list h)))
            (at-hatch:del-ent e))))))
  (if res
    res
    (progn (princ "\n未找到可转换的 SOLID。") nil)))

(defun at-hatch:solid2hatch (/ ss pattern scale res)
  "SOLID 变填充：把所选 SOLID 实体转换为图案填充"
  "填充图元名表或 nil"
  "(at-hatch:solid2hatch)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "SOLID"))))
  (if (null ss)
    (progn (princ "\n未选择 SOLID。") (setq res nil))
    (progn
      (setq pattern (at-hatch:get-pattern "ANSI31"))
      (setq scale (at-hatch:get-scale 1.0))
      (setq res (at-hatch:ai-solid2hatch (pickset:to-list ss) pattern scale))))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已转换 " (itoa (length res)) " 个 SOLID。")) res)
    (progn (princ "\n未转换。") nil)))

;; ---------------------------------------------------------------------------
;; 9. SOLID 满铺多段线
;; ---------------------------------------------------------------------------

(defun at-hatch:solid-dxf (p1 p2 p3 layer color)
  "构建三角 SOLID 的 DXF 组码（第 4 点与第 3 点重合）"
  "组码对表"
  "(at-hatch:solid-dxf '(0 0) '(10 0) '(0 10) nil nil)"
  (list '(0 . "SOLID") '(100 . "AcDbEntity")
        (cons 8 (if (and layer (tblsearch "LAYER" layer)) layer (getvar "CLAYER")))
        (if (and (numberp color) (> color 0) (< color 256)) (cons 62 color))
        '(100 . "AcDbTrace")
        (cons 10 (at-hatch:3d p1))
        (cons 11 (at-hatch:3d p2))
        (cons 12 (at-hatch:3d p3))
        (cons 13 (at-hatch:3d p3))))

(defun at-hatch:ai-pline2solid (pline-ent / lp pts d tri p1 p2 p3 lay col res)
  "AI 入口：把闭合多段线范围满铺 SOLID（耳切三角化，弧段按弦处理）
保留原多段线与图层颜色，返回 SOLID 图元名表"
  "SOLID 图元名表或 nil"
  "(at-hatch:ai-pline2solid (car (entsel)))"
  (setq lp (at-hatch:ent-loop pline-ent))
  (if (null lp)
    (progn (princ "\n未取得闭合边界（需闭合多段线/圆/椭圆/样条）。") nil)
    (progn
      (setq pts (car lp)
            d (entget pline-ent)
            lay (cdr (assoc 8 d))
            col (cdr (assoc 62 d))
            tri (at-hatch:triangulate pts)
            res nil)
      (foreach tr tri
        (setq p1 (nth (car tr) pts)
              p2 (nth (cadr tr) pts)
              p3 (nth (caddr tr) pts))
        (if (entmake (vl-remove nil (at-hatch:solid-dxf p1 p2 p3 lay col)))
          (setq res (append res (list (entlast))))))
      (if res
        res
        (progn (princ "\n三角化失败。") nil)))))

(defun at-hatch:pline2solid (/ ss ents e res del ans)
  "SOLID 满铺多段线：把闭合多段线范围铺满 SOLID"
  "SOLID 图元名表或 nil"
  "(at-hatch:pline2solid)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE,SPLINE"))))
  (if (null ss)
    (progn (princ "\n未选择闭合边界。") (setq res nil))
    (progn
      (setq ents (pickset:to-list ss))
      (foreach e ents
        (setq res (append res (at-hatch:ai-pline2solid e))))
      (if res
        (progn
          (setq ans (getstring "\n删除原边界线? [是(Y)/否(N)] <N>: "))
          (if (or (= (strcase ans) "Y") (= (strcase ans) "YES"))
            (foreach e ents (at-hatch:del-ent e)))))))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已生成 " (itoa (length res)) " 个 SOLID。")) res)
    (progn (princ "\n未生成 SOLID。") nil)))
