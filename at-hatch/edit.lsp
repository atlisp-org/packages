;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 填充 (at-hatch) — 编辑类命令
;; 选现有填充改参数、填充平移/旋转/缩放、由填充生成边界、
;; 批量分离填充、全能裁刀裁填充、修复填充显示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun at-hatch:hatch-center (ent / recs pts p)
  "填充所有边界点的包围盒中心"
  "三维点或 nil"
  "(at-hatch:hatch-center (car (entsel)))"
  (setq recs (at-hatch:hatch-recs ent))
  (foreach r recs
    (setq pts (append pts (cadr r))))
  (if pts (at-hatch:bbox-center pts) nil))

;; ---------------------------------------------------------------------------
;; 3. 选现有填充改参数
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-hatch-select (hatch-ents pattern scale angle / ents e en res)
  "AI 入口：修改已有填充的图案/比例/角度，返回成功修改的填充表。
pattern/scale/angle 为 nil 表示保持原值"
  "填充图元名表或 nil"
  "(at-hatch:ai-hatch-select (car (entsel)) \"ANSI31\" 2.0 45.0)"
  (setq ents (at-hatch:norm-ents hatch-ents) res nil)
  (foreach e ents
    (if (= (cdr (assoc 0 (entget e))) "HATCH")
      (setq en (at-hatch:set-hatch-pattern e pattern scale angle nil))
      (setq en nil))
    (if en (setq res (append res (list en)))))
  (if res
    res
    (progn (princ "\n未修改任何填充（图案名可能无法识别）。") nil)))

(defun at-hatch:hatch-select (/ ss ents e info pattern scale angle res ans)
  "选现有填充改参数：选填充后重新指定图案/比例/角度"
  "填充图元名表或 nil"
  "(at-hatch:hatch-select)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "HATCH"))))
  (if (null ss)
    (progn (princ "\n未选择填充。") (setq res nil))
    (progn
      (setq ents (pickset:to-list ss))
      (setq info (at-hatch:hatch-pattern-info (car ents)))
      (setq pattern (getstring (strcat "\n图案名 <" (if (car info) (car info) "保持") ">: ")))
      (if (= pattern "") (setq pattern nil))
      (setq scale (getreal (strcat "\n比例 <"
                                   (if (numberp (caddr info))
                                     (at-hatch:num->str (caddr info)) "保持")
                                   ">: ")))
      (setq angle (getreal (strcat "\n角度 <"
                                   (if (numberp (nth 3 info))
                                     (at-hatch:num->str (nth 3 info)) "保持")
                                   ">: ")))
      (setq res (at-hatch:ai-hatch-select ents pattern scale angle))))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已修改 " (itoa (length res)) " 个填充。")) res)
    (progn (princ "\n未修改。") nil)))

;; ---------------------------------------------------------------------------
;; 4. 填充平移/旋转/缩放
;; ---------------------------------------------------------------------------

(defun at-hatch:trans-vec (param / p1 p2)
  "把位移参数解析成三维位移向量（支持位移值或两点）"
  "三维点"
  "(at-hatch:trans-vec '((0 0) (10 5)))"
  (if (numberp (car param))
    (at-hatch:3d param)
    (progn
      (setq p1 (car param) p2 (cadr param))
      (if (and (listp p1) (listp p2))
        (list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) 0.0)
        nil))))

(defun at-hatch:base-arg (param ent / p)
  "解析 (基点 值) 或 值 形式的参数，返回 (基点 . 值)"
  "cons，基点可能为 nil"
  "(at-hatch:base-arg '((0 0) 45.0) (car (entsel)))"
  (if (listp (car param))
    (cons (at-hatch:3d (car param)) (cadr param))
    (progn
      (setq p (at-hatch:hatch-center ent))
      (cons p (car param)))))

(defun at-hatch:ai-hatch-move (hatch-ents op param / ents e obj base ang vec res ba)
  "AI 入口：填充平移/旋转/缩放。
op: move 参数为位移 (dx dy) 或两点；rotate 参数为角度(度) 或 (基点 角度)；
scale 参数为比例 或 (基点 比例)"
  "处理后的填充图元名表或 nil"
  "(at-hatch:ai-hatch-move (car (entsel)) \"rotate\" '((0 0) 45.0))"
  (setq ents (at-hatch:norm-ents hatch-ents) res nil)
  (foreach e ents
    (if (= (cdr (assoc 0 (entget e))) "HATCH")
      (progn
        (setq obj (vlax-ename->vla-object e) ba nil)
        (cond
          ((= (strcase (if op op "")) "MOVE")
           (setq vec (at-hatch:trans-vec param))
           (if vec
             (setq ba (not (vl-catch-all-error-p
                             (vl-catch-all-apply
                               'vla-move
                               (list obj (vlax-3d-point '(0.0 0.0 0.0))
                                     (vlax-3d-point vec))))))))
          ((= (strcase (if op op "")) "ROTATE")
           (setq ba (at-hatch:base-arg param e))
           (if (and (car ba) (numberp (cdr ba)))
             (setq ba (not (vl-catch-all-error-p
                             (vl-catch-all-apply
                               'vla-rotate
                               (list obj (vlax-3d-point (car ba))
                                     (at-hatch:d2r (cdr ba)))))))
             (setq ba nil)))
          ((= (strcase (if op op "")) "SCALE")
           (setq ba (at-hatch:base-arg param e))
           (if (and (car ba) (numberp (cdr ba)) (> (cdr ba) 0.0))
             (setq ba (not (vl-catch-all-error-p
                             (vl-catch-all-apply
                               'vla-scaleentity
                               (list obj (vlax-3d-point (car ba)) (float (cdr ba)))))))
             (setq ba nil)))
          (t (setq ba nil)))
        (if ba (setq res (append res (list e)))))))
  (if res
    res
    (progn (princ "\n操作失败（op 应为 move/rotate/scale）。") nil)))

(defun at-hatch:hatch-move (/ ss ents op res p p2 ang f)
  "填充移动对齐：选填充后平移、旋转或缩放"
  "填充图元名表或 nil"
  "(at-hatch:hatch-move)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "HATCH"))))
  (if (null ss)
    (progn (princ "\n未选择填充。") (setq res nil))
    (progn
      (setq ents (pickset:to-list ss))
      (initget "M R S")
      (setq op (getkword "\n操作方式 [移动(M)/旋转(R)/缩放(S)] <移动>: "))
      (cond
        ((= op "R")
         (setq p (getpoint "\n旋转基点: "))
         (setq ang (getreal "\n旋转角度(度): "))
         (if (and p ang) (setq res (at-hatch:ai-hatch-move ents "rotate" (list p ang)))))
        ((= op "S")
         (setq p (getpoint "\n缩放基点: "))
         (setq f (getreal "\n缩放比例: "))
         (if (and p f) (setq res (at-hatch:ai-hatch-move ents "scale" (list p f)))))
        (t
         (setq p (getpoint "\n位移起点: "))
         (setq p2 (getpoint p "\n位移终点: "))
         (if (and p p2)
           (setq res (at-hatch:ai-hatch-move ents "move" (list p p2))))))))
  (at-hatch:undo-end)
  (if res
    (progn (princ "\n已处理填充。") res)
    (progn (princ "\n未处理。") nil)))

;; ---------------------------------------------------------------------------
;; 5. 由填充生成边界
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-hatch-boundary (hatch-ents layer / ents e recs r pl res)
  "AI 入口：由填充边界生成闭合多段线，返回多段线图元表。
layer 为 nil 时用当前图层"
  "多段线图元名表或 nil"
  "(at-hatch:ai-hatch-boundary (car (entsel)) nil)"
  (setq ents (at-hatch:norm-ents hatch-ents) res nil)
  (foreach e ents
    (if (= (cdr (assoc 0 (entget e))) "HATCH")
      (progn
        (setq recs (at-hatch:hatch-recs e))
        (foreach r recs
          (setq pl (at-hatch:loop->pl r layer))
          (if pl (setq res (append res (list pl))))))))
  (if res
    res
    (progn (princ "\n未取得填充边界。") nil)))

(defun at-hatch:hatch-boundary (/ ss res)
  "填充生成边界：由所选填充的边界环生成闭合多段线"
  "多段线图元名表或 nil"
  "(at-hatch:hatch-boundary)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "HATCH"))))
  (if ss
    (setq res (at-hatch:ai-hatch-boundary (pickset:to-list ss) nil))
    (progn (princ "\n未选择填充。") (setq res nil)))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已生成 " (itoa (length res)) " 条边界。")) res)
    (progn (princ "\n未生成边界。") nil)))

;; ---------------------------------------------------------------------------
;; 6. 批量分离填充
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-hatch-isolate (hatch-ents / ents e info recs r h res ok lay col)
  "AI 入口：把多环填充拆成每环一个独立填充，返回新填充表"
  "填充图元名表或 nil"
  "(at-hatch:ai-hatch-isolate (car (entsel)))"
  (setq ents (at-hatch:norm-ents hatch-ents) res nil)
  (foreach e ents
    (if (= (cdr (assoc 0 (entget e))) "HATCH")
      (progn
        (setq info (at-hatch:hatch-pattern-info e)
              lay (cdr (assoc 8 (entget e)))
              col (cdr (assoc 62 (entget e)))
              recs (at-hatch:hatch-recs e)
              ok t)
        (foreach r recs
          (setq h (at-hatch:make-hatch (list (list 1 (cadr r) (caddr r)))
                                       (car info) (caddr info) (nth 3 info)
                                       lay col (nth 4 info)))
          (if h
            (setq res (append res (list h)))
            (setq ok nil)))
        (if (and ok res) (at-hatch:del-ent e)))))
  (if res
    res
    (progn (princ "\n未分离出填充。") nil)))

(defun at-hatch:hatch-isolate (/ ss res)
  "批量分离填充：把多环填充拆成每环一个独立填充"
  "填充图元名表或 nil"
  "(at-hatch:hatch-isolate)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "HATCH"))))
  (if ss
    (setq res (at-hatch:ai-hatch-isolate (pickset:to-list ss)))
    (progn (princ "\n未选择填充。") (setq res nil)))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已分离出 " (itoa (length res)) " 个填充。")) res)
    (progn (princ "\n未分离。") nil)))

;; ---------------------------------------------------------------------------
;; 面域辅助（裁切用）
;; ---------------------------------------------------------------------------

(defun at-hatch:ms ()
  "取模型空间 VLA 对象"
  "VLA 对象"
  "(at-hatch:ms)"
  (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))

(defun at-hatch:pls->regions (pls / objs r1 r2)
  "由多段线图元表创建面域，返回面域 VLA 对象表"
  "VLA 对象表或 nil"
  "(at-hatch:pls->regions (list (car (entsel))))"
  (setq objs (mapcar (function (lambda (e) (vlax-ename->vla-object e))) pls))
  (setq r1 (vl-catch-all-apply 'vlax-invoke (list (at-hatch:ms) 'AddRegion objs)))
  (cond
    ((vl-catch-all-error-p r1) nil)
    ((listp r1)
     (setq r2 nil)
     (foreach o r1 (if (= (type o) 'VLA-OBJECT) (setq r2 (append r2 (list o)))))
     r2)
    ((= (type r1) 'VLA-OBJECT) (list r1))
    (t nil)))

(defun at-hatch:region-bool (acc op other / r)
  "面域布尔运算，成功返回 T（结果留在 acc）；op: 0 并集 1 交集 2 差集"
  "T 或 nil"
  "(at-hatch:region-bool acc 1 other)"
  (setq r (vl-catch-all-apply 'vla-boolean (list acc op other)))
  (not (vl-catch-all-error-p r)))

(defun at-hatch:del-vla (objs / o)
  "删除 VLA 对象表"
  "T 或 nil"
  "(at-hatch:del-vla objs)"
  (foreach o objs
    (vl-catch-all-apply 'vla-delete (list o)))
  t)

(defun at-hatch:hatch-from-region (reg pattern scale angle layer color /
                                   vh ap ent)
  "由面域创建填充（面域轮廓直接作填充边界，支持任意布尔结果形状）"
  "填充图元名或 nil"
  "(at-hatch:hatch-from-region reg \"ANSI31\" 1.0 0.0 nil nil)"
  (if (null reg)
    nil
    (progn
      (setq vh (vl-catch-all-apply
                 'vla-addhatch
                 (list (at-hatch:ms) 0 "ANSI31" :vlax-false)))
      (if (vl-catch-all-error-p vh)
        nil
        (progn
          (setq ap (vl-catch-all-apply
                     'vla-appendouterloop
                     (list vh (vlax-make-variant
                                (vlax-safearray-fill
                                  (vlax-make-safearray vlax-vbobject '(0 . 0))
                                  (list reg))))))
          (if (vl-catch-all-error-p ap)
            (progn (vl-catch-all-apply 'vla-delete (list vh)) nil)
            (progn
              (setq ent (vlax-vla-object->ename vh))
              (if (and (numberp scale) (> scale 0.0))
                (vl-catch-all-apply 'vla-put-patternscale (list vh scale)))
              (if (numberp angle)
                (vl-catch-all-apply 'vla-put-patternangle
                                    (list vh (at-hatch:d2r angle))))
              (if (and layer (tblsearch "LAYER" layer))
                (vl-catch-all-apply 'vla-put-layer (list vh layer)))
              (if (and (numberp color) (> color 0) (< color 256))
                (vl-catch-all-apply 'vla-put-color (list vh color)))
              (entupd ent)
              ent)))))))

;; ---------------------------------------------------------------------------
;; 7. 全能裁刀裁填充
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-clip (hatch-ent clip-boundary inside /
                         recs info lay col pls regs clip-pls clip-regs
                         acc op i h res)
  "AI 入口：用剪裁边界裁切填充（面域布尔运算）。
inside 非 nil 保留边界内部，nil 保留外部；返回新填充图元名或 nil"
  "填充图元名或 nil"
  "(at-hatch:ai-clip (car (entsel)) (car (entsel)) t)"
  (if (null (at-hatch:valid-ent hatch-ent))
    (progn (princ "\n填充无效。") nil)
    (progn
      (setq info (at-hatch:hatch-pattern-info hatch-ent))
      (setq lay (cdr (assoc 8 (entget hatch-ent))))
      (setq col (cdr (assoc 62 (entget hatch-ent))))
      (setq recs (at-hatch:hatch-recs hatch-ent))
      ;; 填充各环 → 多段线 → 面域
      (foreach r recs
        (setq pls (append pls (list (at-hatch:loop->pl r nil)))))
      (setq pls (vl-remove nil pls))
      (setq regs (at-hatch:pls->regions pls))
      ;; 剪裁边界 → 多段线 → 面域
      (foreach r (at-hatch:boundary->loops clip-boundary)
        (setq clip-pls (append clip-pls (list (at-hatch:loop->pl r nil)))))
      (setq clip-pls (vl-remove nil clip-pls))
      (setq clip-regs (at-hatch:pls->regions clip-pls))
      (if (and regs clip-regs)
        (progn
          (setq acc (car regs))
          ;; 填充内部孔洞先差掉
          (setq i 1)
          (while (< i (length regs))
            (at-hatch:region-bool acc 2 (nth i regs))
            (setq i (1+ i)))
          ;; 多个剪裁边界先合并
          (setq i 1)
          (while (< i (length clip-regs))
            (at-hatch:region-bool (car clip-regs) 0 (nth i clip-regs))
            (setq i (1+ i)))
          (setq op (if inside 1 2))
          (if (at-hatch:region-bool acc op (car clip-regs))
            (progn
              (setq h (at-hatch:hatch-from-region acc nil 1.0 0.0 lay nil))
              (if h
                (progn
                  (setq h (at-hatch:set-hatch-pattern
                            h (car info) (caddr info) (nth 3 info) col))
                  (if h
                    (progn (at-hatch:del-ent hatch-ent) (setq res h)))))))))
      ;; 清理临时对象
      (at-hatch:del-vla regs)
      (at-hatch:del-vla clip-regs)
      (foreach i pls (at-hatch:del-ent i))
      (foreach i clip-pls (at-hatch:del-ent i))
      (if res
        res
        (progn (princ "\n裁切失败（边界需闭合且与填充相交）。") nil)))))

(defun at-hatch:clip (/ hss css h res ans)
  "全能裁刀裁填充：先用剪裁边界裁切填充，可选保留内/外部"
  "填充图元名或 nil"
  "(at-hatch:clip)"
  (at-hatch:undo-start)
  (setq hss (ssget '((0 . "HATCH"))))
  (if (null hss)
    (progn (princ "\n未选择填充。") (setq res nil))
    (progn
      (setq h (ssname hss 0))
      (setq css (ssget "\n选择剪裁边界(闭合多段线/圆/椭圆/填充): "
                      '((0 . "LWPOLYLINE,CIRCLE,ELLIPSE,SPLINE,REGION,HATCH"))))
      (if (null css)
        (progn (princ "\n未选择剪裁边界。") (setq res nil))
        (progn
          (setq ans (getstring "\n保留边界内部? [是(Y)/否(N)] <Y>: "))
          (setq res (at-hatch:ai-clip h (ssname css 0)
                                     (not (or (= (strcase ans) "N")
                                              (= (strcase ans) "NO")))))))))
  (at-hatch:undo-end)
  (if res
    (progn (princ "\n已裁切填充。") res)
    (progn (princ "\n未裁切。") nil)))

;; ---------------------------------------------------------------------------
;; 12. 修复填充显示
;; ---------------------------------------------------------------------------

(defun at-hatch:fix-one (ent min-scale / d nm solid sc ang pdefs nloops obj fixsc res)
  "修复单个填充的显示问题，返回修复后的图元名（未改动返回 nil）"
  "图元名或 nil"
  "(at-hatch:fix-one (car (entsel)) 1.0)"
  (setq res nil)
  (if (and (at-hatch:valid-ent ent)
           (= (cdr (assoc 0 (entget ent))) "HATCH"))
    (progn
      (setq d (entget ent)
            nm (cdr (assoc 2 d))
            solid (= 1 (cdr (assoc 70 d)))
            sc (cdr (assoc 41 d))
            ang (at-hatch:r2d (cdr (assoc 52 d)))
            pdefs (at-hatch:dxf-pattern-lines d)
            nloops (cdr (assoc 91 d))
            obj (vlax-ename->vla-object ent))
      (cond
        ((or (null nloops) (< nloops 1))
         (princ "\n跳过一个无边界环的填充。"))
        (t
         ;; 比例为零/负数 → 用 min-scale 修正
         (if (or (null sc) (not (numberp sc)) (<= sc 0.0))
           (progn
             (setq fixsc (if (and (numberp min-scale) (> min-scale 0.0))
                           min-scale 1.0))
             (vl-catch-all-apply 'vla-put-patternscale (list obj fixsc))
             (setq res ent)))
         ;; 非实体填充时检查图案
         (if (not solid)
           (cond
             ;; 图案名空 → 改实体填充
             ((or (null nm) (= nm ""))
              (if (not (vl-catch-all-error-p
                         (vl-catch-all-apply 'vla-setpattern
                                             (list obj 1 "SOLID"))))
                (setq res ent)
                (setq res (at-hatch:rebuild-hatch ent "SOLID" sc ang nil))))
             ;; 缺少图案定义行且非标准图案 → 补定义行
             ((null pdefs)
              (cond
                ((at-hatch:std-pattern-p nm) nil)
                ((at-hatch:pattern-defs nm)
                 (setq res (at-hatch:rebuild-hatch ent nm sc ang nil)))
                (t
                 (princ (strcat "\n图案 " nm " 未载入，改为 ANSI31。"))
                 (setq res (at-hatch:set-hatch-pattern ent "ANSI31" sc ang nil)))))
             (t nil)))
         (if (and res (at-hatch:valid-ent res)) (entupd res)))))
    nil)
  res)

(defun at-hatch:ai-fix-pattern (hatch-ents min-scale / ents e en res)
  "AI 入口：修复填充不显示（未载入图案补定义行、比例为零改比例、空图案改实体）
min-scale 为修正比例（默认 1.0），返回修复成功的填充表"
  "填充图元名表或 nil"
  "(at-hatch:ai-fix-pattern (car (entsel)) 1.0)"
  (setq ents (at-hatch:norm-ents hatch-ents) res nil)
  (foreach e ents
    (setq en (at-hatch:fix-one e min-scale))
    (if en (setq res (append res (list en)))))
  (if res
    res
    (progn (princ "\n没有需要修复的填充。") nil)))

(defun at-hatch:fix-pattern (/ ss res)
  "修复填充显示：修复图案未载入、比例为零、图案名空等导致不显示的问题"
  "填充图元名表或 nil"
  "(at-hatch:fix-pattern)"
  (at-hatch:undo-start)
  (setq ss (ssget '((0 . "HATCH"))))
  (if ss
    (setq res (at-hatch:ai-fix-pattern (pickset:to-list ss) 1.0))
    (progn (princ "\n未选择填充。") (setq res nil)))
  (at-hatch:undo-end)
  (if res
    (progn (princ (strcat "\n已修复 " (itoa (length res)) " 个填充。")) res)
    (progn (princ "\n无需修复。") nil)))
