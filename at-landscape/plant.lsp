;; ============================================================
;; at-landscape/plant.lsp
;; 灌木边缘线 / 立面草 / 苗木标注 / 苗木统计表 / 变线为树
;; ============================================================
(require '(entity:* curve:* pickset:* block:* table:*))

;; ------------------------------------------------------------
;; 1. 灌木边缘线
;; ------------------------------------------------------------
(defun at-landscape:do-shrub-edge
  (ent dist amp / e len c n cnt i d p u nl v off res wavelength phase)
  "核心：在多段线外侧生成灌木边缘线（不规则波浪线）。"
  "边缘线 ename，失败返回 nil"
  "(at-landscape:do-shrub-edge (car (entsel)) 100.0 30.0)"
  (if (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE")
      (progn
        (setq e (e2o ent))
        (setq len (curve:length ent))
        (if (> len 1e-6)
            (progn
              (setq c (at-landscape:centroid (at-landscape:pts ent)))
              (setq n (max 16.0 (min 400.0 (float (/ (fix len) 30)))))
              (setq cnt (if (at-landscape:closed-p ent) n (1+ n)))
              (setq wavelength (max 100.0 (/ len 8.0)))
              (setq phase (* 2.0 pi (at-landscape:rand)))
              (setq res nil)
              (setq i 0)
              (while (< i cnt)
                (setq d (* len (/ (float i) cnt)))
                (setq p (at-landscape:2d
                         (vlax-curve-getpointatdist e d)))
                (setq u (at-landscape:unit
                         (at-landscape:tangent ent d)))
                (setq nl (list (- (cadr u)) (car u)))
                ;; 封闭线：取背离形心的一侧作为外侧
                (if (at-landscape:closed-p ent)
                    (progn
                      (setq v (list (- (car c) (car p))
                                    (- (cadr c) (cadr p))))
                      (if (> (+ (* (car nl) (car v))
                                (* (cadr nl) (cadr v)))
                             0.0)
                          (setq nl (list (- (car nl))
                                         (- (cadr nl)))))))
                (setq off
                      (* amp
                         (+ 0.55
                            (* 0.45
                             (sin (+ (* 2.0 pi (/ d wavelength)) phase)))
                            (* 0.35 (- (at-landscape:rand) 0.5)))))
                (setq res
                      (cons (list (+ (car p) (* off (car nl)))
                                  (+ (cadr p) (* off (cadr nl))))
                            res))
                (setq i (1+ i)))
              (at-landscape:pl (reverse res)
                                 (if (at-landscape:closed-p ent) 1 0)))
            (progn
              (princ "at-landscape:do-shrub-edge 多段线过短")
              nil)))
      (progn
        (princ "at-landscape:do-shrub-edge 必须选择多段线")
        nil)))

(defun at-landscape:shrub-edge (/ ent dist amp res)
  "多段线变灌木边缘线：在边界外侧生成不规则波浪线，模拟灌木丛轮廓。"
  "生成的边缘线 ename"
  "(at-landscape:shrub-edge)"
  (if (and (setq ent (cadr (entsel "\n请选择多段线:")))
           (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE"))
      (progn
        (setq dist (getdist "\n偏距 <100>:"))
        (if (null dist) (setq dist 100.0))
        (setq amp (getdist "\n波幅 <30>:"))
        (if (null amp) (setq amp 30.0))
        (startundo)
        (setq res (at-landscape:do-shrub-edge ent dist amp))
        (endundo)
        (if res
            (princ "\n已生成灌木边缘线。")
            (princ "\n灌木边缘线生成失败。")))
      (princ "\n请选择多段线。"))
  (princ))

(defun at-landscape:ai-shrub-edge (pline-ent dist amp / res)
  "AI 调用入口：在多段线外侧生成灌木边缘线（不规则波浪线）。
  pline-ent: 多段线 ename；dist: 偏距（缺省 100）；amp: 波幅（缺省 30）。"
  "边缘线 ename，失败返回 nil"
  "(at-landscape:ai-shrub-edge (car (entsel)) 100 30)"
  (if (and pline-ent
           (wcmatch (entity:getdxf pline-ent 0) "LWPOLYLINE,POLYLINE"))
      (setq res (at-landscape:do-shrub-edge
                 pline-ent
                 (if (numberp dist) dist 100.0)
                 (if (numberp amp) amp 30.0)))
      (princ "at-landscape:ai-shrub-edge 参数必须是 LWPOLYLINE/POLYLINE ename"))
  res)

;; ------------------------------------------------------------
;; 2. 立面草
;; ------------------------------------------------------------
(defun at-landscape:do-grass (p1 p2 n h / d u tt base tip mid len res i
                                  base-mid mid-pt)
  "核心：在 p1→p2 基线上画 n 根随机弯曲的立面草叶。"
  "草叶 ename 列表"
  "(at-landscape:do-grass '(0 0) '(3000 0) 15 300)"
  (if (and p1 p2 (> (distance p1 p2) 1e-6))
      (progn
        (setq n (max 2 (fix (if (numberp n) n 15))))
        (setq h (if (numberp h) h 300.0))
        (setq d (list (- (car p2) (car p1))
                      (- (cadr p2) (cadr p1))))
        (setq u (at-landscape:unit d))
        (setq res nil)
        (setq i 0)
        (while (< i n)
          ;; 基点：沿基线均布 + 轻微随机抖动
          (setq tt (+ (/ (float i) (float (1- n)))
                      (* (- (at-landscape:rand) 0.5)
                         (/ 0.5 (float (1- n))))))
          (setq base (list (+ (car p1) (* (car d) tt))
                           (+ (cadr p1) (* (cadr d) tt))))
          ;; 草叶方向：垂直基线 + 随机倾斜
          (setq mid (polar '(0 0)
                        (+ (angle '(0 0)
                                  (list (- (cadr d)) (car d)))
                           (* (at-landscape:rand-in -0.35 0.35)
                              (/ pi 180.0) 20.0))
                        1.0))
          (setq len (* h (at-landscape:rand-in 0.55 1.0)))
          (setq tip (polar base (angle '(0 0) mid) len))
          (setq base-mid
                (polar base (angle '(0 0) mid) (* len 0.5)))
          (setq mid-pt
                (polar base-mid (angle '(0 0) u)
                       (* len 0.3 (at-landscape:rand-in -1.0 1.0))))
          (setq res (cons (at-landscape:pl (list base mid-pt tip) 0) res))
          (setq i (1+ i)))
        (reverse res))
      nil))

(defun at-landscape:grass (/ p1 p2 n h res)
  "画立面草：两点确定基线，在基线上画一束随机弯曲的草叶。"
  "生成的草叶 ename 列表"
  "(at-landscape:grass)"
  (if (setq p1 (getpoint "\n草基线起点:"))
      (if (setq p2 (getpoint p1 "\n草基线终点:"))
          (progn
            (setq n (getint "\n草叶数量 <15>:"))
            (if (null n) (setq n 15))
            (setq h (getdist "\n草高 <300>:"))
            (if (null h) (setq h 300.0))
            (startundo)
            (setq res (at-landscape:do-grass p1 p2 n h))
            (endundo)
            (if res
                (princ (strcat "\n已画 " (itoa (length res)) " 根草叶。"))
                (princ "\n画草失败（起点与终点重合）。")))
          (princ))
      (princ))
  (princ))

(defun at-landscape:ai-grass (p1 p2 n h / res)
  "AI 调用入口：两点之间画立面草（多根草叶线，随机弯曲）。
  p1/p2: 基线端点；n: 草叶数（缺省 15）；h: 草高（缺省 300）。"
  "草叶 ename 列表，失败返回 nil"
  "(at-landscape:ai-grass '(0 0 0) '(3000 0 0) 15 300)"
  (if (and p1 p2)
      (setq res (at-landscape:do-grass p1 p2 n h))
      (princ "at-landscape:ai-grass p1 p2 不能为空"))
  res)

;; ------------------------------------------------------------
;; 3. 苗木标注
;; ------------------------------------------------------------
(defun at-landscape:do-plant-text (ents mode type / res pts c e area)
  "核心：标注苗木数量（N 株）或灌木丛面积（X m²）。"
  "生成的文字 ename 列表"
  "(at-landscape:do-plant-text (list (entlast)) \"count\" nil)"
  (cond
   ((or (eq mode "count") (eq mode "COUNT"))
    (if ents
        (progn
          (setq pts (vl-remove-if-not
                     '(lambda (x) x)
                     (mapcar 'at-landscape:center-of ents)))
          (if pts
              (progn
                (setq res (list (at-landscape:pl pts 0)))
                (if (setq c (at-landscape:centroid pts))
                    (setq res (cons
                               (at-landscape:text
                                (strcat (itoa (length pts)) " 株")
                                (list (car c)
                                      (+ (cadr c) (at-landscape:text-size)))
                                (at-landscape:text-size) "LM")
                               res)))
                (reverse res))
              (progn
                (princ "at-landscape:do-plant-text 未找到苗木中心点")
                nil)))
        (progn
          (princ "at-landscape:do-plant-text 未选择苗木")
          nil)))
   ((or (eq mode "area") (eq mode "AREA"))
    (setq res nil)
    (foreach e ents
      (if (at-landscape:closed-p e)
          (progn
            (setq area (at-landscape:area-of e))
            (if (and area (setq c (at-landscape:center-of e)))
                (setq res (cons
                           (at-landscape:text
                            (strcat
                             (if (and type (/= "" type))
                                 (strcat type " ") "")
                             (rtos area 2 2) " m²")
                            c
                            (at-landscape:text-size) "MM")
                           res))))))
    (reverse res))
   (t
    (princ "at-landscape:do-plant-text mode 只能为 count 或 area")
    nil)))

(defun at-landscape:plant-text (/ ss ents mode ty res)
  "苗木标注：选苗木块/圆标注数量（N 株），或选封闭边界标注灌木丛面积（X m²）。"
  "生成的文字 ename 列表"
  "(at-landscape:plant-text)"
  (if (setq ss (ssget
                (list (cons 0 "INSERT,CIRCLE,LWPOLYLINE,POLYLINE,REGION"))))
      (progn
        (setq mode
              (strcase (getkword "\n标注模式 [数量 <C>/面积 <A>]: "
                         "count area")))
        (if (null mode) (setq mode "COUNT"))
        (if (= mode "AREA")
            (setq ty (getstring "\n灌木种类（可留空）: ")))
        (startundo)
        (setq res (at-landscape:do-plant-text
                   (pickset:to-list ss) mode ty))
        (endundo)
        (if res
            (princ (strcat "\n生成 " (itoa (length res)) " 处标注。"))
            (princ "\n没有可标注的图元。")))
      (princ "\n请选择苗木块/圆或封闭边界。"))
  (princ))

(defun at-landscape:ai-plant-text (ents mode type / res)
  "AI 调用入口：标注苗木数量或灌木丛面积。
  ents: ename 列表或选择集；mode: \"count\" 标数量（N 株）/ \"area\" 标面积（X m²）。
  type: 灌木种类（仅 area 模式，可缺省）。"
  "文字 ename 列表，失败返回 nil"
  "(at-landscape:ai-plant-text (ssget) \"count\")
   (at-landscape:ai-plant-text (list (entlast)) \"area\" \"海桐\")"
  (if (and ents mode)
      (progn
        (if (= 'pickset (type ents))
            (setq ents (pickset:to-list ents)))
        (if (listp ents)
            (setq res (at-landscape:do-plant-text
                       ents (strcase mode) type))
            (princ "at-landscape:ai-plant-text ents 须为 ename 列表或选择集")))
      (princ "at-landscape:ai-plant-text ents 与 mode 不能为空"))
  res)

;; ------------------------------------------------------------
;; 4. 苗木统计表
;; ------------------------------------------------------------
(defun at-landscape:do-mm-list (ents group-by / res e name box w h row)
  "核心：按块名或图层统计苗木数量与面积（包围盒面积）。"
  "列表：(名称 数量 面积)"
  "(at-landscape:do-mm-list nil \"name\")"
  (if (null ents)
      (setq ents (pickset:to-list (ssget "x" '((0 . "INSERT"))))))
  (if ents
      (progn
        (setq res nil)
        (foreach e ents
          (if (= "INSERT" (entity:getdxf e 0))
              (progn
                (setq name (if (eq group-by "layer")
                               (entity:getdxf e 8)
                               (block:get-effectivename e)))
                (setq box (entity:getbox e 0))
                (setq w (- (caar (cadr box)) (caar (car box))))
                (setq h (- (cadr (cadr box)) (cadr (car box))))
                (if (setq row (assoc name res))
                    (setq res
                          (subst (list name (1+ (cadr row))
                                         (+ (caddr row) (* w h)))
                                 row res))
                    (setq res (cons (list name 1 (* w h)) res))))))
        (reverse res))
      nil))

(defun at-landscape:make-mm-table (data pt title / row)
  "生成苗木统计表（名称/数量/面积 三列）。"
  "表格 ename"
  "(at-landscape:make-mm-table '((海桐 5 25.0)) (getpoint) \"苗木统计表\")"
  (table:make pt title
              '("名称" "数量" "面积")
              (mapcar
               (function (lambda (r)
                           (list (car r) (itoa (cadr r))
                                 (rtos (caddr r) 2 2))))
               data)))

(defun at-landscape:mm-list (/ ss ents group-by data pt res)
  "苗木统计表：按块名/图层统计苗木数量与面积，生成 CAD 表格。"
  "表格 ename"
  "(at-landscape:mm-list)"
  (if (setq ss (ssget (list (cons 0 "INSERT"))))
      (progn
        (setq group-by
              (strcase (getkword "\n分组方式 [按块名 <N>/按图层 <L>]: "
                         "name layer")))
        (if (null group-by) (setq group-by "NAME"))
        (setq data (at-landscape:do-mm-list
                    (pickset:to-list ss)
                    (if (= group-by "NAME") "name" "layer")))
        (if (setq pt (getpoint "\n请指定表格位置:"))
            (progn
              (startundo)
              (setq res (at-landscape:make-mm-table
                         data pt "苗木统计表"))
              (endundo)
              (if res
                  (princ (strcat "\n共统计 " (itoa (length data))
                                 " 类苗木。"))
                  (princ "\n表格生成失败。")))
            (princ)))
      (princ "\n请选择苗木块。"))
  (princ))

(defun at-landscape:ai-mm-list (blk-names out-file group-by / names ents data)
  "AI 调用入口：苗木统计表（按块名/图层统计数量与面积）。
  blk-names: 块名过滤（字符串或列表，可用逗号/分号/空格分隔）；nil 为全图苗木。
  out-file: CSV 输出路径；nil 不写文件。
  group-by: \"name\"（缺省）或 \"layer\"。"
  "列表：(名称 数量 面积)，失败返回 nil"
  "(at-landscape:ai-mm-list nil nil)
   (at-landscape:ai-mm-list \"海桐,红叶石楠\" \"C:/mm.csv\")"
  (if (stringp blk-names)
      (setq names (vl-remove-if
                   '(lambda (x) (= "" x))
                   (vl-string-split blk-names " ,;\t"))))
  (setq ents (pickset:to-list (ssget "x" '((0 . "INSERT")))))
  (if names
      (setq ents (vl-remove-if-not
                  '(lambda (e)
                     (member (block:get-effectivename e) names))
                  ents)))
  (setq data (at-landscape:do-mm-list ents
              (if (eq group-by "layer") "layer" "name")))
  (if (and out-file data)
      (progn
        (table:write-csv
         (append (list "名称" "数量" "面积")
                 (mapcar
                  (function (lambda (r)
                              (list (car r) (cadr r) (caddr r))))
                  data))
         out-file)
        (princ (strcat "at-landscape:ai-mm-list 已写入 " out-file))))
  (if data data nil))

;; ------------------------------------------------------------
;; 5. 变线为树
;; ------------------------------------------------------------
(defun at-landscape:do-tree-line (ent dist r / e len i d p circ pt res)
  "核心：沿多段线布树符号（树冠圆 + 树干点）。"
  "生成的图元 ename 列表"
  "(at-landscape:do-tree-line (car (entsel)) 500 100)"
  (if (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC")
      (progn
        (setq e (e2o ent))
        (setq len (curve:length ent))
        (setq dist (max 1.0 (if (numberp dist) dist 500.0)))
        (setq r (max 1.0 (if (numberp r) r 100.0)))
        (setq res nil)
        (setq i 0)
        (while (<= (* (float i) dist) len)
          (setq d (min len (* (float i) dist)))
          (setq p (at-landscape:pt-at ent d))
          (if (setq circ (entity:make-circle p r))
              (progn
                (at-landscape:on-layer circ)
                (setq res (cons circ res))))
          (if (setq pt (entity:make-point p))
              (progn
                (at-landscape:on-layer pt)
                (setq res (cons pt res))))
          (setq i (1+ i)))
        (reverse res))
      nil))

(defun at-landscape:tree-line (/ ent dist r res)
  "变线为树：沿多段线布树符号（树冠圆 + 树干点），间距与大小可调。"
  "生成的图元 ename 列表"
  "(at-landscape:tree-line)"
  (if (and (setq ent (cadr (entsel "\n请选择路线线:")))
           (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (progn
        (setq dist (getdist "\n树木间距 <500>:"))
        (if (null dist) (setq dist 500.0))
        (setq r (getdist "\n树冠半径 <100>:"))
        (if (null r) (setq r 100.0))
        (startundo)
        (setq res (at-landscape:do-tree-line ent dist r))
        (endundo)
        (if res
            (princ (strcat "\n共布 " (itoa (/ (length res) 2))
                           " 棵树。"))
            (princ "\n布树失败。")))
      (princ "\n请选择线状图元。"))
  (princ))

(defun at-landscape:ai-tree-line (pline-ent dist r / res)
  "AI 调用入口：沿多段线布树符号（树冠圆 + 树干点）。
  pline-ent: 线 ename；dist: 树间距（缺省 500）；r: 树冠半径（缺省 100）。"
  "图元 ename 列表（圆与点成对），失败返回 nil"
  "(at-landscape:ai-tree-line (car (entsel)) 500 100)"
  (if (and pline-ent
           (wcmatch (entity:getdxf pline-ent 0) "LWPOLYLINE,POLYLINE,SPLINE,LINE,ARC"))
      (setq res (at-landscape:do-tree-line pline-ent dist r))
      (princ "at-landscape:ai-tree-line pline-ent 必须是线状图元"))
  res)
