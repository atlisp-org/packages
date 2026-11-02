;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-planning AI 入口：规划指标批量计算
;; 传入矩形范围，不取点、不缩放、不弹窗，直接返回指标点对表。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @planning:ents-in-range (pt1 pt2 filter / p1 p2 ss res box)
  "按矩形范围筛选图元（不依赖当前视图），返回 ename 列表。"
  "ename 列表或 nil"
  "(@planning:ents-in-range '(0 0) '(10 10) '((0 . \"LWPOLYLINE\")))"
  (setq p1 (list (min (car pt1) (car pt2))
                 (min (cadr pt1) (cadr pt2))))
  (setq p2 (list (max (car pt1) (car pt2))
                 (max (cadr pt1) (cadr pt2))))
  (setq ss (ssget "_X" filter))
  (setq res nil)
  (foreach e (if ss (pickset:to-list ss) nil)
    (setq box (entity:getbox e 0))
    (if (and box
             (point:in-box (car box) p1 p2)
             (point:in-box (cadr box) p1 p2))
        (setq res (cons e res))))
  (reverse res))

(defun @planning:areas-on-layer (pt1 pt2 layers / names res a)
  "按范围统计指定图层上闭合多段线的面积列表。"
  "Real 列表"
  "(@planning:areas-on-layer '(0 0) '(10 10) \"用地红线\")"
  (setq names (if (stringp layers) (string:to-list layers ",") (list layers)))
  (setq res nil)
  (foreach lay names
    (if (and lay (/= lay ""))
        (progn
          (foreach e (@planning:ents-in-range
                      pt1 pt2
                      (list (cons 0 "LWPOLYLINE") (cons 8 lay)))
            (setq a (vl-catch-all-apply 'vla-get-area (list (e2o e))))
            (if (not (vl-catch-all-error-p a))
                (setq res (cons a res)))))))
  (reverse res))

(defun @planning:count-blocks (pt1 pt2 names / lst n)
  "按范围统计指定块名的块数量，names 支持逗号分隔或列表。"
  "整数"
  "(@planning:count-blocks '(0 0) '(10 10) \"*车位*\")"
  (setq lst (if (stringp names) (string:to-list names ",") (list names)))
  (setq n 0)
  (foreach name lst
    (if (and name (/= name ""))
        (setq n (+ n (length
                      (@planning:ents-in-range
                       pt1 pt2
                       (list (cons 0 "INSERT") (cons 2 name))))))))
  n)

(defun @planning:ai-calc (pt1 pt2 / land green building land-total green-total
                                      reduction greening parking)
  "AI 入口：计算矩形范围内的用地/绿地/建筑基底面积和车位数量。
  返回 ((\"land\" . 列表) (\"land-total\" . 面积) (\"green\" . 列表)
        (\"green-total\" . 面积) (\"green-reduction\" . 折减面积)
        (\"greening-rate\" . 绿地率) (\"building\" . 列表)
        (\"building-total\" . 面积) (\"parking\" . 数量))。
  示例: (@planning:ai-calc '(0 0) '(100000 80000))"
  "点对表"
  "(@planning:ai-calc '(0 0) '(100000 80000))"
  (setq land (@planning:areas-on-layer
              pt1 pt2 (@:get-config '@planning:land-layer)))
  (setq green (@planning:areas-on-layer
               pt1 pt2 (@:get-config '@planning:green-layer)))
  (setq building (@planning:areas-on-layer
                  pt1 pt2 (@:get-config '@planning:building-layer)))
  (setq land-total (if land (apply '+ land) 0.0))
  (setq green-total (if green (apply '+ green) 0.0))
  (setq reduction (* -1.0 0.6 2.4 5.3
                     (@planning:count-blocks pt1 pt2 "林荫车位")))
  (setq greening
        (if (> land-total 0.0)
            (/ (+ green-total reduction) land-total)
            nil))
  (setq parking
        (@planning:count-blocks
         pt1 pt2 (@:get-config '@planning:parking)))
  (list (cons "land" land)
        (cons "land-total" land-total)
        (cons "green" green)
        (cons "green-total" green-total)
        (cons "green-reduction" reduction)
        (cons "greening-rate" greening)
        (cons "building" building)
        (cons "building-total" (if building (apply '+ building) 0.0))
        (cons "parking" parking)))

(defun @planning:ai-index (pt1 pt2 overground underground plot parking-underground
                           / calc land-total building-base green-total reduction
                           greening over und plot-area building-all density far
                           parking-over parking-total floor-num density-limit
                           far-limit green-limit)
  "AI 入口：计算主要技术经济指标。
  overground/underground/plot/parking-underground 可为 nil：
  overground 缺失时按“建筑基底面积 × 层数”估算，plot 缺失时取 overground。
  返回点对表，包含面积、密度、容积率、绿地率、车位及限值对比。
  示例: (@planning:ai-index '(0 0) '(100000 80000) 30000 5000 nil 120)"
  "点对表"
  "(@planning:ai-index '(0 0) '(100000 80000) 30000 5000 nil 120)"
  (setq calc (@planning:ai-calc pt1 pt2))
  (setq land-total (cdr (assoc "land-total" calc)))
  (setq building-base (cdr (assoc "building-total" calc)))
  (setq green-total (cdr (assoc "green-total" calc)))
  (setq reduction (cdr (assoc "green-reduction" calc)))
  (setq parking-over (cdr (assoc "parking" calc)))
  (setq floor-num (@:get-config '@planning:floor-num))
  (if (or (null floor-num) (not (numberp floor-num)) (<= floor-num 0))
      (setq floor-num 3))
  (setq over (if (and overground (numberp overground) (> overground 0.0))
                 overground
                 (* building-base floor-num)))
  (setq und (if (and underground (numberp underground)) underground 0.0))
  (setq plot-area (if (and plot (numberp plot) (> plot 0.0)) plot over))
  (setq parking-underground
        (if (and parking-underground (numberp parking-underground))
            parking-underground 0))
  (setq building-all (+ over und))
  (setq density
        (if (> land-total 0.0)
            (* 100.0 (/ building-base land-total))
            nil))
  (setq far
        (if (> land-total 0.0)
            (/ plot-area land-total)
            nil))
  (setq greening
        (if (> land-total 0.0)
            (* 100.0 (/ (+ green-total reduction) land-total))
            nil))
  (setq parking-total (+ parking-over parking-underground))
  (setq density-limit (@:get-config '@planning:building-density-limit))
  (setq far-limit (@:get-config '@planning:floor-area-ratio-limit))
  (setq green-limit (@:get-config '@planning:greening-rate-limit))
  (list (cons "land-total" land-total)
        (cons "building-base" building-base)
        (cons "green-total" green-total)
        (cons "green-reduction" reduction)
        (cons "greening-rate" greening)
        (cons "greening-limit" green-limit)
        (cons "greening-pass" (if (and greening green-limit)
                                  (>= greening green-limit)
                                  nil))
        (cons "overground" over)
        (cons "underground" und)
        (cons "building-all" building-all)
        (cons "plot-area" plot-area)
        (cons "density" density)
        (cons "density-limit" density-limit)
        (cons "density-pass" (if (and density density-limit)
                                 (<= density density-limit)
                                 nil))
        (cons "far" far)
        (cons "far-limit" far-limit)
        (cons "far-pass" (if (and far far-limit) (<= far far-limit) nil))
        (cons "parking-over" parking-over)
        (cons "parking-under" parking-underground)
        (cons "parking-total" parking-total)))
