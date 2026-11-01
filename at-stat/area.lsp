;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-stat 面积统计（area）
;; 面积标注 / 点选标面积 / 房间墙面面积统计 / 标准层面积分摊表 / 苗木统计表
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------- 周长/几何 ----------

(defun at-stat:hatch-perimeter (ent / ed inb cur start len s)
  "HATCH 周长：各边界路径直线段求和（凸度忽略，近似值）。"
  "周长；解析失败返回 nil"
  "示例: (at-stat:hatch-perimeter (car (entsel)))"
  (setq ed (entget ent) inb nil cur nil start nil len 0.0)
  (foreach s ed
    (cond
     ((= 72 (car s))
      (if (and inb cur start)
          (setq len (+ len (distance cur start))))
      (setq inb t cur nil start nil))
     ((and inb (member (car s) '(10 11 12)))
      (if (null cur)
          (progn
            (setq cur (cdr s))
            (setq start (cdr s)))
          (progn
            (setq len (+ len (distance cur (cdr s))))
            (setq cur (cdr s)))))))
  (if inb
      (progn
        (if (and cur start)
            (setq len (+ len (distance cur start))))
        len)
      nil))

(defun at-stat:ent-perimeter (ent / r)
  "封闭实体周长：多段线/样条/圆/弧/椭圆用 vlax-curve；
  SOLID/3DFACE 按边求和；HATCH 按边界路径解析（近似）；REGION 尝试采样。"
  "周长；非封闭或出错返回 nil"
  "示例: (at-stat:ent-perimeter (car (entsel)))"
  (if (not (at-stat:closed-p ent))
      nil
      (cond
       ((member (entity:getdxf ent 0)
                '("LWPOLYLINE" "POLYLINE" "SPLINE" "CIRCLE" "ARC" "ELLIPSE"))
        (if (setq r (vl-catch-all-apply 'curve:length (list ent)))
            (if (numberp r) r nil)
            nil))
       ((or (= "SOLID" (entity:getdxf ent 0))
            (= "3DFACE" (entity:getdxf ent 0)))
        (+ (distance (entity:getdxf ent 10) (entity:getdxf ent 11))
           (distance (entity:getdxf ent 11) (entity:getdxf ent 12))
           (distance (entity:getdxf ent 12) (entity:getdxf ent 13))
           (distance (entity:getdxf ent 13) (entity:getdxf ent 10))))
       ((= "HATCH" (entity:getdxf ent 0))
        (at-stat:hatch-perimeter ent))
       (t
        (if (setq r (vl-catch-all-apply 'curve:length (list ent)))
            (if (numberp r) r nil)
            nil)))))

;; ---------- 面积标注 ----------

(defun at-stat:mark-areas (ents factor layer / res ent a pt te)
  "面积标注：为 ents 中每个封闭实体在其中心绘制面积文字（面积 × factor）。
  ents: 图元列表；factor: 换算系数（nil = 0.0001）；
  layer: 文字图层（nil = 当前图层）。"
  "((实体 面积 文字图元) ...) 列表"
  "示例: (at-stat:mark-areas (at-stat:all-closed-ents) 0.0001 \"STAT\")"
  (if (null factor) (setq factor 0.0001))
  (if (null entity:make-text) (require 'entity:*))
  (setq res nil)
  (foreach ent ents
    (if (and (at-stat:closed-p ent) (setq a (at-stat:ent-area ent)))
        (progn
          (setq pt (at-stat:centroid ent))
          (setq te (entity:make-text (at-stat:fmt (* a factor) 4)
                                      pt (@:scale 2.5) 0 1.0 0 "M"))
          (if te
              (progn
                (if layer (at-stat:ent-layer te layer))
                (setq res (cons (list ent a te) res)))))))
  (reverse res))

(defun at-stat:area-text (/ ents factor layer)
  "面积标注：选择封闭实体后，在其中心绘制面积（×换算系数）文字。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择封闭实体: "
              '((0 . "LWPOLYLINE,POLYLINE,HATCH,SOLID,3DFACE,REGION,SPLINE,ELLIPSE"))))
  (if ents
      (progn
        (setq factor (getreal "\n面积换算系数(0.0001=mm2→m2)<0.0001>: "))
        (setq layer (getstring t "\n文字图层<当前>: "))
        (if (null std:startundo) (require 'std:*))
        (std:startundo *doc*)
        (at-stat:mark-areas ents (if factor factor 0.0001)
                              (if (> (strlen layer) 0) layer nil))
        (std:endundo *doc*)))
  (princ))

(defun at-stat:ai-area-text (ents factor layer / res)
  "AI 调用入口：面积标注（中心绘制面积文字），无交互、不弹窗。
  ents: 封闭实体列表（选择集或 ename 列表）；factor: 换算系数（nil = 0.0001）；
  layer: 文字图层（nil = 当前图层）。
  return: ((实体 面积 文字图元) ...) 列表；无封闭实体返回 nil。
  示例: (at-stat:ai-area-text (at-stat:all-closed-ents) 0.0001 \"STAT\")"
  (setq res (at-stat:mark-areas (at-stat:to-ents ents) factor layer))
  (if res res nil))

;; ---------- 点选标面积 ----------

(defun at-stat:mark-point-areas (pts factor layer / res pt ent a te)
  "点选标面积：为每个点找包含它的闭合边界，在点处绘制面积文字（× factor）。
  pts: 点列表；factor: 换算系数（nil = 0.0001）；layer: 文字图层（nil = 当前）。"
  "((点 面积 边界图元 文字图元) ...) 列表"
  "示例: (at-stat:mark-point-areas '((5000 4000)) 0.0001 nil)"
  (if (null factor) (setq factor 0.0001))
  (if (null entity:make-text) (require 'entity:*))
  (setq res nil)
  (foreach pt pts
    (if (and (setq ent (at-stat:find-boundary pt nil))
             (setq a (at-stat:ent-area ent)))
        (progn
          (setq te (entity:make-text (at-stat:fmt (* a factor) 4)
                                      pt (@:scale 2.5) 0 1.0 0 "M"))
          (if te
              (progn
                (if layer (at-stat:ent-layer te layer))
                (setq res (cons (list pt a ent te) res)))))))
  (reverse res))

(defun at-stat:point-area (/ pts factor layer p)
  "点选标面积：逐个点选区域内部，自动找包含边界并标注面积（Esc 结束）。"
  ""
  ""
  (setq pts nil)
  (while (setq p (getpoint "\n点选区域内一点(Esc结束): "))
    (setq pts (cons p pts)))
  (if pts
      (progn
        (setq factor (getreal "\n面积换算系数(0.0001=mm2→m2)<0.0001>: "))
        (setq layer (getstring t "\n文字图层<当前>: "))
        (if (null std:startundo) (require 'std:*))
        (std:startundo *doc*)
        (at-stat:mark-point-areas (reverse pts) (if factor factor 0.0001) layer)
        (std:endundo *doc*)))
  (princ))

(defun at-stat:ai-point-area (pts factor layer / res)
  "AI 调用入口：点选标面积（点所在闭合边界的面积），无交互、不弹窗。
  pts: 点列表（每点位于一个封闭区域内）；factor: 换算系数（nil = 0.0001）；
  layer: 文字图层（nil = 当前图层）。
  return: ((点 面积 边界图元 文字图元) ...) 列表；未找到边界返回 nil。
  示例: (at-stat:ai-point-area '((5000 4000) (9000 6000)) 0.0001 nil)"
  (setq res (at-stat:mark-point-areas pts factor layer))
  (if res res nil))

;; ---------- 房间墙面面积统计 ----------

(defun at-stat:wall-area-data (rooms height openings
                                 / res i ent a p o ow total-a total-w row b)
  "房间墙面面积统计数据：每个房间墙面面积 = 周长 × 层高。
  rooms: 封闭房间图元列表；height: 层高（nil = 3000）；
  openings: 洞口图元列表（LINE/曲线取长度，INSERT 取包围盒 X 宽），nil = 无洞口。
  返回: 二维矩阵 ((编号 面积 墙面面积) ... 洞口扣除行 合计行)。"
  "二维矩阵"
  "示例: (at-stat:wall-area-data (at-stat:all-closed-ents) 3000 nil)"
  (if (null height) (setq height 3000.0))
  (setq res nil i 1)
  (foreach ent rooms
    (if (and (at-stat:closed-p ent)
             (setq a (at-stat:ent-area ent))
             (setq p (at-stat:ent-perimeter ent)))
        (progn
          (setq res (cons (list (strcat "房间 " (itoa i)) a (* p height)) res))
          (setq i (1+ i)))))
  (setq res (reverse res))
  (if (= 0 (length res))
      nil
      (progn
        (setq total-a 0.0 total-w 0.0)
        (foreach row res
          (setq total-a (+ total-a (cadr row))
                total-w (+ total-w (caddr row))))
        (setq o 0.0)
        (if openings
            (foreach ent openings
              (cond
               ((null ent) nil)
               ((= "INSERT" (entity:getdxf ent 0))
                (if (setq b (entity:getbox ent 0))
                    (setq o (+ o (- (car (cadr b)) (car (car b)))))))
               (t
                (if (setq ow (at-stat:ent-length ent))
                    (setq o (+ o ow))))))
            nil)
        (if (> o 0.0)
            (progn
              (setq res (append res (list (list "洞口扣除" 0.0 (- (* o height))))))
              (setq total-w (- total-w (* o height)))))
        (append res (list (list "合计" total-a total-w))))))

(defun at-stat:wall-area (/ rooms height openings data pt)
  "房间墙面面积统计：选择房间与洞口，按周长×层高统计各房间墙面面积并绘制表格。"
  ""
  ""
  (setq rooms (at-stat:pick-ents "选择房间(封闭轮廓): "
               '((0 . "LWPOLYLINE,POLYLINE,HATCH,SOLID,3DFACE,REGION,SPLINE,ELLIPSE"))))
  (if rooms
      (progn
        (setq height (getreal "\n层高<3000>: "))
        (if (setq openings
                 (ssget "\n选择洞口(门窗/线段, Esc跳过): "
                       '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,SPLINE,ELLIPSE,INSERT"))))
            (setq openings (at-stat:ss-ents openings)))
        (setq data (at-stat:wall-area-data rooms (if height height 3000.0) openings))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "房间墙面面积统计表"
                                  '("房间" "面积" "墙面面积")
                                  data pt))))
  (princ))

(defun at-stat:ai-wall-area (ents height / data)
  "AI 调用入口：房间墙面面积统计（墙面面积=周长×层高），无交互、不弹窗。
  ents: 封闭房间图元列表（选择集或 ename 列表）；height: 层高（nil = 3000）。
  return: 二维矩阵 ((房间编号 面积 墙面面积) ... 合计行)；无有效房间返回 nil。
  示例: (at-stat:ai-wall-area (at-stat:all-closed-ents) 3000)"
  (setq data (at-stat:wall-area-data (at-stat:to-ents ents) height nil))
  (if data data nil))

;; ---------- 标准层面积分摊表 ----------

(defun at-stat:area-analyze-data (pts shared-area
                                    / res i pt ent a total rows row)
  "标准层面积分摊数据：每点识别一个封闭区域（最内层边界），
  分摊面积(shared-area)按各区域面积占总面积比例分配。
  pts: 点列表（每个区域取一点）；shared-area: 分摊面积合计（nil = 0）。
  返回: 二维矩阵 ((编号 面积 分摊面积) ... 合计行)；未找到区域返回 nil。"
  "二维矩阵"
  "示例: (at-stat:area-analyze-data '((5000 4000) (9000 6000)) 20.0)"
  (if (null shared-area) (setq shared-area 0.0))
  (setq res nil i 1)
  (foreach pt pts
    (if (and (setq ent (at-stat:find-boundary pt nil))
             (setq a (at-stat:ent-area ent)))
        (progn
          (setq res (cons (list (strcat "区域 " (itoa i)) a) res))
          (setq i (1+ i)))))
  (if (= 0 (length res))
      nil
      (progn
        (setq res (reverse res)
              total 0.0)
        (foreach row res
          (setq total (+ total (cadr row))))
        (if (= 0.0 total)
            nil
            (progn
              (setq rows
                   (mapcar (function (lambda (r)
                                       (list (car r) (cadr r)
                                             (+ (cadr r)
                                                (* shared-area
                                                   (/ (cadr r) total))))))
                                     res))
              (append rows (list (list "合计" total (+ total shared-area)))))))))

(defun at-stat:area-analyze (/ pts shared data p)
  "标准层面积分摊表：逐点选各区域，计算各区域面积与分摊面积，绘制表格。"
  ""
  ""
  (setq pts nil)
  (while (setq p (getpoint "\n在每个区域内点选一点(Esc结束): "))
    (setq pts (cons p pts)))
  (if pts
      (progn
        (setq shared (getreal "\n需分摊的面积合计(0=无)<0>: "))
        (setq data (at-stat:area-analyze-data (reverse pts) (if shared shared 0.0)))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "标准层面积分摊表"
                                  '("区域" "面积" "分摊面积")
                                  data pt))))
  (princ))

(defun at-stat:ai-area-analyze (zones shared-area table / data mx my p)
  "AI 调用入口：标准层面积分摊表（分摊面积按比例分配），无交互、不弹窗。
  zones: 点列表（每点位于一个封闭区域内）；shared-area: 分摊面积合计（nil = 0）；
  table: t = 在图纸左下角附近额外绘制表格。
  return: 二维矩阵 ((编号 面积 分摊面积) ... 合计行)；未找到区域返回 nil。
  示例: (at-stat:ai-area-analyze '((5000 4000) (9000 6000)) 20.0 nil)"
  (setq data (at-stat:area-analyze-data zones shared-area))
  (if (and table data)
      (progn
        (setq mx 1e9 my 1e9)
        (foreach p zones
          (if (< (car p) mx) (setq mx (car p)))
          (if (< (cadr p) my) (setq my (cadr p))))
        (at-stat:make-table "标准层面积分摊表"
                              '("区域" "面积" "分摊面积")
                              data
                              (list mx (- my (* 2.0 (@:scale 3.5))))))
      nil)
  (if data data nil))

;; ---------- 苗木统计表 ----------

(defun at-stat:mmtj-data (names / res ent name rec)
  "苗木统计数据：全图 INSERT 按名称计数。
  names: 块名列表，nil = 全部块。"
  "二维矩阵: ((名称 数量) ...)，按数量降序"
  "示例: (at-stat:mmtj-data '(\"香樟\" \"银杏\"))"
  (if (null block:get-effectivename) (require 'block:*))
  (setq res nil)
  (foreach ent (at-stat:ss-ents (ssget "x" '((0 . "INSERT"))))
    (setq name (block:get-effectivename ent))
    (if (or (null names) (member name names))
        (if (setq rec (assoc name res))
            (setq res (subst (list name (1+ (cadr rec))) rec res))
            (setq res (cons (list name 1) res)))))
  (if res
      (sort res (function (lambda (a b) (> (cadr a) (cadr b)))))
      nil))

(defun at-stat:mmtj (/ mode all names area-ent data pt ent a)
  "苗木统计表：全图按名称（或全部）统计苗木数量，可加指定面积行，绘制表格。"
  ""
  ""
  (setq mode (getkword "\n统计范围 [全部块(A)/按名称(N)]<A>: " "A N"))
  (setq names nil all t)
  (if (= "N" mode)
      (progn
        (setq all nil)
        (setq names (at-stat:split-str
                     (getstring t "\n输入苗木块名(逗号分隔): ") ","))
        (if (null names) (princ "\nat-stat: 未输入块名，取消。")))
      nil)
  (if (or all (not (null names)))
      (progn
        (if (setq ent
                 (cadr (entsel "\n指定面积的封闭实体(Esc跳过): "
                          '((0 . "LWPOLYLINE,POLYLINE,HATCH,SOLID,3DFACE,REGION,SPLINE,ELLIPSE")))))
            (setq area-ent ent))
        (setq data (at-stat:mmtj-data (if all nil names)))
        (if (and area-ent (at-stat:closed-p area-ent)
                 (setq a (at-stat:ent-area area-ent)))
            (setq data (append data (list (list "指定面积" a)))))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "苗木统计表" '("名称" "数量") data pt))))
  (princ))

(defun at-stat:ai-mmtj (blk-names area-ent / data a)
  "AI 调用入口：苗木统计表（全图按名称计数），无交互、不弹窗。
  blk-names: 苗木块名列表（nil = 全部块）；
  area-ent: 指定面积的封闭实体（追加\"指定面积\"行，nil = 不追加）。
  return: 二维矩阵 ((名称 数量) ... [+ 指定面积行])；无数据返回 nil。
  示例: (at-stat:ai-mmtj '(\"香樟\" \"银杏\") nil)"
  (setq data (at-stat:mmtj-data blk-names))
  (if (and area-ent (at-stat:closed-p area-ent)
           (setq a (at-stat:ent-area area-ent)))
      (setq data (append data (list (list "指定面积" a)))))
  (if data data nil))
