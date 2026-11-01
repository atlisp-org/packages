;; ============================================================
;; at-landscape 迷你景观
;; 复现 iMini 工具集的景观命令：
;; 灌木边缘线/立面草/苗木标注/铺装面积/苗木统计表/
;; 搭桥/挖路/路线里程/界址点/变线为树
;; ============================================================
(vl-load-com)

;; 注册菜单
(@:add-menus
 '(("迷你景观"
    ("灌木边缘线" "(at-landscape:shrub-edge)")
    ("画立面草" "(at-landscape:grass)")
    ("苗木标注" "(at-landscape:plant-text)")
    ("内轮廓面积及填充" "(at-landscape:area)")
    ("苗木统计表" "(at-landscape:mm-list)")
    ("封闭多段线搭桥" "(at-landscape:add-path)")
    ("封闭多段线挖路" "(at-landscape:remove-path)")
    ("路线里程标注" "(at-landscape:mileage)")
    ("界址点编号" "(at-landscape:jzd)")
    ("变线为树" "(at-landscape:tree-line)"))))

;; ------------------------------------------------------------
;; 公共辅助函数
;; ------------------------------------------------------------
(defun at-landscape:layer ()
  "获取迷你景观使用的图层，不存在时自动创建。"
  "图层名"
  "(at-landscape:layer)"
  (if (null (tblsearch "LAYER" "MINI-LANDSCAPE"))
      (layer:make "MINI-LANDSCAPE" 7 nil nil))
  "MINI-LANDSCAPE")

(defun at-landscape:2d (pt)
  "把三维点转换为二维点。"
  "二维点"
  "(at-landscape:2d '(1 2 3))"
  (list (car pt) (cadr pt)))

(defun at-landscape:pts (ent / res next)
  "获取多段线的二维顶点列表（LWPOLYLINE/POLYLINE）。"
  "二维点列表"
  "(at-landscape:pts (car (entsel)))"
  (cond
   ((= "LWPOLYLINE" (entity:getdxf ent 0))
    (mapcar 'at-landscape:2d (curve:pline-2dpoints ent)))
   ((= "POLYLINE" (entity:getdxf ent 0))
    (setq res nil)
    (setq next (entnext ent))
    (while (and (setq next (entnext next))
                (= "VERTEX" (entity:getdxf next 0)))
      (setq res (cons (at-landscape:2d
                       (cadr (assoc 10 (entget next))))
                      res)))
    (reverse res))
   (t nil)))

(defun at-landscape:closed-p (ent)
  "判断图元是否为封闭轮廓（多段线/圆/面域/样条）。"
  "t or nil"
  "(at-landscape:closed-p (car (entsel)))"
  (cond
   ((= "LWPOLYLINE" (entity:getdxf ent 0))
    (= 1 (logand 1 (entity:getdxf ent 70))))
   ((= "POLYLINE" (entity:getdxf ent 0))
    (= 1 (logand 1 (entity:getdxf ent 70))))
   ((= "SPLINE" (entity:getdxf ent 0))
    (= 1 (logand 1 (entity:getdxf ent 70))))
   ((member (entity:getdxf ent 0) '("CIRCLE" "REGION")) t)
   (t nil)))

(defun at-landscape:area-of (ent)
  "获取封闭图元的面积（多段线/圆/面域/样条）。"
  "面积值，未封闭返回 nil"
  "(at-landscape:area-of (car (entsel)))"
  (cond
   ((= "CIRCLE" (entity:getdxf ent 0))
    (* pi (expt (float (entity:getdxf ent 40)) 2)))
   ((and (at-landscape:closed-p ent)
         (wcmatch (entity:getdxf ent 0) "LWPOLYLINE,POLYLINE,REGION,SPLINE"))
    (vla-get-area (e2o ent)))
   (t nil)))

(defun at-landscape:poly-area (pts / sum)
  "按二维点列表计算多边形面积（鞋带公式）。"
  "面积值（绝对值）"
  "(at-landscape:poly-area '((0 0) (10 0) (10 10) (0 10)))"
  (if (< (length pts) 3)
      0.0
      (progn
        (setq pts (append pts (list (car pts))))
        (setq sum 0.0)
        (while (> (length pts) 2)
          (setq sum
                (+ sum
                   (- (* (caar pts) (cadr (cadr pts)))
                      (* (cadar pts) (caar (cadr pts))))))
          (setq pts (cdr pts)))
        (/ (abs sum) 2.0))))

(defun at-landscape:rand ()
  "生成 [0,1) 随机实数（线性同余）。"
  "随机实数"
  "(at-landscape:rand)"
  (if (null *at-landscape-seed*)
      (setq *at-landscape-seed* 1234567.0))
  (setq *at-landscape-seed*
        (rem (* 16807.0 *at-landscape-seed*) 2147483647.0))
  (/ *at-landscape-seed* 2147483647.0))

(defun at-landscape:rand-in (lo hi)
  "生成 [lo,hi) 随机实数。"
  "随机实数"
  "(at-landscape:rand-in -1.0 1.0)"
  (+ lo (* (- hi lo) (at-landscape:rand))))

(defun at-landscape:text-size ()
  "获取默认文字高度（随绘图比例自动缩放）。"
  "文字高度"
  "(at-landscape:text-size)"
  (@::scale 3.5))

(defun at-landscape:pt-at (ent dist)
  "获取曲线上距起点指定距离处的二维点。"
  "二维点"
  "(at-landscape:pt-at (car (entsel)) 100.0)"
  (at-landscape:2d
   (vlax-curve-getpointatdist (e2o ent) (float dist))))

(defun at-landscape:tangent (ent dist / e len p1 p2)
  "获取曲线上指定距离处的二维切向向量。"
  "二维切向向量"
  "(at-landscape:tangent (car (entsel)) 100.0)"
  (setq e (e2o ent))
  (setq len (curve:length ent))
  (setq p1 (at-landscape:2d
            (vlax-curve-getpointatdist e (max 0.0 (- dist 1.0)))))
  (setq p2 (at-landscape:2d
            (vlax-curve-getpointatdist e (min len (+ dist 1.0)))))
  (list (- (car p2) (car p1))
        (- (cadr p2) (cadr p1))))

(defun at-landscape:unit (v / d)
  "二维向量归一化。"
  "单位向量"
  "(at-landscape:unit '(3 4))"
  (setq d (distance '(0 0) v))
  (if (> d 1e-9)
      (list (/ (car v) d) (/ (cadr v) d))
      v))

(defun at-landscape:centroid (pts / x y p)
  "二维点列表的形心。"
  "二维点"
  "(at-landscape:centroid '((0 0) (2 0) (1 2)))"
  (setq x 0.0)
  (setq y 0.0)
  (foreach p pts
    (setq x (+ x (car p)))
    (setq y (+ y (cadr p))))
  (list (/ x (length pts)) (/ y (length pts))))

(defun at-landscape:center-of (ent)
  "获取图元中心（圆心/块插入点/封闭多段线形心）。"
  "二维点，失败返回 nil"
  "(at-landscape:center-of (car (entsel)))"
  (cond
   ((member (entity:getdxf ent 0) '("CIRCLE" "INSERT" "POINT"))
    (at-landscape:2d (entity:getdxf ent 10)))
   ((at-landscape:closed-p ent)
    (at-landscape:centroid (at-landscape:pts ent)))
   (t nil)))

(defun at-landscape:box (pts / minp maxp)
  "点列表包围盒，返回 ((minx miny) (maxx maxy))。"
  "两点列表"
  "(at-landscape:box '((0 0) (10 10)))"
  (if pts
      (progn
        (setq minp (at-landscape:2d (car pts)))
        (setq maxp (at-landscape:2d (car pts)))
        (foreach p (cdr pts)
          (setq minp (list (min (car minp) (car p))
                           (min (cadr minp) (cadr p))))
          (setq maxp (list (max (car maxp) (car p))
                           (max (cadr maxp) (cadr p)))))
        (list minp maxp))
      nil))

(defun at-landscape:on-layer (ename)
  "把图元移到迷你景观图层，颜色随层。"
  "图元名"
  "(at-landscape:on-layer (entlast))"
  (entity:putdxf ename '(8 62) (list (at-landscape:layer) 18))
  ename)

(defun at-landscape:pl (pts closed)
  "在迷你景观图层创建 LWPOLYLINE。"
  "多段线 ename"
  "(at-landscape:pl '((0 0) (10 0)) 0)"
  (at-landscape:on-layer
   (entity:make-lwpolyline pts nil 0 closed 0.0)))

(defun at-landscape:text (str pt h align)
  "在迷你景观图层创建单行文字。"
  "文字 ename"
  "(at-landscape:text \"A=100\" '(0 0) 3.5 \"MM\")"
  (at-landscape:on-layer
   (entity:make-text str pt h 0.0 0.75 0 align)))

(defun at-landscape:--hatch (ent color pattern scale / obj)
  "内部：创建填充并设置颜色、比例，返回 ename。"
  "填充 ename"
  "(at-landscape:--hatch (car (entsel)) 3 \"SOLID\" 1.0)"
  (setq obj (entity:addhatch (list ent) nil pattern))
  (if obj
      (progn
        (vla-put-patternscale obj (if (numberp scale) scale 40.0))
        (entity:putdxf (vlax-vla-object->ename obj) 62 color)
        (vlax-vla-object->ename obj))
      nil))

(defun at-landscape:hatch (ent color pattern scale / res)
  "对封闭图元做纯色或图案填充。"
  "填充 ename，失败返回 nil"
  "(at-landscape:hatch (car (entsel)) 3 \"SOLID\")"
  (setq res
        (vl-catch-all-apply
         'at-landscape:--hatch
         (list ent color pattern scale)))
  (if (vl-catch-all-error-p res)
      (progn
        (princ (strcat "at-landscape:hatch 填充失败: "
                       (vl-catch-all-error-message res)))
        nil)
      res))

(defun at-landscape:pick-color (colors i)
  "取填充颜色（颜色表循环取色，表为空时 1~7 随机）。"
  "颜色号"
  "(at-landscape:pick-color '(1 3 5) 2)"
  (if colors
      (nth (rem i (length colors)) colors)
      (1+ (fix (* 7.0 (at-landscape:rand))))))

(defun at-landscape:trim (s)
  "去除字符串首尾空白。"
  "字符串"
  "(at-landscape:trim \" a b \")"
  (vl-string-trim " \t\r\n" s))

(defun at-landscape:dir ()
  "获取本包所在目录路径（以 / 结尾）。"
  "路径字符串，失败返回 nil"
  "(at-landscape:dir)"
  (@::package-path "at-landscape"))

(defun at-landscape:words (name / fp line res)
  "读取包目录下的植物词库文件。"
  "植物名称列表"
  "(at-landscape:words \"shrub-names.txt\")"
  (setq fp nil)
  (setq res nil)
  (if (and (at-landscape:dir)
           (setq fp (open
                     (strcat (at-landscape:dir) name)
                     "r")))
      (progn
        (while (setq line (read-line fp))
          (if (and line (/= "" (at-landscape:trim line)))
              (setq res (cons (at-landscape:trim line) res))))
        (close fp)
        (reverse res))
      (progn
        (princ (strcat "at-landscape:words 未找到词库文件 " name))
        nil)))
