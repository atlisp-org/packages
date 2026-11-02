;; ============================================================
;; at-edit 局部放大（详图） - zoom-detail.lsp
;; 框选源区域 → 复制窗内图元 → 按倍数缩放 → 移到目标中心，
;; 并绘制边界（圆/矩形）、引线、标注文字。
;; 复制用纯 DXF 几何变换 + entmakex（不依赖 VLA 缩放、
;; 不使用 command），MCP/agent 可直接调用。
;; 复用 lib/src 函数：point:mid / pickset:to-list /
;; entity:make-circle / entity:make-rectangle / entity:make-line /
;; entity:make-text / layer:make
;; ============================================================

(vl-load-com)

(defun at-edit:zoom-box (p1 p2)
  "两点角转 (左下 右上) 二维盒"
  "((x1 y1) (x2 y2))"
  "(at-edit:zoom-box '(3 5) '(1 2)) => ((1 2) (3 5))"
  (list (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)))
        (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2)))))

(defun at-edit:zoom-ray-edge (center hx hy dx dy / tx ty tt)
  "中心在 center、半宽 hx/hy 的矩形沿单位向量 (dx dy) 的边界点"
  "二维点"
  "(at-edit:zoom-ray-edge '(0 0) 10 5 0.8 0.6)"
  (cond
    ((and (equal dx 0.0 1e-9) (equal dy 0.0 1e-9)) center)
    (t
     (progn
       (setq tx (if (> (abs dx) 1e-9) (/ hx (abs dx)) 1e18))
       (setq ty (if (> (abs dy) 1e-9) (/ hy (abs dy)) 1e18))
       (setq tt (min tx ty))
       (list (+ (car center) (* tt dx))
             (+ (cadr center) (* tt dy)))))))

(defun at-edit:zoom-pt (pt base ip factor)
  "单点变换: pt' = ip + (pt - base) * factor"
  "三维点"
  "(at-edit:zoom-pt '(1 1 0) '(0 0 0) '(10 0 0) 2.0)"
  (list (+ (car ip) (* factor (- (car pt) (car base))))
        (+ (cadr ip) (* factor (- (cadr pt) (cadr base))))
        (+ (caddr ip) (* factor (- (caddr pt) (caddr base))))))

(defun at-edit:zoom-vec (v factor)
  "二维/三维向量按 factor 缩放"
  "向量"
  "(at-edit:zoom-vec '(1 2 0) 3.0)"
  (list (* factor (car v))
        (* factor (cadr v))
        (if (and (> (length v) 2) (numberp (caddr v)))
            (* factor (caddr v)) 0.0)))

(defun at-edit:zoom-type-desc (typ)
  "详图复制支持的图元类型 → (点组码表 长度组码表 向量组码表)
   不支持的类型（HATCH/DIMENSION/光栅等）返回 nil"
  "列表或 nil"
  "(at-edit:zoom-type-desc \"LINE\")"
  (cond
    ((= typ "LINE") (list '(10 11) nil nil))
    ((member typ '("POINT" "SOLID" "3DFACE")) (list '(10) nil nil))
    ((member typ '("CIRCLE" "ARC")) (list '(10) '(40) nil))
    ((= typ "TEXT") (list '(10) '(40) nil))
    ((= typ "MTEXT") (list '(10) '(40 41) nil))
    ((= typ "LWPOLYLINE") (list '(10) '(41 42 43 44 45 46) nil))
    ((= typ "POLYLINE") (list '(10) '(40 41) nil))
    ((= typ "INSERT") (list '(10) '(41 42 43) nil))
    ((= typ "ELLIPSE") (list '(10 20) nil nil))
    ((= typ "SPLINE") (list '(10 11) nil '(70 71)))
    (t nil)))

(defun at-edit:zoom-xform-dxf (dxf base ip factor / desc pts lens vecs out x code)
  "变换 DXF 几何组码：按 base→ip 位移、factor 缩放；
   点组码平移缩放，长度组码（半径/字高/宽度/比例缩放因子）乘 factor，
   角度/曲率/宽高比不变。不支持的类型返回 nil。"
  "新 DXF 点对表或 nil"
  "(at-edit:zoom-xform-dxf (entget (car (entsel))) '(0 0 0) '(10 0 0) 2.0)"
  (if (setq desc (at-edit:zoom-type-desc (cdr (assoc 0 dxf))))
    (progn
      (setq pts (car desc) lens (cadr desc) vecs (caddr desc))
      (foreach x dxf
        (setq code (car x))
        (cond
          ((member code pts)
           (setq out (cons (cons code
                                 (at-edit:zoom-pt (cdr x) base ip factor)) out)))
          ((and lens (member code lens) (numberp (cdr x)))
           (setq out (cons (cons code (* factor (cdr x))) out)))
          ((and vecs (member code vecs) (listp (cdr x)))
           (setq out (cons (cons code (at-edit:zoom-vec (cdr x) factor)) out)))
          (t (setq out (cons x out))))
      (reverse out))
    nil)))

(defun at-edit:zoom-clean-dxf (dxf / res x code)
  "去掉不应进入 entmake 的组码：句柄/所有者/反应器/常量等"
  "DXF 点对表"
  "(at-edit:zoom-clean-dxf (entget (car (entsel))))"
  (foreach x dxf
    (setq code (car x))
    (if (not (or (member code '(-1 5 330 360 -2 102 -3))
                 (and (numberp code) (>= code 1000) (<= code 1071))))
      (setq res (cons x res))))
  (reverse res))

(defun at-edit:zoom-copy-xform (ent base ip factor / dxf r)
  "复制 ent 并一次完成 位移(base→ip)+缩放(factor) 的几何变换
   （DXF 变换 + entmakex，不依赖 VLA 缩放）。
   返回新图元 ename；不支持的类型返回 nil。"
  "图元名或 nil"
  "(at-edit:zoom-copy-xform (car (entsel)) '(0 0 0) '(10 0 0) 2.0)"
  (if (setq dxf (at-edit:zoom-xform-dxf (entget ent) base ip factor))
    (progn
      (setq r (vl-catch-all-apply 'entmakex
                                  (list (at-edit:zoom-clean-dxf dxf))))
      (if (vl-catch-all-error-p r) nil r))
    nil))

(defun at-edit:ai-zoom-detail (p1 p2 factor insert-pt shape layer label mode
      / box cx cy w h ip sel shape2 r w2 h2 ss lst news skipped cen e new
        c bnd lead lab th corners corner dx dy dd end old)
  "AI 入口：局部放大（详图）。
   框选源区域，复制窗内图元，按 factor 放大并移到 insert-pt 中心，
   绘制边界（圆/矩形）、引线和标注。
   p1/p2: 窗口两角（二维/三维点） factor: 放大倍数（详图比原图大 factor 倍，
   必须 >0，缺省 2.0） insert-pt: 详图中心点 shape: 边界形式
   \"circle\"（缺省）/ \"rect\" layer: 可选图层名（不存在时自动创建，
   复制的图元也放到该层） label: 可选标注文字（缺省 \"1:<factor>\"）
   mode: 选择模式 \"W\" 完全在窗内（缺省）/ \"C\" 与窗交叉。
   支持复制：LINE/POINT/SOLID/3DFACE/CIRCLE/ARC/TEXT/MTEXT/
   LWPOLYLINE/POLYLINE/INSERT/ELLIPSE/SPLINE，其余类型跳过并计入 skipped。
   返回 ((\"copies\" . 新图元列表) (\"boundary\" . 边界 ename)
         (\"leader\" . 引线 ename) (\"label\" . 标注 ename)
         (\"skipped\" . 跳过个数) (\"factor\" . 倍数)
         (\"center\" . 详图中心)) 或 nil。
   示例: (at-edit:ai-zoom-detail '(0 0) '(20 20) 2 '(100 0))"
  "点对表或 nil"
  "(at-edit:ai-zoom-detail '(0 0) '(20 20) 2 '(100 0) \"circle\")"
  (at-edit:ensure-libs)
  (cond
    ((or (not (listp p1)) (not (listp p2)) (not (listp insert-pt)))
     (progn (princ "at-edit: 局部放大 - 缺少窗口角点或目标中心") nil))
    ((and (not (null factor)) (not (numberp factor)))
     (progn (princ "at-edit: 局部放大 - factor 必须为数字") nil))
    ((and (numberp factor) (<= factor 0.0))
     (progn (princ "at-edit: 局部放大 - factor 必须大于 0") nil))
    (t
     (progn
       (setq factor (if (numberp factor) factor 2.0))
       (setq sel (if (and (stringp mode) (/= mode "")
                          (= (substr (strcase mode) 1 1) "C"))
                     "C" "W"))
       (setq shape2 (if (and (stringp shape) (/= shape "")
                             (= (substr (strcase shape) 1 1) "R"))
                        "rect" "circle"))
       (setq box (at-edit:zoom-box p1 p2))
       (setq cx (car (point:mid (car box) (cadr box)))
             cy (cadr (point:mid (car box) (cadr box)))
             w  (- (car (cadr box)) (car (car box)))
             h  (- (cadr (cadr box)) (cadr (car box)))
             ip (at-edit:3d insert-pt))
       (if (<= (at-edit:2d-dist (car box) (cadr box)) 1e-9)
           (progn (princ "at-edit: 局部放大 - 窗口区域太小") nil)
           (progn
             (setq ss (ssget sel (list (at-edit:3d (car box))
                                       (at-edit:3d (cadr box)))
                              '((0 . "*"))))
             (if (and ss (> (sslength ss) 0))
                 (progn
                   ;; ---- 图层（复制的图元也放到该层） ----
                   (setq old (getvar "CLAYER"))
                   (if (and (stringp layer) (/= layer ""))
                       (progn
                         (if (not (tblsearch "layer" layer))
                             (layer:make layer nil nil nil))
                         (setvar "CLAYER" layer)))
                   ;; ---- 复制 + 位移 + 缩放（一次 DXF 变换） ----
                   (setq news '()
                         skipped 0
                         lst (pickset:to-list ss)
                         cen (list cx cy 0.0))
                   (foreach e lst
                     (if (setq new (at-edit:zoom-copy-xform e cen ip factor))
                         (setq news (append news (list new)))
                         (setq skipped (1+ skipped))))
                   (if news
                       (progn
                         ;; ---- 边界（围绕详图中心 ip，包住放大后的图元） ----
                         (setq r (* 0.5 (at-edit:2d-dist (car box)
                                                         (cadr box))
                                    factor))
                         (if (= shape2 "rect")
                             (progn
                               (setq w2 (* w factor) h2 (* h factor))
                               (setq bnd (entity:make-rectangle
                                          (list (- (car ip) (/ w2 2.0))
                                                (- (cadr ip) (/ h2 2.0)))
                                          (list (+ (car ip) (/ w2 2.0))
                                                (+ (cadr ip) (/ h2 2.0)))))
                               (setq r (min (/ w2 2.0) (/ h2 2.0))))
                             (setq bnd (entity:make-circle
                                        (list (car ip) (cadr ip) 0.0) r)))
                         ;; ---- 引线：源窗口最近角 → 边界 ----
                         (setq corners
                               (list (car box)
                                     (cadr box)
                                     (list (car (car box))
                                           (cadr (cadr box)))
                                     (list (car (cadr box))
                                           (cadr (car box)))))
                         (setq corner (car corners))
                         (foreach c corners
                           (if (< (at-edit:2d-dist c ip)
                                  (at-edit:2d-dist corner ip))
                               (setq corner c)))
                         (setq dd (at-edit:2d-dist (list cx cy) corner))
                         (if (> dd 1e-6)
                             (progn
                               (setq dx (/ (- (car corner) cx) dd)
                                     dy (/ (- (cadr corner) cy) dd))
                               (if (= shape2 "rect")
                                   (setq end (at-edit:zoom-ray-edge
                                              (list (car ip) (cadr ip))
                                              (/ w2 2.0) (/ h2 2.0) dx dy))
                                   (setq end (list (+ (car ip) (* r dx))
                                                   (+ (cadr ip) (* r dy)))))
                               (setq lead (entity:make-line
                                           (at-edit:3d corner)
                                           (at-edit:3d end))))
                             (setq end nil lead nil))
                         ;; ---- 标注 ----
                         (setq th (max 2.0 (* 0.12 r)))
                         (setq lab (entity:make-text
                                    (if (and (stringp label) (/= label ""))
                                        label
                                        (strcat "1:" (rtos factor 2 0)))
                                    (list (car ip) (- (cadr ip) r th) 0.0)
                                    th 0 0.8 0 "M"))
                         (setvar "CLAYER" old)
                         (list (cons "copies" news)
                               (cons "boundary" bnd)
                               (cons "leader" lead)
                               (cons "label" lab)
                               (cons "skipped" skipped)
                               (cons "factor" factor)
                               (cons "center" (list cx cy 0.0))))
                       (progn
                         (setvar "CLAYER" old)
                         (princ "at-edit: 局部放大 - 未能复制图元")
                         nil))))
                 (progn
                   (princ "at-edit: 局部放大 - 窗内没有图元")
                   nil)))))))

(defun at-edit:zoom-detail (/ p1 p2 ip f shape layer label res)
  "交互：局部放大（详图）
   框选源区域，指定放大倍数和详图中心点，自动绘制边界/引线/标注。"
  "结果点对表或 nil"
  "(at-edit:zoom-detail)"
  (at-edit:ensure-libs)
  (@:help "框选要放大的源区域")
  (setq p1 (getcorner "\n窗口第一角点: "))
  (if p1 (setq p2 (getcorner p1 "\n窗口第二角点: ")))
  (if (and p1 p2
           (setq ip (getpoint "\n详图中心点: ")))
    (progn
      (setq f (getreal "\n放大倍数 <2.0>: "))
      (setq shape (getstring t "\n边界形式 [圆 <C>/矩形 R]: "))
      (setq layer (getstring t "\n图层名(空=当前层): "))
      (setq label (getstring t "\n标注(空=默认1:n): "))
      (setq res (at-edit:ai-zoom-detail
                 p1 p2 (if f f 2.0) ip
                 (if (and (stringp shape) (/= shape "")
                          (= (substr (strcase shape) 1 1) "R"))
                     "rect" "circle")
                 (if (and (stringp layer) (/= layer "")) layer)
                 (if (and (stringp label) (/= label "")) label)
                 nil))
      (if res
          (progn
            (@:prompt
             (strcat "完成，复制了 "
                     (itoa (length (cdr (assoc "copies" res))))
                     " 个图元"))
            res)
          (@:prompt "局部放大失败"))))
  nil)
