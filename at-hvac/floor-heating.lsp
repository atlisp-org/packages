(defun hvac:floor-heating-auto-snake (spacing / ss ent box width height rows cols row-height col-width start-x start-y x y i pts dxf-list ss-list ent-list)
  "自动在图中所有闭合矩形内绘制蛇形地暖盘管
参数:
  spacing: 管间距（默认150）
返回值: 生成的多段线列表
示例:
  (hvac:floor-heating-auto-snake 150)"
  (if (not spacing)
      (setq spacing 150))
  (princ (strcat "\n正在搜索闭合多段线..."))
  (setq ss-list nil)
  (foreach type '("LWPOLYLINE" "POLYLINE")
    (setq ss (ssget "_X" (list (cons 0 type) (cons 70 1))))
    (if ss
        (progn
          (princ (strcat "\n找到 " (itoa (sslength ss)) " 个闭合" type))
          (setq ss-list (cons ss ss-list)))))
  (if (not ss-list)
      (progn (princ "\n未找到闭合多段线!") (exit)))
  (princ "\n开始绘制盘管...")
  (setq i 0 total 0)
  (foreach ss ss-list
    (repeat (sslength ss)
      (setq ent (ssname ss i)
            box (entity:getbox ent 0)
            width (abs (- (car (cadr box)) (car (car box))))
            height (abs (- (cadr (cadr box)) (cadr (car box)))))
      (if (and (> width spacing) (> height spacing))
          (progn
            (princ (strcat "\n处理区域 " (itoa (1+ total)) "..."))
            (setq rows (fix (/ height spacing))
                  cols (fix (/ width spacing))
                  row-height (/ height (1+ rows))
                  col-width (/ width (1+ cols))
                  start-x (car (car box))
                  start-y (cadr (car box))
                  pts nil
                  y (+ start-y row-height)
                  j 0)
            (repeat (1+ rows)
              (if (evenp j)
                  (progn (setq x start-x)
                         (repeat cols
                           (setq pts (cons (list x y) pts))
                           (setq x (+ x col-width))))
                  (progn (setq x (+ start-x (* col-width cols)))
                         (repeat cols
                           (setq pts (cons (list x y) pts))
                           (setq x (- x col-width)))))
              (setq y (+ y row-height))
              (setq j (1+ j)))
            (setq pts (reverse pts))
            (if (> (length pts) 1)
                (progn
                  (setq dxf-list (list (cons 0 "LWPOLYLINE") 
                                      (cons 100 "AcDbEntity")
                                      (cons 8 "0")
                                      (cons 100 "AcDbPolyline")
                                      (cons 90 (length pts))
                                      (cons 70 0)))
                  (foreach p pts
                    (setq dxf-list (append dxf-list (list (cons 10 p) (cons 40 0) (cons 41 0) (cons 42 0)))))
                  (entmakex dxf-list)
                  (princ " 完成")
                  (setq total (1+ total))))))
      (setq i (1+ i)))
    (setq i 0))
  (princ (strcat "\n共处理 " (itoa total) " 个区域")))

(defun hvac:floor-heating-auto (spacing / ss ent box width height cx cy radius step-angle angle d pts dxf-list i)
  "自动在图中所有闭合矩形内绘制螺旋形地暖盘管
参数:
  spacing: 管间距（默认150）
返回值: 生成的多段线列表
示例:
  (hvac:floor-heating-auto 150)"
  (if (not spacing)
      (setq spacing 150))
  (princ (strcat "\n正在搜索闭合多段线..."))
  (setq ss (ssget "_X" (list (cons 0 "LWPOLYLINE") (cons 70 1))))
  (if (not ss)
      (progn (princ "\n未找到闭合多段线!") (exit)))
  (princ (strcat "\n找到 " (itoa (sslength ss)) " 个闭合区域"))
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          box (entity:getbox ent 0)
          width (abs (- (car (cadr box)) (car (car box))))
          height (abs (- (cadr (cadr box)) (cadr (car box)))))
    (if (and (> width spacing) (> height spacing))
        (progn
          (princ (strcat "\n处理第 " (itoa (1+ i)) " 个区域..."))
          (setq cx (+ (car (car box)) (/ width 2.0))
                cy (+ (cadr (car box)) (/ height 2.0))
                radius (/ (min width height) 2.0)
                step-angle (/ (* 360.0 spacing) (* 2.0 3.14159 radius))
                angle 0.0
                d spacing
                pts nil)
          (while (> radius d)
            (setq pts (cons (list (+ cx (* radius (cos angle))) (+ cy (* radius (sin angle)))) pts)
                  angle (+ angle step-angle)
                  radius (- radius (/ spacing 10.0))))
          (setq pts (reverse pts))
          (if (> (length pts) 1)
              (progn
                (setq dxf-list (list (cons 0 "LWPOLYLINE") 
                                    (cons 100 "AcDbEntity")
                                    (cons 8 "0")
                                    (cons 100 "AcDbPolyline")
                                    (cons 90 (length pts))
                                    (cons 70 0)))
                (foreach p pts
                  (setq dxf-list (append dxf-list (list (cons 10 p) (cons 40 0) (cons 41 0) (cons 42 0)))))
                (entmakex dxf-list)
                (princ " 完成")))))
    (setq i (1+ i)))
  (princ "\n全部完成!"))

(defun hvac:floor-heating (boundary spacing / box pts width height rows cols row-height col-width start-x start-y x y i start-pos)
  "在闭合多段线内绘制地暖盘管
参数:
  boundary: 闭合多段线或选择该多段线时的提示
  spacing: 管间距（默认150）
  type: 盘管类型，0=蛇形（默认），1=螺旋形
  start-corner: 起点角落，0=左下，1=左上，2=右下，3=右上（默认0）
  tube-diameter: 管径（默认0）
返回值: 生成的多段线图元名
示例:
  (hvac:floor-heating nil 150)
  (hvac:floor-heating (car (entsel)) 200 0 1)"
  (princ "\n选择地暖区域边界（闭合多段线）: ")
  (if (not boundary)
      (setq boundary (car (entsel))))
  (if (not boundary)
      (progn (princ "\n未选择边界!") (exit)))
  (if (not (equal (cdr (assoc 70 (entget boundary))) 1))
      (progn (princ "\n选择的对象不是闭合多段线!") (exit)))
  (if (not spacing)
      (setq spacing 150))
  (setq box (entity:getbox boundary 0)
        width (abs (- (car (cadr box)) (car (car box))))
        height (abs (- (cadr (cadr box)) (cadr (car box)))))
  (if (< width spacing)
      (progn (princ "\n区域宽度小于管间距!") (exit)))
  (if (< height spacing)
      (progn (princ "\n区域高度小于管间距!") (exit)))
  (setq rows (fix (/ height spacing))
        cols (fix (/ width spacing))
        row-height (/ height (1+ rows))
        col-width (/ width (1+ cols))
        start-x (car (car box))
        start-y (cadr (car box))
        start-pos 0)
  (cond
    ((= start-pos 0)
     (progn (setq start-x (car (car box)) 
                  start-y (cadr (car box)))))
    ((= start-pos 1)
     (progn (setq start-x (car (car box)) 
                  start-y (cadr (cadr box)))))
    ((= start-pos 2)
     (progn (setq start-x (car (cadr box)) 
                  start-y (cadr (car box)))))
    ((= start-pos 3)
     (progn (setq start-x (car (cadr box)) 
                  start-y (cadr (cadr box))))))
  (setq pts nil
        y (+ start-y row-height)
        i 0)
  (repeat (1+ rows)
    (if (evenp i)
        (progn (setq x start-x)
               (repeat cols
                 (setq pts (cons (list x y) pts))
                 (setq x (+ x col-width))))
        (progn (setq x (+ start-x (* col-width cols)))
               (repeat cols
                 (setq pts (cons (list x y) pts))
                 (setq x (- x col-width)))))
    (setq y (+ y row-height))
    (setq i (1+ i)))
  (setq dxf-list (list (cons 0 "LWPOLYLINE") 
                       (cons 100 "AcDbEntity")
                       (cons 8 "0")
                       (cons 100 "AcDbPolyline")
                       (cons 90 (length pts))
                       (cons 70 0)))
  (foreach p pts
    (setq dxf-list (append dxf-list (list (cons 10 p) (cons 40 0) (cons 41 0) (cons 42 0)))))
  (entmakex dxf-list))

(defun hvac:floor-heating-spiral (boundary spacing / box pts width height cx cy radius step-angle angle d)
  "在闭合多段线内绘制螺旋形地暖盘管
参数:
  boundary: 闭合多段线
  spacing: 管间距（默认150）
返回值: 生成的多段线图元名
示例:
  (hvac:floor-heating-spiral (car (entsel)) 150)"
  (if (not boundary)
      (progn (princ "\n选择地暖区域边界（闭合多段线）: ")
             (setq boundary (car (entsel)))))
  (if (not boundary)
      (progn (princ "\n未选择边界!") (exit)))
  (if (not (equal (cdr (assoc 70 (entget boundary))) 1))
      (progn (princ "\n选择的对象不是闭合多段线!") (exit)))
  (if (not spacing)
      (setq spacing 150))
  (setq box (entity:getbox boundary 0)
        width (abs (- (car (cadr box)) (car (car box))))
        height (abs (- (cadr (cadr box)) (cadr (car box)))))
  (setq cx (+ (car (car box)) (/ width 2.0))
        cy (+ (cadr (car box)) (/ height 2.0))
        radius (/ (min width height) 2.0)
        step-angle (/ (* 360.0 spacing) (* 2.0 3.14159 radius))
        angle 0.0
        d spacing)
  (setq pts nil)
  (while (> radius d)
    (setq pts (cons (list (+ cx (* radius (cos angle))) (+ cy (* radius (sin angle)))) pts)
          angle (+ angle step-angle)
          radius (- radius (/ spacing 10.0))))
(setq pts (reverse pts))
  (if (> (length pts) 1)
      (progn
        (setq dxf-list (list (cons 0 "LWPOLYLINE") 
                            (cons 100 "AcDbEntity")
                            (cons 8 "0")
                            (cons 100 "AcDbPolyline")
                            (cons 90 (length pts))
                            (cons 70 0)))
        (foreach p pts
          (setq dxf-list (append dxf-list (list (cons 10 p) (cons 40 0) (cons 41 0) (cons 42 0)))))
        (entmakex dxf-list))
      (princ "\n生成的点太少!")))