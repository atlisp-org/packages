(defun at-structure:draw-stirrup (pt-base b h)
  "绘制箍筋，b h为矩形箍内相对纵筋的宽和高，如果一个为0，则绘制单肢箍"
  
  (setq pt5 (polar pt-base (* 1.25 pi) (* 18 (sqrt 2))))
  (entity:make-point pt-base)
  (cond
   ((zerop b)
    (setq pt6 (polar pt5 (* 0.75 pi) 80)
	  pt4 (polar pt5 0 33)
	  pt3 (polar pt4 (* 0.5 pi) (+ h 36))
	  pt2 (polar pt3 pi 33)
	  pt1 (polar pt2 (* 1.25 pi) 80))
    (entity:make-lwpline-bold
     (list pt1 pt2 pt3 pt4 pt5 pt6 )
     nil 0 0 8)
    )
   ((zerop h)
    (setq pt6 (polar pt5 (* 1.75 pi) 80)
	  pt4 (polar pt5 (* 0.5 pi) 33)
	  pt3 (polar pt4 0 (+ b 36))
	  pt2 (polar pt3 (* 1.5 pi) 33)
	  pt1 (polar pt2 (* 1.25 pi) 80))
    (entity:make-lwpline-bold
     (list pt1 pt2 pt3 pt4 pt5 pt6 )
     nil 0 0 8)
    )
   (t
    (setq pt6 (polar pt5 0 (+ b 36))
	  pt4 (polar pt5 (* 0.5 pi)  (+ h 36))
	  pt3 (polar pt4 0 (+ b 36))
	  pt7 pt3
	  pt2 (polar pt3 (* 1.5 pi) 30)
	  pt1 (polar pt2 (* 1.25 pi) 80)
	  pt8 (polar pt3 pi 30)
	  pt9 (polar pt8 (* 1.25 pi) 80))
    (entity:make-point (polar pt3 (* 1.25 pi) (* 18 (sqrt 2))))
    (entity:make-lwpline-bold
     (list pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9)
     nil 0 0 8)
    )))

(defun at-structure:draw-one-rebar (pt dia)
    "绘制点筋 , pt为中心点，dia 为直径"
    (entity:make-circle pt (* 0.5 dia)))
(defun at-structure:draw-edge-rebar (start end count dia / delta)
    (if (> count 0)
	(progn
	  (setq delta (/ (distance start end)
			  (1- count)))
	  (setq i -1)
	  (repeat count
		  (at-structure:draw-one-rebar
		   (polar start
			  (angle start end)
			  (* (setq i (1+ i)) delta))
		   dia)))))
;;角筋
(defun at-structure:draw-rebar (pt-base width height cover rebar-config
					/ inner-w inner-h x1 y1 x2 y2)
  "绘制柱配筋（支持角筋+边中部钢筋+箍筋）
参数:
  pt: 插入点(左下角)
  width: 柱宽
  height: 柱高
  cover: 保护层厚度
  rebar-config: 配筋配置表，如:
    (4 25 3 22 2 20 8) 角筋4根25,b边中部3根22,h边中部2根20,箍筋8
返回: 图元名列表"
  "List"
  (setq inner-w (- width (* 2.0 cover)))
  (setq inner-h (- height (* 2.0 cover)))
  (setq pt1 (polar pt-base (* 0.25 pi)
		   (* (sqrt 2) cover)))
  (setq pt2 (mapcar '+
		    pt1
		    (list inner-w inner-h)))
  ;;角筋
  (foreach
   p (list pt1 pt2
	   (polar pt1 0 inner-w)
	   (polar pt1 (* 0.5 pi) inner-h))
   (at-structure:draw-one-rebar p (nth 1 rebar-config)))
  ;; b 中间纵筋
  (setq delta (/ (- width (* 2.0 cover))
		 (1+ (nth 2 rebar-config))))
		  
  (at-structure:draw-edge-rebar
   (polar pt1 0 delta)
   (polar pt1 0 (* delta (nth 2 rebar-config)))
   (nth 2 rebar-config)
   (nth 3 rebar-config))
  (at-structure:draw-edge-rebar
   (polar pt2 pi delta)
   (polar pt2 pi (* delta (nth 2 rebar-config)))
   (nth 2 rebar-config)
   (nth 3 rebar-config))
  ;; h 中间纵筋
  (setq delta (/ (- height (* 2.0 cover))
		  (1+ (nth 4 rebar-config))))
  (at-structure:draw-edge-rebar
   (polar pt1 (* 0.5 pi) delta)
   (polar pt1 (* 0.5 pi) (* delta (nth 4 rebar-config)))
   (nth 4 rebar-config)
   (nth 5 rebar-config))
  (at-structure:draw-edge-rebar
   (polar pt2 (* 1.5 pi) delta)
   (polar pt2 (* 1.5 pi) (* delta (nth 4 rebar-config)))
   (nth 4 rebar-config)
   (nth 5 rebar-config))
  ;; 箍筋
  (at-structure:draw-stirrup
   pt1
   inner-w inner-h
   )
  (entlast))

(defun at-structure:draw-column-section (pt width height / pts)
  "绘制混凝土框架柱断面
参数:
  pt: 插入点(左下角)
  width: 柱宽(b方向)
  height: 柱高(h方向)
返回: 生成的LWPOLYLINE图元名"
  "Ename"
  (setq pts (list pt
         (list (+ (car pt) width) (cadr pt))
         (list (+ (car pt) width) (+ (cadr pt) height))
         (list (car pt) (+ (cadr pt) height))
         pt))
  (entity:make-lwpolyline pts nil 0 1 0)
  (entity:make-text (strcat "b=" (rtos width 2 0) " h=" (rtos height 2 0))
        (polar pt (* 0.5 pi) (/ height 2))
        250 0 0.7 0 72)
  (entlast))

(defun c:col ()
  (at-structure:draw-column-section
   (setq pt-base (getpoint "base point:"))
   500 550)
  (at-structure:draw-rebar
   pt-base 500 550 35
   '(4 25 3 22 2 20 8))
  )
  
