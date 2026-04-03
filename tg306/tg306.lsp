;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'tg306:first 用于 应用包 tg306 的 第一个配置项 first 
(@:define-config 'tg306:first "我是配置项 tg306:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'tg306:first) ;; 获取配置顶的值
;; (@:set-config 'tg306:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "钢连接框架" "高强螺栓排" "(tg306:menu-draw-gqls)" )
(defun tg306:hello ()
  (@:help (strcat "这里的内容用于在运行这个功能开始时，对用户进行功能提示。\n"
		  "如怎么使用，注意事项等。\n当用户设置了学习模式时，会在命令行或弹窗进行提示。\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (alert (strcat "钢连接框架 的第一个功能.\n"
		 "创建了一个配置项 tg306:first .\n"
		 "这个配置项的值为: " (@:get-config 'tg306:first)
		 ))
  (princ)
  )
(defun tg306:menu-draw-gqls ()
  (setq m  (getint "水平排数: "))
  (setq dm  (getreal "水平间距: "))
  (setq n  (getint "竖向排数: "))
  (setq s  (getreal "竖向间距: "))
  (tg306:draw-gqls (getpoint "初始点: ") m dm n s))
  
(defun tg306:draw-gqls(pt-base m dm n s / i j)
  "m 水平排数，dm 水平间距，n 竖向排数，s 竖向间距"
  (setq j 0)
  (repeat
   n
   (setq i 0)
   (repeat
    m
    (block:insert "高强螺栓孔" ""
		  (polar
		   (polar pt-base (* 0.5 pi) (* j s))
		   0  (* i dm))
		  0 1)
    (setq i (1+ i)))
   (setq j (1+ j))))
(defun tg306:draw-ring-ls (pt-base m dm n s / i j)
  "m 水平排数，dm 水平间距，n 竖向排数，s 竖向间距"
  (setq j 0)
  (repeat
   n
   (setq i 0)
   (repeat
    m
    (if (or (= j 0)(= j (1- n))
	    (= i 0)(= i (1- m)))
	(block:insert "螺栓孔" ""
		      (polar
		       (polar pt-base (* 0.5 pi) (* j s))
		       0  (* i dm))
		      0 1))
    (setq i (1+ i)))
   (setq j (1+ j))))

(defun tg306:draw-yyb (pt-base l w h po)
  "绘制翼缘板,l 长，w宽,h厚度，po:bool ,po 坡口角"
  (setq yyb
	(entity:make-lwpline-bold
	 (list pt-base
	       (polar pt-base 0 l)
	       (polar (polar pt-base 0 l) (* 0.5 pi) h)
	       (polar (polar pt-base 0 10) (* 0.5 pi) h));;45度坡口
	 nil 0 1 0))
  (entity:putdxf yyb 39  w))
	  
(defun tg306:draw-fb (pt-base l w h )
  "绘制腹板,l 长，w宽,h厚度，po:bool ,是否打坡口"
  (setq fb
	(entity:make-lwpline-bold
	 (list (polar pt-base 0 35)
	       (setq pt-tmp (polar pt-base 0 l))
	       (setq pt-tmp (polar pt-tmp (* 0.5 pi) w))
	       (setq pt-tmp (polar pt-tmp  pi  (- l 35)))
	       (setq pt-tmp
		     (polar 
		      (polar pt-tmp  pi 35)
		      (* 1.5 pi) 35)
		     )
	       (setq pt-tmp (polar pt-tmp (* 1.5 pi) (- w 70))))
	 (list 0 0 0 -0.414 0 -0.414)
	 0 1 0))
  (entity:putdxf fb 39 h))

(defun tg306:draw-beam-joint  (pt-base height width ft yt d m dm n s l1 l2)
  "梁钢连接件,height 钢梁高，width,钢梁宽 yt 翼厚ft 腹厚，d m dm n s ：螺栓数据，l1，外露长度"
  (tg306:draw-yyb pt-base (+ l1 l2)  width yt (* 0.25 pi))
  (tg306:draw-yyb (polar pt-base (* 0.5 pi) (- height yt)) (+ l1 l2)  width yt (* 0.25 pi))
  (tg306:draw-fb
   (polar (polar pt-base 0 10)(* 0.5 pi)yt)
   (- (+ l1 l2) 10)
   (- height yt yt)
   ft)
  ;; 螺栓
  (tg306:draw-gqls (polar (polar pt-base 0 60) (* 0.5 pi) d)
		   m dm n s)
  ;;混凝土界线
  (entity:make-line
   (setq pt-tmp (polar pt-base 0 l1))
   (polar pt-tmp (* 0.5 pi) height))
  
  )

(defun tg306:make-rec-by-center(pt-base b h)
  (entity:make-rectangle
   (polar
    (polar pt-base pi (* 0.5 b))
    (* 1.5 pi)
    (* 0.5 h))
   (polar
    (polar pt-base 0 (* 0.5 b))
    (* 0.5 pi)
    (* 0.5 h))
   ))
  
;;; 柱底
(defun tg306:column-bottom-joint (pt-base c-b c-h f-b f-h m dm n s square-bute-t / diff)
  ;;; 平面
  ;;法兰
  (setq diff (* 2 60))
  (tg306:make-rec-by-center pt-base (+ c-b diff)(+ c-h diff))
  ;; 方管
  (setq diff (* 2 -65))
  (tg306:make-rec-by-center pt-base (+ c-b diff)(+ c-h diff))
  (setq diff (* 2 -85))
  (tg306:make-rec-by-center pt-base (+ c-b diff)(+ c-h diff))
  ;; 螺栓劲板
  (setq diff 0)
  (tg306:draw-ring-ls 
   (polar
    (polar pt-base pi (+(* 0.5 c-b) diff))
    (* 1.5 pi)
    (+ (* 0.5 c-h) diff))
   m dm n s)
  ;; TODO: 劲板
  
  
  ;;; 正立面
  (setq pt-front (polar pt-base (* 0.5 pi) (* 1.5 c-h)))
  (entity:make-point pt-front)
  ;;底板
  (tg306:make-rec-by-center
   (polar pt-front (* 0.5 pi) 15)
   (+ c-b 120) 30)
  (entity:putdxf 
   (tg306:make-rec-by-center
    (polar pt-front (* 1.5 pi) 15)
    (+ c-b 120) 30)
   62 252)

  
  ;;方管
  (tg306:make-rec-by-center
   (polar pt-front (* 0.5 pi) (+ 30 (* 0.5 (+ 200(* 0.5  (max c-b c-h))))))
   (+ c-b -130) (+ 200 (* 0.5 (max c-b c-h))))
  (tg306:make-rec-by-center
   (polar pt-front (* 0.5 pi) (+ 30 (* 0.5 (+ 200 (* 0.5 (max c-b c-h))))))
   (+ c-b (- -130 (* 2 square-bute-t))) (+ 200 (* 0.5 (max c-b c-h))))
  ;; 螺栓
  (setq pt-tmp (polar pt-front pi (* 0.5 (1- m) dm)))
  (setq i 0)
  (repeat m
	  (block:insert "螺栓立面" "" (polar pt-tmp 0 (* i dm)) 0 1)
	  (setq i (1+ i)))
  ;;劲板
  (setq pt-tmp
	(polar pt-front pi (- (* 0.5 dm (1- m)) (* 0.5 dm)))
	)
  (setq i 0)
  (repeat (1- m)
	  ;;上
	  (tg306:make-rec-by-center
	   (polar (polar pt-tmp 0 (* i dm))
		  (* 0.5 pi)
		  80)
	   20 100)
	  ;;下
	  (entity:putdxf 
	   (tg306:make-rec-by-center
	    (polar (polar pt-tmp 0 (* i dm))
		   (* 1.5 pi)
		   105)
	    20 150)
	   62 252)
	  (setq i (1+ i)))


  
  ;;; 侧立面
  (setq pt-left
	(polar 
	 (polar pt-base 0 (+ (* 1.5 c-b) c-h))
	 (* 1.5 pi)
	 (* 0.5 c-h )))
	
  (entity:make-point pt-left)
  ;;底板
  (tg306:make-rec-by-center
   (polar pt-left (* 0.5 pi) 15)
   (+ c-h 120) 30)
  (entity:putdxf 
   (tg306:make-rec-by-center
    (polar pt-left (* 1.5 pi) 15)
    (+ c-h 120) 30)
   62 252)
  ;;方管
  (tg306:make-rec-by-center
   (polar pt-left (* 0.5 pi) (+ 30 (* 0.5 (+ 200(* 0.5  (max c-b c-h))))))
   (+ c-h -130) (+ 200 (* 0.5 (max c-b c-h))))
  (tg306:make-rec-by-center
   (polar pt-left (* 0.5 pi) (+ 30 (* 0.5 (+ 200 (* 0.5 (max c-b c-h))))))
   (+ c-h (- -130 (* 2 square-bute-t))) (+ 200 (* 0.5 (max c-b c-h))))

  ;; 螺栓
  (setq pt-tmp (polar pt-left pi (* 0.5 (1- n) s)))
  (setq i 0)
  (repeat n
	  (block:insert "螺栓立面" "" (polar pt-tmp 0 (* i s)) 0 1)
	  (setq i (1+ i)))
  ;;劲板
  (setq pt-tmp
	(polar pt-left pi (- (* 0.5 s (1- n)) (* 0.5 s)))
	)
  (setq i 0)
  (repeat (1- n)
	  ;;上
	  (tg306:make-rec-by-center
	   (polar (polar pt-tmp 0 (* i s))
		  (* 0.5 pi)
		  80)
	   20 100)
	  ;;下
	  (entity:putdxf
	   (tg306:make-rec-by-center
	    (polar (polar pt-tmp 0 (* i s))
		   (* 1.5 pi)
		   105)
	    20 150)
	   62 252)
	  (setq i (1+ i)))
 
  )
(defun c:tt ()
  (tg306:column-bottom-joint (getpoint) 500 700 620 820 6 100 6 140 20)
  )
