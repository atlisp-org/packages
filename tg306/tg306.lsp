;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'tg306:first 用于 应用包 tg306 的 第一个配置项 first 
(@:define-config 'tg306:first "我是配置项 tg306:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'tg306:first) ;; 获取配置顶的值
;; (@:set-config 'tg306:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "钢连接框架" "高强螺栓排" "(tg306:menu-draw-gqls)" )
(@:add-menu "钢连接框架" "绘预制梁" "(tg306:batch-draw-beam)" )
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
  (progn ;; 平面
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
    (setq pt-tmp
	  (polar pt-base pi (- (* 0.5 dm (1- m)) (* 0.5 dm)))
	  )
    (setq i 0)
    (repeat (1- m)
	    ;;上
	    (tg306:make-rec-by-center
	     (polar (polar pt-tmp 0 (* i dm))
		    (* 0.5 pi)
		    (- (* 0.5 c-h) 2.5))
	     20 125)
	    ;;下
	    (tg306:make-rec-by-center
	     (polar (polar pt-tmp 0 (* i dm))
		    (* 1.5 pi)
		    (- (* 0.5 c-h)2.5))
	     20 125)
	    (setq i (1+ i)))
    (setq pt-tmp
	  (polar pt-base (* 1.5 pi) (- (* 0.5 s (1- n)) (* 0.5 s)))
	  )
    (setq i 0)
    (repeat (1- n)
	    ;;左
	    (tg306:make-rec-by-center
	     (polar (polar pt-tmp (* 0.5 pi) (* i s))
		    pi
		    (- (* 0.5 c-b) 2.5))
	     125 20)
	    ;;右
	    (tg306:make-rec-by-center
	     (polar (polar pt-tmp (* 0.5 pi) (* i s))
		    0
		    (- (* 0.5 c-b) 2.5))
	     125 20)
	    (setq i (1+ i)))
    (progn ;; 标注
      ;; 水平
      (setq pt-hl
	    (polar 
	     (polar pt-base (* 1.5 pi) (+ 60 (* 0.5 c-h)))
	     pi (+ 60(* 0.5 c-b))))
      (setq pt-hr
	    (polar 
	     (polar pt-base (* 1.5 pi) (+ 60 (* 0.5 c-h)))
	     0 (+ 60(* 0.5 c-b))))
      (entity:dimhorizontal
       (setq pt-tmp(polar pt-hr pi 60))  pt-hr
       (polar (point:mid pt-tmp pt-hr) (* 1.5 pi) 120)
       )
      (entity:dimhorizontal
       pt-hl (setq pt-tmp (polar pt-hl 0 60))
       (polar (point:mid pt-hl pt-tmp) (* 1.5 pi) 120)
       )
      ;;总
      (entity:dimhorizontal
       pt-hl pt-hr
       (polar (point:mid pt-hl pt-hr) (* 1.5 pi) 200)
       )
      ;;螺栓
      (setq i  0)
      (repeat
       (1- m)
       (entity:dimhorizontal
	pt-tmp (setq pt-tmp2 (polar pt-tmp 0 dm))
	(polar (point:mid pt-tmp pt-tmp2) (* 1.5 pi) 120)
	)
       (setq pt-tmp pt-tmp2)
       (setq i (1+ i)))
      
      ;; 竖向
      (setq pt-vb
	    (polar 
	     (polar pt-base (* 1.5 pi) (+ 60 (* 0.5 c-h)))
	     pi (+ 60(* 0.5 c-b))))
      (setq pt-vt (polar pt-vb (* 0.5 pi) (+ c-h  60 60)))
      ;;总
      (entity:dimvertical
       pt-vb pt-vt
       (polar (point:mid pt-vb pt-vt) pi 200)
       )
      (entity:dimvertical
       (setq pt-tmp( polar pt-vt (* 1.5 pi) 60)) pt-vt
       (polar (point:mid pt-tmp pt-vt) pi 120)
       )
      (entity:dimvertical
       pt-vb (setq pt-tmp(polar pt-vb (* 0.5 pi) 60))
       (polar (point:mid pt-vb pt-tmp) pi 120)
       )
      ;;标螺栓
      (setq i  0)
      (repeat
       (1- n)
       (entity:dimvertical
	pt-tmp (setq pt-tmp2 (polar pt-tmp (* 0.5 pi) s))
	(polar (point:mid pt-tmp pt-tmp2) pi 120)
	)
       (setq pt-tmp pt-tmp2)
       (setq i (1+ i)))
      
      )
    )
  (progn  ;;; 正立面
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
    )

  (progn ;;; 侧立面
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
  
  )


(defun tg306:get-beam-h(colu)
  "取柱周梁的最大高度"
  800 ;;应该取梁高最大值这里先按800
  )

;;需读取四边梁数据
(defun tg306:column-top-elev (pt-front c-b m dm )
    "c-b 柱宽/高，m 螺栓排数，dm间距"
    (entity:make-point pt-front)

    (progn ;;顶板
      (tg306:make-rec-by-center
       (polar pt-front (* 1.5 pi) 15)
       (+ c-b 120) 30)
      (entity:putdxf ;;上柱板
       (tg306:make-rec-by-center
	(polar pt-front (* 0.5 pi) 15)
	(+ c-b 120) 30)
       62 252))
    (progn ;;方管
      (tg306:make-rec-by-center
       (polar pt-front
	      (* 1.5 pi)
	      (+ 30 (* 0.5 (+ 260 -120 b-h (* 0.5  (max c-b c-h))))))
       (+ c-b -130)
       (+ 260 b-h -120 (* 0.5 (max c-b c-h))))
      (tg306:make-rec-by-center
       (polar pt-front
	      (* 1.5 pi)
	      (+ 30 (* 0.5 (+ 260 b-h -120 (* 0.5 (max c-b c-h))))))
       (+ c-b (- -130 (* 2 square-bute-t)))
       (+ 260 b-h -120 (* 0.5 (max c-b c-h))))
      )
    (progn ;; 螺栓劲板
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
	      ;; (entity:putdxf 
	      ;;  (tg306:make-rec-by-center
	      ;;   (polar (polar pt-tmp 0 (* i dm))
	      ;; 	     (* 0.5 pi)
	      ;; 	     80)
	      ;;   20 100)
	      ;;  62 252)
	      ;;下
	      (tg306:make-rec-by-center
	       (polar (polar pt-tmp 0 (* i dm))
		      (* 1.5 pi)
		      105)
	       20 150)
	      (setq i (1+ i))))
    (progn ;;梁底接板，不同梁高怎么处理？
      (tg306:make-rec-by-center
       (polar pt-front (* 1.5 pi) (- b-h 120 10))
       (+ c-b 120) 20)
      ;; 梁腹接板，需读梁中位置及梁腹厚度，如与螺栓冲突，
      (setq hf 430
	    l1 305 )
      
      (tg306:column-top-beam
       (polar 
	(polar pt-front (* 1.5 pi) 30)
	0 (- (* 0.5 c-b) 65))
       hf l1 nil)
      (tg306:column-top-beam
       (polar 
	(polar pt-front (* 1.5 pi) 30)
	pi (- (* 0.5 c-b) 65))
       hf l1 t)
      )
    
    (progn ;; 环板
      (tg306:make-rec-by-center
       (polar pt-front (* 1.5 pi) (+ b-h -120 260 10))
       (- c-b 40) 20)
      )
    ;;梁腹接板
    
    )
(defun tg306:column-top-beam (pt-base hf l1 mirrorp)
  (setq l1 (- l1 50))
  (setq pts
	(list (list 0 0)
	      (list 0 (* -1 hf))
	      (list 125 0)
	      (list 0 30) ;; 不是50 
	      (list l1 0)
	      (list 0 (- hf 50))
	      (list (* -1 l1) 0)
	      (list 0 20)))
  (if mirrorp
      (setq pts
	    (mapcar '(lambda(x)
		       (mapcar '* '(-1 1 1)
			       x))
		    pts)))
	  
  (setq lwpts nil)
  (setq pt-tmp pt-base)
  (foreach pt pts
	   (print pt)
	   (setq lwpts
		 (cons (setq pt-tmp
			     (mapcar '+
				     pt-tmp pt))
		       lwpts)))
  (print lwpts)
  (entity:make-lwpolyline lwpts nil 0 1 0)
  
  )
;;; 柱顶连接件
(defun tg306:column-top-joint (pt-base c-b c-h f-b f-h m dm n s square-bute-t b-h / diff)
  "b-h: 梁高" 
  (progn ;; 平面
    ;;法兰
    (setq diff (* 2 60))
    (tg306:make-rec-by-center pt-base (+ c-b diff)(+ c-h diff))
    ;; 方管
    (setq diff (* 2 -65))
    (tg306:make-rec-by-center pt-base (+ c-b diff)(+ c-h diff))
    (setq diff (* 2 -85))
    (tg306:make-rec-by-center pt-base (+ c-b diff)(+ c-h diff))
    (progn;; 螺栓劲板
      (setq diff 0)
      (tg306:draw-ring-ls 
       (polar
	(polar pt-base pi (+(* 0.5 c-b) diff))
	(* 1.5 pi)
	(+ (* 0.5 c-h) diff))
       m dm n s)
      ;;  劲板
     
      (setq pt-tmp
	    (polar pt-base pi (- (* 0.5 dm (1- m)) (* 0.5 dm)))
	    )
      (setq i 0)
      (repeat (1- m)
	      ;;上
	      (tg306:make-rec-by-center
	       (polar (polar pt-tmp 0 (* i dm))
		      (* 0.5 pi)
		      (- (* 0.5 c-h) 2.5))
	       20 125)
	      ;;下
	      (tg306:make-rec-by-center
	       (polar (polar pt-tmp 0 (* i dm))
		      (* 1.5 pi)
		      (- (* 0.5 c-h)2.5))
	       20 125)
	      (setq i (1+ i)))
      (setq pt-tmp
	    (polar pt-base (* 1.5 pi) (- (* 0.5 s (1- n)) (* 0.5 s)))
	    )
      (setq i 0)
      (repeat (1- n)
	      ;;左
	      (tg306:make-rec-by-center
	       (polar (polar pt-tmp (* 0.5 pi) (* i s))
		      pi
		      (- (* 0.5 c-b) 2.5))
	       125 20)
	      ;;右
	      (tg306:make-rec-by-center
	       (polar (polar pt-tmp (* 0.5 pi) (* i s))
		      0
		      (- (* 0.5 c-b) 2.5))
	       125 20)
	      (setq i (1+ i)))
      )
    (progn ;; 标注
      ;; 水平
      (setq pt-hl
	    (polar 
	     (polar pt-base (* 1.5 pi) (+ 60 (* 0.5 c-h)))
	     pi (+ 60(* 0.5 c-b))))
      (setq pt-hr
	    (polar 
	     (polar pt-base (* 1.5 pi) (+ 60 (* 0.5 c-h)))
	     0 (+ 60(* 0.5 c-b))))
      (entity:dimhorizontal
       (setq pt-tmp(polar pt-hr pi 60))  pt-hr
       (polar (point:mid pt-tmp pt-hr) (* 1.5 pi) 120)
       )
      (entity:dimhorizontal
       pt-hl (setq pt-tmp (polar pt-hl 0 60))
       (polar (point:mid pt-hl pt-tmp) (* 1.5 pi) 120)
       )
      ;;总
      (entity:dimhorizontal
       pt-hl pt-hr
       (polar (point:mid pt-hl pt-hr) (* 1.5 pi) 200)
       )
      ;;螺栓
      (setq i  0)
      (repeat
       (1- m)
       (entity:dimhorizontal
	pt-tmp (setq pt-tmp2 (polar pt-tmp 0 dm))
	(polar (point:mid pt-tmp pt-tmp2) (* 1.5 pi) 120)
	)
       (setq pt-tmp pt-tmp2)
       (setq i (1+ i)))
      
      ;; 竖向
      (setq pt-vb
	    (polar 
	     (polar pt-base (* 1.5 pi) (+ 60 (* 0.5 c-h)))
	     pi (+ 60(* 0.5 c-b))))
      (setq pt-vt (polar pt-vb (* 0.5 pi) (+ c-h  60 60)))
      ;;总
      (entity:dimvertical
       pt-vb pt-vt
       (polar (point:mid pt-vb pt-vt) pi 200)
       )
      (entity:dimvertical
       (setq pt-tmp( polar pt-vt (* 1.5 pi) 60)) pt-vt
       (polar (point:mid pt-tmp pt-vt) pi 120)
       )
      (entity:dimvertical
       pt-vb (setq pt-tmp(polar pt-vb (* 0.5 pi) 60))
       (polar (point:mid pt-vb pt-tmp) pi 120)
       )
      ;;标螺栓
      (setq i  0)
      (repeat
       (1- n)
       (entity:dimvertical
	pt-tmp (setq pt-tmp2 (polar pt-tmp (* 0.5 pi) s))
	(polar (point:mid pt-tmp pt-tmp2) pi 120)
	)
       (setq pt-tmp pt-tmp2)
       (setq i (1+ i)))
      
      )
    )
  (progn  ;;; 立面
    (setq pt-front (polar pt-base (* 0.5 pi) (+ 2000  c-h)))
    (tg306:column-top-elev pt-front c-b m dm)
    (setq pt-left
	  (polar 
	   (polar pt-base 0 (+ (* 1.5 c-b) c-h))
	   (* 0.5 pi)
	   (* 0.5 c-h )))
    (tg306:column-top-elev pt-left c-h n s))

  )

(defun tg306:get-beam-joint-info (b  h / info)
  (setq info (assoc (strcat "矩"(itoa (fix b))"x"(itoa(fix h))) build1-beam))
  (setq joint-info (string:to-list  (substr (cadr info) 3)"x"))
  (setq height (read (car joint-info)))
  (setq width  (read (cadr joint-info)))
  (setq ft  (read (caddr joint-info)))
  (setq yt  (read (last joint-info)))
  (setq d (nth 3 info)
	n (1+ (nth 2 info))
	dm 70
	m (nth 4 info)
	s (nth 5 info)
	l1 (nth 6 info)
	l2 300)
  )
(defun tg306:get-columntop-joint-para (ct-list)
  (setq bxh
	(string:to-list (string:l2s-ansi (cdr (string:s2l-ansi (car ct-list))))"x"))
  (setq f-bxh 	(string:to-list (cadr ct-list)"x"))
  (setq c-b (read (car bxh))
	c-h (read(cadr bxh))
	f-b (read(car f-bxh))
	f-h (read(cadr f-bxh))
	m (1+ (read(nth 2 ct-list)))
	dm (read(nth 3  ct-list))
	n (1+ (read(nth 4 ct-list)))
	s (read(nth 5 ct-list))
	bute-t 20)
  (list c-b c-h f-b f-h m dm n s bute-t)
  )
(defun tg306:beam-info (beam-ent)
  "梁信息，宽高，连接件信息"
  (list
   (cons 10 (entity:getdxf beam-ent))
   (cons 'b (block:get-dynprop beamblk "宽"))
   (cons 'h (cdr (assoc "h" (block:get-attributes beam-ent))))
   (cons 'joint 0)))
(defun tg306:draw-beam (beamblk pt-base / b-b b-h)
  "绘制梁构件图"
  ;; 从梁宽高和连接表中取连接件信息
  (princ (strcat "\n绘制" (cdr (assoc "编号" (block:get-attributes beamblk)))))
  (setq b-b (block:get-dynprop beamblk "宽"))
  (setq b-h (read (cdr (assoc "高" (block:get-attributes beamblk)))))
  
  (tg306:get-beam-joint-info b-b b-h)
  ;;图框
  (setq tk 
	(block:insert "图框-好逐易工程服务" ""
		      (polar 
		       (polar pt-base 0 7000)
		       (* 1.5 pi) 3600)
		      0 0.1))
  (block:set-dynprop tk "map-sheet" "A2+0.25")
  (block:set-attributes tk (list  (cons "图名"  (cdr (assoc "编号" (block:get-attributes beamblk))))))
  ;; 左接头
  (tg306:draw-beam-joint
   (polar (polar pt-base 0 60)
	  (* 1.5 pi) (- b-h 60))
   height width ft yt d m dm n s l1 l2)
  ;;混
  (setq beam-ent
	(block:insert "预制梁-混立" ""
		      (polar pt-base 0 (+ 60 l1)) 0 1))
  (block:set-dynprop beam-ent "h" b-h)
  ;;标注
  (entity:dimhorizontal
   pt-base
   (setq pt-tmp (polar pt-base 0 (- (block:get-dynprop beamblk "标志长度") 120)))
   (polar (point:mid pt-base pt-tmp) (* 1.5 pi) (+ b-h 200))
   )
  (princ "..OK")
  )
(defun tg306:batch-draw-beam ()
  (setq beams
	(pickset:sort 
	 (pickset:to-list (block:ssget nil "beam-yz" nil))
	 "xy" 10)
	)
  (setq pt-base (getpoint))
  (foreach beam beams
	   (tg306:draw-beam beam pt-base)
	   (setq pt-base (polar pt-base 0 7600))
	   )
  
  
  )  

(setq build1-beam
      (list
       '("矩250x450" "WH330x190x10x14" 1 130 2 70 235)
       '("矩250x600" "WH480x190x14x16" 4 100 5 70 445)
       '("矩300x600" "WH480x240x10x16" 4 100 3 70 305)
       '("矩350x600" "WH480x290x10x16" 4 100 3 70 305)
       '("矩250x500" "WH380x190x8x14" 2 100 2 100 305)
       '("矩200x400" "WH280x140x6x8" 1 100 2 90 305)))     

(defun tg306:get-list-from-texts()
  (setq txts
	(pickset:sort 
	 (pickset:to-list (ssget  '((0 . "text"))))
	 "Yx"
	 '(100 100)))
  (mapcar 'tg306:get-columntop-joint-para
	  (list:split (mapcar 'text:get-mtext  txts) 7))
  )
(defun tg306:draw-colutop ()
  (setq ctop-lst (tg306:get-list-from-texts))
  (setq pt-base (getpoint))
  (foreach ctop ctop-lst
	   (apply 'tg306:column-top-joint
		  (append (list  pt-base)  ctop (list 600)))
	   (setq pt-base (polar pt-base 0 5000))
	   )
  
  )
(defun c:tt ()
  ;; (tg306:column-bottom-joint (getpoint) 500 700 620 820 6 100 6 140 20)
  ;;(tg306:column-top-joint (getpoint) 500 700 620 820 6 100 6 140 20 800)
  ;;(tg306:batch-draw-beam)
  (tg306:draw-colutop)
  )
