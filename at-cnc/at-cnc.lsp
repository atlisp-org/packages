;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:define-config '@cnc:init 0 "首行是否加载初始化指令")
(@:define-config '@cnc:units 0.001  "加工精细度，即最小精度，单位为mm,默认为0.001 即1微米")
(@:define-config '@cnc:r  6.0  "刀具直径，生成刀路时将从成品边界偏移半个直径。")
(@:define-config '@cnc:motor-speed 4000 "主轴马达转速")
(@:define-config '@cnc:f 50  "进给速率")
(@:define-config '@cnc:cutter-compensation-left 0 "刀具左补偿值")
(@:define-config '@cnc:cutter-compensation-right 0 "刀具右补偿值")
(@:define-config '@cnc:chopping 0 "工作时是否加冲程")
(@:define-config '@cnc:k-thickness 0.3 "扩孔厚度")
(@:define-config '@cnc:k-times  30 "扩孔次数")
(@:define-config '@cnc:rub-times 3 "磨孔次数")
(@:define-config '@cnc:rub-f  300 "磨孔进给速率")
(@:define-config '@cnc:U-axis 1  "是否有U轴")
(@:define-config '@cnc:umotor-speed 800 "U轴马达转速")
(@:define-config '@cnc:to-origin 1  "完成后是否回库")
(@:define-config '@cnc:thickness 10.0 "要加工工件的厚度，如果设定了曲线厚度，则采用曲线厚度值")
(@:define-config '@cnc:layer-route "CNC_ROUTE" "生成的中心轴刀路曲线所在图层")
(@:define-config '@cnc:candle "" "Candle 文件路径，用于打开新生成的 nc 文件")
(@:define-config '@cnc:nc-files "CNC" "生成的NC 文件路径")
(@:define-config '@cnc:syntek 1 "SYNTEK CE系统，U轴最大行程为2.0")

;; 向系统中添加菜单 
(@:add-menus '("@CNC"
	       ("生成G代码" (at-cnc:gen-gcode))
	       ("配置@CNC" (at-cnc:setup))
	       ("删除刀路" (at-cnc:remove-route))
	       ("打开Candle" (at-cnc:open-candle))
	       ("打开NC库" (at-cnc:explorer))
	       ))
(defun at-cnc:setup (/ res)
  (@:help '("配置@CNC"))
  (setq @:tmp-search-str "@CNC")
  (@:edit-config)
  )
(defun at-cnc:n2s (num)
  "数字转字符串，一些老的CNC设备需要参数有小数点。"
  (setq num (rtos num 2 cnc:units))
  (if (null (member (ascii ".")(vl-string->list num)))
      (strcat num ".0")
    num)
  )
(defun at-cnc:motor-on (speed)
  (write-line (strcat "M3 S" (itoa(fix speed))) fp-cnc)
  )
(defun at-cnc:motor-off ()
  (write-line "M5 " fp-cnc)
  )
(defun at-cnc:umotor-on (speed)
  (write-line (strcat "M13 S" (itoa(fix speed))) fp-cnc)
  )
(defun at-cnc:umotor-off ()
  (write-line "M15 " fp-cnc)
  )
(defun at-cnc:open-candle ()
  (@:help '("用 Candle 打开生成的nc文件进行CAM"))
  (if (= "" (@:get-config '@cnc:candle))
      (if (setq path (getfiled "Candle 文件" "D:\\" "exe" 8))
	  
	  (@:set-config '@cnc:candle path))
    )
  (if (findfile(@:get-config '@cnc:candle))
      (startapp (@:get-config '@cnc:candle)
		(strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)
			"\\@cnc.nc")
		)))
(defun at-cnc:explorer ()
  (@:help '("打开生成的 NC 所在的文件夹。"))
  (startapp (strcat "explorer /e,\""
		    (setq nc-dir
			  (vl-string-trim
			   "\\/"
			   (if (member (ascii":")(vl-string->list (@:get-config '@cnc:nc-files)))
			       (@:get-config '@cnc:nc-files)
			     (strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)))))"\"")))

(defun at-cnc:remove-route ()
  (mapcar 'entdel
	  (pickset:to-list
	   (ssget "x"
		  (list
		   '(0 . "circle,lwpolyline,line")
		   (cons 8 (@:get-config '@cnc:layer-route))))))
  )

(defun at-cnc:lwpl2gcode (ent / pts bulges route cnc-f i times rub)
  (setq times (@:get-config '@cnc:k-times))
  (setq rub nil)
  (while (>= times 0)
    (if (= 3 (entity:getdxf ent 62))
	(vla-offset (e2o ent)
		    (* -0.5 (+ (@:get-config '@cnc:r)
			       (* times
				  (/ (@:get-config '@cnc:k-thickness)
				     (@:get-config '@cnc:k-times))))
		       (if (curve:clockwisep ent) 1 -1)
		       )
		    )
      (vla-offset (e2o ent)
		  (* 0.5 (+ (@:get-config '@cnc:r)
			    (* times
			       (/ (@:get-config '@cnc:k-thickness)
				  (@:get-config '@cnc:k-times))))
		     (if (curve:clockwisep ent) 1 -1)
		     )
		  ))
    (setq route (entlast))
    (entity:putdxf route 8 (@:get-config '@cnc:layer-route))
    (entity:putdxf route 62 256)
    (repeat (if (> times 0) 1 (1+ (@:get-config '@cnc:rub-times)))
	    (if (= times 0)
		(if rub
		    (setq cnc-f (@:get-config '@cnc:rub-f))
		  (progn
		    (setq rub t)
		    (setq cnc-f (@:get-config '@cnc:f))))
	      (setq cnc-f (@:get-config '@cnc:f)))
	    (setq bulges(curve:pline-convexity route))
	    (setq pts (curve:get-points route))
	    ;; 马达
	    ;;进刀
	    (setq pt0 (mapcar '- (car pts) at-cnc:pt-base))
	    (write-line
	     (strcat "G90 G00 "
		     "X" (at-cnc:n2s (car pt0)) " "
		     "Y" (at-cnc:n2s (cadr pt0)) " ")
	     fp-cnc)
            ;; 如果原来是用U 轴铣圆的，现在换主刀
	    (if pre-circle-r
		(progn
		  (write-line "M00 (pause for z spindle)" fp-cnc)
		  (setq pre-circle-r nil)))
	    (write-line (strcat "G90 G01 Z-"
				(if (and (entity:getdxf route 39)
					 (/= (entity:getdxf route 39) 0))
				    (at-cnc:n2s (abs (entity:getdxf route 39)))
				  (at-cnc:n2s (@:get-config '@cnc:thickness)))
				" F" (itoa cnc-f)" "
				)
			fp-cnc)
	    (setq pre-pt pt0)
	    (foreach pt% (cdr pts)
		     (setq pt (mapcar '- pt% at-cnc:pt-base))
		     (if (= 0 (car bulges))
			 (write-line
			  (strcat "G90 G01 "
				  "X" (at-cnc:n2s (car pt)) " "
				  "Y" (at-cnc:n2s (cadr pt)) " "
				  "Z-"           (if (and (entity:getdxf route 39)
							  (/= (entity:getdxf route 39) 0))
						     (at-cnc:n2s (abs (entity:getdxf route 39)))
						   (at-cnc:n2s (@:get-config '@cnc:thickness)))
				  "F" (itoa cnc-f)" "
				  )
			  fp-cnc)
		       (progn
			 (setq co (curve:bulge2o pre-pt pt (car bulges)))
			 (setq ij (mapcar '- co pre-pt))
			 (write-line
			  (strcat "G17 G90 G0"
				  (if (< (car bulges) 0) "2 " "3 ")
				  "X" (at-cnc:n2s (car pt)) " "
				  "Y" (at-cnc:n2s (cadr pt)) " "
				  "R" (if (> (abs (car bulges)) 1)"-" "")
				  (at-cnc:n2s (distance pre-pt co))" "
				  "Z-"				
				  (if (and (entity:getdxf route 39)
					   (/= (entity:getdxf route 39) 0))
				      (at-cnc:n2s (abs (entity:getdxf route 39)))
				    (at-cnc:n2s (@:get-config '@cnc:thickness)))
				  ;; "I" (at-cnc:n2s (car ij))" "
				  ;; "J" (at-cnc:n2s (cadr ij))" "
				  "F" (itoa cnc-f)" "
				  )
			  fp-cnc))
		       )
		     (setq pre-pt pt)
		     (setq bulges (cdr bulges))
		     )
	    (if (= 1 (entity:getdxf route 70))
		(if (= 0 (car bulges))
		    (write-line
		     (strcat "G90 G01 "
			     "X" (at-cnc:n2s (car pt0)) " "
			     "Y" (at-cnc:n2s (cadr pt0)) " "
			     "Z-"
			     (if (and (entity:getdxf route 39)
				      (/= (entity:getdxf route 39) 0))
				 (at-cnc:n2s (abs (entity:getdxf route 39)))
			       (at-cnc:n2s (@:get-config '@cnc:thickness)))           
			     "F" (itoa cnc-f)" "
			     )
		     fp-cnc)
		  (progn
		    (setq co (curve:bulge2o pre-pt pt0 (car bulges)))
		    (setq ij (mapcar '- co pre-pt))
		    (write-line
		     (strcat "G17 G90 G0"
			     (if (< (car bulges) 0) "2 " "3 ")
			     "X" (at-cnc:n2s (car pt0)) " "
			     "Y" (at-cnc:n2s (cadr pt0)) " "
			     "R" (if (> (abs (car bulges)) 1)"-" "")
			     (at-cnc:n2s (distance pre-pt co))" "
			     "Z-"
			     (if (and (entity:getdxf route 39)
				      (/= (entity:getdxf route 39) 0))
				 (at-cnc:n2s (abs (entity:getdxf route 39)))
			       (at-cnc:n2s (@:get-config '@cnc:thickness)))
			     ;; "I" (at-cnc:n2s (car ij))" "
			     ;; "J" (at-cnc:n2s (cadr ij))" "
			     "F" (itoa cnc-f)" "
			     )
		     fp-cnc))
		  )
	      ;; 不闭合的曲线，需要抬起后回到起点
	      (write-line "G90 G00 Z10" fp-cnc)
	      )
	    )
    (setq times (1- times)))
  ;;出刀
  (write-line "G90 G00 Z10" fp-cnc)
  )
(defun at-cnc:circle2gcode (ent / pts bulges route cnc-f i times rub kthickness u)
  (setq kthickness (@:get-config '@cnc:k-thickness))
  (if (> kthickness 2.0)(setq kthickness 2.0))
  (setq times (@:get-config '@cnc:k-times))
  (setq rub nil)
  (setq pt (mapcar '- (entity:getdxf ent 10)  at-cnc:pt-base))
  ;; 定位
  (write-line
   (strcat "G90 G00 "
	   "X" (at-cnc:n2s (car pt)) " "
	   "Y" (at-cnc:n2s (cadr pt)) " ")
   fp-cnc)
  ;; 开启U轴马达
  (if (= 1 (@:get-config '@cnc:U-axis))
      (at-cnc:umotor-on (@:get-config '@cnc:umotor-speed)))
  
  ;; 需重新定刀
  (if (= 1 (@:get-config '@cnc:syntek))
      (if (> (- (entity:getdxf ent 40)
		pre-circle-r)
	     (- 2.0 (@:get-config '@cnc:k-thickness)))
	  (progn
	    (write-line "M00 (pause for align U spindle)" fp-cnc)
	    (setq pre-circle-r (entity:getdxf ent 40)))
	(progn ;; 半径修正因子
	  (setq r-n (- (entity:getdxf ent 40)
		       pre-circle-r))
	  )
	))
  ;; 工作面
  (write-line
   (strcat "G01" " Z0.0"
	   " F" (itoa (@:get-config '@cnc:rub-f))" "
	   )
   fp-cnc)
  (write-line (strcat "G01"
		      " Z-"
		      (if (and (entity:getdxf ent 39)
			       (/= (entity:getdxf ent 39) 0))
			  (at-cnc:n2s (abs(entity:getdxf ent 39)))
			(at-cnc:n2s (@:get-config '@cnc:thickness)))
		      )
	      fp-cnc)
  (while (>= times 0)
    (if(< (- (entity:getdxf ent 40) (* 0.5 (@:get-config '@cnc:r)) kthickness)
    	  0)
	(progn
    	  (@:alert "刀具直径太大。无法蹚孔。")
	  (sssetfirst nil (ssadd ent))
	  (exit)))
    (if (= 3 (entity:getdxf ent 62))
	(vla-offset (e2o ent)
		    (* -0.5 (+ (@:get-config '@cnc:r)
			       (* times
				  (/ kthickness
				     (@:get-config '@cnc:k-times))))
		       (if (curve:clockwisep ent) 1 -1)
		       )
		    )
      (progn
	;; 内径分析，刀具是否可执行
	(vla-offset (e2o ent)
		    (* 0.5 (+ (@:get-config '@cnc:r)
			      (* times
				 (/ kthickness
				    (@:get-config '@cnc:k-times))))
		       (if (curve:clockwisep ent) 1 -1)
		       )
		    )
	))
    (setq route (entlast))
    (entity:putdxf route 8 (@:get-config '@cnc:layer-route))
    (entity:putdxf route 62 256)
    (setq g41 (fix(@:get-config '@cnc:cutter-compensation-left)))
    ;; U 轴
    ;; 开启U轴马达
    (repeat (if (> times 0) 1 (1+ (@:get-config '@cnc:rub-times)))
	    (if (= times 0)
		(if rub
		    (setq cnc-f (@:get-config '@cnc:rub-f))
		  (progn
		    (setq rub t)
		    (setq cnc-f (@:get-config '@cnc:f))))
	      (setq cnc-f (@:get-config '@cnc:f)))
	    (if (= 1 (@:get-config '@cnc:U-axis))
		(progn
		  ;; R平面,U定位
		  (write-line
		   (strcat "G01 U"
			   (if (= 1 (@:get-config '@cnc:syntek))
			       (strcat "-" (at-cnc:n2s
					    (+ r-n
					       (* (-  (@:get-config '@cnc:k-times) times)
						  (/ kthickness (@:get-config '@cnc:k-times))))
					    ))
			     (at-cnc:n2s (entity:getdxf route 40)))
			   " F"(itoa cnc-f)
			   )
		   fp-cnc)
		  ;; (write-line (strcat "G01"
		  ;; 		      " Z-"
		  ;; 		      (if (and (entity:getdxf route 39)
		  ;; 			       (/= (entity:getdxf route 39) 0))
		  ;; 			  (at-cnc:n2s (abs(entity:getdxf route 39)))
		  ;; 			(at-cnc:n2s (@:get-config '@cnc:thickness)))
		  ;; 		      )
		  ;; 	      fp-cnc)
		  ;; Z轴不用回
		  ;; (write-line
		  ;;  (strcat " Z0.0"
		  ;; 	   " F" (itoa cnc-f)" "
		  ;; 	   )
		  ;;  fp-cnc)
		  )
	      (progn ;; 无U轴
		(setq pt (mapcar '- (entity:getdxf route 10)  at-cnc:pt-base))
		(setq pt-arc0 (polar pt 0 (entity:getdxf route 40)))
		(setq pt-arc1 (polar pt pi (entity:getdxf route 40)))
		(write-line (strcat "G41 D"(itoa G41)) fp-cnc)
		;;进刀
		(write-line
		 (strcat "G90 G00 "
			 "X" (at-cnc:n2s (car pt-arc0)) " "
			 "Y" (at-cnc:n2s (cadr pt-arc0)) " ")
		 fp-cnc)
		(write-line (strcat "G90 G01 Z-"
				    (if (and (entity:getdxf route 39)
					     (/= (entity:getdxf route 39) 0))
					(at-cnc:n2s (abs(entity:getdxf route 39)))
				      (at-cnc:n2s (@:get-config '@cnc:thickness)))
				    " F" (itoa cnc-f)" "
				    )  
			    fp-cnc)
		(write-line
		 (strcat "G90 G02 "
			 "X" (at-cnc:n2s (car pt-arc1)) " "
			 "Y" (at-cnc:n2s (cadr pt-arc1)) " "
			 "I-"(at-cnc:n2s (entity:getdxf route 40))
			 "F" (itoa cnc-f)" "
			 )
		 fp-cnc)
		(write-line
		 (strcat "G90 G02 "
			 "X" (at-cnc:n2s (car pt-arc0)) " "
			 "Y" (at-cnc:n2s (cadr pt-arc0)) " "
			 "I"(at-cnc:n2s (entity:getdxf route 40))
			 "F" (itoa cnc-f)" "
			 )
		 fp-cnc)
		))
	    )
    (setq times (1- times)))
  (write-line "G90 G00 Z10.0" fp-cnc)
  (if (= 1 (@:get-config '@cnc:U-axis))
      (progn
	;; U轴回位,关电机
	(write-line "G90 G00 U0.0 " fp-cnc)
	(at-cnc:umotor-off)))
  ;; 出刀
  ;; (write-line "G90 G28 U0.0" fp-cnc)
  )

(defun at-cnc:gen-gcode (/ *error* curves fp-cnc filename nc-dir pre-circle-r)
  (defun *error* (msg)
    (if (= 'file (type fp-cnc))(close fp-cnc))
    (@:*error* msg))
  (setq cnc:units (round (/ (log (/ 1.0 (@:get-config '@cnc:units)))(log 10))))
  (if (null (member (@:get-config '@cnc:layer-route)(layer:list)))
      (layer:make (@:get-config '@cnc:layer-route) 2 nil nil))
  (at-cnc:remove-route)
  (setq curves (pickset:to-list (ssget '((0 . "lwpolyline,circle")))))
  ;; 按先圆后曲排序，且圆按半径从小到大排序
  (setq curves (vl-sort curves '(lambda(x y)
				  (cond
				   ((and (= "CIRCLE" (entity:getdxf x 0))
					 (= "LWPOLYLINE" (entity:getdxf y 0)))
				    t)
				   ((and (= "CIRCLE" (entity:getdxf x 0))
					 (= "CIRCLE" (entity:getdxf y 0))
					 (< (entity:getdxf x 40)
					    (entity:getdxf y 40)))
				    t)
				   ((and (= "LWPOLYLINE" (entity:getdxf x 0))
					 (= "LWPOLYLINE" (entity:getdxf y 0)))
				    t)
				   ))))
  (if (= "CIRCLE"  (entity:getdxf (car curves) 0))
      (setq pre-circle-r 0))
  (setq at-cnc:pt-base (append (car (pickset:getbox curves (+ 5 (@:get-config '@cnc:r)))) (list 0)))
  (if (member (ascii":")(vl-string->list (@:get-config '@cnc:nc-files)))
      (@:mkdir (@:path (@:get-config '@cnc:nc-files)))
    (@:mkdir (@:path (strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)))))
  (setq nc-dir
	(vl-string-trim
	 "\\/"
	 (if (member (ascii":")(vl-string->list (@:get-config '@cnc:nc-files)))
	     (@:get-config '@cnc:nc-files)
	   (strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)))))
  (setq filename (strcat  nc-dir "\\@cnc_"(datetime:current-time "yyyymodd-hhmmss")".nc"))
  (setq fp-cnc
	(open filename "w"))

  ;; 初始化
  (if (= 1 (@:get-config '@cnc:init))
	 (write-line "G71G94G80G90G40G49G50G69" fp-cnc))
  
  ;; 开启主轴马达
  (at-cnc:motor-on (@:get-config '@cnc:motor-speed))
  ;; (if (= 1 (@:get-config '@cnc:U-axis))
  ;;     (progn
  ;; 	;; U轴回位
  ;; 	(write-line "G90 G00 U0.0 " fp-cnc)
  ;; 	))
  ;; 抬起，开冷却
  (write-line "G90 G00 Z30.0 " fp-cnc)
  (write-line "M8" fp-cnc)
  (if (= 1 (@:get-config '@cnc:chopping))
      (write-line "M100" fp-cnc))
  (foreach curve curves
	   (cond
	    ((= "LWPOLYLINE" (entity:getdxf curve 0))
	     (at-cnc:lwpl2gcode curve))
	    ((= "CIRCLE" (entity:getdxf curve 0))
             (at-cnc:circle2gcode curve)
	     )
	    )
	   )
  ;; 归零
  (at-cnc:motor-off)
  (if (= 1 (@:get-config '@cnc:chopping))
      (write-line "M101" fp-cnc))
  ;; 关冷却
  (write-line "M9" fp-cnc)
  (if (= 1 (@:get-config '@cnc:to-origin))
      (write-line "G90 G00 X0.0 Y0.0 Z30.0" fp-cnc))
  (write-line "M30" fp-cnc)
  
  (close fp-cnc)
  (if (findfile (strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)
			"\\@cnc.nc"))
      (vl-file-delete
       (strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)
			"\\@cnc.nc")))
  (vl-file-copy filename
		(strcat (getenv "userprofile")"\\"(@:get-config '@cnc:nc-files)
			"\\@cnc.nc"))
  (@:prompt "生成G代码文件 @cnc.nc")
  (princ)
  )
