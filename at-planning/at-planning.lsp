;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-planning:first 用于 应用包 at-planning 的 第一个配置项 first 
(@:define-config '@planning:land-layer "用地红线,用地界线" "用地红线图层。")
(@:define-config '@planning:building-layer "建筑轮廓" "建筑基底轮廓线图层。")
(@:define-config '@planning:green-layer "绿地线" "绿地图层。")
(@:define-config '@planning:road-layer "道路" "道路图层。")
(@:define-config '@planning:openspace-layer "开敞空间" "城市开敞空间图层。")
(@:define-config '@planning:square-layer "城市广场" "城市广场图层。")
(@:define-config '@planning:parking-layer "停车场" "用于统计停车场面积的图层")
(@:define-config '@planning:parking "*车位*" "用于统计停车位的图块名")
(@:define-config '@planning:floor-area-ratio-limit 2.0 "容积率限值,单位10000m2/ha")
(@:define-config '@planning:building-density-limit 35 "建筑密度限值,单位%")
(@:define-config '@planning:greening-rate-limit 20 "绿地率限值")
(@:define-config '@planning:floor-num 3 "建筑层数，当没有给出建筑面积时，按占地面积x层数计入总面积")

;; 向系统中添加菜单 
(@:add-menus
 '("规划"
   ("规划设置"(at-planning:setup))
   ("指定范围"(at-planning:set-range))
   ("生成绿地"(at-planning:gen-greenland))
   
   ("用地面积"(at-planning:land-area))
   ("绿地面积" (at-planning:area-of-green))
   ("填充物件" (at-planning:hatch-zone))
   ("车位折减" (at-planning:reduction-green))
   ("算绿地率" (at-planning:greening-rate))
   ("数据输入" (at-planning:input))
   ("经济指标" (at-planning:make-index))
   ))

(defun at-planning:setup (/ res)
   (setq @::tmp-search-str "@planning")
  (@::edit-config-dialog))
(defun at-planning:set-range ()
  (@::help "指定要计算的矩形范围")
  (@::prompt "请指定计算范围")
  (setq at-planning:range-pt1
	(getpoint "左下角点:"))
  (setq at-planning:range-pt2
	(getcorner at-planning:range-pt1 "右上角点:"))

  (if (or (null at-planning:range-pt1)
	  (null at-planning:range-pt2))
      (at-planning:set-range)
      )
  )
(defun at-planning:zoom ()
  (if (or (null at-planning:range-pt1)
	  (null at-planning:range-pt2))
      (at-planning:set-range)
      )
  (vla-zoomwindow *ACAD* (point:to-ax at-planning:range-pt1)
		  (point:to-ax  at-planning:range-pt2)))
(defun at-planning:sum (lst)
  (cond
    ((atom lst)
     lst)
    ((listp lst)
     (apply '+ lst))))

(defun at-planning:calc (/ sum-area land-curve green-cruve)
  "计算面积指标"
  (at-planning:zoom)
  ;;地块面积
  (setq land-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
			  (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:land-layer))
				))))
  (setq @planning:*land-area* (mapcar '(lambda(x)(vla-get-area (e2o x))) land-curve))
  (if (> (at-planning:sum @planning:*land-area*) 0)
      (progn
	;; 绿地面积
	(setq green-curve (pickset:to-list
			   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
				  (list (cons 0 "lwpolyline")
					(cons 8 (@:get-config '@planning:green-layer))
					))))
	(setq @planning:*greening-area* (mapcar '(lambda(x)(vla-get-area (e2o x))) green-curve))
	(setq @planning:*green-all-area* (at-planning:sum @planning:*greening-area*))
	
	;; 林荫车位折减
	(setq lst-car (pickset:to-list
                       (ssget"w" at-planning:range-pt1 at-planning:range-pt2
			     (list (cons 0 "insert")
				   (cons 2 "林荫车位")
				   ))))
	(setq @planning:*green-reduction-area* (* -1 0.6 2.4 5.3 (length lst-car)))
	;; 绿地率
	(setq @planning:*greening-rate*
	      (/ (+ @planning:*green-all-area*
		    @planning:*green-reduction-area*
		    )
		 (at-planning:sum @planning:*land-area*))
	      )
	;; 建筑基底面积
	(setq building-curve (pickset:to-list
			 (ssget"w" at-planning:range-pt1 at-planning:range-pt2
			       (list (cons 0 "lwpolyline")
				     (cons 8 (@:get-config '@planning:building-layer))
				     ))))
	(setq @planning:*building-floor1-area* (mapcar '(lambda(x)(vla-get-area (e2o x))) building-curve))
	;; 车位
	(setq @planning:*parking-overground*
	      (length
	       (pickset:to-list
                (ssget"w" at-planning:range-pt1 at-planning:range-pt2
		      (list (cons 0 "insert")
			    (cons 2 (@:get-config '@planning:parking))
			    )))))
	)))
(defun at-planning:land-area (/ sum-area)
  (@::help "生成各用地地块的面积，及汇总面积")
  (@:prompt "请选择用地红线闭合曲线:")
  (at-planning:zoom)
  (setq lst-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
			  (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:land-layer))
				))))
  (setq sum-area 0)
  (foreach curve lst-curve
	   (setq sum-area (+ sum-area (vla-get-area (e2o curve))))
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@::draw-scale))
             0
             0.72
             0
             "mm")
            8
            (@:get-config '@planning:land-layer)
	    ))
  ;; (setq @planning:*land-area* sum-area)
  (setq @m:*result* sum-area)
  (@m:draw)
  (princ))
(defun at-planning:area-of-green (/ sum-area)
  (@::help "生成各绿地地块的面积，及汇总面积")
  (at-planning:zoom)
  (if (setq ss-tmp (ssget  "w" at-planning:range-pt1 at-planning:range-pt2
		  (list (cons 0 "text")
				  (cons 8 (@:get-config '@planning:green-layer))
				  )))
      (pickset:erase ss-tmp))
  (@:prompt "请选择绿地闭合曲线:")
  (setq lst-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
		    (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:green-layer))
				))))
  (setq sum-area 0)
  (foreach curve lst-curve
	   (setq sum-area (+ sum-area (vla-get-area (e2o curve))))
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@::draw-scale))
             0
             0.72
             0
             "mm")
            8
            (@:get-config '@planning:green-layer)
	    ))
  ;; (setq @planning:*green-all-area* sum-area)
  (setq @m:*result* sum-area)
  (@m:draw)
  (princ))
(defun at-planning:hatch-zone ()
  (@::help "重新填充建筑物、绿地、广场、开敞空间地块")
  ;; (@:prompt "请选择绿地闭合曲线:")
  (at-planning:zoom)
  (foreach hatchlayer
	   (list
	    (@:get-config '@planning:green-layer)
	    (@:get-config '@planning:building-layer)
	    (@:get-config '@planning:openspace-layer)
	    (@:get-config '@planning:square-layer))
	   (if hatchlayer
	       (progn
		 (if (setq toerase (ssget "w" at-planning:range-pt1 at-planning:range-pt2
					  (list (cons 0 "hatch")
						(cons 8 hatchlayer)
						)))
		     (pickset:erase toerase))
		 (setq lst-curve (pickset:to-list
				  (ssget
				   "w" at-planning:range-pt1 at-planning:range-pt2
				   (list (cons 0 "lwpolyline")
					 (cons 8 hatchlayer)
					 ))))
		 (setvar "hpname" "solid")
		 (setvar "hpcolor" ".")
		 (setvar "hplayer" (car (string:to-list hatchlayer",")))
		 (foreach curve lst-curve
			  (command "-hatch" "s" curve "" "")
			  ))))
  (if (setq sshatch (ssget  "w" at-planning:range-pt1 at-planning:range-pt2 '((0 . "hatch"))))
      (command  "_.draworder" sshatch ""  "B")
      )
  (princ))
(defun at-planning:reduction-green ()
  (@::help '("计算填充绿地林荫车位折减面积。"
	     "车位尺寸 2.4x5.3m2，按60%折减。"))
  (@:prompt "请选择林荫车位:")
  (setq lst-car (pickset:to-list
                 (ssget "w" at-planning:range-pt1 at-planning:range-pt2
			(list (cons 0 "insert")
			      (cons 2 "林荫车位")
			      ))))
  ;; (setq @planning:*green-reduction-area*  (* -1 0.6 2.4 5.3 (length lst-car)))
  (@:prompt (strcat "发现林荫车位 " (itoa (length lst-car))" 个"))
  (setq @m:*result*  (* -1 0.6 2.4 5.3 (length lst-car)))
  (@m:draw)
  (princ))

(defun at-planning:greening-rate ()
  (@::help "计算绿地率")
  (if (and (numberp @planning:*land-area*)
	   (numberp @planning:*green-all-area*)
	   (numberp @planning:*green-reduction-area*))
      (progn
	(setq @m:*result*
	      (/ (+ @planning:*green-all-area*
		    @planning:*green-reduction-area*
		    )
		 @planning:*land-area*))
	(@m:draw))
      (@::prompt "请先进行地块及绿地计算。"))
  (princ))
(defun at-planning:area-of-building (/ sum-area)
  (@::help "生成各建筑基底的面积，及汇总面积")
  (at-planning:zoom)
  (pickset:erase (ssget "w" at-planning:range-pt1 at-planning:range-pt2
		  (list (cons 0 "text")
				  (cons 8 (@:get-config '@planning:building-layer))
				  )))
  (@:prompt "请选择建筑基底闭合曲线:")
  (setq lst-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
		    (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:building-layer))
				))))
  (setq sum-area 0)
  (foreach curve lst-curve
	   (setq sum-area (+ sum-area (vla-get-area (e2o curve))))
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@::draw-scale))
             0
             0.72
             0
             "mm")
            8
            (@:get-config '@planning:building-layer)
	    ))
  ;;(setq @planning:*building-floor1-area* sum-area)
  (setq @m:*result* sum-area)
  (@m:draw)
  (princ))
(defun at-planning:make-index ()
  (if (or(or (null  @planning:*building-overground-area*)
	     (zerop @planning:*building-overground-area*))
	 (null  @planning:*building-underground-area*)
	 (or (null @planning:*plot-area*)
	     (zerop @planning:*plot-area*))
	 (null @planning:*parking-underground*))
      (at-planning:input))
  (if (at-planning:calc)
      (progn
	(if (zerop @planning:*building-overground-area*)
	    (progn
	      (setq @planning:*plot-area*
		    (setq @planning:*building-overground-area*
			  (* (at-planning:sum @planning:*building-floor1-area*)
			     (@::get-config '@planning:floor-num)))
		    )
	      ))
	(setq @planning:*building-all-area*
	      (+ @planning:*building-overground-area*
		 @planning:*building-underground-area*))
	(ui:dyndraw (table:make
		     '(0 0 0)
		     "主要技术经济指标一览表"
		     '("项目名称""单位""数量""备注")
		   (append
		    (list
		     (list "总建设用地" "㎡"
			   (at-planning:sum  @planning:*land-area*)
			   (strcat (rtos (/ (at-planning:sum  @planning:*land-area*) 666.6) 2 3)
				   "亩")
			   ))
		    (if (> (length @planning:*land-area*)1)
			(progn
			  (setq i 0)
			  (mapcar '(lambda(x)
				     (list (strcat "其中:  地块"(itoa (setq i (1+ i))))
					   "㎡"
					   x
				     ""))
				  @planning:*land-area*)))
		    (list
		     (list "总建筑面积" "㎡" @planning:*building-all-area* "")
		     (list "其中：地上建筑面积" "㎡" @planning:*building-overground-area* "")
		     (list "其中：地下建筑面积" "㎡" @planning:*building-underground-area* "")
		     (list "计容面积" "㎡" @planning:*plot-area* "")
		     (list "建筑基底面积" "㎡" (at-planning:sum @planning:*building-floor1-area*) "")
		     (list "绿地面积" "㎡" (+ @planning:*green-all-area*
					      @planning:*green-reduction-area*)
			   "")
		     (list "建筑密度" "%" (strcat (@:to-string(* 100 (/ (at-planning:sum @planning:*building-floor1-area*) (at-planning:sum @planning:*land-area*))))"%") (strcat "≤" (itoa(fix(@:get-config '@planning:building-density-limit))) "%"))
		     (list "容积率" "万㎡/ha"
			   (/ @planning:*plot-area*
			      (at-planning:sum @planning:*land-area*)
			      )
			   (strcat "≤"(@:to-string(@:get-config '@planning:floor-area-ratio-limit))))
		     (list "绿地率" "%" (strcat (@:to-string (* 100 @planning:*greening-rate*))"%")
			   (strcat "≥" (itoa(fix(@:get-config '@planning:greening-rate-limit))) "%"))
		     (list "非机动车停车位" "㎡" "" "")
		     (list "总车位" "个" (itoa (+ @planning:*parking-overground*
						  @planning:*parking-underground*))
			   "")
		     (list "其中：地上" "个" (itoa @planning:*parking-overground*) "")
		     (list "其中：地下" "个" (itoa @planning:*parking-underground*) "")
		     )))
		  '(0 0 0))
      
      )))
(defun at-planning:input (/  res)
  (@::help "需要手动输入总图中无法计算的数据")
  (setq res 
	(ui:input
	 "建筑数据"
	 (list (list "地上建筑面积" (if @planning:*building-overground-area*  @planning:*building-overground-area* 0.0))
	       (list "地下建筑面积" (if @planning:*building-underground-area*   @planning:*building-underground-area* 0.0))
	       (list "计容面积" (if @planning:*plot-area* @planning:*plot-area*  0.0))
	       (list "地下车位数" (if @planning:*parking-underground* @planning:*parking-underground* 0)))))
  (list
   ;; (setq @planning:*building-all-area* (cdr (assoc "建筑总面积" res)))
   (setq @planning:*building-overground-area* (cdr (assoc "地上建筑面积" res)))
   (setq @planning:*building-underground-area* (cdr (assoc "地下建筑面积" res)))
   (setq @planning:*plot-area* (cdr (assoc "计容面积" res)))
   (setq @planning:*parking-underground* (cdr (assoc "地下车位数" res)))
   ))
  
(defun at-planning:gen-greenland ()
  (@::help "在点击位置生成闭合的绿地曲线")
  (command  "boundary" (getpoint) "")
  (entity:putdxf (entlast) 8
		 (string:subst-all "" "*"
				   (car (string:to-list  (@::get-config '@planning:green-layer)","))))
  )
