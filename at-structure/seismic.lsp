(@:add-menu "结构抗震" "影响系数amax" "(seismic:menu-amax)")
(@:add-menu "结构抗震" "特征周期值" "(seismic:menu-period-of-ground-motion)")
(@:add-menu "结构抗震" "防震缝宽度" "(seismic:menu-gap-of-seismic)")
(defun seismic:menu-period-of-ground-motion(/ res)
  (setq res (ui:input "特征周期值" '(("地震分组" (1 2 3) "设计地震分组")("场地类别" ("I0" "I1" "II" "III" "IV") "场地类别"))))
  (alert (strcat "地震分组：第 " (itoa (cdr (assoc "地震分组" res))) " 组    场地类别: " (cdr (assoc "场地类别" res))"\n"
		 "地震特征周期值为: " 
		 (rtos (seismic:period-of-ground-motion
			(cdr (assoc "地震分组" res))
			(vl-position (cdr (assoc "场地类别" res))'("I0" "I1" "II" "III" "IV")))
		       2 2)))
   )
(defun seismic:menu-amax(/ res)
  (setq res (ui:input "地震影响系数最大值" '(("设防烈度" (6 7 7.5 8 8.5 9) "抗震设防烈度")("是否多遇" T ))))
  (alert (strcat "抗震设防烈度：" (rtos (cdr (assoc "设防烈度" res)) 2 1) " 度  " (if (cdr (assoc "是否多遇" res)) "多遇" "罕遇")"\n"
		 "地震影响系数最大值: "
		 (rtos (seismic:amax
			(cdr (assoc "设防烈度" res))
			(cdr (assoc "是否多遇" res)))
		       2 2)))
  )
(defun seismic:menu-gap-of-seismic(/ res)
  
  (setq res (ui:input "防震缝宽度计算" '(("结构类型" ("框架结构""框架-剪力墙结构""剪力墙结构"))
					 ("设防烈度" (6 7 8 9) "抗震设防烈度")
					 ("建筑物高度" 33.0 ))))
  (alert (strcat
	  (cdr (assoc "结构类型" res)) "  "
	  (rtos (cdr (assoc "设防烈度" res)) 2 1) " 度  高度: "
	  (rtos (cdr (assoc "建筑物高度" res)) 2 3)
	  "\n防震缝宽度："
	  (rtos (seismic:gap-of-seismic
		 (cdr (assoc "设防烈度" res))
		 (cdr (assoc "结构类型" res))
		 (cdr (assoc "建筑物高度" res))
		 )
		2 2)))
  )



(defun seismic:period-of-ground-motion (group-of-seismic category-of-site / table)
  "特征周期值, group-of-seismic: 1 2 3 , 场地类别取值 I0 I1 II III IV或 0 1 2 3 4"
  (setq table '((0.20 0.25 0.35 0.45 0.65)
		(0.25 0.30 0.40 0.55 0.75)
		(0.30 0.35 0.45 0.65 0.90)))
  (if (and (<= 1 group-of-seismic 3)
	   (<= 0 category-of-site 4))
      (nth category-of-site (nth (1- group-of-seismic) table))
    nil))

(defun seismic:amax (liedu duoyu-or-hanyu / table)
  "水平地震影响系数最大值,参数：烈度 6 7 7.5 8 8.5 9,多遇罕遇 T or nil."
  
  (setq table '((0.04 0.08 0.12 0.16 0.24 0.32)
		(0.28 0.50 0.72 0.90 1.20 1.40)))
  (nth (vl-position liedu '(6 7 7.500 8 8.500 9))
       (if duoyu-or-hanyu
	   (car table)
	 (cadr table))))
  
(defun seismic:gap-of-seismic(liedu type-of-stru height / table)
  "防震缝宽度，参数：烈度，结构类型，建筑物高度"
  ""
  (setq table '(("框架结构" . 1)("框架-剪力墙结构" . 0.7)("剪力墙结构" . 0.5)))
  (if (cdr (assoc type-of-stru table))
      (max 100
	   (* (cdr (assoc type-of-stru table))
	      (+ 100
		 (* 20
		    (/ (float (- height 15))
		       (float (- 11.0 liedu))))
		 )))
    1000
    ))
  
  
