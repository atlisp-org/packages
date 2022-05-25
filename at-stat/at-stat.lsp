;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-stat:first 用于 应用包 at-stat 的 第一个配置项 first 
;;(@:define-config 'at-stat:first "我是配置项 at-stat:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-stat:first) ;; 获取配置顶的值
;; (@:set-config 'at-stat:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "统计表格" "圆心半径" "(at-stat:stat-circle)" )
(@:add-menu "统计表格" "多段线点坐标" "(at-stat:stat-lwpl)" )
;;(@:add-menu "统计表格" "矩形" "(list-rec-wxh:stat)" )

(defun at-stat:hello ()
  (@:help (strcat "这里的内容用于在运行这个功能开始时，对用户进行功能提示。\n"
		  "如怎么使用，注意事项等。\n当用户设置了学习模式时，会在命令行或弹窗进行提示。\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (alert (strcat "统计表格 的第一个功能.\n"
		 "创建了一个配置项 at-stat:first .\n"
		 "这个配置项的值为: " (@:get-config 'at-stat:first)
		 ))
  (princ)
  )
(defun at-stat:stat-circle (/ grp-by-y grp% flag)
  ;;一条龙排序，先y
  (setq circles
	(vl-sort (pickset:to-list (ssget '((0 . "circle"))))
		 '(lambda (x y)
		    (<= (cadr (entity:getdxf x 10))
		       (cadr (entity:getdxf y 10))))))
  ;;分组
  (setq grp-by-y '())
  (setq grp% (cons (car circles) nil))
  (setq flag T)
  (foreach ent% (cdr circles)
	   (if (equal (cadr (entity:getdxf (car grp%) 10))
		      (cadr (entity:getdxf ent% 10))
		      0.001)
	       (setq grp% (cons ent% grp%))
	     (progn
	       (setq grp-by-y (cons
			       (vl-sort grp%
					'(lambda (e1 e2)
					   (if flag
					       (> (car (entity:getdxf e1 10))
						  (car (entity:getdxf e2 10)))
					     (< (car (entity:getdxf e1 10))
						(car (entity:getdxf e2 10))))))
			       grp-by-y))
	       (setq flag (not flag))
	       (setq grp% (cons ent% nil)))))
  (if grp%
      (setq grp-by-y (cons
		      (vl-sort grp%
			       '(lambda (e1 e2)
				  (if flag
				      (> (car (entity:getdxf e1 10))
					 (car (entity:getdxf e2 10)))
				    (< (car (entity:getdxf e1 10))
				       (car (entity:getdxf e2 10))))))
		      grp-by-y)))
  (setq grp-by-y (reverse grp-by-y))
  (setq n 0)
  (foreach grp% grp-by-y
	   (foreach ent% grp%
		    (entity:make-text
		     (itoa (setq n (1+ n)))
		     (entity:getdxf ent% 10)
		     (* 1.5 (entity:getdxf ent% 40))
		     0 0.8 0 "MM")))
  ;; 半径，坐标
  (setq grp-by-y (apply 'append grp-by-y))
  (table:make (getpoint ) "统计" '("No." "R" "X" "Y")
	      (mapcar '(lambda (x) (list
				    (1+ (- (length grp-by-y)
					   (length (member x grp-by-y))))
				    (entity:getdxf x 40)
				    (car (entity:getdxf x 10))
				    (cadr (entity:getdxf x 10))))
		      grp-by-y))
  )
(defun at-stat:stat-lwpl ()
  (setq pl (car (entsel "请选择一个多段线：")));;选多段线
  (setq pts (curve:pline-3dpoints pl));;取点
  (setq res '())
  (setq pre-pt (car pts))
  (setq i 100)
  (foreach pt pts
	   (entity:make-text
	    (strcat "J"(itoa (setq i (1+ i)))) pt 300 0 0.8 0 "LB") ;;点编号
	   (setq res
		 (cons (list
			(strcat "J" (itoa i))
			(car pt)
			(cadr pt)
			(distance pt pre-pt))
		       res))
	   (setq pre-pt pt))
  (setq res (reverse res))
  (table:make (getpoint "请输入表格位置点：") "表格" '("NO" "X" "Y" "L") res)
  )
  
(defun at-stat:get-wxh (ent / pts result-pts tmp-pts angle% area% i%)
  "获取宽和高的列表"
  (setq pts (@:get-lwpoints ent))
  ;; 坐标变换直到面积最小
  (setq i% 0)
  (setq tmp-pts pts)
  (setq result-pts pts)
  (setq area% (* (- (apply 'max (mapcar 'car pts))
		    (apply 'min (mapcar 'car pts))
		    )
		 (- (apply 'max (mapcar 'cadr pts))
		    (apply 'min (mapcar 'cadr pts))
		    )))
  (while (< i% 3)
    (setq angle% (- (angle (nth i% pts)(nth (1+ i%) pts))))
    (setq tmp-pts (mapcar '(lambda (x) (m:coordinate-rotate x angle%)) pts))
    (if  (> area% (* (- (apply 'max (mapcar 'car tmp-pts))
			(apply 'min (mapcar 'car tmp-pts))
			)
		     (- (apply 'max (mapcar 'cadr tmp-pts))
			(apply 'min (mapcar 'cadr tmp-pts))
			)))
	(progn
	  (setq result-angle angle%)
	  (setq result-pts tmp-pts)
	  (setq area%  (* (- (apply 'max (mapcar 'car tmp-pts))
			     (apply 'min (mapcar 'car tmp-pts))
			     )
			  (- (apply 'max (mapcar 'cadr tmp-pts))
			     (apply 'min (mapcar 'cadr tmp-pts))
			     )))))
    (setq i% (1+ i%)))
  
  (strcat (rtos (min (- (apply 'max (mapcar 'car result-pts))
			(apply 'min (mapcar 'car result-pts))
			)
		     (- (apply 'max (mapcar 'cadr result-pts))
			(apply 'min (mapcar 'cadr result-pts))
			))
		2 2)
	  "x"
	  (rtos (max (- (apply 'max (mapcar 'car result-pts))
			(apply 'min (mapcar 'car result-pts))
			)
		     (- (apply 'max (mapcar 'cadr result-pts))
			(apply 'min (mapcar 'cadr result-pts))
			))
		2 2))
  )

