(@:add-menu "统计" "图块名" "(@:stat-block-by-name)")
(@:add-menu "统计" "块属性" "(@:menu-stat-block-by-attribute)")
(@:add-menu "统计" "动态块特性" "(@:menu-stat-block-by-properties)")
(@:add-menu "统计" "单行文本" "(@:menu-stat-text)")
(@:add-menu "统计" "文数汇总" '(at-math:sumtxt))
(@:add-menu "统计" "图元统计" "(@math:stat-entity-gui)")
(@:add-menu "统计" "图元颜色" "(@math:stat-color)")
(@:add-menu "统计" "电气设备" "(@stat:telec-equip)")
(@:add-menu "统计" "面域按层" "(@stat:region-by-layer)")
(@:add-menu "统计" "--" "--")
(@:add-menu "统计" "输出结果" "(stat:print)")
(@:add-menu "统计" "绘制结果" "(stat:draw)")
;;(@:add-menu "统计" "块属性" "(@:stat-block-by-attribute)")
(defun @:stat-block-by-name ()
  "统计选中块的块名及数量。"
  (setq @:tmp-stat-result  ;; 统计结果
	(stat:stat         ;; 统计函数
	 (mapcar (function
		  (lambda (x) ;; 匿名函数：从选择集列表中取出要统计的项。
		   (block:get-effectivename x))) ;; 示例: 从图元取出图名。
		 (pickset:to-list (ssget '((0 . "insert"))))))) ;; 要统计的图元。
  (if @:tmp-stat-result (stat:draw))
  )

(defun @:stat-block-by-attribute (attribute-name block-name)
  "统计选中的指定块名中的某一属性的值及数量。"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (if (wcmatch (block:get-effectivename x) block-name)
		       (cdr (assoc attribute-name (block:get-attributes x)))
		       )))
		 (pickset:to-list (ssget '((0 . "insert")))))))
  (if @:tmp-stat-result (stat:draw))
  )

(defun @:menu-stat-block-by-attribute (/ blk-name attribute-name)
  (setq blk-name (getstring "请输入要统计的块名称:"))
  (setq attribute-name (getstring "请输入要分类统计的块属性的名称:"))
  (@:stat-block-by-attribute attribute-name blk-name))

(defun @:stat-block-by-properties (prop-name block-name)
  "统计选中的指定块名中的某一动态块特性的值及数量。"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (if (wcmatch (block:get-effectivename x) block-name)
		       (cdr (assoc prop-name (block:get-dynamic-properties x)))
		       )))
		 (pickset:to-list (ssget '((0 . "insert")))))))
  (if @:tmp-stat-result (stat:draw)))

(defun @:menu-stat-block-by-properties (/ blk-name prop-name)
  (setq blk-name (getstring "请输入要统计的块名称:"))
  (setq prop-name (getstring "请输入要分类统计的动态块特性的名称:"))
  (@:stat-block-by-properties prop-name blk-name))


(defun @:stat-entity (stat-item ssfilter)
  "stat-item: 统计项目(dxf 组码比如图层 为8 ); ssfilter 选择集过滤"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (cdr (assoc stat-item (entget x)))))
		 (pickset:to-list (ssget ssfilter)))))
  (if @:tmp-stat-result (stat:draw)))

(defun @math:stat-color (/ ents)
  "stat-color: 统计实体的颜色"
  (prompt "请选择图元:")
  (setq ents (pickset:to-list (ssget )))
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		    (entity:get-color x)))
		 ents)))
  (if @:tmp-stat-result (stat:draw)))

(defun @math:stat-entity-gui (/ name dxf)
  (@:help "根据需要的统计项目统计选中的选择集。")
  (setq dxf nil)
  (setq name (ui:select "请选择要统计的项" (mapcar 'cdr dxf-common)))
  (foreach n% dxf-common
	   (if (= name (cdr n%))
	       (setq dxf (car n%))))
  (if dxf 
      (@:stat-entity dxf nil)))

(defun @:menu-stat-text (/ ssfilter)
  (setq strfilter (getstring "请输入文本通配符(示例 GBZ* ?BZ* LL*): "))
  (if (/= "" strfilter)
      (setq ssfilter (list  '(0 . "TEXT,TCH_TEXT") (cons 1  strfilter)))
      (setq ssfilter (list  '(0 . "TEXT,TCH_TEXT"))))
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (cdr (assoc 1 (entget x)))))
		 (pickset:to-list (ssget ssfilter)))))
  (if @:tmp-stat-result (stat:draw)))

(defun @stat:telec-equip ()
  (@:help (strcat 
	   "统计电气设备。用于天正电气带计算机名的设备块。"))
  (setq @:tmp-stat-result  ;; 统计结果
	(stat:stat         ;; 统计函数
	 (mapcar (function
		  (lambda (x) ;; 匿名函数：从选择集列表中取出要统计的项。
		    (car
		     (string:to-list 
		      (block:get-effectivename x)
		      "(")
		     ))) ;; 示例: 从图元取出图名。
		 (pickset:to-list (ssget '((0 . "insert")(2 . "*$equip*")))))))
  (if @:tmp-stat-result (stat:draw)))
(if (null dxf-common)
    (setq dxf-common
	  '((0 . "图元类型")
	    (8 . "图层")
	    (6 . "线型")
	    (62 . "颜色号")
	    (48 . "线型比例")
	    (40 . "半径"))))
      
(defun @stat:region-by-layer ()
  (@:help (strcat "按不同的图层统计面域的面积。"))
  (setq @:tmp-stat-result
	(stat:classify
	 (mapcar (function
		  (lambda (x)
		   (cons
		    (entity:getdxf x 8)
		    (vla-get-area (e2o x)))
		   ))
		 (pickset:to-list (ssget '((0 . "region"))))
		 ))))
