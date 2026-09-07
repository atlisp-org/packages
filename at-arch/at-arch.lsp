;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-arch:first 用于 应用包 at-arch 的 第一个配置项 first 
(@:define-config '@arch:parking-blk "车位*,机械车位*" "车位图块名")
(@:define-config '@arch:parking-number-order "Yx" "编号排序方式")

;; (@:get-config 'at-arch:first) ;; 获取配置顶的值
;; (@:set-config 'at-arch:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menus
 '(("@建筑"
    ("建筑设置" "(at-arch:setup)" )
    ("未命名房间" "(at-arch:locate-unnamed-space)" )
    ("定位房间" "(at-arch:menu-locate-space)" )
    ("检查窗地比" "(at-arch:w/space)")
    ("选择同形房间" "(at-arch:sel-same-space)")
    ("选择同名房间" "(at-arch:sel-same-name)")
    ("显隐房间面积" "(at-arch:onoff-spacearea)")
    ("总建筑面积" "(at-arch:sum-spacearea)")
    ("面积汇总" "(at-arch:area-summary)")
    ("导出面积CSV" "(at-arch:export-space-csv)")
    )
   ("@建筑说明"
    ("公建说明" "(at-arch:insert-block \"公建说明\")")
    ("住宅说明" "(at-arch:insert-block \"住宅说明\")")
    ("防水专篇" "(at-arch:insert-block \"防水设计-专篇\")")
    ("绿建专篇" "(at-arch:insert-block \"绿建专篇\")")
    )
   ("@建筑车库"
    ("插入车位" "(at-arch:insert-parking)" )
    ("插入无障碍车位" "(at-arch:insert-accparking)" )
    ("插入机械车位" "(at-arch:insert-machineparking)" )
    ("车位编号" "(at-arch:parking-numbering)" )
    )
   ("@门窗统计"
    ("门窗统计表" "(at-arch:door-window-stat)")
    )
   ))

(defun at-arch:setup (/ res)
   (setq @::tmp-search-str "@arch")
   (@::edit-config-dialog))

(defun at-arch:insert-parking (/ downfile)
  (setq downfile "at-arch/车位.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))(sleep 5))
      )
  (if (findfile (strcat "packages/" downfile))
      (progn
	(ui:dyndraw
	 (block:insert "车位" (@::package-path "at-arch") '(0 0 0)0 1)
	 '(0 0 0)))
      ))
(defun at-arch:insert-accparking (/ downfile)
  (setq downfile "at-arch/车位-无障碍.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))(sleep 5))
    (ui:dyndraw
     (block:insert "车位-无障碍" (@::package-path "at-arch") '(0 0 0)0 1)
     '(0 0 0))))
(defun at-arch:insert-machineparking ()
  (setq downfile "at-arch/机械车位.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))(sleep 5))
    (ui:dyndraw
     (block:insert "机械车位" (@::package-path "at-arch") '(0 0 0)0 1)
     '(0 0 0))))

  
(defun at-arch:parking-numbering (/ parkings inputint atts)
  ;; 以下部分为你为实现某一功能所编写的代码。
  (@::prompt "请框选要进行编号的车位")
  (setq parkings
	(pickset:sort
	 (pickset:to-list  (block:ssget  nil (@::get-config '@arch:parking-blk) nil))
	 (@::get-config '@arch:parking-number-order) 
	 (mapcar '@::scale '(8 8))))

  ;; 编号

  (if (null parking-curr-number)
      (setq parking-curr-number 0))
  (if (setq inputint  (getint (strcat "请输入起始号<"(itoa (1+ parking-curr-number))">:")))
      (setq parking-curr-number (1- inputint)))
  
  (foreach park% parkings
	   (setq atts (list:sort (block:get-attributes park%)
				 '(lambda (x y)
				    (< (car x)(car y)))))
	   (foreach att atts
		    (if (wcmatch (car att) "NUMBER*")
			(block:set-attributes
			 park%
			 (list
			  (cons
			   (car att)
			   (itoa (setq parking-curr-number (1+ parking-curr-number)))))))))
  (princ)
  )
(defun at-arch:locate-unnamed-space (/ spaces)
  (if (setq spaces (ssget "x" '((0 . "TCH_SPACE")(1 . "房间"))))
      (if(setq corner (entity:getbox (ssname spaces 0) 100))
	 (command "zoom" "w" (car corner) (cadr corner)))
      (@::prompt "没有发现未命名房间。")
      ))
(defun at-arch:locate-space (code / spaces)
  (if (string:numberp  code)
      (setq spaces (ssget "x" (list '(0 . "TCH_SPACE")(cons 2 code))))
      (setq spaces (ssget "x" (list '(0 . "TCH_SPACE")
				    (cons 1 code)))))
  (if spaces
      (progn 
	(if(setq corner (entity:getbox (ssname spaces 0) 100))
	    (command "zoom" "w" (car corner) (cadr corner)))
	(princ (strcat "选中房间的总净面积："
		       (rtos (apply '+
				    (mapcar
			       '(lambda(x)
				  (entity:getdxf x 41))
			       (pickset:to-list spaces)))
			     2 3)
		       ))
	(sssetfirst nil spaces))
      (@::prompt "没有发现编号房间。")
      ))

(defun at-arch:menu-locate-space()
  (@::help '("当输入数字时以按定位。"
	     "当输入非数字时，按名称定位，支持*号通配符，如 *井，*电梯* 等 。"))
  (at-arch:locate-space (getstring "请输入房间编号或名称:")))
(defun at-arch:w/space (/ spaces)
  (@::help '("检查窗地比,将不满足要求的房间标红。"
	     "天正房间信息不会自动更新，当更改窗户型号后，需重新执行搜索房间功能。"
	     ))
  (if (setq spaces (ssget '((0 . "TCH_SPACE")(8 . "SPACE"))))
      (ui:dyndraw
       (table:make
	'(0 0 0)
	"窗地比核查表"
	(list "房间编号""房间名称""外窗面积""地面面积""窗地比""合规性")
	(mapcar
	 '(lambda(x / name ewa ua w/ua)
	   (list 
	    (vlax-get-property x 'code)
	    (setq name  (vlax-get-property x 'name))
	    (setq ewa (vlax-get-property x 'ExtWinArea))
	    (setq ua (vlax-get-property x 'useArea))
	    (setq w/ua
	     (if(not (zerop (atof ua)))(/ (atof ewa)(atof  ua))0))
	    (if (null (at-arch:check-w/ua name w/ua))
		(progn
		  (setq ci (color:interface))
		  (vla-put-colorindex ci 12)
		  (vla-put-truecolor
		   x
		   ci)
		  "X")
		(progn
		  (setq ci (color:interface))
		  (vla-put-colorindex ci 256)
		  (vla-put-truecolor
		   x
		   ci)
		""))
	    ))
	 (mapcar 'e2o (pickset:to-list spaces))))
       '(0 0 0))))

(setq at-arch:w/ua
      (list (cons "卧室,起居*,厨房"  (/ 1.0 7.0))
	    (cons "设计*,绘图*" (/ 1 4.0))
	    (cons "办公*,会议*" (/ 1 5.0))
	    (cons "复印*,档案*,*包间*" (/ 1 6.0))
	    (cons "走廊,走道,楼梯*,卫*" (/ 1 10.0))))
(defun at-arch:check-w/ua (name res / w/ua i flag)
  (setq w/ua at-arch:w/ua)
  (setq i 0)
  (setq flag t)
  (while (and
	  (nth i w/ua)
	  (not (wcmatch name (car(nth i w/ua)))))
    (setq i  (1+ i))
    )
  (if (nth i w/ua)
      (setq flag (< (cdr (nth i w/ua)) res)))
  flag)
(defun at-arch:sel-same-space (/ ent1 s1 filters)
  (@::help "选择相同面积的房间")
  (@:prompt "请点选一个图形:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" '((0 . "TCH_SPACE"))))))
  (setq filters (list
		 (cons 0
		       (entity:getdxf ent1 0))))
  (setq s1
	(vl-remove-if-not
	 '(lambda(x)
	   (and 
	    (equal
	     (entity:getdxf x 41)
	     (entity:getdxf ent1 41)
	     0.005)
	    (equal
	     (entity:getdxf x 42)
	     (entity:getdxf ent1 42)
	     10)
	    ))
	 (pickset:to-list(ssget "x" filters))))
  (sssetfirst nil (pickset:from-list s1)))

(defun at-arch:sel-same-name (/ ent1 s1 filters)
  (@::help "选择相同名称的房间")
  (@:prompt "请点选一个图形:")
  (setq ent1 (car (pickset:to-list(ssget ":E:S" '((0 . "TCH_SPACE"))))))
  (setq filters (list
		 (cons 0
		       (entity:getdxf ent1 0))
		 (cons 1 (entity:getdxf ent1 1)))
	)
  (setq s1
	(pickset:to-list(ssget "x" filters)))
  (princ (strcat "选中房间的总净面积："
		 (rtos (apply '+
			      (mapcar
			       '(lambda(x)
				  (entity:getdxf x 41))
			       s1))
		       2 3)
		 ))
  (sssetfirst nil (pickset:from-list s1)))
(defun  at-arch:onoff-spacearea ()
  (@::help "切换房间面积的显示/隐藏")
  (setq spaces (pickset:to-list (ssget "x"'((0 . "TCH_SPACE")(1 . "~*建筑面积")))))
  (mapcar
   '(lambda(x)
     (if (= "是" (vlax-get(e2o x) 'showarea ))
	 (vlax-put (e2o x) 'showarea "否")
	 (vlax-put (e2o x) 'showarea "是")))
   spaces)
  )
(defun  at-arch:sum-spacearea ()
  (@::help "求各单块建筑面积的和")
  (setq spaces (pickset:to-list (ssget "x"'((0 . "TCH_SPACE")(1 . "*建筑面积")))))
  (princ
   (setq res
	 (apply '+
		(mapcar
		 '(lambda(x)
		   (entity:getdxf x 41))
		 spaces))

	 ))
  )

;;; ============================================================
;;; 门窗统计表
;;; ============================================================
(defun at-arch:door-window-stat (/ ss doors windows stats i)
  "统计所选门窗块的型号和数量，生成统计表"
  (@:help '("选择门窗块，按型号分类统计数量，生成门窗统计表。"))
  (@:prompt "请选择需要统计的门窗块:")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
      (progn
        (setq stats nil)
        (foreach blk (pickset:to-list ss)
          (setq bname (block:get-effectivename blk))
          (if bname
              (if (assoc bname stats)
                  (setq stats (subst (cons bname (1+ (cdr (assoc bname stats))))
                                     (assoc bname stats)
                                     stats))
                (setq stats (cons (cons bname 1) stats)))))
        ;; 排序
        (setq stats (vl-sort stats '(lambda (a b) (< (car a) (car b)))))
        ;; 输出统计
        (princ "\n=== 门窗统计 ===")
        (setq i 0)
        (foreach s stats
          (setq i (1+ i))
          (princ (strcat "\n" (itoa i) ". " (car s) " x " (itoa (cdr s)))))
        (princ (strcat "\n共 " (itoa (length stats)) " 种门窗"))
        (princ (strcat "\n总计 " (itoa (apply '+ (mapcar 'cdr stats))) " 樘"))
        ;; 生成表格
        (if (ui:confirm1 "是否在图中生成统计表?" "是-否")
            (at-arch:make-table stats)))
    (@:prompt "未选中任何块。"))
  (princ))

(defun at-arch:make-table (stats / pt0 row-height col-width i)
  "生成门窗统计表"
  (setq pt0 (getpoint "\n请选择表格插入点:"))
  (if pt0
      (progn
        (setq row-height (* (@:get-config '@::draw-scale) 8))
        (setq col-width (* (@:get-config '@::draw-scale) 40))
        ;; 表头
        (entity:make-text "序号"
                          (list (+ (car pt0) (* 0.5 col-width))
                                (- (cadr pt0) (* 0.5 row-height)) 0)
                          (* row-height 0.6) 0 0.72 0 "mm")
        (entity:make-text "门窗型号"
                          (list (+ (car pt0) (* 1.5 col-width))
                                (- (cadr pt0) (* 0.5 row-height)) 0)
                          (* row-height 0.6) 0 0.72 0 "mm")
        (entity:make-text "数量"
                          (list (+ (car pt0) (* 2.5 col-width))
                                (- (cadr pt0) (* 0.5 row-height)) 0)
                          (* row-height 0.6) 0 0.72 0 "mm")
        ;; 数据行
        (setq i 0)
        (foreach s stats
          (setq i (1+ i))
          (entity:make-text (itoa i)
                            (list (+ (car pt0) (* 0.5 col-width))
                                  (- (cadr pt0) (* (+ i 0.5) row-height)) 0)
                            (* row-height 0.6) 0 0.72 0 "mm")
          (entity:make-text (car s)
                            (list (+ (car pt0) (* 1.5 col-width))
                                  (- (cadr pt0) (* (+ i 0.5) row-height)) 0)
                            (* row-height 0.6) 0 0.72 0 "mm")
          (entity:make-text (itoa (cdr s))
                            (list (+ (car pt0) (* 2.5 col-width))
                                  (- (cadr pt0) (* (+ i 0.5) row-height)) 0)
                            (* row-height 0.6) 0 0.72 0 "mm"))
        ;; 总计行
        (entity:make-text "总计"
                          (list (+ (car pt0) (* 1.5 col-width))
                                (- (cadr pt0) (* (+ i 1.5) row-height)) 0)
                          (* row-height 0.6) 0 0.72 0 "mm")
        (entity:make-text (itoa (apply '+ (mapcar 'cdr stats)))
                          (list (+ (car pt0) (* 2.5 col-width))
                                (- (cadr pt0) (* (+ i 1.5) row-height)) 0)
                          (* row-height 0.6) 0 0.72 0 "mm"))))

;;; ============================================================
;;; 房间面积报告导出
;;; ============================================================
(defun at-arch:export-space-csv (/ ss fp filepath spaces)
  "将房间面积信息导出为CSV文件"
  (@:help '("选择房间，将房间编号、名称、面积导出为CSV文件。"))
  (@:prompt "请选择需要导出的房间:")
  (setq ss (ssget '((0 . "TCH_SPACE"))))
  (if ss
      (progn
        (setq spaces (pickset:to-list ss))
        (setq filepath (getfiled "保存房间面积报告" "" "csv" 1))
        (if filepath
            (progn
              (setq fp (open filepath "w"))
              (write-line "房间编号,房间名称,面积(㎡)" fp)
              (foreach space spaces
                (write-line
                 (strcat
                  (vlax-get-property space 'code) ","
                  (vlax-get-property space 'name) ","
                  (vlax-get-property space 'useArea))
                 fp))
              (close fp)
              (@:prompt (strcat "已导出到: " filepath)))
          (@:prompt "未选择保存路径。")))
    (@:prompt "未选中任何房间。"))
  (princ))

;;; ============================================================
;;; 建筑面积汇总
;;; ============================================================
(defun at-arch:area-summary (/ ss spaces total-area area-by-name)
  "按房间名称分类汇总建筑面积"
  (@:help '("选择房间，按名称分类统计建筑面积。"))
  (@:prompt "请选择需要统计的房间:")
  (setq ss (ssget '((0 . "TCH_SPACE"))))
  (if ss
      (progn
        (setq spaces (pickset:to-list ss))
        (setq total-area 0.0)
        (setq area-by-name nil)
        (foreach space spaces
          (setq name (vlax-get-property space 'name))
          (setq area (atof (vlax-get-property space 'useArea)))
          (setq total-area (+ total-area area))
          (if (assoc name area-by-name)
              (setq area-by-name
                    (subst (cons name (+ (cdr (assoc name area-by-name)) area))
                           (assoc name area-by-name)
                           area-by-name))
            (setq area-by-name (cons (cons name area) area-by-name))))
        ;; 排序输出
        (setq area-by-name
              (vl-sort area-by-name '(lambda (a b) (> (cdr a) (cdr b)))))
        (princ "\n=== 建筑面积汇总 ===")
        (foreach item area-by-name
          (princ (strcat "\n" (car item) ": "
                         (rtos (cdr item) 2 2) " ㎡")))
        (princ (strcat "\n总计: " (rtos total-area 2 2) " ㎡")))
    (@:prompt "未选中任何房间。"))
  (princ))
