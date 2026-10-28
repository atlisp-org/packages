;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-arch:first 用于 应用包 at-arch 的 第一个配置项 first 
(@:define-config '@block:block-name "块名" "用于排号的块名称。可填多个，用逗号/分号/空格分隔，如 \"车位,车位-无障碍\"，多块名统一连续编号。")
(@:define-config '@block:attribute-name "属性名" "用于排号的块内属性的名称。")
(@:define-config '@block:attribute-prefix "" "用于排号的块内属性值前缀。")
(@:define-config '@block:attribute-suffix "" "用于排号的块内属性值后缀。")
(@::define-config '@block:overlay-index 0 "覆盖用于排号的块内属性原值,0全覆盖;n,替换原内容的以-分隔的第n部分。") 
(@:define-config '@block:xref-layer "xref-lock" "用于放置外部参照的图层名。")
(@:define-config '@block:sort-order "xY" "排序规则。xyXY任意两两组合,例如yX,y在前表示y坐标优先，大X表示从右到左排序。填 snake(或 S/蛇形) 则按行蛇形排序。")
(@:define-config '@block:sort-fuzz "10,10" "按位置排序时，坐标的容差。逗号用于分隔不同轴的容差。")
(@:define-config '@block:snake-row-tol 1200 "蛇形排序(snake)：同一排的 y 容差，取车位深度的一半左右。")
(@:define-config '@block:snake-min-row 5 "蛇形排序(snake)：一行数量达到该值才换向；墙边单列等零星车位不换向。")
(@:define-config '@block:size 1000 "用于检测块重叠时，块的最大尺寸。")
(@:define-config '@block:overlap-tol 0.1 "重叠检测容差：两个块包围盒的重叠量小于等于该值视为不重叠，用于排除并排块(边线相接)的误判。")
(@:define-config '@block:lib "D:/Design/standard/lib/" "以块方式管理的图库路径。")
;; (@:get-config 'at-arch:first) ;; 获取配置顶的值
;; (@:set-config 'at-arch:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单
(@:add-menus
 '((_"Block")
   ((_"Block Config") (@block:config))
   ((_"Block Replace") (@block:subst))
   ((_"block auto numbering") (@block:set-number))
   ((_"Numbering by route")(@block:numbering-by-route))
   ((_"Anyblock numbering") (@block:set-any-block-number))
   ((_"Block numbering Config") (@block:setup))
   ((_"Set as decomposable") (@block:explodable))
   ((_"Set as non decomposable") (@block:explode-disable))
   ((_"Change block base point") (@block:menu-change-base))
   ((_"Align block base point") (@block:menu-align-base))
   ((_"Insert all block") (@block:insert-all))
   ((_"Select same block") (@block:select-same))
   ((_"统计嵌套块个数") (@block:count-nested))
    ("块间复制" (@block:copy-by-blk))
   ))
(@:add-menus
 (list (strcat (_"Block") "2")
   '((_"块视图切换") "(@block:outline-dialog)")
   '((_"Positioning overlapping blocks") (@block:overblocks))
   '((_"定位任意重叠块") (@block:overblocks2))
   '((_"一炸到底") (@block:explode-all))
   '((_"写块到库") (@block:write-file))
   '((_"炸剪裁块") (@block:menu-explode-cliped))
   '((_"属性样式刷") (@block:match-att-style))
   '((_"多重转普通") (@block:minsert2insert))
   '((_"保护转块") (@block:tchpr2insert))
   '((_"分堆建块") (@block:block-cluster))
   '((_"设置分堆间隙") (@block:set-clustergap))
   '((_"按线转块") (@block:rotate-blk-by-line))
   ))
;; (@:add-menu "块操作" "连续插块" "(@block:menu-inserts)")
(defun @block:config (/ res) 
  (setq @::tmp-search-str "@BLOCK")
  (@::edit-config-dialog))
(defun @block:setup (/ block-name attribute-name en0 lst-att i% opt% initget% )
  "设置要进行编号的图块，选择一个图块，设置要处理的图块."
  (setq en0 (car (entsel "请点选一个属性块:")))
  (if (and (= "INSERT" (entity:getdxf en0 0))
	   (block:get-effectivename en0) ;; 有名
	   (= 1 (entity:getdxf en0 66))) ;; 属性块
            (progn
	      ;; 检查是否有相关属性。
	      (setq lst-att (block:get-attributes en0))
	      (cond
		((= 1 (length lst-att))
		 (@:set-config '@block:block-name (block:get-effectivename en0))
		 (@:set-config '@block:attribute-name (car (car lst-att))))
		((< 1 (length lst-att))
		 (@:set-config '@block:block-name (block:get-effectivename en0))
		 (if (and (setq attribute-name
				(ui:select "请选择要进行编号的属性:" (mapcar 'car lst-att)))
			  (assoc attribute-name lst-att))
		     (@:set-config '@block:attribute-name attribute-name)
		     (alert "所输入的属性名不是当前选中的块的属性。")))
		)
	      (alert (strcat "当前要进行编号的属性块为 \n  " (@:get-config '@block:block-name) " \n"
			     "当前要进行编号的属性名为 \n  " (@:get-config '@block:attribute-name) 
			     ))
	      (princ)
	      )
	    (progn ;; 输入图框块名
	      (alert "所选块图元不是块，或者为匿名块或无属性块，不满足本程序要求。")
	      (setq blk-name  (getstring (strcat "请输入要进行编号的图块名 <" (@:get-config '@block:block-name) ">: ")))
	      (if (/= "" blk-name)
		  (progn
		    (@:set-config '@block:block-name blk-name)
      		    (alert (strcat "当前工程管理的图框块名设置为 \n"  (@:get-config '@pm:tukuang) " 。"
				   "\n 因为是手动输入的，有可能不满足程序要求。"))
		    (if (/= "" (setq attribute-name (getstring (strcat "请输入要进行编号的图块的属性名 < " (@:get-config '@block:attribute-name) " >: "))))
			(@:set-config '@block:attribute-name attribute-name))
	      (princ))))
  
	    ))

(defun @block:name-list (x / lst res)
  "把块名(字符串或列表)规范化为去重的块名列表。
  x: 字符串(逗号/分号/空格分隔多个块名)或块名列表。
  return: 块名列表；空值返回 nil。
  示例: (@block:name-list \"车位,车位-无障碍\") => (\"车位\" \"车位-无障碍\")"
  (setq lst (cond ((null x) nil)
                  ((listp x) x)
                  ((= 'str (type x))
                   (string:to-list (vl-string-translate "; \t" ",,," (vl-string-trim " \t" x)) ","))
                  (t nil)))
  (setq res nil)
  (foreach s lst
           (setq s (vl-string-trim " \t" s))
           (if (and (/= "" s) (not (member s res)))
               (setq res (cons s res))))
  (reverse res))
(defun @block:block-names ()
  "当前编号配置的块名列表(@block:block-name，多个块名用逗号/分号/空格分隔)。
  return: 块名列表；未配置返回 nil。"
  (@block:name-list (@:get-config '@block:block-name)))
(defun @block:block-filter (names / lst)
  "构造编号用的 ssget 过滤器：属性块且块名属于 names，另含匿名块(用于动态块)。
  names: 块名列表，如 (\"车位\" \"车位-无障碍\")。
  return: ssget 过滤器列表。
  示例: (ssget (@block:block-filter '(\"车位\" \"车位-无障碍\")))"
  (setq lst (list '(0 . "insert") '(66 . 1) '(-4 . "<or")))
  (foreach n names
           (setq lst (append lst (list (cons 2 n)))))
  (append lst (list (cons 2 "`**") '(-4 . "or>"))))
(defun @block:has-attribute-p (blk attname / atts)
  "块参照是否含有指定属性(标签不区分大小写)。
  blk: 块参照 ename；attname: 属性名。
  return: 有 T，无 nil。
  示例: (@block:has-attribute-p blkref \"NUMBER\")"
  (and (setq atts (block:get-attributes blk))
       (member (strcase attname)
               (mapcar '(lambda (a) (strcase (car a))) atts))))
(defun @block:missing-attribute-hint (blks attname / names s)
  "对缺少指定属性的块打印提示(princ，不弹窗)，并把提示存到 @block:last-hint 供 AI/脚本读取。
  blks: 缺少属性的块 ename 列表；attname: 属性名。
  return: 跳过的块数。
  示例: (@block:missing-attribute-hint blks \"NUMBER\")"
  (setq names nil)
  (foreach e blks
           (setq s (block:get-effectivename e))
           (if (not (member s names)) (setq names (cons s names))))
  (setq s nil)
  (foreach n (reverse names)
           (setq s (cons (strcat n " "
                                 (itoa (length (vl-remove-if-not
                                                '(lambda (e) (= (block:get-effectivename e) n))
                                                blks))))
                         s)))
  (setq @block:last-hint
        (strcat "已跳过 " (itoa (length blks)) " 个没有属性 \"" attname "\" 的块（"
                (string:from-list (reverse s) "、")
                "）：这类块无法写属性编号，需要时请改用文字编号。"))
  (princ (strcat "\n[编号] " @block:last-hint))
  (length blks))
(defun @block:number-string (blk n attname / raw idx num)
  "按编号配置生成块 blk 的编号字符串。
  n: 序号(数字)；attname: 属性名。
  return: 前后缀+编号字符串；@block:overlay-index 非 0 时按原值 \"-\" 分段只替换该段。
  示例: (@block:number-string blkref 3 \"NUMBER\") => \"03\""
  (setq num (string:number-format (itoa n) 2 0 "0")
	idx (@:get-config '@block:overlay-index))
  (strcat (@:get-config '@block:attribute-prefix)
	  (if (and idx (/= 0 idx)
		   (setq raw (string:to-list
			      (cdr (assoc attname (block:get-attributes blk)))
			      "-")))
	      (string:from-list
	       (list:replace-index raw
				   (if (>= (length raw) idx)
				       (1- idx)
				       (1- (length raw)))
				   num)
	       "-")
	      num)
	  (@:get-config '@block:attribute-suffix)))
(defun @block:sort-by-snake (blks rowtol minrow / items rows cur cury main loose tmp dir res)
  "按行列蛇形排序，适合车位/桩位等成排布置的对象。
  blks: 块参照 ename 列表。
  rowtol: 同一排的 y 容差(默认 1200)，取车位深度的一半左右。
  minrow: 一行数量达到该值才算“成排”(默认 5)并参与换向。
  return: 排序后的 ename 列表。
  说明: 用包围盒中心分行(有些块插入点不在块上)，成排的行内按 x 排序，
        第 1 排从左到右、整排后换向，形成蛇形；
        不足 minrow 的零星块(墙边单列等)排在最后，按 x 分列从左到右、列内自上而下。
  示例: (@block:sort-by-snake blks 1200 5)"
  (setq rowtol (if (numberp rowtol) rowtol 1200.0)
        minrow (if (numberp minrow) minrow 5))
  (setq items
        (mapcar
         '(lambda (x / bx p)
            (setq bx (vl-catch-all-apply 'entity:getbox (list x 0)))
            (if (and bx (listp bx) (cadr bx))
                (list x (/ (+ (car (car bx)) (car (cadr bx))) 2.0)
                      (/ (+ (cadr (car bx)) (cadr (cadr bx))) 2.0))
                (progn
                  (setq p (entity:getdxf x 10))
                  (list x (car p) (cadr p)))))
         blks))
  ;; 按 y 从大到小分行(每行以行首 y 为参考，同一行 y 差不超过 rowtol)
  (setq items (vl-sort items '(lambda (a b) (> (caddr a) (caddr b)))))
  (setq rows nil cur nil cury nil)
  (foreach it items
           (if (and cur (<= (abs (- cury (caddr it))) rowtol))
               (setq cur (cons it cur))
               (progn
                 (if cur (setq rows (cons (cons cury (reverse cur)) rows)))
                 (setq cur (list it)
                       cury (caddr it)))))
  (if cur (setq rows (cons (cons cury (reverse cur)) rows)))
  (setq rows (reverse rows))
  ;; 成排走蛇形；零星块(墙边单列)放最后按列排
  (setq main nil
        loose nil)
  (foreach r rows
           (if (>= (length (cdr r)) minrow)
               (setq main (cons r main))
               (setq loose (append loose (cdr r)))))
  (setq main (vl-sort main '(lambda (a b) (> (car a) (car b)))))
  (setq dir 1
        res nil)
  (foreach r main
           (setq cur (vl-sort (cdr r) '(lambda (a b) (< (cadr a) (cadr b)))))
           (if (= dir -1) (setq cur (reverse cur)))
           (setq res (append res (mapcar 'car cur)))
           (setq dir (- dir)))
  (setq tmp nil)
  (foreach it loose
           (setq tmp (cons (list (fix (/ (+ (cadr it) 50.0) 100.0))
                                 (- (caddr it))
                                 (car it))
                           tmp)))
  (setq tmp (vl-sort tmp
                     '(lambda (a b)
                        (if (= (car a) (car b))
                            (< (cadr a) (cadr b))
                            (< (car a) (car b))))))
  (append res (mapcar 'caddr tmp)))
(defun @block:set-number (/ start ss-list ss1 fuzz names)
  
  (if (null (setq names (@block:block-names)))
      (@block:setup))
  (if (progn
	(setq ss1 (ssget (@block:block-filter names)))
	(setq ss-list
	      (vl-remove-if-not
	       '(lambda(x)
		  (member (block:get-effectivename x) names))
	       (pickset:to-list ss1))))
      (progn
	;; 排序
	(sssetfirst nil (pickset:from-list ss-list))
	(setq fuzz (mapcar 'atof (string:to-list (@:get-config '@block:sort-fuzz)",")))
	;; 排序：snake/S/蛇形 走行列蛇形，否则按 sort-order 的 xyXY 组合排序
	(setq ss-list
	      (if (member (strcase (@:get-config '@block:sort-order))
			  '("SNAKE" "S" "蛇形"))
		  (@block:sort-by-snake ss-list
					(@:get-config '@block:snake-row-tol)
					(@:get-config '@block:snake-min-row))
		  (pickset:sort ss-list (@:get-config '@block:sort-order) fuzz)))
	(setq start (getint "请输入块起始编号<1>:"))
	(if (null start) (setq start 1))
	(@block:number-blocks ss-list (@:get-config '@block:attribute-name) start))
      (progn
	(alert "未选中设置的图块。请设置要进行操作的图块。")
	(@block:setup))))

(defun @block:ai-number-by-snake (blkname attname start ss / names blks)
  "AI 调用入口：按行列蛇形顺序给指定属性块连续编号，无交互、不弹窗。
  blkname: 块名或块名列表(可用逗号/分号/空格分隔，如 \"车位,车位-无障碍\")。
  attname: 块内属性名。
  start: 起始编号(缺省 1)。
  ss: 限定范围(选择集或 ename 列表)；**全图传 nil**（AutoLISP 实参不可省略，少传会报“参数太少”）。
  return: 按编号顺序的块 ename 列表；未找到匹配块或参数不合法返回 nil。
  说明: 排序规则同 @block:sort-by-snake（成排蛇形，零星车位按列排在最后），
        行容差取 @block:snake-row-tol，成排阈值取 @block:snake-min-row；
        编号串按编号配置生成（前后缀、@block:overlay-index）。
  示例: (@block:ai-number-by-snake "车位" "NUMBER" 1 nil)
        (@block:ai-number-by-snake "车位,车位-无障碍" "NUMBER" 101 nil)
        (@block:ai-number-by-snake "车位" "NUMBER" 1 (ssget))"
  (setq names (@block:name-list blkname))
  (setq blks (cond ((null ss) (pickset:to-list (ssget "x" '((0 . "INSERT")))))
                   ((= 'pickset (type ss)) (pickset:to-list ss))
                   ((listp ss) ss)
                   (t (list ss))))
  (setq blks (vl-remove-if-not
              '(lambda (x)
                (and (= "INSERT" (entity:getdxf x 0))
                     (member (block:get-effectivename x) names)))
              blks))
  (if (and blks attname)
      (@block:number-blocks
       (@block:sort-by-snake blks
                             (@:get-config '@block:snake-row-tol)
                             (@:get-config '@block:snake-min-row))
       attname start)))
(defun @block:set-any-block-number (/ num1 start ss-list ss1 )
  (@::prompt "任意块编号，含有给定属性的任意块编号。")
  (if (= "" (@:get-config '@block:block-name))
      (@block:setup))
  (if (setq ss1 (ssget ;; "_C" pt1 (getcorner pt1 "\n选择对象:")
		 (list (cons 0 "insert"))))
      (progn
	(setq ss-list (pickset:to-entlist ss1))
	(setq ss-list
	      (vl-sort ss-list
		       '(lambda (en1 en2)
			 (if (> (caddr (assoc 10 (entget en1)))
				(+ (@:scale 10) (caddr (assoc 10 (entget en2)))))
			     T
			     (if (and (equal (caddr (assoc 10 (entget en1)))
					     (caddr (assoc 10 (entget en2)))
					     (@:scale 10))
				      (< (cadr (assoc 10 (entget en1)))
					 (cadr (assoc 10 (entget en2)))))
				 T
				 nil)
			     ))))
	(setq start (getint "请输入块起始编号<1>:"))
	(if (null start) (setq start 1))
	(setq num1 0)
	(foreach en0 ss-list
		 (if (member (@:get-config '@block:attribute-name)
			     (mapcar 'car (block:get-attributes en0)))
		     (progn
		       (block:set-attributes
			en0
			(list (cons (@:get-config '@block:attribute-name)
				    (strcat
				     (@:get-config '@block:attribute-prefix)
				     (if (< (+ num1 start) 10) "0" "")
				     (itoa (+ num1 start))
				     (@:get-config '@block:attribute-suffix)
				     ))))
		       (setq num1 (1+ num1))))
		 ))
      (progn
	(alert "未选中设置的图块。请设置要进行操作的图块。")
	(@block:setup))))

(defun @block:subst ()
  (@::prompt (strcat "将目标块替换成源块. \n"
		  "步骤: \n"
		  "      1. 单选源块；\n"
		  "      2. 框选目标块。"))
  
  (setq blk-src (car (entsel "请选择源块: ")))
  (princ "\n")
  (prompt "请选择目标块:")
  ;; 选择块并将选择集转换为块图元列表
  (setq blks-target (pickset:to-list (ssget '((0 . "insert"))))) 
  ;; 取源块的dxf 组码 2 的值，并将该值 设置目标块的组码 2 的值.
  (mapcar '(lambda (x) (entity:putdxf x 2
			(entity:getdxf blk-src 2)))
	  blks-target))
		  
(defun @block:explodable (/ blk )
  (@::prompt (strcat "将目标块设置为可分解. \n"
		  "步骤: \n"
		  "      1. 单选设置为可分解的块；\n"))
  
  (setq blk (car (entsel "请选择块: ")))
  (if (= "INSERT" (entity:getdxf blk 0))
      (vla-put-explodable
       (vla-item *BLKS* (block:get-effectivename blk))
       :vlax-true)
  ))
		  
(defun @block:explode-disable (/ blk )
  (@::prompt (strcat "将目标块设置为不可分解. \n"
		  "步骤: \n"
		  "      1. 单选设置为不可分解的块；\n"))
  
  (setq blk (car (entsel "请选择块: ")))
  (if (= "INSERT" (entity:getdxf blk 0))
      (vla-put-explodable
       (vla-item *BLKS* (block:get-effectivename blk))
       :vlax-false)
      ))
(defun @block:get-corner (blk)
  (entity:getbox blk 0)
  )
(defun @block:outline-dialog (/ dcl_fp dcl-tmp dcl_id pkg para% menu%
			   frames curr-page per-page after-panel-cmd
			   zoom-w
			   run-function after-panel corner
			   page-up page-down *error*)
  "属性块大纲，用于快速切换块视图"
  (@::prompt "属性块大纲，用于快速切换块视图\n不支持动态块。")
  (defun *error* (msg)
    ;; 重启动处理 
    (if (= 'file (type dcl_fp))
	(close (dcl_fp)))
    (princ (strcat msg ))
    (princ))
  (setq frames (pickset:to-list (ssget "x" (list '(0 . "insert")
						 (cons 2 (@:get-config '@block:block-name))))))
  (setq frames (vl-sort
		frames
		'(lambda (x y)
		  (< 
		   (cdr (assoc (@:get-config '@block:attribute-name) (block:get-attributes x)))
		   (cdr (assoc (@:get-config '@block:attribute-name) (block:get-attributes y)))))))
  (setq per-page (@:get-config '@:outline-per-page))
  (if (<= (length frames) per-page)
      (setq curr-page 0)
      (setq curr-page (@:get-config '@:outline-curr-page)))
  (defun zoom-w (corner)
    (command "zoom" "w" (car corner) (cadr corner)))
  (defun after-panel ( func )
    (if (= 'str (type func))
	(eval (read func))))
  (defun page-up ()
    (setq curr-page (1- curr-page))
    (@:set-config '@:outline-curr-page curr-page)
    (done_dialog 10)
    (setq after-panel-cmd "(@block:outline-dialog)"))
  (defun page-down ()
    (setq curr-page (1+ curr-page))
    (@:set-config '@:outline-curr-page curr-page)
    (done_dialog 10)
    (setq after-panel-cmd "(@block:outline-dialog)"))
  (defun run-function (corner)
    ;; (princ func)
    (done_dialog 10)
    (setq after-panel-cmd corner))
  (if frames
      (progn
	;; 生成 dcl 文件
	(setq dcl-tmp (strcat @:*tmp-path* "tmp-outline-panel.dcl" ))
	(setq dcl_fp (open dcl-tmp "w"))
	(write-line (strcat "panel : dialog {"
			    "label = \"属性块切换\"; ")
		    dcl_fp)
	(setq i% 0)(setq bt-width 38)
	(write-line ":image{ height=0.1; color=250; fixed_height=true;}:row{label=\"\";" dcl_fp)
	(setq c% 0)(setq j% 0)
	;;(setq bt-menu-column (nth (+ c% (* per-page curr-page)) menus-list))
	;; 一列数据
	(foreach blk%  frames
		 (if (= 0 (rem j% per-page))
		     (progn
		       (setq flag-col T)
			 (write-line (strcat ":column{label=\"\";children_alignment=top;fixed_width=true;children_fixed_width=true;width="
					     (itoa bt-width) ";" )
				     dcl_fp)))
		   (setq r% 0)
		   (progn
		     (write-line (strcat ":button{ fixed_width=true;children_fixed_width=true;children_alignment=left;width="
					 (itoa bt-width)
					 ";fixed_height=true;"
					 " key=\"c""_"(itoa (setq j% (1+ j%)))"\"; "
					 "label=\""
					 ;; 显示的序列文本
					 (cdr (assoc (@:get-config '@block:attribute-name) (block:get-attributes blk%)))
					 "\"; "
					 " action=\"(run-function \\\"(zoom-w '"
					 (@:string-subst "\\\\\\\"" "\"" (vl-prin1-to-string (@block:get-corner blk%)))
					 ")\\\")\";is_enabled=true;}")
				 dcl_fp)
		     (setq r% (1+ r%)))
		   ;;(write-line "}" dcl_fp)
		   (setq c% (1+ c%))(setq i% (1+ i%))
		   (if (or (and flag-col  (= 0 (rem j% per-page)))
			   (= j% (length frames)))
		       (progn 
			 (write-line "}" dcl_fp)
			 (setq flag-col nil)))
		   )
	(write-line "}" dcl_fp)
	(write-line ":image{ height=0.1; color=250; fixed_height=true;}" dcl_fp)
	;;分页
	;; (if (> (length frames) per-page)
	;;      (write-line ":row{alignment=centered;children_alignment=centered;:button{label=\"<\";key=\"prev\";is_enabled=false;}:spacer{} :text_part{key=\"curr_total\"; value=\"\";alignment=\"centered\";width=10;}:button{label=\">\";key=\"next\";is_enabled=false;}}"
	;;  		dcl_fp))
	(write-line " :spacer {} ok_cancel; }" dcl_fp)
	(close dcl_fp)
	
	(setq dcl_id (load_dialog dcl-tmp))
	(if (not (new_dialog "panel" dcl_id))
	    (exit))
	
	(action_tile "accept" "(done_dialog 1)")
	(action_tile "prev" "(page-up)")
	(action_tile "next" "(page-down)")
	(if (= 0 curr-page) (mode_tile "prev" 1) (mode_tile "prev" 0))
	(if (= (/ (1- (length frames)) per-page) curr-page) (mode_tile "next" 1) (mode_tile "next" 0))
	(start_dialog)
	(unload_dialog dcl_id)
	(vl-file-delete dcl-tmp)
	(after-panel after-panel-cmd))
      ))
(defun @block:select-same (/ blk blks)
  (@::prompt (strcat "选择一个块，然后选中选定范围或全部的同名块。"))
  (prompt  "请选择一个块:")
  (while (null(and (setq blk (ssget "_:S:E" '((0 . "insert"))))
		   (setq blk (ssname blk 0))))
    (@:prompt "\n未选中块，请选择一个块:"))
  (@:prompt "请框选范围.如果没有给定范围，则选中所有同名块.")
  (if (null(setq blks (pickset:to-list (ssget '((0 . "INSERT"))))))
      (setq blks (pickset:to-list (ssget "x" '((0 . "INSERT"))))))
  
  (setq blks (vl-remove-if '(lambda (x) (/= (block:get-effectivename blk)
					 (block:get-effectivename x)))
			   blks))
  (sssetfirst nil (pickset:from-entlist blks))
  (princ))
(defun @block:rename-noname (/ name newname i)
  (setq name "atlisp.cn")
  (vlax-for
   blk *BLKS*
   (if (= "" (vla-get-name blk))
       (progn
	 (setq i 0)
	 (while (tblsearch "block" (strcat name (itoa (setq i (1+ i))))))
	 (setq newname (strcat name (itoa i)))
	 (vla-put-name blk newname)))))


(defun @block:menu-change-base(/ blkref blkname pt pt-oldbase pt-nb v1)
  (setq blkref (car (entsel "请选择要改变基点的块:")))
  (setq blkname (entity:getdxf blkref 2))
  (setq pt (getpoint (entity:getdxf blkref 10) "请选择目标基点:"))
  (setq pt-oldbase
	(vlax-safearray->list
	 (vlax-variant-value (vla-get-origin (block:get-obj-by-name blkname)))))
  (setq pt-nb (block:wcs2bcs
	       pt
	       pt-oldbase
	       (entity:getdxf blkref 10)
	       (entity:getdxf blkref 50)
	       (entity:getdxf blkref 41)
	       ))
  (setq v1 (mapcar '- pt-nb pt-oldbase))
  (vla-put-origin
   (block:get-obj-by-name blkname)
   (vlax-3d-point pt-nb))
  ;; TODO: 平移已有块
  (if (ui:confirm1 "是否保持已有块显示位置不变?" "是-否")
      (mapcar
       '(lambda(blkref)
	 (entity:putdxf
	  blkref
	  10
	  (m:coordinate (entity:getdxf blkref 10)
	   (m:coordinate-scale
	    (m:coordinate-rotate v1 (entity:getdxf blkref 50))
	    (entity:getdxf blkref 41)))
	  ))
       (setq blkrefs (pickset:to-list (block:ssget "x" blkname nil))))
      )
  (command "regen")
  (princ)
  )
(defun @block:menu-align-base(/ blkref blkname pt pt-oldbase pt-nb v1)
  (prompt  "请选择要对齐基点的块:")
  (setq blkrefs (pickset:to-list (ssget '((0 . "insert")))))
  (setq pt (getpoint (entity:getdxf (car blkrefs) 10) "请选择目标基点:"))
  (foreach blkref blkrefs
	   (setq blkname (entity:getdxf blkref 2))
	   (setq pt-oldbase
		 (vlax-safearray->list
		  (vlax-variant-value (vla-get-origin (block:get-obj-by-name blkname)))))
	   (setq pt-nb (block:wcs2bcs
			pt
			pt-oldbase
			(entity:getdxf blkref 10)
			(entity:getdxf blkref 50)
			(entity:getdxf blkref 41)
			))
	   (setq v1 (mapcar '- pt-nb pt-oldbase))
	   (vla-put-origin
	    (block:get-obj-by-name blkname)
	    (vlax-3d-point pt-nb))
	   ;; TODO: 平移已有块
	   ;; (if (ui:confirm1 "是否保持已有块显示位置不变?" "是-否")
	   (mapcar
	    '(lambda(blkref)
	      (entity:putdxf
	       blkref
	       10
	       (m:coordinate (entity:getdxf blkref 10)
		(m:coordinate-scale
		 (m:coordinate-rotate v1 (entity:getdxf blkref 50))
		 (entity:getdxf blkref 41)))
	       ))
	    (pickset:to-list (block:ssget "x" blkname nil)))
	   )
  (command "regen")
  (princ)
  )
;; (defun @block:change-base (blkname pt-nb / blkobj)
;;   (vla-put-origin
;;    (block:get-obj-by-name blkname)
;;    (vlax-3d-point pt-nb))
;;   )
(defun @block:menu-inserts ( )
  "连续插块"
  ;; 选择块名。
  
  )
(defun @block:explode-all (/ blks )
  (@::prompt "将框选区域内的块全部分解。")
  (setq pt1 (getpoint "选择第一点:"))
  (setq pt2 (getcorner pt1 "请选择区域第二点"))
  (while (setq blks (ssget "c" pt1 pt2 '((0 . "insert"))))
    ;;将属性变为文字
    (foreach blk (pickset:to-list blks)
	     (vla-explode (e2o blk))
	     (vla-delete (e2o blk)))
    (vla-Regen *doc* acActiveViewport)))
(defun @block:explode-chain (blkref / objs-x)
  (if (= 'ename (type blkref)) (setq blkref (e2o blkref)))
  (cond
    ((string-equal (vla-get-ObjectName blkref)"AcDbBlockReference")
     (if (setq objs-x (vla:get-value (vla-Explode blkref)))
	 (progn
	   (vla-delete blkref)
	   (mapcar 'vla-update objs-x)
	   (mapcar '@block:explode-chain objs-x))
	 ))
    (t blkref)))

(defun @block:explode-loop (/ explode-loop)
  (sssetfirst nil
	      (pickset:from-list
	       (mapcar 'o2e
		       (list:flatten
			(mapcar '(lambda(blkref)
				  (list:flatten (@block:explode-chain blkref)))
				(pickset:to-list (ssget '((0 . "insert"))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 统计一个图中指定块名的总参照个数(含嵌套块)。
;; 块 A 为单桩块，块 B/C/D 为承台块，B/C/D 内嵌 A。
;; 例如 B 内含 2 个 A，C 内含 3 个 A，
;; 则总数 = 模型空间中 A 的个数 + 2*B个数 + 3*C个数。
;; 支持多级嵌套：B 不含 A，但 B 内含 C，C 内含 A，也能正确计算。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq @block:cnt-nested-memo nil)
(defun @block:count-inblk (name targetname / total e d d0 b cmem)
  "计算块定义 name 中单个实例内所含 targetname 块的总个数。
   递归遍历其内部所有 insert，支持多级嵌套。结果缓存在 memo 中。"
  (if (setq cmem (assoc name @block:cnt-nested-memo))
      (cdr cmem)
      (progn
	;; 先缓存为 0，防止循环嵌套时无限递归
	(setq @block:cnt-nested-memo (cons (cons name 0) @block:cnt-nested-memo))
	(setq total 0)
	(if (setq b (tblobjname "block" name))
	    (progn
	      (setq e (entnext b))
	      (while (and e
			  (setq d0 (cdr (assoc 0 (entget e))))
			  (/= "ENDBLK" d0))
		(if (equal "INSERT" d0)
		    (setq d (block:get-effectivename e))
		    (setq d nil))
		(if d
		    (if (= d targetname)
			(setq total (1+ total))
		      (if (tblsearch "block" d)
			  (setq total (+ total (@block:count-inblk d targetname))))))
		(setq e (entnext e)))))
	(setq @block:cnt-nested-memo
	      (subst (cons name total)
		     (assoc name @block:cnt-nested-memo)
		     @block:cnt-nested-memo))
	total)))

(defun @block:count-nested (target / pt eff cn dir-n n-total c
			     lst-contrib lst-contrib-ss lst-bl lst-di)
  "统计一个图中指定块名的总参照个数(含嵌套块)，并选中模型中包含该块的块参照。"
  (@::prompt "统计一个图中指定块名的总个数(含嵌套块)。")
(while (null target)
    (if (setq pt (entsel "\n请选择要统计的目标块:"))
	(setq target (block:get-effectivename (car pt)))))
  (setq @block:cnt-nested-memo nil
	lst-contrib nil lst-contrib-ss nil dir-n 0 n-total 0)
  (foreach ins (pickset:to-list (ssget "x" '((0 . "insert"))))
	   (setq eff (block:get-effectivename ins))
	   (cond
	     ((and eff (= eff target))
	      (setq dir-n (1+ dir-n))
	      (setq n-total (1+ n-total)))
	     ((and eff (tblsearch "block" eff))
	      (setq cn (@block:count-inblk eff target))
	      (if (> cn 0)
		  (progn
		    (setq n-total (+ n-total cn))
		    (setq lst-contrib-ss (cons ins lst-contrib-ss))
		    (setq lst-contrib (cons eff lst-contrib)))))
	     ))
  ;; 汇总明细：块名 -> (模型内实例数 单实例所含target数)
  (setq lst-bl nil)
  (foreach n lst-contrib
	   (if (not (member n lst-bl)) (setq lst-bl (cons n lst-bl))))
  (setq lst-di nil)
  (foreach n lst-bl
	   (setq c 0)
	   (foreach e lst-contrib-ss
		    (if (= (block:get-effectivename e) n)
			(setq c (1+ c))))
	   (setq lst-di (cons (list n c (cdr (assoc n @block:cnt-nested-memo)))
			      lst-di)))
  ;; 输出
  (princ (strcat "\n目标块 [" target "] 的总个数: "
		 (itoa n-total)))
  (princ (strcat "  (模型空间直接: " (itoa dir-n)))
  (if lst-di
      (progn
	(princ (strcat " , 含于块内: "
		       (itoa (- n-total dir-n))))
	(princ "\n其中:")
	(foreach x lst-di
		 (princ (strcat "\n  块[" (car x) "]  模型中有 "
				(itoa (cadr x)) " 个, 每实例含 target "
				(itoa (caddr x)) " 个")))
	(princ ")"))
      (princ ")"))
  (princ "\n")
  ;; 选中模型中包含目标块的块参照(如 B、C)
  (if lst-contrib-ss
      (sssetfirst nil (pickset:from-entlist lst-contrib-ss)))
  (list (cons "total" n-total) (cons "direct" dir-n) lst-di))
