;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-arch:first 用于 应用包 at-arch 的 第一个配置项 first 
(@:define-config '@block:block-name "块名" "用于排号的块名称。")
(@:define-config '@block:attribute-name "属性名" "用于排号的块内属性的名称。")
(@:define-config '@block:attribute-prefix "" "用于排号的块内属性值前缀。")
(@:define-config '@block:attribute-suffix "" "用于排号的块内属性值后缀。")
(@:define-config '@block:xref-layer "xref-lock" "用于放置外部参照的图层名。")
(@:define-config '@block:sort-order "xY" "排序规则。xyXY任意两两组合,例如yX,y在前表示y坐标优先，大X表示从右到左排序。")
(@:define-config '@block:sort-fuzz "10,10" "按位置排序时，坐标的容差。逗号用于分隔不同轴的容差。")
(@:define-config '@block:size 1000 "用于检测块重叠时，块的最大尺寸。")
(@:define-config '@block:lib "D:/Design/standard/lib/" "以块方式管理的图库路径。")
;; (@:get-config 'at-arch:first) ;; 获取配置顶的值
;; (@:set-config 'at-arch:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单
(@:add-menus
 '((_"Block")
   ((_"Block Config") (@block:config))
   ((_"Block Replace") (@block:subst))
   ((_"Block auto numbering") (@block:set-number))
   ((_"Numbering by route")(@block:numbering-by-route))
   ((_"Anyblock numbering") (@block:set-any-block-number))
   ((_"Block numbering Config") (@block:setup))
   ((_"Set as decomposable") (@block:explodable))
   ((_"Set as non decomposable") (@block:explode-disable))
   ((_"Change block base point") (@block:menu-change-base))
   ((_"Insert all block") (@block:insert-all))
   ((_"Select same block") (@block:select-same))
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

(defun @block:set-number (/ num1 start ss-list ss1 fuzz)
  
  (if (= "" (@:get-config '@block:block-name))
      (@block:setup))
  (if (progn
	(setq ss1 (ssget (list '(0 . "insert")'(66 . 1)
			       '(-4 . "<or")
			       (cons 2 (@:get-config '@block:block-name))
			       (cons 2 "`**")
			       '(-4 . "or>"))
			 ))
	(setq ss-list
	      (vl-remove-if-not
	       '(lambda(x)
		  (= (block:get-effectivename x)
		     (@:get-config '@block:block-name)))
	       (pickset:to-list ss1))))
      (progn
	;; 排序
	(sssetfirst nil (pickset:from-list ss-list))
	(setq fuzz (mapcar 'atof (string:to-list (@:get-config '@block:sort-fuzz)",")))
	(setq ss-list (pickset:sort ss-list (@:get-config '@block:sort-order) fuzz))
	(setq start (getint "请输入块起始编号<1>:"))
	(if (null start) (setq start 1))
	(setq num1 0)
	(foreach en0 ss-list
		 (block:set-attributes
		  en0
		  (list (cons (@:get-config '@block:attribute-name)
			      (strcat
			       (@:get-config '@block:attribute-prefix)
			       (if (< (+ num1 start) 10) "0" "")
			       (itoa (+ num1 start))
			       (@:get-config '@block:attribute-suffix)
			       ))))
		 (setq num1 (1+ num1))
		 ))
      (progn
	(alert "未选中设置的图块。请设置要进行操作的图块。")
	(@block:setup))))

(defun @block:set-any-block-number (/ num1 start ss-list ss1 )
  (@:help "任意块编号，含有给定属性的任意块编号。")
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
  (@:help (strcat "将目标块替换成源块. \n"
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
  (@:help (strcat "将目标块设置为可分解. \n"
		  "步骤: \n"
		  "      1. 单选设置为可分解的块；\n"))
  
  (setq blk (car (entsel "请选择块: ")))
  (if (= "INSERT" (entity:getdxf blk 0))
      (vla-put-explodable
       (vla-item *BLKS* (block:get-effectivename blk))
       :vlax-true)
  ))
		  
(defun @block:explode-disable (/ blk )
  (@:help (strcat "将目标块设置为不可分解. \n"
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
  (@:help "属性块大纲，用于快速切换块视图\n不支持动态块。")
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
  (@:help (strcat "选择一个块，然后选中选定范围或全部的同名块。"))
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


(defun @block:menu-change-base(/ blkref blkname pt )
  
  (setq blkref (car (entsel "请选择要改变基点的块:")))
  (setq blkname (entity:getdxf blkref 2))

  (setq pt (getpoint (entity:getdxf blkref 10) "请选择目标基点:"))
  ;;TODO 考虑 210 220 230
  ;; (if (< (apply 'min (mapcar 'last (entity:getdxf blkref '(210 220 230)))) 0)
  ;;     (alert "图块拉伸方向有误，不能正确设置基点。"))
  (setq pt (mapcar '(lambda (x y z)
		      (/ (- x y) z))
		   pt (entity:getdxf blkref 10)
		   (entity:getdxf blkref '(41 42 43))
		   ))
  (princ "aaa")
  (print blkname)
  (print pt)
  (@block:change-base blkname pt))
  
(defun @block:change-base (blkname pt-nb / blkobj)
  (vlax-for blk *BLKS*
	    (if (= (vla-get-name blk) blkname)
		(setq blkobj blk)))
  (vla-put-origin
   blkobj
   (vlax-3d-point
    (mapcar
     '+
     pt-nb
     (vlax-safearray->list
      (vlax-variant-value (vla-get-origin blkobj)))
     )))
  (command "regen")
  )
(defun @block:menu-inserts ( )
  "连续插块"
  ;; 选择块名。
  
  )
(defun @block:explode-all (/ blks )
  (@:help "将框选区域内的块全部分解。")
  (setq pt1 (getpoint "选择第一点:"))
  (setq pt2 (getcorner pt1 "请选择区域第二点"))
  (while (setq blks (ssget "c" pt1 pt2 '((0 . "insert"))))
    ;;将属性变为文字
    (foreach blk (pickset:to-list blks)
	     (vla-explode (e2o blk))
	     (vla-delete (e2o blk)))
    (vla-Regen *doc* acActiveViewport)))
  
