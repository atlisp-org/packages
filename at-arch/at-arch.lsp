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
