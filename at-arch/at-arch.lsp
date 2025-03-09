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
 '("@建筑"
   ("建筑设置" "(at-arch:setup)" )
   ("插入车位" "(at-arch:insert-parking)" )
   ("插入无障碍车位" "(at-arch:insert-accparking)" )
   ("插入机械车位" "(at-arch:insert-machineparking)" )
   ("车位编号" "(at-arch:parking-numbering)" )
   ("未命名房间" "(at-arch:locate-unnamed-space)" )
   ("定位房间" "(at-arch:menu-locate-space-by-code)" )
   ("统计窗地比" "(at-arch:w/space)")
   
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
(defun at-arch:locate-space-by-code (code / spaces)
  (if (setq spaces (ssget "x" (list '(0 . "TCH_SPACE")(cons 2 code))))
      (if(setq corner (entity:getbox (ssname spaces 0) 100))
	 (command "zoom" "w" (car corner) (cadr corner)))
      (@::prompt "没有发现编号房间。")
      ))
(defun at-arch:menu-locate-space-by-code()
  (at-arch:locate-space-by-code (getstring "请输入房间编号:")))
(defun at-arch:w/space (/ spaces)
  (@::help "窗地比统计")
  (if (setq spaces (ssget '((0 . "TCH_SPACE")(8 . "SPACE"))))
      (ui:dyndraw
       (table:make
	'(0 0 0)
	"窗地比统计表"
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
		"")
	    ))
	 (mapcar 'e2o (pickset:to-list spaces))))
       '(0 0 0))))
	       
(defun at-arch:check-w/ua (name res)
  (cond
    ((wcmatch name "卧室,起居*,厨房")
     (<  (/ 1.0 7) res))
    ((wcmatch name "设计*,绘图*")
     (<  (/ 1.0 4) res))
     ((wcmatch name "办公*,会议*")
      (<  (/ 1.0 5) res))
     ((wcmatch name "复印*,档案*")
      (<  (/ 1.0 6) res))
     ((wcmatch name "走廊,走道,楼梯*,卫*")
      (<  (/ 1.0 10) res))
    (t t)
    ))
