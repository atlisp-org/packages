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
   
   ("车位编号" "(at-arch:parking-numbering)" )))

(defun at-arch:setup (/ res)
   (setq @::tmp-search-str "@arch")
   (@::edit-config-dialog))

(defun at-arch:insert-parking ()
  (setq downfile "at-arch/车位.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))(sleep 5))
    (ui:dyndraw
     (block:insert "车位" (@::package-path "at-arch") '(0 0 0)0 1)
     '(0 0 0))))
(defun at-arch:insert-accparking ()
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
