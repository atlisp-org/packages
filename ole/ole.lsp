(@::define-config 'ole:gap 300  "插入图像的水平间隙")
(@::define-config 'ole:scale 1.0  "插入图像与原图的比例")
(@::define-config 'ole:width 3000.0 "图像宽度")
(@::define-config 'ole:img-types "jpg,png"  "图像文件类型,以逗号分隔")
(@::define-config 'ole:title-size 50   "图像名称字高")
(@::define-config 'ole:title-style "黑体" "图像名称字体样式，需用户定义")
(@:add-menu "ole图像" "ole设置" "(ole:setup)" )
(@:add-menu "ole图像" "批量插入" "(ole:multi-insert)")
(@:add-menu "ole图像" "插入图像" "(ole:insert-img)")
(@:add-menu "ole图像" "配置环境" "(ole:install)" )
(@:add-menu "ole图像" "批量光栅" "(ole:multi-rasterimage)")

(defun ole:setup (/ res)
   (setq @::tmp-search-str "ole:")
   (@::edit-config-dialog))
(defun ole:install ()
  (@::cmd "shell"
	  (strcat "powershell "
		  (@::package-path "ole")"install.ps1")))
(defun ole:multi-insert ()
  (@::prompt '("将文件夹中的图像文件jpg/png,插入到当前dwg中"
	       ))
  (setq i 0)
  (if (setq folder (system:get-folder "请选择要插入的图片文件所有在文件夹"))
      (progn
	(setq pt-ins (getpoint "插入点:"))
	(if (setq imglst
		  (vl-sort
		   (vl-remove ""
			      (apply 'append
				     (mapcar '(lambda(img-type)
					       (vl-directory-files folder
						(strcat "*."
						 (vl-string-trim  "*." img-type))
						1))
					     (string:to-list (@::get-config 'ole:img-types)",")
					     )))
		   '<))
	    (progn
	      (if (findfile (strcat @::*tmp-path*
				    "oleimg.lst"))
		  (vl-file-delete (strcat @::*tmp-path*
					  "oleimg.lst")))
	      (setq fp (open (strcat @::*tmp-path*
				     "oleimg.lst")

			     "w"))
	      (foreach img imglst
		       (write-line
			(strcat folder"\\" img)
			fp))
	      (close fp)
	      (or @::enable-start
		  (@::check-pgp)
		  (@::patch-pgp) 
		  )
	      (@::cmd "shell-bg"
		      (strcat "atlisp-ole "
			      (strcat @::*tmp-path*
				     "oleimg.lst")))
	      )
	    (@::prompt"没有发现图像文件")
	    ))
      ))
(defun ole:insert-img (/ imgfile )
  (@::prompt '("将图像文件jpg/png,以OLE方式插入到当前dwg中"
	       ))
  (if (setq imgfile (getfiled "请选择要插入的图片" "" "png" 8))
      (progn
	(setq pt-ins (getpoint "插入点:"))
	(or @::enable-start
	    (@::check-pgp)
	    (@::patch-pgp) 
	    )
	(@::cmd "shell-bg"
		(strcat "atlisp-ole "imgfile)
		)
	))
      )
(defun ole:calc-ptins()
  (if (null pt-ins)(setq pt-ins '(0 0 0)))
  (setq box (entity:getbox (entlast) 0))
  (setq pt-ins (polar pt-ins
		      0
		      ;;(+ 100 (- (caadr box)(caar box)))
		      (+ (@::get-config 'ole:gap)
			 (@::get-config 'ole:width)
			 )))
  )
(defun ole:align-ole (ole target / minp maxp cur delta res)
  "将OLE对象的左下角对齐到target点(使用包围盒实测偏移)"
  (setq res
	(vl-catch-all-apply
	 (function (lambda ()
		     (vla-getboundingbox ole 'minp 'maxp)
		     (setq minp (vlax-safearray->list (vlax-variant-value minp)))
		     (setq cur (vlax-safearray->list
				 (vlax-variant-value
				   (vla-get-InsertionPoint ole))))
		     (setq delta (mapcar '- target minp))
		     (vla-put-InsertionPoint ole
		       (point:to-ax (mapcar '+ cur delta)))))))
  ;; 若包围盒失败则直接按target设置插入点
  (if (vl-catch-all-error-p res)
      (vla-put-InsertionPoint ole (point:to-ax target)))
  (vla-update ole))
(defun ole:scale-img(/ ole info ole-pt-ins width height)
  (setq ole  (e2o(entlast)))
  (if ole
      (progn
	;; 检查是否有光栅图像信息
	(if (and *ole:raster-info* 
		 (>= *ole:raster-index* 0)
		 (< *ole:raster-index* (length *ole:raster-info*)))
	    (progn
	      ;; 使用原始光栅图像的位置和大小
	      (setq info (nth *ole:raster-index* *ole:raster-info*))
	      (setq ole-pt-ins (cadr info))
	      (setq width (caddr info))
	      (setq height (cadddr info))
	      ;; 禁用宽高比锁定，以便独立设置宽度和高度
	      (vla-put-LockAspectRatio ole :vlax-false)
	      ;; 设置OLE对象的宽度和高度
	      (vla-put-width ole width)
	      (vla-put-height ole height)
	      (vla-update ole)
	      ;; 按包围盒实测对齐左下角到原光栅图像插入点
	      (ole:align-ole ole ole-pt-ins)
	      ;; 递增索引
	      (setq *ole:raster-index* (1+ *ole:raster-index*))
	      )
	    ;; 使用默认配置
	    (progn
	      (if (not
		   (equal (vla-get-width ole)
			  (@::get-config 'ole:width)
			  (* 0.01 (@::get-config 'ole:width))
			  ))
		  (progn
		    (vla-put-width ole (@::get-config 'ole:width))
		    ))
	      ))
	(vla-update ole)
	))
  )

(defun ole:make-title (str)
  (entity:make-text
   str
   (polar pt-ins (* 1.5 pi)
	  (* 2.0 (@::get-config 'ole:title-size)))
   (@::get-config 'ole:title-size)
   0 0.9 0 "LB")
  (if (member (@::get-config 'ole:title-style) (tbl:list"textstyle"))
      (entity:putdxf (entlast)
		     7
		     (@::get-config 'ole:title-style)))
  )
(defun ole:osmode-off ()
  (if (< (getvar "osmode") 16384)
      (setvar "osmode" (+ (getvar "osmode") 16384))))
(defun ole:osmode-on ()
  (if (>= (getvar "osmode") 16384)
      (setvar "osmode" (- (getvar "osmode") 16384))))

(defun ole:multi-rasterimage (/ ss i ent ent-data imgdef imgdef-data 
                                 img-path img-info-lst width height fp pt-ins
                                 img-obj img-width img-height)
  (@::prompt '("将当前DWG中的光栅图像参照以OLE方式嵌入到DWG中"
	       ))
  ;; 选择所有IMAGE实体
  (setq ss (ssget "_X" '((0 . "IMAGE"))))
  (if (null ss)
      (@::prompt "当前DWG中没有光栅图像参照")
    (progn
      ;; 获取所有IMAGE实体的信息
      (setq img-info-lst '())
      (setq i 0)
      (repeat (sslength ss)
	(setq ent (ssname ss i))
	(setq ent-data (entget ent))
	;; 组码340指向IMAGEDEF实体
	(setq imgdef (cdr (assoc 340 ent-data)))
	(if imgdef
	    (progn
	      (setq imgdef-data (entget imgdef))
	      ;; IMAGEDEF实体的组码1是文件路径
	      (setq img-path (cdr (assoc 1 imgdef-data)))
	      ;; 获取插入点（组码10）
	      (setq pt-ins (cdr (assoc 10 ent-data)))
	      ;; 使用VLA接口获取实际显示尺寸（ImageWidth/ImageHeight）
	      (setq img-obj (vlax-ename->vla-object ent))
	      (setq img-width (vla-get-ImageWidth img-obj))
	      (setq img-height (vla-get-ImageHeight img-obj))
	      (setq width img-width)
	      (setq height img-height)
	      (if (and img-path (findfile img-path))
		  (setq img-info-lst (append img-info-lst
					    (list (list img-path pt-ins width height)))))
	      ))
	(setq i (1+ i))
	)
      (if (null img-info-lst)
	  (@::prompt "没有找到有效的光栅图像文件路径")
	(progn
	  (@::prompt (strcat "找到 " (itoa (length img-info-lst)) " 个光栅图像"))
	  ;; 存储图像信息到全局变量，供后续调整使用
	  (setq *ole:raster-info* img-info-lst)
	  (setq *ole:raster-index* 0)
	  ;; 写入列表文件（只包含路径）
	  (if (findfile (strcat @::*tmp-path* "oleimg.lst"))
	      (vl-file-delete (strcat @::*tmp-path* "oleimg.lst")))
	  (setq fp (open (strcat @::*tmp-path* "oleimg.lst") "w"))
	  (foreach img img-info-lst
	    (write-line (car img) fp))
	  (close fp)
	  ;; 使用atlisp-ole命令批量处理
	  (or @::enable-start
	      (@::check-pgp)
	      (@::patch-pgp)
	      )
	  (@::cmd "shell-bg"
		  (strcat "atlisp-ole "
			  (strcat @::*tmp-path* "oleimg.lst")))
	  (@::prompt "光栅图像OLE嵌入完成")
	  ))
      )))

(defun ole:adjust-raster-ole ()
  "调整OLE对象位置和大小，使其与原光栅图像一致"
  (if (and *ole:raster-info* 
	   (>= *ole:raster-index* 0)
	   (< *ole:raster-index* (length *ole:raster-info*)))
      (let* ((info (nth *ole:raster-index* *ole:raster-info*))
             (ole-pt-ins (cadr info))
             (width (caddr info))
             (height (cadddr info))
             (ole (e2o (entlast))))
	(if ole
	    (progn
	      ;; 禁用宽高比锁定，以便独立设置宽度和高度
	      (vla-put-LockAspectRatio ole :vlax-false)
	      ;; 设置OLE对象的宽度和高度
	      (vla-put-width ole width)
	      (vla-put-height ole height)
	      (vla-update ole)
	      ;; 按包围盒实测对齐左下角到原光栅图像插入点
	      (ole:align-ole ole ole-pt-ins)
	      ;; 更新显示
	      (vla-update ole)
	      ;; 递增索引
	      (setq *ole:raster-index* (1+ *ole:raster-index*))
	      ))))
  )
