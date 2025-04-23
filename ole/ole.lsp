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
(defun ole:scale-img(/ ole)
  (setq ole  (e2o(entlast)))
  (if (not
       (equal (vla-get-width ole)
	      (@::get-config 'ole:width)
	      (* 0.01 (@::get-config 'ole:width))
	      ))
      (progn
	(vla-put-width ole (@::get-config 'ole:width))
	))
  (vla-put-InsertionPoint ole (point:to-ax pt-ins))
  (vla-update  ole)
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
