(@::define-config 'ole:gap 300  "插入图像的水平间隙")
(@::define-config 'ole:scale 1.0  "插入图像与原图的比例")
(@::define-config 'ole:width 3000.0 "图像宽度")
(@::define-config 'ole:img-types "jpg,png"  "图像文件类型,以逗号分隔")
(@:add-menu "ole图像" "ole设置" "(ole:setup)" )
(@:add-menu "ole图像" "批量插入" "(ole:multi-insert)" )
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
	      (@::cmd "shell"
		      (strcat "atlisp-ole "
			      (strcat @::*tmp-path*
				     "oleimg.lst")))
	      )
	    (@::prompt"没有发现图像文件")
	    ))
      ))
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
      (vla-put-width ole (@::get-config 'ole:width)))
  (vla-put-InsertionPoint ole (point:to-ax pt-ins))
  (vla-update  ole)
  )
  
(defun ole:osmode-off ()
  (if (< (getvar "osmode") 16384)
      (setvar "osmode" (+ (getvar "osmode") 16384))))
(defun ole:osmode-on ()
  (if (>= (getvar "osmode") 16384)
      (setvar "osmode" (- (getvar "osmode") 16384))))
