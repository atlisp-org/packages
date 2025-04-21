(@::define-config 'ole:gap 100  "插入图像的水平间隙")
(@::define-config 'ole:scale 1.0  "插入图像与原图的比例")

(@:add-menu "ole图像" "批量插入" "(ole:multi-insert)" )
(@:add-menu "ole图像" "安装python" "(ole:setup)" )

(defun ole:setup ()
  (@::prompt '("安装ole应用包的运行环境"))
  (@::cmd "powershell-bg"
	  "winget install python.python.3.13"))

(defun ole:multi-insert ()
  (@::prompt '("将文件夹中的图像文件jpg/png,插入到当前dwg中"
	       "本功能需要python运行环境"
	       ))
  (setq i 0)
  (if (setq folder (system:get-folder "请选择要插入的图片文件所有在文件夹"))
      (progn
	(setq pt-ins (getpoint "插入点:"))
	(if (setq imglst
	      (vl-sort
	       (append
		(vl-directory-files folder "*.jpg" 1)
		(vl-directory-files folder "*.png" 1))
	       '<))
	    (progn
	      (setq fp (open (strcat @::*tmp-path*
				     "oleimg.lst")

			     "w"))
	      (foreach img imglst
		       (write-line
			(strcat folder"\\" img)
			fp))
	      (close fp)
	      (@::cmd "shell-bg"
		      (strcat "atlisp-ole "
			      (strcat @::*tmp-path*
				     "oleimg.lst")))
	      )
	    
	    ))
      ))
(defun ole:calc-ptins()
  (if (null pt-ins)(setq pt-ins '(0 0 0)))
  (setq box (entity:getbox (entlast) 0))
  (setq pt-ins (polar pt-ins
		      0
		      (+ 100 (- (caadr box)
				(caar box))))))
  
(defun ole:osmode-off ()
  (if (< (getvar "osmode") 16384)
      (setvar "osmode" (+ (getvar "osmode") 16384))))
(defun ole:osmode-on ()
  (if (>= (getvar "osmode") 16384)
      (setvar "osmode" (- (getvar "osmode") 16384))))
