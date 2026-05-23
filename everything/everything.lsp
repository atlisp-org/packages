(@:add-menu "外部程序" "Everything" "(everything:start)" )
(defpackage :everything 
  (:use :cl)
  (:export :open)
  )
(defun everything:start ( / app )
  ;; 可执行文件路径
  (setq app "bin\\everything.exe")
  (if (null (findfile app))
      ;; 下载压缩包
      (@:down-and-unzip "archives/everything.zip" "bin"))
  ;;运行外部可执行程序
  (if (findfile app)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg" (strcat  @:*prefix* app ))
	(setvar "cmdecho" 1)))
  (princ)
  )
