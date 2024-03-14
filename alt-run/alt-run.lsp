;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'alt-run:first for package alt-run 's configitem first 
;; (@:get-config 'alt-run:first) 
;; (@:set-config 'alt-run:first  "New Value")
;; Add menu in @lisp panel

(@:add-menu "外部程序" "ALTRun" "(alt-run:open)" )
(defpackage :alt-run 
  (:use :cl)
  (:export :open)
  )
(defun alt-run:open ( / app )
  ;; 可执行文件路径
  (setq app "bin\\ALTRun\\ALTRun.exe")
  (if (null (findfile app))
      ;; 下载压缩包
      (@:down-and-unzip "archives/ALTRun.zip" "bin"))
  ;;运行外部可执行程序
  (if (findfile app)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg" (strcat  @:*prefix* app ))
	(setvar "cmdecho" 1)))
  (princ)
  )
