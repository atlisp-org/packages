;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'autohotkey:first for package autohotkey 's configitem first 
(@:add-menu "外部程序" "AutoHotKey" "(autohotkey:open)" )

(defun autohotkey:open (/ app )
  ;; 可执行文件路径
  (setq app "bin\\AutoHotKey\\AutoHotkey.exe")
  (if (null (findfile app))
      ;; 下载压缩包
      (@:down-and-unzip "archives/AutoHotKey.zip" "bin"))
  ;;运行外部可执行程序
  (if (findfile app)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg" (strcat  @:*prefix* app ))
	(setvar "cmdecho" 1)))
  (princ)
  )
