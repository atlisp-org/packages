;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:add-menu "结构" "型钢截面特性" "(xgjmtxcx:open)" )

(defun xgjmtxcx:open ( / app )
  ;; 可执行文件路径
  (setq app "bin/xgjmtxcxgj_v1.0.exe")
  (if (null (findfile app))
      ;; 下载压缩包
      (@:down-file "bin/xgjmtxcxgj_v1.0.exe"))
  ;;运行外部可执行程序
  (if (findfile app)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg" (strcat  @:*prefix* app ))
	(setvar "cmdecho" 1)))
  (princ)
  )
