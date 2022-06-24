;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@:define-config 'block-update:path "D:\\block" "要更新块的目录.")
(@:define-config 'block-update:number 5 "要更新块的数量.")
(@:add-menu "块操作" "更新块" "(block-update:update)" )
(defun block-update:update (/ blks n%)
  (alert "要更新的图块文件不能在CAD中处于打开状态。请选保存关闭后使用。")
  (setq blks
	(vl-directory-files (@:get-config 'block-update:path) "*.dwg"))
  (vl-sort blks '(lambda (x y)
		  (> (datetime:mktime(vl-file-systime (strcat  (@:get-config 'block-update:path) "\\" x)))
		   (datetime:mktime(vl-file-systime (strcat  (@:get-config 'block-update:path) "\\" y))))))
  (setvar "attreq" 0)
  (setq n% 0)
  (while (or (< n%  (@:get-config 'block-update:number))
	     (< n%  (length blks)))
    (command "-insert" (strcat (vl-filename-base (nth n% blks))"="(@:get-config 'block-update:path) "\\"
			       (nth n% blks)) '(0 0 0) "1" "1" 0)
    (entdel (entlast))
    (setq n% (1+ n%)))
  (setvar "attreq" 1)
  (princ)
  )
(@:add-menu "块操作" "设置更新块" "(block-update:setup)" )
(defun block-update:setup ()
  (@:set-config 'block-update:path
		(system:get-folder "请选择要更新的块文件的目录："))
  (@:set-config 'block-update:number
		(getint (strcat "请选择要更新的块的数量 <"(itoa (@:get-config 'block-update:number))">：")))
  (alert (strcat "目录:"(@:get-config 'block-update:path)"\n数量:" (itoa(@:get-config 'block-update:number))))
 
  (princ)
  )
(defun c:rb () (block-update:update))
(defun c:rbb () (block-update:setup))
