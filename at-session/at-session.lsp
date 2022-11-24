;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-session:first 用于 应用包 at-session 的 第一个配置项 first 
;; (@:get-config 'at-session:first) ;; 获取配置顶的值
;; (@:set-config 'at-session:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单

(@:add-menus
 '("会话管理"
   ("恢复会话" (at-session:open))
   ("保存会话" (at-session:save-current))
   ("关闭会话" (at-session:close)))
 )
(defun at-session:open (/ fp session docs)
  (@:help '("打开历史会话。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (setq fp (open (strcat @:*prefix-config* "session") "r"))
  (setq session (read (read-line fp)))
  (close fp)
  (if session
      (foreach doc (cddr session)
	       (if (and (not (member doc docs))
			(findfile doc))
		   (vla-open *DOCS* doc))))
  (@:log "INFO" "Resume session.")
  (princ)
  )
(defun at-session:save-current (/ docs fp *error*)
  (defun *error* (msg)
    (if (= 'file (type fp)) (close fp))
    (@:*error* msg))
  (@:help '("保存当前会话"))
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (setq res (ui:input "请输入会话名" '(("会话名"))))
  (setq session
	(cons (@:timestamp)
	      (cons (cdr (assoc "会话名" res))
		    (reverse docs))))
  (setq fp (open (strcat @:*prefix-config* "session") "w"))
  (write-line (vl-prin1-to-string session) fp)
  (close fp)
  (@:log "INFO" "Save session.")
  (princ)
  )
(defun at-session:close (/ fp session docs)
  (@:help '("保存并关闭会话 DWG 文档。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (setq fp (open (strcat @:*prefix-config* "session") "r"))
  (setq session (read (read-line fp)))
  (close fp)
  (vlax-for doc *DOCS*
	    (if (and
		 (/= "" (vla-get-fullname doc))
		 (member (vla-get-fullname doc) (cddr session))
		 (/= doc *DOC*)
		 )
		(vla-close doc :vlax-true)
	      ))
  (@:log "INFO" "Resume session.")
  (if (/= "" (vla-get-fullname *DOC*))
      (vla-close *DOC* :vlax-true))
  (princ)
  )
  
