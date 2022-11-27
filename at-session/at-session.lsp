;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-session:first 用于 应用包 at-session 的 第一个配置项 first 
;; (@:get-config 'at-session:first) ;; 获取配置顶的值
;; (@:set-config 'at-session:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单

(@:add-menus
 '("会话管理"
   ("上班" (at-session:goto-work))
   ("恢复会话" (at-session:open))
   ("历史会话" (at-session:history))
   ("保存会话" (at-session:save-current))
   ("关闭会话" (at-session:close))
   ("下班" (at-session:knock-off))
   ("--" "--")
   ("关所有dwg" (at-session:save-and-close-all))
   )
 )
(defun align-str (n str / flag)
  (if (null str)(setq str ""))
  (setq flag nil)
  (while (< (string:bytelength str) n)
    (if flag
	  (setq str (strcat " " str))
      (setq str (strcat str " ")))
    (setq flag (not flag)))
  str)
(defun at-session:read (/ sessions)
  (if (findfile (strcat @:*prefix-config* "session"))
      (read (@:get-file-contents (strcat @:*prefix-config* "session"))
	    )))
(defun at-session:write (sessions / *error* fp)
  (defun *error* (msg)
    (if (= 'file (type fp)) (close fp))
    (@:*error* msg)
    nil)
  (setq fp (open (strcat @:*prefix-config* "session") "w"))
  (write-line (vl-prin1-to-string sessions) fp)
  (close fp)
  t
  )

(defun at-session:goto-work (/ fp session docs)
  (@:help '("打开最近保存的名称为 ‘下班’ 的会话。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (setq sessions (at-session:read))
  (while (and sessions
	      (/= "下班" (cadr sessions)))
    (setq sessions (cdr sessions)))
  (setq session (car (at-session:read)))
  (if (cddr session)
      (progn
	(foreach doc (cddr session)
		 (if (and (not (member doc docs))
			  (findfile doc))
		     (vla-open *DOCS* doc)))
	(@:log "INFO" "Resume session.")))
  (princ)
  )
(defun at-session:open (/ fp session docs)
  (@:help '("打开最近保存的会话。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (setq session (car (at-session:read)))
  (if (cddr session)
      (progn
	(foreach doc (cddr session)
		 (if (and (not (member doc docs))
			  (findfile doc))
		     (vla-open *DOCS* doc)))
	(@:log "INFO" "Resume session.")))
  (princ)
  )
(defun at-session:history (/ sessions res)
  (@:help "显示历史会话")
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  ;;(setq fp (open (strcat @:*prefix-config* "session") "r"))
  (setq sessions (at-session:read))
  (setq n (apply 'max (mapcar '(lambda(x)(string:bytelength (cadr x))) sessions)))
  (setq res
	(ui:select "请选择历史会话，并打开会话"
		   (mapcar '(lambda(x)
			      (strcat (car x)
				      " | "
				      (align-str n (cadr x))
				      " | "
				      (itoa (length (cddr x)))
				      "dwgs"
				      ))
			   sessions)))
  (if res
      (progn
	(setq docs nil)
	(vlax-for doc *DOCS*
		  (if (/= "" (vla-get-fullname doc))
		      (setq docs (cons (vla-get-fullname doc) docs))))
	(setq session (assoc (vl-string-trim " " (car (string:to-list res "|"))) sessions))
	(if (cddr session)
	    (progn
	      (foreach doc (cddr session)
		       (if (and (not (member doc docs))
				(findfile doc))
			   (vla-open *DOCS* doc)))
	      (@:log "INFO" "Resume session.")))))
  (princ)
  )

(defun at-session:save-current (/ sessions docs fp *error*)
  (defun *error* (msg)
    (if (= 'file (type fp)) (close fp))
    (@:*error* msg))
  (@:help '("保存当前会话"))
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (if docs
      (progn
	(setq res (ui:input "请输入会话名" '(("会话名"))))
	(setq sessions (at-session:read))
	(if (atom (car sessions)) (setq sessions nil))
	(setq session
	      (cons (rtos (getvar "cdate") 2 6)
		    (cons (cdr (assoc "会话名" res))
			  (reverse docs))))
	(at-session:write (cons session sessions))
	(close fp)
	(@:log "INFO" "Save session."))
    (@:log "INFO" "No DWG file were opened.")
    )
  (princ)
  )
(defun at-session:close (/ fp session docs)
  (@:help '("关闭最近一次会话记录的 DWG 文档。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (setq session (car (at-session:read)))
  (vlax-for doc *DOCS*
	    (if (and
		 (/= "" (vla-get-fullname doc))
		 (member (vla-get-fullname doc) (cddr session))
		 (/= (vla-get-fullname doc) (vla-get-fullname *DOC*))
		 )
		(vla-close doc :vlax-true)
	      ))
  (@:log "INFO" "Resume session.")
  (if (and (/= "" (vla-get-fullname *DOC*))
	   (member (vla-get-fullname *DOC*) (cddr session)))
      (progn
	(vla-save *DOC*)(vla-sendcommand *DOC* "close ")))
  (princ)
  )

(defun at-session:save-and-close-all (/ docs)
  (@:help '("保存并关闭所有已打开的 DWG 文档。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (vlax-for doc *DOCS*
	    (if (and (/= "" (vla-get-fullname doc))
		     (/= (vla-get-fullname *DOC*)(vla-get-fullname doc))
		     )
		(vla-close doc :vlax-true)
	      ))
  (if (and (/= "" (vla-get-fullname *DOC*))
	   )
      (progn
	(vla-save *DOC*)(vla-sendcommand *DOC* "close ")))
  (princ)
  )

(defun at-session:knock-off (/ sessions docs fp *error*)
  (@:help "下班前记录当前打开的所有 dwg图档，并关闭所有dwg图档。")
  (defun *error* (msg)
    (if (= 'file (type fp)) (close fp))
    (@:*error* msg))
  (@:help '("保存当前会话"))
  (setq docs nil)
  (vlax-for doc *DOCS*
	    (if (/= "" (vla-get-fullname doc))
		(setq docs (cons (vla-get-fullname doc) docs))))
  (if docs
      (progn
	(setq sessions
	      (vl-remove-if
	       '(lambda(x)(equal "下班" (cadr x)))
	       (at-session:read)))
	
	(if (atom (car sessions)) (setq sessions nil))
	(setq session
	      (cons (rtos (getvar "cdate") 2 6)
		    (cons "下班"
			  (reverse docs))))
	(at-session:write (cons session sessions))
	(@:log "INFO" "Save session."))
    (@:log "INFO" "No DWG file were opened.")
    )
  (at-session:save-and-close-all)
  (vla-quit *ACAD*)
  (princ)
  )
