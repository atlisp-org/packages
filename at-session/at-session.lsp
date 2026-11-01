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
    ("备份当前图纸" (at-session:backup-current))
    ("下班" (at-session:knock-off))
    ("--" "--")
    ("关所有dwg" (at-session:save-and-close-all))
    ("打开目录" (at-session:open-directory-dwgs))))
;; 如果没有，则创建空文件
(if (null (findfile (strcat @:*prefix-config* "session"))) 
  (progn 
    (setq fp (open (strcat @:*prefix-config* "session") "w"))
    (write-line "" fp)
    (close fp)))
(defun align-str (n str / flag) 
  "对齐字符串"
  (if (null str) (setq str ""))
  (setq flag nil)
  (while (< (string:bytelength str) n) 
    (if flag 
      (setq str (strcat " " str))
      (setq str (strcat str " ")))
    (setq flag (not flag)))
  str)
(defun at-session:read (/ sessions) 
  (if (findfile (strcat @:*prefix-config* "session")) 
    (read (@:get-file-contents (strcat @:*prefix-config* "session")))))
(defun at-session:write (sessions / *error* fp) 
  (defun *error* (msg) 
    (if (= 'file (type fp)) (close fp))
    (@:*error* msg)
    nil)
  (setq fp (open (strcat @:*prefix-config* "session") "w"))
  (write-line (vl-prin1-to-string sessions) fp)
  (close fp)
  t)

;; ------------------------------------------------------------
;; 图纸备份（提取自明经论坛「图纸自动备份」需求，见 thread-101521）
;; ------------------------------------------------------------
(defun at-session:backup-dir (dir / d)
  "内部：确定备份目录并创建，返回以 / 结尾的路径；失败返回 nil。"
  "目录字符串或 nil"
  "(at-session:backup-dir nil)"
  (setq d (if (and dir (/= dir ""))
              dir
              (strcat (getvar "DWGPREFIX") "备份")))
  (if (not (vl-string-search (substr d (strlen d)) "/\\"))
      (setq d (strcat d "/")))
  (if (not (vl-file-directory-p d))
      (vl-mkdir d))
  (if (vl-file-directory-p d) d nil))

(defun at-session:backup-stamp (/ s)
  "内部：当前时间戳 YYYYMMDD-HHMMSS。"
  "字符串"
  "(at-session:backup-stamp)"
  (setq s (rtos (getvar "CDATE") 2 6))
  (vl-string-translate "." "-" s))

(defun at-session:backup-prune (dir base keep / files n del)
  "内部：只保留最新的 keep 份备份，返回删除份数。"
  "删除的份数"
  "(at-session:backup-prune \"D:/bak/\" \"plan\" 10)"
  (setq keep (max 1 (fix (if (numberp keep) keep 10))))
  (setq files (vl-directory-files dir (strcat base "-*.dwg") 1))
  (setq files (vl-sort files
                       '(lambda (a b)
                          (> (strcase a) (strcase b)))))
  (setq n 0 del 0)
  (foreach f files
    (setq n (1+ n))
    (if (> n keep)
        (if (vl-file-delete (strcat dir f))
            (setq del (1+ del)))))
  del)

(defun at-session:ai-backup-current (dest keep / prefix name base dir
                                     stamp file n)
  "AI 调用入口：把当前已保存的 DWG 复制到备份目录（不修改当前图形）。
  dest: 备份目录，nil = 图纸目录下的「备份」；keep: 保留份数，缺省 10。
  返回备份文件全路径，失败返回 nil。
  示例: (at-session:ai-backup-current nil 10)"
  "备份文件路径或 nil"
  "(at-session:ai-backup-current \"D:/bak\" 10)"
  (if (/= 1 (getvar "DWGTITLED"))
      (progn
        (princ "at-session:ai-backup-current 当前图纸尚未保存，无法备份")
        nil)
      (progn
        (setq prefix (getvar "DWGPREFIX"))
        (setq name (getvar "DWGNAME"))
        (setq base (vl-filename-base name))
        (setq dir (at-session:backup-dir dest))
        (if (null dir)
            (progn
              (princ "at-session:ai-backup-current 无法创建备份目录")
              nil)
            (progn
              (setq stamp (at-session:backup-stamp))
              (setq file (strcat dir base "-" stamp ".dwg"))
              (setq n 1)
              (while (and (findfile file) (< n 100))
                (setq file (strcat dir base "-" stamp "-" (itoa n) ".dwg"))
                (setq n (1+ n)))
              (if (vl-file-copy (strcat prefix name) file)
                  (progn
                    (at-session:backup-prune dir base keep)
                    file)
                  (progn
                    (princ (strcat "at-session:ai-backup-current 备份失败: "
                                   file))
                    nil)))))))

(defun at-session:backup-current (/ keep res)
  "备份当前图纸：复制到「备份」目录，按份数清理旧备份。"
  "备份文件路径或 nil"
  "(at-session:backup-current)"
  (setq keep (getint "\n保留备份份数 <10>: "))
  (if (null keep) (setq keep 10))
  (setq res (at-session:ai-backup-current nil keep))
  (if res
      (princ (strcat "\n已备份: " res))
      (princ "\n备份失败。"))
  (princ))

(defun at-session:goto-work (/ fp session docs) 
  (@:help '("打开最近保存的名称为 ‘下班’ 的会话。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS* 
    (if (/= "" (vla-get-fullname doc)) 
      (setq docs (cons (vla-get-fullname doc) docs))))
  (setq sessions (at-session:read))
  (while 
    (and 
      sessions
      (/= "下班" (cadr sessions)))
    (setq sessions (cdr sessions)))
  (setq session (car (at-session:read)))
  (if (cddr session) 
    (progn 
      (foreach doc (cddr session) 
        (if 
          (and 
            (not (member doc docs))
            (findfile doc))
          (vla-open *DOCS* doc)))
      (@:log "INFO" "Resume session.")))
  (princ))
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
        (if 
          (and 
            (not (member doc docs))
            (findfile doc))
          (vla-open *DOCS* doc)))
      (@:log "INFO" "Resume session.")))
  (princ))
(defun at-session:history (/ sessions res) 
  (@:help "显示历史会话")
  (vlax-for doc *DOCS* 
    (if (/= "" (vla-get-fullname doc)) 
      (setq docs (cons (vla-get-fullname doc) docs))))
  ;;(setq fp (open (strcat @:*prefix-config* "session") "r"))
  (setq sessions (at-session:read))
  (setq n (apply 'max (mapcar '(lambda (x) (string:bytelength (if (cadr x) (cadr x) 0))) sessions)))
  (setq res (ui:select 
              "请选择历史会话，并打开会话"
              (mapcar 
                '(lambda (x) 
                   (strcat 
                     (car x)
                     " | "
                     (align-str n (cadr x))
                     " | "
                     (itoa (length (cddr x)))
                     "dwgs"))
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
            (if 
              (and 
                (not (member doc docs))
                (findfile doc))
              (vla-open *DOCS* doc)))
          (@:log "INFO" "Resume session.")))))
  (princ))

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
      (setq session (cons 
                      (rtos (getvar "cdate") 2 6)
                      (cons 
                        (if (cdr (assoc "会话名" res)) 
                          (cdr (assoc "会话名" res))
                          "")
                        (reverse docs))))
      (at-session:write (cons session sessions))
      (@:log "INFO" "Save session."))
    (@:log "INFO" "No DWG file were opened."))
  (princ))
(defun at-session:close (/ fp session docs) 
  (@:help '("关闭最近一次会话记录的 DWG 文档。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS* 
    (if (/= "" (vla-get-fullname doc)) 
      (setq docs (cons (vla-get-fullname doc) docs))))
  (setq session (car (at-session:read)))
  (vlax-for doc *DOCS* 
    (if 
      (and 
        (/= "" (vla-get-fullname doc))
        (member (vla-get-fullname doc) (cddr session))
        (/= (vla-get-fullname doc) (vla-get-fullname *DOC*))
        (= (vla-get-readonly doc) :vlax-false))
      (if (= (vla-get-readonly doc) :vlax-true) 
        (vla-close doc :vlax-true)
        (vla-close doc :vlax-false))))
  (@:log "INFO" "Resume session.")
  (if 
    (and 
      (/= "" (vla-get-fullname *DOC*))
      (member (vla-get-fullname *DOC*) (cddr session)))
    (progn 
      (vla-save *DOC*)
      (vla-sendcommand *DOC* "close ")))
  (princ))

(defun at-session:save-and-close-all (/ docs) 
  (@:help '("保存并关闭所有已打开的 DWG 文档。"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq docs nil)
  (vlax-for doc *DOCS* 
    (if (/= "" (vla-get-fullname doc)) 
      (setq docs (cons (vla-get-fullname doc) docs))))
  (vlax-for doc *DOCS* 
    (if 
      (and 
        (/= "" (vla-get-fullname doc))
        (/= (vla-get-fullname *DOC*) (vla-get-fullname doc)))
      (vla-close doc :vlax-true)))
  (if (and (/= "" (vla-get-fullname *DOC*))) 
    (progn 
      (vla-save *DOC*)
      (vla-sendcommand *DOC* "close ")))
  (princ))

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
      (setq sessions (vl-remove-if 
                       '(lambda (x) (equal "下班" (cadr x)))
                       (at-session:read)))

      (if (atom (car sessions)) (setq sessions nil))
      (setq session (cons 
                      (rtos (getvar "cdate") 2 6)
                      (cons 
                        "下班"
                        (reverse docs))))
      (at-session:write (cons session sessions))
      (@:log "INFO" "Save session."))
    (@:log "INFO" "No DWG file were opened."))
  (at-session:save-and-close-all)
  (vla-quit *ACAD*)
  (princ))
(defun at-session:open-directory-dwgs (/ folder docs) 
  (@:help "打开选定文件夹下的所有dwg文件")
  (setq folder (system:get-folder "请选择要打开的文件夹"))
  (setq dwgs (mapcar '(lambda (x) (strcat folder "\\" x)) (vl-directory-files folder "*.dwg" 1)))

  (if dwgs 
    (progn 
      (setq docs nil)
      (vlax-for doc *DOCS* 
        (if (/= "" (vla-get-fullname doc)) 
          (setq docs (cons (vla-get-fullname doc) docs))))
      (foreach doc dwgs 
        (if 
          (and 
            (not (member doc docs))
            (findfile doc))
          (vla-open *DOCS* doc)))
      (@:log "INFO" "Resume session.")))
  (princ))
