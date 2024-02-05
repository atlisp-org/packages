;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'pdftk:first for package pdftk 's configitem first 
(@:define-config 'pdftk:stamp "D:\\Design\\standard\\stamp.pdf" "用于加为戳记的pdf文件")
(@:define-config 'pdftk:background "D:\\Design\\standard\\background.pdf" "用于作为水印的pdf文件。被加水印的PDF文件须为透明背景。")
(@:define-config 'pdftk:pre-folder "D:\\" "选择pdf文件路径时，最后一次操作的文件位置。")
(if (null (findfile (@:get-config 'pdftk:stamp)))
    (@:mkdir (@:path (vl-filename-directory(@:get-config 'pdftk:stamp)))))
;; (@:get-config 'pdftk:first) 
;; (@:set-config 'pdftk:first  "New Value")
;; Add menu in @lisp panel
(@:add-menu "出图排版" "PDF设置" "(pdftk:setup)" )
(@:add-menu "出图排版" "合并PDF" "(pdftk:menu-merge)" )
(@:add-menu "出图排版" "拆分PDF" "(pdftk:menu-burst)" )
(@:add-menu "出图排版" "PDF加戳记" "(pdftk:menu-stamp)" )
(@:add-menu "出图排版" "PDF加水印" "(pdftk:menu-background)" )
(@:add-menu "出图排版" "解密PDF" "(pdftk:menu-decrypt)" )
(@:add-menu "出图排版" "加密PDF" "(pdftk:menu-encrypt)" )
(or @:enable-start
    (@:check-pgp)
    (@:patch-pgp) 
    )
(defun pdftk:setup (/ res)
  "pdf tools"
  (setq res 
	(ui:input "配置信息"
		  (mapcar '(lambda (x) (list (strcase (vl-symbol-name (car x)) T)(cadr x)(cddr x)))
			  (vl-remove-if '(lambda (x) (not (wcmatch (vl-symbol-name (car x)) "PDFTK:*")))
					(if @:*config.db*
					    @:*config.db* (@:load-config))))))
  (foreach res% res
   	   (@:set-config (read (car res%)) (cdr res%)))
  )
(defun pdftk:download (/ app)
  (setq app "bin\\pdftk.exe")
  (if (null (findfile "bin\\iconv.exe"))
      (@:down-and-unzip "archives/iconv.zip" "bin"))
  (if (null (findfile app))
      ;; 下载压缩包
      (@:down-and-unzip "archives/pdftk.zip" "bin"))
  (if (null (findfile "packages\\pdftk\\background.pdf"))
      (@:down-pkg-file (@:uri)"pdftk/background.pdf" "stable"))
  (if (null (findfile "packages\\pdftk\\stamp.pdf"))
      (@:down-pkg-file (@:uri)"pdftk/stamp.pdf" "stable"))
  
  )

(defun pdftk:menu-merge (/ folder)
  (@:help "合并选择的文件夹下的所有pdf,到父级目录，并打开该目录。")
  (if (setq folder (system:get-folder "请选择要合并的PDF文件夹"))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir folder))
	(pdftk:merge folder)
	(system:explorer (strcat folder "\\..\\" )))))
(defun pdftk:merge (folder / app files filetmp filename-bk fp-bookmark file-bk file-bk-utf8)
  "将参数folder 文件夹下的 pdf 文件合并成一个文件。合并后的文件名为 `文件夹-merge-all.pdf' "
  ""
  "(pdftk:merge \"D:\\Output\\A项目\")"
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  (if (and (findfile app)
	   folder
	   (vl-directory-files folder "*.pdf" 1))
      (progn
	;;(@:patch-pgp-shell)
	(setq files "*.pdf")
	(setq filetmp "merge-tmp.pdf")
	(setq filename-bk "merge-all.pdf")
	(setvar "cmdecho" 0)
	;;创建标签文件。
	(setq fp-bookmark (open (strcat folder "\\bk.txt")"w"))
	(setq i 0)
	(foreach bk% (vl-directory-files folder "*.pdf" 1)
		 (write-line "BookmarkBegin" fp-bookmark)
		 (write-line (strcat "BookmarkTitle: "
				     (vl-filename-base bk%))
			     fp-bookmark)
		 (write-line "BookmarkLevel: 1" fp-bookmark)
		 (write-line (strcat "BookmarkPageNumber: "
				     (itoa (setq i (1+ i))))
			     fp-bookmark))
	(close fp-bookmark)
	;; 转码
	(setq file-bk (strcat folder "\\bk.txt"))
	(setq file-bk-utf8 (strcat folder "\\bk-utf8.txt"))
	(command "shell-bg"
		 (strcat  @:*prefix* "bin\\iconv.exe -f gb2312 -t utf-8 \""
			  file-bk "\" >> \"" file-bk-utf8  "\""
			  ))
	;;(sleep 3)
	(print folder)

	(command "start-bg"
		 (strcat " /D \"" folder "\""
			 " /MIN /B  "
			 @:*prefix* app  " "
			 files " "
			 "output " filetmp
			 ))
	(while (null (findfile (strcat folder "\\" filetmp)))
	  (sleep 3))
	(command "start-bg"
		 (strcat  " /D \"" folder "\""
			  " /MIN /B  "
			  @:*prefix* app  " "
			  filetmp " "
			  "update_info_utf8 "
			  " bk-utf8.txt "
			  " output \"..\\"
			  (vl-filename-base folder) "-" filename-bk "\" "
			  ))
	(while (null (findfile (strcat folder "\\..\\"
				       (vl-filename-base folder)
				       "-" filename-bk)))
	  (sleep 3))
	(vl-file-delete file-bk)
	(vl-file-delete file-bk-utf8)
	(vl-file-delete (strcat folder "\\" filetmp))
	
	(setvar "cmdecho" 1)
	(system:explorer (strcat folder "\\..\\" ))
	))
  (princ)
  )

(defun pdftk:menu-burst (/ pdf-file)
  (@:help "拆分选择的 pdf 文件，并打开文件所在目录。")
  (if (setq pdf-file  (getfiled "请选择要拆分的PDF文件" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir (vl-filename-directory pdf-file )))
	(pdftk:burst pdf-file)
	(system:explorer (vl-filename-directory pdf-file)))))
(defun pdftk:burst (pdf-filename / app )
  "拆分 pdf 文件为单页文件"
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  (if (and (findfile app)
	   (= 'str (type-of pdf-filename))
	   (findfile pdf-filename))
      (progn
	(setvar "cmdecho" 0)
	(command
	 "start-bg"
	 (strcat " /D \"" (vl-filename-directory pdf-filename) "\""
		 " /MIN /B  "
		 @:*prefix* app
		 " \"" pdf-filename "\" "
		 "burst " 
		 ))
	(setvar "cmdecho" 1)
	))
  (princ)
  )

(defun pdftk:menu-encrypt (/ filename pw permission permissions)
  (@:help "选择一个 pdf 文件,设置所有者密码和用户密码，并打开文件所在目录。")
  (setq permission '(("Printing"  nil "高质量打印")
		     ("DegradedPrinting" nil "低质量打印")
		     ("ModifyContents" nil "编辑内容,同时允许 Assembly.")
		     ("Assembly" nil "组装")
		     ("CopyContents" nil "复制内容，同时允许 ScreenReaders")
		     ("ScreenReaders" nil "屏幕阅读器")
		     ("ModifyAnnotations" nil "编辑注释，同时允许 FillIn")
		     ("FillIn" nil "?填充?")
		     ("AllFeatures" nil "以上所有.")))

  (if (setq filename (getfiled "请选择要加密的PDF文件" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir (vl-filename-directory filename )))
	(setq pw (ui:input "请输入密码用于加密PDF:"
			   '(("所有者密码:" "" "请输入所有者密码" T)
			     ("所有者密码2:" "" "再次输入以确认" T)
			     ("用户密码:"  "" "请输入使用者密码" T)
			     ("用户密码2:" "" "再次输入以确认" T))))
	(setq permissions
	      (mapcar '(lambda (x) (car (string:to-list x ": ")))
		      (ui:select-multi "请选择用户权限:" (mapcar '(lambda (x) (strcat (car x) ": " (last x))) permission))))
	(if (null permissions)
	    (setq permissions '("")))
	
	(while (and pw
		    (or 
		     (not (eq (cdr (assoc "所有者密码:" pw))(cdr (assoc "所有者密码2:" pw))))
		     (not (eq (cdr (assoc "用户密码:" pw))(cdr (assoc "用户密码2:" pw))))))
	  (alert "请注意所有者或用户的两次输入必须相同。所有者和用户的密码可以不同。")
	  (setq pw (ui:input "请输入密码用于加密PDF:"
			     '(("所有者密码:" "" "请输入所有者密码" T)
			       ("所有者密码2:" "" "再次输入以确认" T)
			       ("用户密码:"  "" "请输入使用者密码" T)
			       ("用户密码2:" "" "再次输入以确认" T))))
	  )
	(if pw
	    (progn
	      (pdftk:encrypt filename (cdr (assoc "所有者密码:" pw)) (cdr (assoc "用户密码:" pw)) permissions)
	      (system:explorer (vl-filename-directory filename)))
	  ))))

(defun pdftk:encrypt (filename owner-pw user-pw permissions / app)
  "为 pdf 文件设置密码及用户权限。"
  ;; 可执行文件路径
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  ;;(setq files (vl-directory-files (system:get-folder) "*.pdf"))
  (if (= 'str (type permissions))
      (setq permissions (list permissions)))
  (if (findfile app)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg"
		 (strcat " /D \"" (vl-filename-directory filename) "\""
			 " /MIN /B  "
			 @:*prefix* app  " \""
			 filename "\" "
			 "output \""
			 (strcat (vl-filename-directory filename) "\\"
				 (vl-filename-base filename) "-加密.pdf\" ")
			 "owner_pw \"" owner-pw  "\" "
			 "user_pw \"" user-pw "\" "
			 "allow " (string:from-lst permissions " ") " "
			 ))
	(setvar "cmdecho" 1)
	))
  (princ)
  )

(defun pdftk:menu-decrypt (/ pdf-file)
  (@:help "选择一个 pdf 文件,去除所有者密码，并打开文件所在目录。")
  (if (setq pdf-file (getfiled "请选择要解密的PDF文件" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir (vl-filename-directory pdf-file )))
	(pdftk:decrypt
	 pdf-file
	 (cdr (assoc "所有者密码:"
		     (ui:input "请输入密码用于解密PDF:"
			       '(("所有者密码:" "" "请输入所有者密码" T))))))
	(system:explorer (vl-filename-directory filename))
	)))

(defun pdftk:decrypt (filename owner-pw / app )
  ;; 可执行文件路径
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  (if (and (findfile app)
	   owner-pw)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg"
		 (strcat " /D \"" (vl-filename-directory filename) "\""
			 " /MIN /B  "
			 @:*prefix* app  " \""
			 filename "\" "
			 "input_pw \"" owner-pw"\" "
			 "output \""
			 (strcat (vl-filename-directory filename) "\\"
				 (vl-filename-base filename) "-解密.pdf\" ")
			 ))
	(setvar "cmdecho" 1)
	))
  (princ)
  )
(defun pdftk:menu-stamp (/ filename)
  (@:help "选择一个文件，给该文件加上戳记，戳记文件在pdf设置中进行设置。")
  (if (setq filename (getfiled "请选择要加戳记的PDF文件" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config  'pdftk:pre-folder(system:dir (vl-filename-directory filename)))
	(pdftk:stamp filename)
	(system:explorer (vl-filename-directory filename)))))

(defun pdftk:stamp (filename / app)
  ;; 可执行文件路径
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  (if (null (findfile  (@:get-config 'pdftk:stamp)))
      (progn
	(vl-file-copy (strcat (@:package-path "pdftk") "stamp.pdf")
		      (@:get-config 'pdftk:stamp))
	(sleep 2)))
  ;;(setq files (vl-directory-files (system:get-folder) "*.pdf"))
  (if (and (findfile app)
	   filename)
      (progn
	;;(setq folder (system:get-folder "请选择要合并的PDF文件夹"))
	;;(setq files (strcat folder  "\\*.pdf"))
	(if (/= "" (@:get-config 'pdftk:stamp))
	    (progn
	      (setvar "cmdecho" 0)
	      (command "start-bg"
		       (strcat " /D \"" (vl-filename-directory filename) "\""
			       " /MIN /B  "
			       @:*prefix* app  " \""
			       filename "\" "
			       "stamp \"" (@:get-config 'pdftk:stamp)"\" "
			       "output \""
			       (strcat (vl-filename-directory filename) "\\"
				       (vl-filename-base filename) "-stamp.pdf\" ")
			       ))
	      (setvar "cmdecho" 1)
	      ))
	))
  (princ)
  )
(defun pdftk:menu-background (/ filename)
  (@:help "选择一个文件，给该文件加上水印，水印文件在pdf设置中进行设置。所选的 pdf 文件必须为透明背景才有效果。")
  (if (setq filename  (getfiled "请选择要加水印的PDF文件" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config  'pdftk:pre-folder (system:dir (vl-filename-directory filename)))
	(pdftk:background filename)
	(system:explorer (vl-filename-directory filename))
	)))

(defun pdftk:background (filename / app)
  ;; 可执行文件路径
  (setq app "bin\\pdftk.exe")
  (pdftk:download)
  (if (null (findfile (@:get-config 'pdftk:background)))
      (progn
	(vl-file-copy (strcat (@:package-path "pdftk") "background.pdf")
		      (@:get-config 'pdftk:background))
	(sleep 2)
	))
  ;;(setq files (vl-directory-files (system:get-folder) "*.pdf"))
  (if (and (findfile app)
	   filename)
      (progn
	;;(setq folder (system:get-folder "请选择要合并的PDF文件夹"))
	;;(setq files (strcat folder  "\\*.pdf"))
	(if (/= "" (@:get-config 'pdftk:background))
	    (progn
	      (setvar "cmdecho" 0)
	      (command "start-bg"
		       (strcat " /D \"" (vl-filename-directory filename) "\""
			       " /MIN /B  "
			       @:*prefix* app  " \""
			       filename "\" "
			       "background \""(@:get-config 'pdftk:background)"\" "
			       "output \""
			       (strcat (vl-filename-directory filename) "\\"
				       (vl-filename-base filename) "-background.pdf\" ")
			       ))
	      (setvar "cmdecho" 1)
	      ))
	))
  (princ)
  )
