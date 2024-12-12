;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'pdftk:first for package pdftk 's configitem first 
(@:define-config 'pdftk:stamp "D:\\Design\\standard\\stamp.pdf" "���ڼ�Ϊ���ǵ�pdf�ļ�")
(@:define-config 'pdftk:background "D:\\Design\\standard\\background.pdf" "������Ϊˮӡ��pdf�ļ�������ˮӡ��PDF�ļ���Ϊ͸��������")
(@:define-config 'pdftk:pre-folder "D:\\" "ѡ��pdf�ļ�·��ʱ�����һ�β������ļ�λ�á�")
(if (null (findfile (@:get-config 'pdftk:stamp)))
    (@:mkdir (@:path (vl-filename-directory(@:get-config 'pdftk:stamp)))))
;; (@:get-config 'pdftk:first) 
;; (@:set-config 'pdftk:first  "New Value")
;; Add menu in @lisp panel
(@::add-menus '("PDF���"
	       ("PDF����" "(pdftk:setup)" )
	       ("�ϲ�PDF" "(pdftk:menu-merge)" )
	       ("���PDF" "(pdftk:menu-burst)" )
	       ("PDF�Ӵ���" "(pdftk:menu-stamp)" )
	       ("�����Ӵ���" "(pdftk:menu-batch-stamp)" )
	       ("PDF��ˮӡ" "(pdftk:menu-background)")
	       ("������ˮӡ" "(pdftk:menu-batch-background)")
	       ("����PDF" "(pdftk:menu-decrypt)" )
	       ("����PDF" "(pdftk:menu-encrypt)" )
	       ))
(or @:enable-start
    (@:check-pgp)
    (@:patch-pgp) 
    )
(defun pdftk:setup (/ res)
  "pdf tools"
  (setq res 
	(ui:input "������Ϣ"
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
      ;; ����ѹ����
      (@:down-and-unzip "archives/pdftk.zip" "bin"))
  (if (null (findfile "packages\\pdftk\\background.pdf"))
      (@:down-pkg-file (@:uri)"pdftk/background.pdf" "stable"))
  (if (null (findfile "packages\\pdftk\\stamp.pdf"))
      (@:down-pkg-file (@:uri)"pdftk/stamp.pdf" "stable"))
  
  )

(defun pdftk:menu-merge (/ folder)
  (@::prompt "�ϲ�ѡ����ļ����µ�����pdf,������Ŀ¼�����򿪸�Ŀ¼��")
  (if (setq folder (system:get-folder "��ѡ��Ҫ�ϲ���PDF�ļ���"))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir folder))
	(pdftk:merge folder)
	(system:explorer (strcat folder "\\..\\" )))))
(defun pdftk:merge (folder / app files filetmp filename-bk fp-bookmark file-bk file-bk-utf8)
  "������folder �ļ����µ� pdf �ļ��ϲ���һ���ļ����ϲ�����ļ���Ϊ `�ļ���-merge-all.pdf' "
  ""
  "(pdftk:merge \"D:\\Output\\A��Ŀ\")"
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
	;;������ǩ�ļ���
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
	;; ת��
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
  (@::prompt "���ѡ��� pdf �ļ��������ļ�����Ŀ¼��")
  (if (setq pdf-file  (getfiled "��ѡ��Ҫ��ֵ�PDF�ļ�" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir (vl-filename-directory pdf-file )))
	(pdftk:burst pdf-file)
	(system:explorer (vl-filename-directory pdf-file)))))
(defun pdftk:burst (pdf-filename / app )
  "��� pdf �ļ�Ϊ��ҳ�ļ�"
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
  (@::prompt "ѡ��һ�� pdf �ļ�,����������������û����룬�����ļ�����Ŀ¼��")
  (setq permission '(("Printing"  nil "��������ӡ")
		     ("DegradedPrinting" nil "��������ӡ")
		     ("ModifyContents" nil "�༭����,ͬʱ���� Assembly.")
		     ("Assembly" nil "��װ")
		     ("CopyContents" nil "�������ݣ�ͬʱ���� ScreenReaders")
		     ("ScreenReaders" nil "��Ļ�Ķ���")
		     ("ModifyAnnotations" nil "�༭ע�ͣ�ͬʱ���� FillIn")
		     ("FillIn" nil "?���?")
		     ("AllFeatures" nil "��������.")))

  (if (setq filename (getfiled "��ѡ��Ҫ���ܵ�PDF�ļ�" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir (vl-filename-directory filename )))
	(setq pw (ui:input "�������������ڼ���PDF:"
			   '(("����������:" "" "����������������" T)
			     ("����������2:" "" "�ٴ�������ȷ��" T)
			     ("�û�����:"  "" "������ʹ��������" T)
			     ("�û�����2:" "" "�ٴ�������ȷ��" T))))
	(setq permissions
	      (mapcar '(lambda (x) (car (string:to-list x ": ")))
		      (ui:select-multi "��ѡ���û�Ȩ��:" (mapcar '(lambda (x) (strcat (car x) ": " (last x))) permission))))
	(if (null permissions)
	    (setq permissions '("")))
	
	(while (and pw
		    (or 
		     (not (eq (cdr (assoc "����������:" pw))(cdr (assoc "����������2:" pw))))
		     (not (eq (cdr (assoc "�û�����:" pw))(cdr (assoc "�û�����2:" pw))))))
	  (alert "��ע�������߻��û����������������ͬ�������ߺ��û���������Բ�ͬ��")
	  (setq pw (ui:input "�������������ڼ���PDF:"
			     '(("����������:" "" "����������������" T)
			       ("����������2:" "" "�ٴ�������ȷ��" T)
			       ("�û�����:"  "" "������ʹ��������" T)
			       ("�û�����2:" "" "�ٴ�������ȷ��" T))))
	  )
	(if pw
	    (progn
	      (pdftk:encrypt filename (cdr (assoc "����������:" pw)) (cdr (assoc "�û�����:" pw)) permissions)
	      (system:explorer (vl-filename-directory filename)))
	  ))))

(defun pdftk:encrypt (filename owner-pw user-pw permissions / app)
  "Ϊ pdf �ļ��������뼰�û�Ȩ�ޡ�"
  ;; ��ִ���ļ�·��
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
				 (vl-filename-base filename) "-����.pdf\" ")
			 "owner_pw \"" owner-pw  "\" "
			 "user_pw \"" user-pw "\" "
			 "allow " (string:from-lst permissions " ") " "
			 ))
	(setvar "cmdecho" 1)
	))
  (princ)
  )

(defun pdftk:menu-decrypt (/ pdf-file)
  (@::prompt "ѡ��һ�� pdf �ļ�,ȥ�����������룬�����ļ�����Ŀ¼��")
  (if (setq pdf-file (getfiled "��ѡ��Ҫ���ܵ�PDF�ļ�" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config 'pdftk:pre-folder (system:dir (vl-filename-directory pdf-file )))
	(pdftk:decrypt
	 pdf-file
	 (cdr (assoc "����������:"
		     (ui:input "�������������ڽ���PDF:"
			       '(("����������:" "" "����������������" T))))))
	(system:explorer (vl-filename-directory filename))
	)))

(defun pdftk:decrypt (filename owner-pw / app )
  ;; ��ִ���ļ�·��
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
				 (vl-filename-base filename) "-����.pdf\" ")
			 ))
	(setvar "cmdecho" 1)
	))
  (princ)
  )
(defun pdftk:menu-stamp (/ filename)
  (@::prompt "ѡ��һ���ļ��������ļ����ϴ��ǣ������ļ���pdf�����н������á�")
  (if (setq filename (getfiled "��ѡ��Ҫ�Ӵ��ǵ�PDF�ļ�" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config  'pdftk:pre-folder(system:dir (vl-filename-directory filename)))
	(pdftk:stamp filename)
	(system:explorer (vl-filename-directory filename)))))
(defun pdftk:menu-batch-stamp (/ filename)
  (@::prompt "ѡ��һ���ļ��У������ļ����µ�pdf�ļ����ϴ��ǣ������ļ���pdf�����н������á�")
  (if (setq pathname (system:get-folder "��ѡ��Ҫ�Ӵ��ǵ�PDF�ļ���"))
      (progn
	(@:set-config  'pdftk:pre-folder (system:dir pathname))
	(setq pdfs (vl-directory-files pathname "*.pdf"))
	(foreach
	 pdf% pdfs
	 (if (null (wcmatch (strcase pdf% t) "*-background.pdf"))
	     (pdftk:stamp (strcat (system:dir pathname) pdf%))))
	(system:explorer (system:dir pathname))
	)))
(defun pdftk:stamp (filename / app)
  ;; ��ִ���ļ�·��
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
	;;(setq folder (system:get-folder "��ѡ��Ҫ�ϲ���PDF�ļ���"))
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
  (@::prompt "ѡ��һ���ļ��������ļ�����ˮӡ��ˮӡ�ļ���pdf�����н������á���ѡ�� pdf �ļ�����Ϊ͸����������Ч����")
  (if (setq filename  (getfiled "��ѡ��Ҫ��ˮӡ��PDF�ļ�" (@:get-config 'pdftk:pre-folder) "pdf" 8))
      (progn
	(@:set-config  'pdftk:pre-folder (system:dir (vl-filename-directory filename)))
	(pdftk:background filename)
	(system:explorer (vl-filename-directory filename))
	)))
(defun pdftk:menu-batch-background (/ filename)
  (@::prompt "ѡ��һ���ļ��У������ļ����µ�pdf�ļ�����ˮӡ��ˮӡ�ļ���pdf�����н������á���ѡ�� pdf �ļ�����Ϊ͸����������Ч����")
  (if (setq pathname (system:get-folder "��ѡ��Ҫ��ˮӡ��PDF�ļ���"))
      (progn
	(@:set-config  'pdftk:pre-folder (system:dir pathname))
	(setq pdfs (vl-directory-files pathname "*.pdf"))
	(foreach
	 pdf% pdfs
	 (if (null (wcmatch (strcase pdf% t) "*-background.pdf"))
	     (pdftk:background (strcat (system:dir pathname) pdf%))))
	(system:explorer (system:dir pathname))
	)))

(defun pdftk:background (filename / app)
  ;; ��ִ���ļ�·��
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
	;;(setq folder (system:get-folder "��ѡ��Ҫ�ϲ���PDF�ļ���"))
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
