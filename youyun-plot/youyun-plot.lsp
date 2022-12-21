(@:add-menu "出图排版" "有云批打" "(youyun-plot:bplot)")
(defun youyun-plot:bplot (/ mainpath) 
  ;; (setq mainpath (strcat (getenv "userprofile") "\\Documents\\有云批量打印"))
  (setq mainpath (strcat 
                  (vl-registry-read "HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" 
                                    "Personal"
				    )
                  "\\有云批量打印"
                  )
	)
  ;; 不是链接
  (if (and (findfile mainpath)
	   (null(member "pkg.lsp" (vl-directory-files mainpath "*.*" 1))))
      (vl-file-rename
       mainpath
       (strcat mainpath ".b")))
  ;; 创建目录链接
  (if (null (findfile mainpath))
      (progn
	(@:cmd 
	 "start-bg"
	 (strcat "mklink /J " 
		 "\""
		 mainpath
		 "\" "
		 (@:package-path "youyun-plot")
		 )
	 )))
  (load ".cache/Intern.fas")
  (setvar "secureload" 0)
  (@:mkdir (@:path (strcat (@:package-path "youyun-plot") "有云字体替换")))

  ;; 复制 pdftk 
  (if (null (findfile (strcat mainpath "\\pdftk.exe"))) 
      (progn 
	(@:down-and-unzip "archives/pdftk.zip" "bin")
	)
    )
  (if 
      (and (null (findfile (strcat mainpath "\\pdftk.exe"))) 
           (findfile (strcat @:*prefix* "bin\\pdftk.exe"))
	   )
      (foreach file% '("pdftk.exe" "libiconv2.dll") 
	       (vl-file-copy (strcat @:*prefix* "bin\\" file%) 
			     (strcat mainpath "\\" file%)
			     )
	       )
    )
  
  ;; 检查文件大小
  (if 
      (and (findfile "packages/youyun-plot/youyun-plot.zip") 
           (< (vl-file-size (findfile "packages/youyun-plot/youyun-plot.zip")) (@:get-filesize-from-web "youyun-plot/youyun-plot.zip"))
	   )
      (progn
	(alert "文件不完整，请重新安装。"))
    (progn
      (if 
          (and (null (findfile (strcat (@:package-path "youyun-plot") "批打PDF-设置.txt")))
               (findfile (strcat (@:package-path "youyun-plot") "youyun-plot.zip"))
               )
          (@:unzip 
           (strcat (@:package-path "youyun-plot") "youyun-plot.zip")
           "packages\\youyun-plot"
           )
	)
      (foreach file% 
               (vl-directory-files (@:package-path "youyun-plot\\预设模板\\初始设置") "*.*" 1)
               (if 
		   (and (null (member (vl-filename-extension file%) '(".lsp" ".fas"))) 
			(null (findfile (strcat mainpath "\\" file%)))
			)
		   (vl-file-copy 
		    (findfile (strcat (@:package-path "youyun-plot\\预设模板\\初始设置") file%))
		    (strcat mainpath "\\" file%)
		    )
		 )
	       )
      (if (/= "failure" (load "packages/youyun-plot/有云批量打印.vlx" "failure"))
	  (C:PDF1))
      )
    )
  (setvar "secureload" 1)
  (princ)
  )

