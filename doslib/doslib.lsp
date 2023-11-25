(defun doslib:load (/ vers arxname darx *error* filesize)
  (defun *error* (msg) ;;错误中断提示
    (princ (strcat "\n程序加载失败，文件 " arxname " 缺失")) (princ) )
  
  (setq vers (substr (getvar "acadver") 1 2))
  (setq arxname (strcat "DOSLib" vers (if (= (getenv "PROCESSOR_ARCHITECTURE") "AMD64") "x64" "") ".arx")) 
  (if (or(null (findfile arxname)) ;; 没有 arx 文件，或大小不一致需下载
	 (and (setq filesize (@:get-filesize-from-web (strcat "doslib/" arxname)))
	  (< (vl-file-size (strcat @:*prefix* "packages/doslib/" arxname))
	     filesize)))
      (progn
	(@:down-pkg-file (@:uri) (strcat "doslib/" arxname) "stable")
	(prin1 "Download DOSLib support file need long time. Please waitting. ")
	(while (< (vl-file-size (strcat @:*prefix* "packages/doslib/" arxname))
		  filesize)
	  (sleep 5))
	(if (= (vl-file-size (strcat @:*prefix* "packages/doslib/" arxname))
	       filesize)
	    (progn
	      (vl-file-copy (strcat @:*prefix* "packages/doslib/" arxname)
			    (strcat @:*prefix* arxname))
	      ))
	(if (and (setq doslib-arx (findfile arxname)) ;; 有 doslib 文件 
		 (= (vl-file-size (strcat @:*prefix* "packages/doslib/" arxname))
		    (vl-file-size doslib-arx)))
	    (if (null (member arxname (arx))) ;; 还没加载
		(arxload doslib-arx))
	  (progn
	    (vl-file-copy (strcat @:*prefix* "packages/doslib/" arxname)
			  (strcat @:*prefix* arxname))
	    (if (null (member arxname (arx))) ;; 还没加载
		(arxload doslib-arx))
	    )))
      (arxload arxname)))
(doslib:load)
