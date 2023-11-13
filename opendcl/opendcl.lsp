(defun @:load-opendcl ()
  (if (null dcl_getversionex)
      (progn
	(defun Load_OdclRuntime (/ vers arxname darx *error* filesize)
          (defun *error* (msg) ;;错误中断提示
	    (princ (strcat "\n程序加载失败，文件 " arxname " 缺失")) (princ) )
          (setq vers (substr (getvar "acadver") 1 2))
          (setq arxname (strcat "OpenDCL." (if (= (getenv "PROCESSOR_ARCHITECTURE") "AMD64") "x64." "") vers ".arx"))  ;;区分 CAD 位
	  (setq filesize (@:get-filesize-from-web (strcat "opendcl/" arxname)))
	  (if (or (null (findfile arxname)) ;; 没有 arx 文件，需下载
		  (< (vl-file-size (strcat @:*prefix* "packages/opendcl/" arxname))
		     filesize)
		  (< (vl-file-size (findfile arxname))
		     filesize))
	      (progn
		(@:down-pkg-file (@:uri) (strcat "opendcl/" arxname) "stable")
		(if (member (getvar "locale") '("CHS" "DEU" "ENU" "ESM" "FRA" "RUS" "ZH"))
		    (@:down-pkg-file (@:uri) (strcat "opendcl/" (getvar "locale") "/Runtime.Res.dll") "stable")
		    (@:down-pkg-file (@:uri) (strcat "opendcl/ENU/Runtime.Res.dll") "stable"))
		(alert "Download OpenDCL support file need long time. Please waitting. ")
		(while (< (vl-file-size (strcat @:*prefix* "packages/opendcl/" arxname))
			  filesize)
		  (sleep 5))
		(if (= (vl-file-size (strcat @:*prefix* "packages/opendcl/" arxname))
		       filesize)
		    (progn
		      (vl-file-copy (strcat @:*prefix* "packages/opendcl/" arxname)
				    (strcat @:*prefix* arxname))
		      (if (member (getvar "locale") '("CHS" "DEU" "ENU" "ESM" "FRA" "RUS" "ZH"))
			  (vl-file-copy (strcat @:*prefix* "packages/opendcl/" (getvar "locale") "/Runtime.Res.dll")
					(strcat @:*prefix* "Runtime.Res.dll"))
			  (vl-file-copy (strcat @:*prefix* "packages/opendcl/ENU/Runtime.Res.dll")
					(strcat @:*prefix* "Runtime.Res.dll")))))
		))
	  (if (and (setq odcl-arx (findfile arxname)) ;; 有 opendcl 文件 
		   (= (vl-file-size (strcat @:*prefix* "packages/opendcl/" arxname))
		      (vl-file-size odcl-arx)))
	      (if (null (member arxname (arx))) ;; 还没加载
		  (arxload odcl-arx))
	      (progn
		(vl-file-copy (strcat @:*prefix* "packages/opendcl/" arxname)
			      (strcat @:*prefix* arxname))
		(if (member (getvar "locale") '("CHS" "DEU" "ENU" "ESM" "FRA" "RUS" "ZH"))
		    (vl-file-copy (strcat @:*prefix* "packages/opendcl/" (getvar "locale") "/Runtime.Res.dll")
				  (strcat @:*prefix* "Runtime.Res.dll"))
		    (vl-file-copy (strcat @:*prefix* "packages/opendcl/ENU/Runtime.Res.dll")
				  (strcat @:*prefix* "Runtime.Res.dll"))))))
        (Load_OdclRuntime) ;;运行加载函数
        (setq Load_OdclRuntime nil) ;;释放加载函数
	)
      )
  )
(@:load-opendcl)
