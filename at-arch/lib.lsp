
(defun at-arch:insert-block (dwgname / downfile)
  (@::help "插入图块模板")
  (setq downfile (strcat "at-arch/" dwgname ".dwg"))
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))(sleep 5))
      )
  (if (findfile (strcat "packages/" downfile))
      (progn
	(ui:dyndraw
	 (block:insert dwgname (@::package-path "at-arch") '(0 0 0)0 1)
	 '(0 0 0)))
    ))
