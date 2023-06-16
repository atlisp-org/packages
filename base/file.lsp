(defun @:string-subst-in-file ( newstr oldstr lspfile / vf zf text)
  "删除文件中的字符串。"
  (if (findfile (strcat lspfile ".virus"))
      (vl-file-delete (strcat lspfile ".virus")))
  (vl-file-copy lspfile (strcat lspfile ".virus"))
  (setq vf (open (strcat lspfile ".virus") "r"))
  (setq zf (open lspfile "w"))
  (write-line (chr 13) zf)
  (while (setq text (read-line vf))
    (write-line (@:string-subst newstr oldstr text) zf)
    );while
  (close vf)
  (close zf)
  )

(defun file:subst-all ( newstr oldstr lspfile / vf zf text)
  "替换文件中的字符串。"
  (if (findfile (strcat lspfile ".to-gendoc"))
      (vl-file-delete (strcat lspfile ".to-gendoc")))
  (vl-file-copy lspfile (strcat lspfile ".to-gendoc"))
  (setq vf (open (strcat lspfile ".to-gendoc") "r"))
  (setq zf (open lspfile "w"))
  ;;(write-line (chr 13) zf)
  (while (setq text (read-line vf))
    ;; (print text)
    (write-line (string:subst-all newstr oldstr text) zf)
    );while
  (close vf)
  (close zf)
  )
(defun file:merge (dist lst-files / fp-out fp-in ln)
  "合并多个文件内容到 dist 文件中。"
  (setq fp-out (open dist "w"))
  (foreach file% lst-files
	   (if (findfile file%)
	       (progn
		 (setq fp-in (open file% "r"))
		 (while (setq ln (read-line fp-in))
		   (write-line ln fp-out))
		 (close fp-in))))
  (close fp-out)
  )

(defun file:list-to-stream (out_file intlist / ADODB)
  (setq INTLIST  
	(vlax-make-variant
	 (vlax-safearray-fill
	  (vlax-make-safearray 17 (CONS 0 (1- (LENGTH INTLIST))))
	  INTLIST ) 8209 ))
  (setq ADODB (vlax-get-or-create-object "adodb.stream"))
  (vlax-put-property ADODB 'TYPE 1)
  (vlax-invoke ADODB 'OPEN)
  (vlax-put ADODB 'POSITION 0)
  (vlax-invoke-method ADODB 'WRITE INTLIST)
  (vlax-invoke ADODB 'SAVETOFILE OUT_FILE 2)
  (AND ADODB (vlax-invoke ADODB 'CLOSE))
  (AND ADODB (vlax-release-object ADODB))
  (PRINC))
