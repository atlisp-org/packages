
(defun string:sort-by-number (lst / b b-a c len maxn str2 str3 str4 xx)
  (defun xx (str n)
    (apply 'strcat (mapcar '(lambda (x) (if (wcmatch x "~*[~0-9]*") (try-bu0 n (atoi x)) x)) (try-StrRegExp str "\\D+|\\d+")))
    )
  (setq
   str2 (mapcar '(lambda (x) (try-StrRegExp x "\\D+|\\d+")) lst)
   str3 (try-lst-nto1 str2)
   str4 (vl-remove-if-not '(lambda (x) (wcmatch x "~*[~0-9]*")) str3)
   len (mapcar 'strlen str4)
   maxn(apply 'max len);寻找最大的数字
   b (mapcar '(lambda(x) (xx x maxn)) lst)
   b-a (mapcar 'list b lst)
   c (try-str-sort b-a 0)
   )
  (mapcar 'cadr c)
  )


(defun try-bu0 (n0 num / _000)
  (setq _000 "")
  (repeat(- n0 (strlen(rtos num 2 0)))(setq _000(strcat "0" _000)))
  (strcat _000 (rtos num 2 0))
  )
(defun try-StrRegExp(str1 expr)
  (_Replace str1 expr nil "")
  )
(defun try-lst-nto1(lst / lst1 a)
  (setq lst1 '())
  (if (listp lst)
      (progn
	(foreach x lst
		 (setq a (try-lst-nto1 x))
		 (if (listp a)
		     (setq lst1 (append lst1  a))
		     (setq lst1 (append lst1 (list a)))
		     )
		 )
	)
      (setq lst1 lst)
      )
  lst1
  )

(defun try-str-sort (lst n / a b lst-ret str_ac str_sort strs)
  (setq 
   strs(mapcar '(lambda(x)(nth n x))lst)
   str_ac(mapcar 'cons strs lst)
   str_sort(acad_strlsort strs)
   )
  (if str_sort
      (progn 
	(while(setq a(car str_sort))
	  (setq 
	   str_sort(cdr str_sort)
	   b(assoc a str_ac)
	   n(vl-position b str_ac);寻找索引
	   str_ac(try-lst-move str_ac n);删除表中指定索引元素
	   lst-ret(cons (cdr b)lst-ret)
	   )
	  )
	(reverse lst-ret)
	)
      )
  )
(defun _Replace(str1 str2 bull str3 / lst matchcollect reg)
  (setq lst '())
  (setq reg (vlax-create-object "vbscript.regexp")) ;创建正则表达式
  (if (null reg)
      (progn
	(alert "发现系统vbscript没有注册，现尝试对其注册")
	(command"shell" "copy %systemroot%\\System32\\vbscript.dll %systemroot%\\System\\")
					;(command"shell" "copy C:\\Windows\\System32\\vbscript.dll C:\\Windows\\")
	(command"shell" "regsvr32 vbscript.dll")
	(setq reg (vlax-create-object "vbscript.regexp"))
	(if (null reg) 
	    (progn 
	      (setq file32 "C:\\Windows\\System32\\vbscript.dll"
		    file "C:\\Windows\\System\\vbscript.dll"
		    vbsfile32(findfile file32)
		    vbsfile(findfile file)
		    )
	      (cond 
		((and vbsfile32 (null vbsfile))(vl-file-copy vbsfile32 file))
		((and vbsfile (null vbsfile32))(vl-file-copy vbsfile file32))
		)
	      (command"shell" "regsvr32 vbscript.dll")
	      (setq reg (vlax-create-object "vbscript.regexp"))
	      (if (null reg)(princ "\nvbscript组件注册失败，请在以下目录寻找vbscript.dll文件并复制到以下几个目录中\nC:\\Windows、C:\\Windows\\System32、C:\\Windows\\System、C:\\Windows\\SysWOW64"))
	      (princ)
	      )
	    )
	)
      )
  (vlax-put-property reg 'global -1) ;是否匹配全部 （-1是 ，0 不是）
  (vlax-put-property reg 'Multiline -1);是否多行匹配 （-1是 ，0 不是）
  (vlax-put-property reg 'IgnoreCase -1);是否忽略大小写 （-1是 ，0 不是）
  (vlax-put-property reg 'pattern str2);lisp \\
  ;; 	1.(vlax-invoke-method reg 'test str)判断字符串是否与正则表达式匹配
  (if (vlax-invoke-method reg 'test str1)
      ;; 	2.(vlax-invoke-method reg 'Execute str)生成匹配集合	  
      (progn (setq matchcollect (vlax-invoke-method reg 'Execute str1))
	     ;; 	3.打印匹配的每个集合元素的value		
	     (vlax-for match_item matchcollect (setq lst(cons(eval (vlax-get-property match_item 'value))lst)))
	     )
      )
	;;; 	4.替换匹配的值	(vlax-invoke-method reg 'Replace str "replace")	生成str副本  	
  (setq lst(reverse lst))
  (if bull
      (setq lst(vlax-invoke-method reg 'Replace str1 str3)))
	;;;  ----------------- end 正则表达式方法
  (vlax-release-object reg);释放内存
  lst
  )
(defun try-lst-move(lst n / i)
  (setq i -1)
  (vl-remove-if '(lambda (x) (= (setq i (1+ i)) n)) lst)
  )
