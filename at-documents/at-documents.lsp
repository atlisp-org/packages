;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-documents:first 用于 应用包 at-documents 的 第一个配置项 first 
;;(@:define-config 'at-documents:first "我是配置项 at-documents:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-documents:first) ;; 获取配置顶的值
;; (@:set-config 'at-documents:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-devmenu  (_"Support") "@通用函数库" "(at-documents:lib-manager-dialog)" )
(if (findfile "packages/base/base-whole-dev.lsp")
    (load "packages/base/base-whole-dev.lsp")
  (@:down-pkg-file (@:uri) "base/base-whole-dev.lsp" "stable")
  )
(defun at-documents:load-libdoc (/ files fp opt%)
  (setq @:*libdoc* '())
  (setq @:*libdoc-category* '())
  (if (setq files (vl-directory-files (strcat @:*prefix* "packages\\at-documents\\") "*.libdoc" 1))
      (foreach file% files
	       (setq fp (open (strcat @:*prefix* "packages\\at-documents\\" file%) "r"))
	       (while (setq opt% (read-line fp))
		 (if (/= opt% "")
		     (setq @:*libdoc* (append @:*libdoc* (list (read opt%))))))
	       (setq @:*libdoc-category* (append @:*libdoc-category* (list (vl-filename-base file%))))
	       (close fp)))
  (setq @:*libdoc* (vl-sort (vl-remove nil @:*libdoc*) '(lambda (e1 e2) (< (car e1)(car e2)))))
  (setq @:*libdoc-category* (vl-remove nil @:*libdoc-category*))
  )

(defun at-documents:lib-manager-dialog (/ dcl-tmp dcl_fp dcl_id pkg para% libdocs-list curr-page 
				 page-up page-down
				 @:package-callback-accept @:package-callback-libdoc-name
				 list-libdoc-by-category list-libdoc-by-author list-libdoc-by-search
				 toggle-change show-detail show-src init-dialog indent)
  (defun indent (str / curr-ind% res)
    (setq curr-ind% 0)
    (setq res '())
    (foreach char% (vl-string->list str)
	     (cond
	      ((= 40 char%)
		 (setq curr-ind% (1+ curr-ind%)))
	      ((= 41 char%)
	       (setq curr-ind% (1- curr-ind%))))
	     (setq res (cons char% res))
	     (if (= 10 char%)
		 (repeat (* 4 curr-ind%)
			 (setq res (cons 32 res)))))
	       
    (vl-list->string (reverse res))
    )
				 
  (defun page-up ()
    (setq curr-page (1- curr-page))
    (show-libdocs-list))
  (defun page-down ()
    (setq curr-page (1+ curr-page))
    (show-libdocs-list))
  (defun show-detail (number / func-info)
    (setq func-info (nth (+ (* 20 curr-page) (1- number)) libdocs-list))
    (alert (strcat "函数名:  " (@:to-string(car func-info))
		   "\n功能说明: " (@:to-string(cadr func-info))
		   "\n用法:\n  "(@:to-string (caddr func-info))
		   "\n参数:\n"(@:to-string(nth 3 func-info))
		   "\n返回值:\n   "(@:to-string(nth 4 func-info))
		   "\n示例:\n  "(@:to-string (nth 5 func-info))
		   ))
    )
  (defun show-src (number / func-info)
    (setq func-info (nth (+ (* 20 curr-page) (1- number)) libdocs-list))
    (alert (fun:src-code (car func-info)))
    ;; (alert (strcat "函数名:  " (@:to-string(car func-info))
		;;    "\n----\n"
		;;    (indent
		;;     (strcase
		;;      (@:string-subst
		;;       "\"\n" "\" "
		;;       (@:string-subst
		;;        ")\n" ") "
		;;        (vl-prin1-to-string
		;; 	(cons 'defun
		;; 	     (cons (read(car func-info))
		;; 		   (defun-q-list-ref (read(car func-info))))))))
		;;      T)
		;;     )))
    )
  (defun list-libdoc-by-category ( / i%)
    ;; (set_tile "author" "0")		
    (if (= 0 (atoi (get_tile "category")))
	(setq libdocs-list @:*libdoc*)
	(setq libdocs-list (vl-remove nil (mapcar '(lambda (x) (if (eq
								    (nth (1- (atoi (get_tile "category"))) @:*libdoc-category*)
								    (car (string:to-lst (strcase (car x)  T) ":")))
								   x nil))
						  @:*libdoc*))))
    (setq curr-page 0)
    (show-libdocs-list))
  (defun list-libdoc-by-search ( / i% search-str)
    (setq search-str (strcase (get_tile "search_box") T))
    (setq libdocs-list 
	  (vl-remove nil (mapcar '(lambda (x)
				   (if (or (vl-string-search search-str (strcase (car x) T))
					   (vl-string-search search-str (strcase (cadr x) T))
					   (vl-string-search search-str (strcase (caddr x) T))
					   )
				       x nil))
				 @:*libdoc*)))
    (setq curr-page 0)
    (show-libdocs-list)
    )
  (defun show-libdocs-list ()
    (setq i% 0)
    (repeat (min 20 (- (length libdocs-list) (* 20 curr-page)))
	    (set_tile (strcat "pkgn"(itoa (1+ i%))) (caddr (nth (+ i% (* 20 curr-page)) libdocs-list)))
	    (set_tile (strcat "pkgdesc"(itoa (1+ i%))) (cadr (nth (+ i% (* 20 curr-page)) libdocs-list)))
	    (mode_tile (strcat "pkg"(itoa (1+ i%))) 0)(mode_tile (strcat "pkgs"(itoa (1+ i%))) 0)
	    (setq i% (1+ i%)))
    (while (< i% 20)
      (set_tile (strcat "pkgn"(itoa (1+ i%))) "")
      (set_tile (strcat "pkgdesc"(itoa (1+ i%))) "")
      (mode_tile (strcat "pkg"(itoa (1+ i%))) 1)
      (mode_tile (strcat "pkgs"(itoa (1+ i%))) 1)
      (setq i% (1+ i%)))
    (set_tile "curr_total" (strcat (itoa (1+ curr-page)) "/" (itoa (1+ (/ (1- (length libdocs-list)) 20)))))
    (if (= 0 curr-page) (mode_tile "prev" 1) (mode_tile "prev" 0))
    (if (= (/ (1- (length libdocs-list)) 20) curr-page) (mode_tile "next" 1) (mode_tile "next" 0)))
  (defun init-dialog ()
    (setq libdocs-list @:*libdoc*)
    (setq curr-page 0)
    (show-libdocs-list))
  ;; 读取包列表
  (at-documents:load-libdoc)
  (setq libdocs-list @:*libdoc*)
  (setq curr-page 0)
  ;; 生成 dcl 文件
  (setq dcl-tmp (strcat @:*tmp-path* "tmp-lib-man.dcl" ))
  ;; (setq dcl-tmp (vl-filename-mktemp nil nil ".dcl" ))
  (setq dcl_fp (open dcl-tmp "w"))
  (write-line (strcat "lib_man : dialog {"
		      "label = \"" (_"Function Library") "\";"
		      ":column {children_alignment=left;:row { "
		      "  :popup_list{label=\""(_"Category")":\";key=\"category\";fixed_width=true;width=30;}"
		      ;; "  :popup_list{label=\""(_"Author")":\";key=\"author\";fixed_width=true;width=30;}"
		      "  :edit_box {key=\"search_box\";action=\"(list-libdoc-by-search)\";}:button{label=\"search\";key=\"searchbtn\";fixed_width=true;width=10;is_default=true;}}}"
		      ": column { ") dcl_fp)
  (write-line (strcat "label=\""(_"base function.") "\";") dcl_fp)
  (write-line (strcat ": row { : text { value=\"          "(_"Name") "\";width=48;fixed_width=true;}"
			 ": text { value=\"          "(_"?") "\";width=10;fixed_width=true;}"
			 " : text { value=\"\t   "(_"Description")"\"; width=85;}} : spacer {}")
	      dcl_fp)
  (setq i% 0)
  (repeat 20
	  (write-line (strcat ":row{"
			      ":text{key=\"pkgn"(itoa (1+ i%))"\";value=\"\";width=48;fixed_width=true;}"
			      ":button{key=\"pkg"(itoa (1+ i%))"\";label=\"Detail\";width=10;fixed_width=true;action=\"(show-detail "(itoa (1+ i%))")\";is_enabled=false;}"
			      ":button{key=\"pkgs"(itoa (1+ i%))"\";label=\"Src\";width=10;fixed_width=true;action=\"(show-src "(itoa (1+ i%))")\";is_enabled=false;}"
			      ":text{key=\"pkgdesc"(itoa (1+ i%))"\";value=\"\";width=85;fixed_width=true;}}")
		      dcl_fp)
	  (setq i% (1+ i%)))
  (write-line ":spacer{}}:spacer{}" dcl_fp)
  (write-line ":row{alignment=centered;children_alignment=centered;:button{label=\"<\";key=\"prev\";is_enabled=false;}:spacer{} :text_part{key=\"curr_total\"; value=\"\";alignment=\"centered\";width=10;}:button{label=\">\";key=\"next\";is_enabled=false;}}"
	      ;;(if (= 0 curr-page) "false" "true")
	      ;;(strcat (itoa (1+ curr-page)) "/" (itoa (1+ (/ (length libdocs-list) 20))))
	      ;;(if (= (/ (length libdocs-list) 20) curr-page) "false" "true")
	      dcl_fp
	  )
  (write-line ":spacer{} ok_cancel; }" dcl_fp)
  (close dcl_fp)
  
  (setq dcl_id (load_dialog dcl-tmp))
  (if (not (new_dialog "lib_man" dcl_id ))
      (exit))
  (start_list "category")
  (add_list "")
  (mapcar 'add_list @:*libdoc-category*)
  (end_list)
  ;; (start_list "author")
  ;; (add_list "")
  ;; (mapcar 'add_list @:*libdoc-author*)
  ;; (end_list)
  (action_tile "category" "(list-libdoc-by-category)")
  ;; (action_tile "author" "(list-libdoc-by-author)")
  (action_tile "searchbtn" "(list-libdoc-by-search)")
  (action_tile "prev" "(page-up)")
  (action_tile "next" "(page-down)")
  (init-dialog)
  ;;(show-libdocs-list)
  (start_dialog)
  ;;(new_dialog "Package-man" dcl_id "(show-libdocs-list)" )
  (unload_dialog dcl_id)
  )
(defun at-documents:gen-libdoc (/ files fp opt%)
  ;; 删除原临时
  (while (setq files (vl-directory-files (strcat @:*prefix* "packages\\base\\") "lib-*.to-gendoc.lsp" 1))
    (foreach file% files
	     (vl-file-delete (strcat @:*prefix* "packages\\base\\" file%))))
  (if (setq files (vl-directory-files (strcat @:*prefix* "packages\\base\\") "lib-*.lsp" 1))
      (foreach file% files
	       (dev:gen-doc (strcat @:*prefix* "packages\\base\\" file%))))
  (if (setq files (vl-directory-files (strcat @:*prefix* "packages\\base\\") "lib-*.to-gendoc.lsp" 1))
      (foreach file% files
	       (vl-file-delete (strcat @:*prefix* "packages\\base\\" file%))))
  
  ;; 删除空文档
  (if (setq files (vl-directory-files (strcat @:*prefix* "packages\\at-documents\\") "*.libdoc" 1))
      (foreach file% files
	       (if (= 0 (vl-file-size (strcat @:*prefix* "packages\\at-documents\\" file%)))
		   (vl-file-delete (strcat @:*prefix* "packages\\at-documents\\" file%))))
      ))
(defun gen-base-doc () (at-documents:gen-libdoc))

(defun @doc:gen-openai (/ fp)
  (at-documents:load-libdoc)
  (setq fp (open (strcat @:*prefix* "openai.csv") "w"))
  (write-line "技能名称,标准问题,标准问题的相似度阈值,补充用户问法（多个用##分隔）,机器人回答（多个用##分隔）,意图优先级,是否禁用"
	      fp)
  (write-line (strcat "@lisp函数库,@lisp函数库,0.91,,* 提供以下类别："
		      (string:from-lst @:*libdoc-category* "LINE_BREAK")
		      "LINE_BREAK请输入 类别:* 获取该类别下的函数。e.g.  block:* LINE_BREAK"
		      "请输入 函数名获取的函数用法。e.g.  block:insert "
		      ",1,false") fp)
  (foreach category% @:*libdoc-category*
	   (write-line
	    (strcat "@lisp函数库,"
		    category%":*,"
		    "0.91,"
		    ",";;0.8<eos>1<eos>问法1##0.8<eos>1<eos>问法2
		    ;; 生成该类的函数列表长度不大于600  换行用 LINE_BREAK
		    (string:from-list
		     (vl-remove
		      nil
		      (mapcar '(lambda (x)
				 (if 
				     (wcmatch x (strcat category% ":*"))
				     x nil))
				 (mapcar 'car @:*libdoc*)))
		     "LINE_BREAK")
		     ",1,false")
	    fp))
  (foreach func-info @:*libdoc*
	   (write-line
	    (strcat "@lisp函数库,"
		    (string:subst-all
		     "，" ","(@:to-string (car func-info)))","
		    "0.91,"
		    ",";;0.8<eos>1<eos>问法1##0.8<eos>1<eos>问法2
		    ;; 生成该类的函数列表长度不大于600  换行用 LINE_BREAK
		    ;; 去 ,
		    (if (member 34 (vl-string->list
				    (setq str
			  (string:subst-all
			   "\"\"" "\""
			   (string:subst-all
			    "LINE_BREAK" "\n"
			    (string:subst-all
			     "，" ","
			     (strcat
			      "函数用法:\n  "(@:to-string (caddr func-info))
			      "\n* 功能说明:\n  " (@:to-string(cadr func-info))
			      "\n* 参数:\n"(@:to-string(nth 3 func-info))
			      "\n* 返回值:\n   "(@:to-string(nth 4 func-info))
			      "\n* 示例:\n  "(@:to-string (nth 5 func-info))
			      )))))))
			(strcat "\""str"\"")
		      str)
		    ",1,false")
	    fp))  
  (close fp))
(defun @doc:gen-openai-pkg (/ fp)
  (@:package-update)
  (setq fp (open (strcat @:*prefix* "openai-pkg.csv") "w"))
  (write-line "技能名称,标准问题,标准问题的相似度阈值,补充用户问法（多个用##分隔）,机器人回答（多个用##分隔）,意图优先级,是否禁用"
	      fp)
  (write-line (strcat "CAD应用云,应用列表,0.91,,* 提供以下类别的应用：LINE_BREAK"
		      (string:from-lst @:*pkgs-category* "LINE_BREAK")
		      "LINE_BREAK请输入 类别:* 获取该类别下的应用。"
		      ",1,false") fp)
  (foreach category% @:*pkgs-category*
	   (write-line
	    (strcat "CAD应用云,"
		    category%":*,"
		    "0.91,"
		    ",";;0.8<eos>1<eos>问法1##0.8<eos>1<eos>问法2
		    ;; 生成该类的函数列表长度不大于600  换行用 LINE_BREAK
		    "该分类有以下应用包：LINE_BREAK"
		    (string:from-list
		     (vl-remove
		      nil
		      (mapcar '(lambda (x)
				 (strcat "* "(@:pkg x ':full-name) "  ID: " (@:pkg x ':name)))
			      (@:package-get-pkgs-by-category category%)))
		     "LINE_BREAK")
		    "LINE_BREAK 请输入包名或包ID获取应用包信息。"
		    ",1,false")
	    fp))
  (foreach pkg% @:*pkgs*
	   (write-line
	    (strcat "CAD应用云,"
		    (string:subst-all
		     "，" ","(@:pkg pkg% ':name))","
		     "0.91,"
		    "0.8<eos>1<eos>"
		    (@:pkg pkg% ':full-name)
		    ",";;0.8<eos>1<eos>问法1##0.8<eos>1<eos>问法2
		    ;; 生成该类的函数列表长度不大于600  换行用 LINE_BREAK
		    ;; 去 ,
		    (if (member 34 (vl-string->list
				    (setq str
			  (string:subst-all
			   "\"\"" "\""
			   (string:subst-all
			    "LINE_BREAK" "\n"
			    (string:subst-all
			     "，" ","
			     (strcat
			      "应用包:\n  "(@:to-string (@:pkg pkg% ':full-name))
			      "\n* 作者:\n  "(@:to-string (@:pkg pkg% ':author))
			      "\n* 版本:\n"(@:to-string (@:pkg pkg% ':version))
			      "\n* 简介:\n   "(@:to-string (@:pkg pkg% ':DESCRIPTION))
			      "\n更多内容请访问 http://atlisp.cn/package-info/"
			      (@:pkg pkg% ':name) 
			      )))))))
			(strcat "\""str"\"")
		      str)
		    ",1,false")
	    fp))  
  (close fp))
