;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 向系统中添加菜单 
(@:add-menu "@试验室" "自然语言交互" '(at-nlp:about))
(defun at-nlp:about ()
  (@:help (strcat "使用自然语言与 CAD 进行交互，如 绘图，修改颜色等\n"
		  "NLP的初始尝试。\n"
		  "示例:\n 绘制半径为10的圆\n"
		  "绘制直径为10的圆\n"
		  "选择所有半径等于5的圆\n"
		  "修改直径为20\n"
		  "修改为绿色\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (princ)
  )
(defun at-nlp:parse-corpus (lst-str )
  "词语分段并注词性"
  (defun flatten (lst / lst1)
    "将多维列表展平为一维。单向箔。"
    "list"
    "(list:flatten '(a (b c)
      (d (e))))"
    (foreach x lst
	     (cond ((or (and
			 x
			 (atom x)
			 (/= "" x))
			(p:dotpairp x)
			(and (listp x)
			     (= (length x) 1)
			     (atom (car x))
			     (/= "" (car x))
			     ))
		    (setq lst1 (append lst1 (list x))))
		   ((listp x)
		    (setq lst1 (append lst1 (flatten x))))
		   )))
  (flatten
  (mapcar '(lambda (x / res)
	      (if (and x (/= "" x))
		  (progn
		    ;;(setq res (list str))
		    (setq flag nil)
		    (setq res x)
		    (foreach
		     verb (append at-nlp:*verb* at-nlp:*entity*
				  at-nlp:*attribute*
				  at-nlp:*bool* at-nlp:*prep*)
		     (if (null flag)
		     (cond
		      ((= (car verb) x)
		       (setq flag t)
		       (setq res (cons (car verb) (cdr verb))))
		      ((vl-string-search (car verb) x)
		       (setq lst-str (string:to-list x (car verb)))
		       (setq flag t)
		       (if (> (length lst-str) 1)
			   (progn
			     (setq res
				   (list (at-nlp:parse-corpus (list(car lst-str)))
					 (cons (car verb) (cdr verb))
					 (at-nlp:parse-corpus (cdr lst-str)))))
			 (setq res x)))
		      ;; ((null (vl-string-search (car verb) x))
		      ;;  (setq res x))
		      )))
		    res)
		x))
	  lst-str))
  )
  
