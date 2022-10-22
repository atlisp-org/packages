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
				  at-nlp:*attribute* at-nlp:*color*
				  at-nlp:*bool* at-nlp:*compare* at-nlp:*prep*)
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
(defun at-nlp:lst-sym (res-corpus)
  (mapcar '(lambda(x)
	     (if (cddr x)
		 (cons (cddr x)
		       (cadr x))
	       x)
	     )
	  res-corpus))
(defun at-nlp:gen-code (res-corpus / res)
  (setq res (at-nlp:lst-sym res-corpus))
  (@:debug "INFO" (vl-prin1-to-string res))
  (defun dxf-pair (dxf str)
    (cond
     ((or (<= 0 dxf 9)
	  (<= 100 dxf 101)
	  (<= 300 dxf 309)
	  (<= 410 dxf 419)
	  (<= 430 dxf 439)
	  (<= 470 dxf 479)
	  (<= 999 dxf 1009))
      (cons dxf str))
     ((or (<= 40 dxf 59)
	  (<= 110 dxf 149)
	  (<= 210 dxf 239)
	  (<= 460 dxf 469)
	  (<= 1010 dxf 1059)
	  )
      (cons dxf (atof str)))
     ((or (<= 60 dxf 99)
	  (<= 160 dxf 179)
	  (<= 270 dxf 289)
	  (<= 370 dxf 389)
	  (<= 400 dxf 409)
	  (<= 420 dxf 429)
	  (<= 440 dxf 459)
	  (<= 1060 dxf 1071)
	  )
      (cons dxf (atoi str)))
     ((<= 10 dxf 39)
      (cons dxf (point:2d->3d(mapcar 'atof (string:to-list str ",")))))
     (t 
      (cons dxf str))))
  (defun parse-attribute (att / lst-att )
    (while (setq att (member (assoc 'attribute att) att))
      (if (and (= 'compare (car (nth 1 att)))
	       (null (cadr (nth 2 att))))
	  (cond
	   ((= "=" (cdr (nth 1 att)))
	    (setq lst-att
		  (cons
		   (dxf-pair (cdr (assoc 'attribute att))
			     (car (nth 2 att)))
		   lst-att))
	    )
	   (t
	    (setq lst-att
		  (cons
		   (cons -4 "<AND")
		   (cons
		    (if (= (cdr (assoc 'attribute att)) 10)
			(cons -4 (strcat (cdr (nth 1 att))","(cdr (nth 1 att))",*"))
		      (cons -4 (cdr (nth 1 att))))
		    (cons
		     (dxf-pair (cdr (assoc 'attribute att))
			       (car (nth 2 att)))
		     (cons (cons -4 "AND>")
			   lst-att)))))
	    ))
	(setq lst-att
	      (cons 
	       (dxf-pair (cdr (assoc 'attribute att))
			 (car (nth 1 att)))
	       lst-att))
	)
      (setq att (cddr att))
      )
    lst-att)
  ;; 更新选择集
  (defun at-nlp:entmod (res / att-pairs)
    (setq att-pairs (parse-attribute res))
    (mapcar 'entmod 
	    (mapcar '(lambda(lst-ent)
		       (foreach att-pair att-pairs
				(setq lst-ent (subst att-pair (assoc (car att-pair) lst-ent) lst-ent)))
		       lst-ent)
		    (mapcar 'entget (pickset:to-list tmp-ss)))
	    ))
  ;; (@:debug "INFO" (vl-prin1-to-string (parse-attribute res)))
  (cond
   ((= (read (cdr (assoc 'verb res))) 'ssget)
    
    (list
     'sssetfirst
     nil
     (list 'setq 'tmp-ss
	   (append
	    (list 'ssget)
	    (if (= "x" (cdr (assoc 'prep res)))
		(list (cdr (assoc 'prep res))))
	    (list 
	     (cons 'quote
		   (list
		    (append
		     (if (cdr (assoc 'entity res))
			 (list (cons 0 (cdr (assoc 'entity res)))))
		     (if (cdr (assoc 'color res))
			 (list (cons 62 (cdr (assoc 'color res)))))
		     (parse-attribute res)
		     )))))))
    )
   ;;(entmod (subst  
   ((= (read (cdr (assoc 'verb res))) 'entmod)
    (cons 'at-nlp:entmod
	  (list (cons 'quote (list res))))
    )
   ))
