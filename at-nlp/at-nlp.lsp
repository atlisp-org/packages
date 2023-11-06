;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 向系统中添加菜单 
(@:add-menu "@试验室" "关于NLP" '(@nlp:about))
(defun @nlp:about ()
  (@:help (strcat "使用自然语言与 CAD 进行交互，如 绘图，修改颜色等\n"
		  "NLP的初始尝试。\n"
		  "示例:\n 选择所有坐标大于0,0的图元\n"
		  "选择所有半径等于5的圆\n"
		  "修改直径为200\n"
		  "修改为绿色\n"
		  ))
  (princ)
  )
(defun @nlp:flatten (lst / lst1)
    "将多层点对列表展平为一维。"
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
		    (setq lst1 (append lst1 (@nlp:flatten x))))
		   )))
(defun at-nlp:parse-corpus (lst-str )
  "词语分段并注词性"
  (if (= 'str (type lst-str))
      (setq lst-str (list lst-str)))
  (@nlp:flatten
   (mapcar '(lambda (x / res)
	      (if (and x (/= "" x))
		  (progn
		    (setq flag nil)
		    (setq res x)
		    (foreach
		     verb (append at-nlp:*verb* at-nlp:*entity*
				  at-nlp:*attribute* at-nlp:*color*
				  at-nlp:*bool* at-nlp:*compare* at-nlp:*prep*
				  at-nlp:*tangent* at-nlp:*tangent-attribute*
				  )
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
			  )))
		    res)
		x))
	   lst-str))
  )
(defun at-nlp:lst-sym (res-corpus)
  (vl-remove-if '(lambda (x)
		   (= 'str (type x)))
		(mapcar '(lambda(x)
			   (if (and
				(listp x)
				(listp (cdr x))
				(cddr x))
			       (cons (cddr x)
				     (cadr x))
			     x)
			   )
			res-corpus)))
(defun at-nlp:parse-attribute (att / lst-att )
  (setq att (@nlp:flatten
	     (mapcar '(lambda(x)
		       (if(= 'color (car x))
			  (list
			   (cons 'attribute 62)
			   (cons 'compare "=")
			   (cons (itoa (cdr x)) nil))
			 x))
		     att)))
  (while (setq att (member (assoc 'attribute att) att))
    (setq curr-att-dxf (cdr (car att)))
    (setq att (cdr att))
    (setq curr-bool "and")
    (setq curr-att nil)
    (while (and att
		(or (/= 'attribute (caar att))
		    (and (= 'attribute (caar att))
			 (= curr-att-dxf (cdr (car att))))
		    ))
      (if (and (= 'attribute (caar att))
	       (= curr-att-dxf (cdar att)))
	  (setq att (cdr att)))
      (cond
	((and (= 'compare (car (nth 0 att)))
	      (null (cadr (nth 1 att))))
	 (cond
	   ((= "=" (cdr (nth 0 att)))
	    (setq curr-att
		  (cons
		   (dxf-pair curr-att-dxf
			     (car (nth 1 att)))
		   curr-att))
	    )
	   (t
	    (setq curr-att
		  (cons
		   (if (= curr-att-dxf 10)
		       (cons -4 (strcat (cdr (nth 0 att))","(cdr (nth 0 att))",*"))
		       (cons -4 (cdr (nth 0 att))))
		   (cons
		    (dxf-pair curr-att-dxf
			      (car (nth 1 att)))
		    curr-att)))))
	 (setq att (cddr att))
	 )
	((=  'bool  (car (nth 0 att)))
	 (setq curr-bool (cdr (nth 0 att)))
	 (setq att (cdr att)))
	(t (setq att (cdr att)))
	))
    (setq lst-att
	  (if(> (length curr-att) 1)
	     (append
	      (list (cons -4  (strcat "<" curr-bool)))
	      curr-att
	      (list (cons -4 (strcat curr-bool">")))
	      lst-att)
	     (append
	      curr-att
	      lst-att)))
    )
  lst-att)
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

(defun at-nlp:gen-code (res-corpus / res)
  (setq res (at-nlp:lst-sym res-corpus))
  "更新选择集"
  (defun at-nlp:entmod (res / att-pairs)
    (setq att-pairs (at-nlp:parse-attribute res))
    (mapcar 'entmod 
	    (mapcar '(lambda(lst-ent)
		       (foreach att-pair att-pairs
				(if (assoc (car att-pair) lst-ent)
				    (setq lst-ent (subst att-pair (assoc (car att-pair) lst-ent) lst-ent))
				  (setq lst-ent (append lst-ent (list att-pair)))
				  ))
		       lst-ent)
		    (mapcar 'entget (pickset:to-list (ssget "I"))))
	    )
    (command "regen"))
  (cond
   ((string-equal (cdr (assoc 'verb res)) "ssget")
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
		     (at-nlp:parse-attribute res)
		     )))))))
    )
   ((string-equal (cdr (assoc 'verb res)) "entmod")
    (cons 'at-nlp:entmod
	  (list (cons 'quote (list res))))
    )
   ))
(defun @nlp:nlp(str)
  (at-nlp:gen-code(at-nlp:parse-corpus (string:to-list str ";")))
  )
