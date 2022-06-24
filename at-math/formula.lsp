;;;感谢fxsm为我们做的这个转化表达式为lisp函数的程序
(defun formula->lisp (str / lst format1 format1_1 Fsxm-Apply
			 format2 format3 format4 ln lg sqr ASIN
			 acos tan ctan sin2 cos2 tan2
			 )

;;;分离出变量与函数
  (defun format1 (str / char funs lastfun lst tmp lastchar)
    (setq lastfun "(")
    (setq funs '("+" "-" "*" "/" "^" "%" "(" ")" " "))
    (setq tmp "")
    (while (/= str "")
      (setq char (substr str 1 1))
      (setq str (substr str 2))
      (if	(and (member char funs)
		     ;;负号特别处理
		     (not (and lastfun (/= lastfun ")") (= char "-")))
		     (not (and lastchar (or (= char "-") (= char "+"))))
		     )
	  (progn
	    (setq lst      (vl-list* char tmp lst)
		  tmp      ""
		  lastfun  char
		  lastchar nil
		  )
	    ;;(princ)
	    )
	(progn
	  (setq tmp      (strcat tmp char)
		lastfun  nil
		lastchar (= char "E")
		)
	  ;;"e"科学计数法特别处理"2.718281828459045"
	  )
	)
      )
    (vl-remove "" (vl-remove " " (reverse (cons tmp lst))))
    )
;;;处理简单无优先级别函数运算
  (defun format1_1 (lst funs / fun lasta nlst tmp)
    (foreach a lst
	     (cond
	      ((setq tmp (assoc (strcase a) funs))
	       (setq fun (cadr tmp))
	       )
	      ((and (= a "(") fun)
	       (setq nlst (vl-list* fun "(" nlst))
	       (setq fun nil)
	       )
	      ((and (= a "(")
		    (not (member lasta '(nil "+" "-" "*" "/" "^" "%" "(" ")")))
		    )
	       (setq nlst (vl-list* lasta "(" (cdr nlst)))
	       )
	      (t (setq nlst (cons a nlst)))
	      )
	     (setq lasta a)
	     )
    (reverse nlst)
    )
;;;带return的apply
  (defun Fsxm-Apply ($Sym $Lst / $$ return $rt)
    (defun Return (var) (setq Return nil) (setq $$ var) (exit))
    (setq $rt (vl-catch-all-apply $Sym $Lst))
    (if Return
	$rt
      $$
      )
    )
  ;;递归处理括号
  (defun format2 (lst / a i lst2 nlst tmp var)
    (setq i 0)
    (while lst
      (setq a (car lst))
      (setq lst (cdr lst))
      (setq i (1+ i))
      (cond ((= a "(")
	     (setq var (fsxm-apply 'format2 (list lst)))
	     (repeat (car var) (setq lst (cdr lst)))
	     (setq i (+ i (car var)))
	     (setq nlst (cons (cadr var) nlst))
	     (setq tmp (cons (cadr var) tmp))
	     )
	    ((= a ")")
	     (return (list i (reverse tmp)))
	     )
	    (t
	     (setq tmp (cons a tmp))
	     (setq nlst (cons a nlst))
	     )
	    )
      )
    (reverse nlst)
    )
  ;;递归转化计算式格式
  (defun format3 (lst funs / lasta nlst tmp fun)
    (foreach a lst
	     (cond ((setq fun (assoc a funs))
		    (setq tmp (list lasta (cadr fun)))
		    )
		   (t
		    (if (listp a)
			(setq a (format3 a funs))
		      )
		    (if tmp
			(setq lasta (reverse (cons a tmp))
			      nlst	 (cons lasta (cdr nlst))
			      tmp	 nil
			      )
		      (setq lasta a
			    nlst	 (cons lasta nlst)
			    )
		      )
		    )
		   )
	     )
    (reverse nlst)
    )
  ;;递归处理掉多余的括号,
  ;;常量str->浮点数real 变量str->符号sym
  (defun format4 (lst)
    (mapcar '(lambda (a / x)
	       (cond ((listp a)
		      (if	(listp (car a))
			  (format4 (car a))
			(format4 a)
			)
		      )
		     ((= (type a) 'str)
		      (or	(setq x (distof a))
				(setq x (read a))
				)
		      x
		      )
		     (t a)
		     )
	       )
	    lst
	    )
    )
;;;自然对数
  (defun ln (d)
    (log d)
    )
;;;对数
  (defun lg (d)
    (* 0.4342944819032518276511289189166 (log d))
    )
;;;平方函数
  (defun sqr (d)
    (* d d)
    )
;;;反正弦函数
  (defun asin (d)
    (atan d (sqrt (- 1 (* d d))))
    )
;;;反余弦函数
  (defun acos (d)
    (atan (sqrt (- 1 (* d d))) d)
    )
;;;正切函数
  (defun tan (d)
    (/ (sin d) (cos d))
    )
;;;余切函数
  (defun ctan (d)
    (/ (cos d) (sin d))
    )
  (defun sin2 (d)
    (sin (* d (/ pi 180)))
    )
  (defun cos2 (d)
    (cos (* d (/ pi 180)))
    )
  (defun tan2 (d / r)
    (setq r (* d (/ pi 180)))
    (/ (sin r) (cos r))
    )

  ;;预处理 去空字符&转括号
  (setq str (vl-string-translate "{[]}\t\n," "(())   " str))
  (setq str (strcase str))
  ;;分离出变量与函数
  (setq lst (format1 str))
  ;;处理无优先级别函数运算
  ;;(setq lst (format1_1 lst '(("COS" cos2) ("SIN" sin2) ("TAN" tan2))))
  (setq	lst (format1_1 lst
		       '(("LN" ln)
			 ("LG" lg)
			 ("SQR" sqr)
			 ("ASIN" asin)
			 ("ACOS" acos)
			 ("CTAN" ctan)
			 ("TAN" tan)
			 )
		       )
	)
  ;;递归处理括号
  (setq lst (format2 lst))
  ;;优先计算  开方
  (setq lst (format3 lst '(("^" expt))))
  ;;再次计算  乘 除 取模
  (setq lst (format3 lst '(("*" *) ("/" /) ("%" rem))))
  ;;最后计算 加减
  (setq lst (format3 lst '(("+" +) ("-" -))))
  ;;后处理
  (car (format4 lst))
  )
;; ;;====================功能测试1:====================
;; (setq str1 (strcat "(1/(cos(-2)*-3)+"
;; 		   "min(22,abs(-5),0.5,8)"
;; 		   "*(2-5))/3^(sin(pi/5)+2)-1e+2*5"
;; 	   )
;; )
;; (eval (trans_format str1))		;-> -500.201
;; (eval (trans_format "min(22 , abs(-5) , 0.5 , 8)")) ;-> 0.5
;; ;;因min(22,abs(-5),0.5,8) -> 0.5 现在用cal验证结果
;; (setq str2 "(1/(cos(-2)*-3)+0.5*(2-5))/3^(sin(pi/5)+2)-1e+2*5")
;; (c:cal str2)				;-> -500.201

;; ;;功能测试通过


;; ;;====================效率测试====================
;; ;;计时子函数
;; (defun time0 () (setq t0 (getvar "TDUSRTIMER")))
;; (defun time1 ()
;;   (princ "用时:")
;;   (princ (* (- (getvar "TDUSRTIMER") t0) 86400))
;;   (princ "(S)")
;;   (princ)
;; )
;; (setq str "(1/(cos(-2)*-3)+0.5*(2-5))/3^(sin(pi/5)+2)-1e+2*5")
;; (defun c:t1 (/ t0)			;用CAL对比
;;   (time0)
;;   (repeat 5000 (cal str))
;;   (time1)
;; )
;; (defun c:t2 (/ t0)			;多次eval+多次trans_format(比cal慢)
;;   (time0)
;;   (repeat 5000 (eval (trans_format str)))
;;   (time1)
;; )
;; (defun c:t3 (/ t0)			;多次eval+1次trans_format(与cal差不多)
;;   (time0)
;;   (setq trans_lst (trans_format str))
;;   (repeat 5000 (eval trans_lst))
;;   (time1)
;; )
;; (defun c:t4 (/ t0 test)			;1次eval+1次trans_format(比cal快)
;;   (time0)
;;   (eval (list 'defun 'test nil (trans_format str)))
;;   (repeat 5000 (test))
;;   (time1)
;; )

;; ;;无痕提出的一种方法
;; ;;(setq wcs (vla-GetInterfaceObject (vlax-get-acad-object) "ScriptControl"))
;; ;;(vlax-put-property wcs "language" "vbs")
;; ;;(vla-eval wcs "1+4+5*2+(5+5)/2+((6+6)/2+(5+5)/2)")  ;返回 ->31.0


