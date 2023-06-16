(defun format (stream ctrl-string variables
	       / flag-instruct  flag-comma number1 number2 result  to-string tmp-str init-flag)
  ;;" D 十进制整数 "
  ;;" $ 小数点后2位，F 浮点数 E 指数形式"
  ;;" @ 带正号 v 从参数中取得小数的位数 # 剩余的参数的个数。"
  ;;" A 字符串 S 可 read 读回。"
  ;;"TODO: X 16进制, O 8进制, B 二进制, R 任意进制。"
  ;;"TODO: 迭代 ~{ ~} ，条件格式化 ~[ ~] " 
  
  (defun to-string (para)
    (cond
      ((= 'INT (type-of para)) (itoa para))
      ((= 'REAL (type-of para)) (rtos para 2 3))
      ((= 'STR (type-of para)) para)
      ((= 'LIST (type-of para)) (vl-prin1-to-string para ))
      ((= 'SYM (type-of para)) (vl-symbol-name para))
      )
    )
  (defun init-flag ()
    (setq flag-instruct nil
	  flag-comma nil
	  number1 ""
	  number2 ""))
  (init-flag)
  (setq result "")
  (while (/= "" ctrl-string)
    (if flag-instruct
	(cond
	  ;; 修饰符
	  ((= (ascii ",")(ascii ctrl-string))
	   ;; comma
	   (setq flag-comma T)
	   )
	  ((= (ascii "v")(ascii ctrl-string))
	   ;; v
	   (setq number1 (to-string (car variables)))
	   (setq variables (cdr variables))
	   )
	  ((= (ascii "#")(ascii ctrl-string))
	   ;; #
	   (setq number1 (to-string (length variables)))
	   (setq variables (cdr variables))
	   )
	  
	  ((and (> (ascii ctrl-string) 47)
		(> 58 (ascii ctrl-string)))
	   ;; 数字
	   (if flag-comma
	       (setq number2 (strcat number2 (substr ctrl-string 1 1)))
	       (setq number1 (strcat number1 (substr ctrl-string 1 1)))
	       ))

	  ((=  (ascii "~") (ascii ctrl-string)) ;; 处理指令，当连续时，输出~
	   (setq result (strcat result "~"))
	   (init-flag))
	  ((= (ascii "%")(ascii ctrl-string))
	   ;; 换行
	   (setq result (strcat result "\n"))
	   (init-flag))
	  ((= (ascii "&")(ascii ctrl-string))
	   (setq result (strcat result "\n"))
	   (init-flag))
	  ((= (ascii "A")(ascii (strcase (substr ctrl-string 1 1))))
	   ;; 字符串
	   (setq result (strcat result (to-string (car variables))))
	   (setq variables (cdr variables))
	   (init-flag))
	  ((= (ascii "D")(ascii (strcase (substr ctrl-string 1 1))))
	   ;; 整数及小数
	   (if (/= "" number1) ; 处理占位符
	       (progn 
		 (setq tmp-str  (to-string (car variables)))
		 (if (> (atoi number1)(strlen tmp-str))
		     (repeat (- (atoi number1)(strlen tmp-str))
			     (setq result (strcat result " "))))
		 (setq tmp-str "")))
	   (setq result (strcat result (to-string (car variables))))
	   (setq variables (cdr variables))
	   (init-flag))
	  ((= (ascii "F")(ascii (strcase (substr ctrl-string 1 1))))
	   ;; 浮点数
	   (if (= "" number2)
	       (setq tmp-str (rtos (car variables) 2 3))
	       (setq tmp-str (rtos (car variables) 2 (atoi number2))))
	   (if (/= "" number1) ; 处理占位符
	       (progn 
		 (if (> (atoi number1)(strlen tmp-str))
		     (repeat (- (atoi number1) (strlen tmp-str))
			     (setq result (strcat result " "))))))
	   (setq result (strcat result tmp-str))
	   (setq tmp-str "")
	   (setq variables (cdr variables))
	   (init-flag))
	  ((= (ascii "E")(ascii (strcase (substr ctrl-string 1 1))))
	   ;; 指数形式
	   (if (= "" number2)
	       (setq tmp-str (rtos (car variables) 1 3))
	       (setq tmp-str (rtos (car variables) 1 (atoi number2))))
	   (if (/= "" number1) ; 处理占位符
	       (progn 
		 (if (>  (atoi number1)(strlen tmp-str))
		     (repeat (- (atoi number1) (strlen tmp-str))
			     (setq result (strcat result " "))))))
	   (setq result (strcat result tmp-str))
	   (setq tmp-str "")
	   (setq variables (cdr variables))
	   (init-flag))	  
	  ((= (ascii "$")(ascii (strcase (substr ctrl-string 1 1))))
	   ;; 货币
	   (if (= "" number1)
	       (setq result (strcat result (rtos (car variables) 2 2)))
	       (setq result (strcat result (rtos (car variables) 2 (atoi number1)))))
	   (setq variables (cdr variables))
	   (init-flag))
	  
	  )
	(cond
	  ((=  (ascii "~") (ascii ctrl-string)) ;; 处理指令，当连续时，输出~
	   (setq flag-instruct T))	
	  (t
	   (setq result (strcat result (substr ctrl-string 1 1)))
	   )))
    (setq ctrl-string (substr ctrl-string 2)))

  (cond
    ((= T stream)
     (princ result)
     (princ))
    ((= nil stream)
     result)
    ((= (type stream) 'FILE)
     (write-line result stream))
    )
  )
  
