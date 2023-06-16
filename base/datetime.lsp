
(defun-q datetime:current-time (str-fmt)
  "格式化日期时间，yyyy 年 mo 月 dd 日 hh 时 mm 分 ss 秒"
  "日期时间字符串"
  "(datetime:current-time \"yyyy-mo-dd hh:mm:ss\""
  (menucmd (strcat "M=$(edtime,$(getvar,date)," str-fmt ")")))

(defun-q datetime:get-current-day ()
  "返回日期"
  (substr (itoa (fix (getvar "CDATE"))) 7 2)
  )
(defun-q datetime:get-current-month ()
  "返回月份"
  (substr (itoa (fix (getvar "CDATE"))) 5 2)
  )
(defun-q datetime:get-current-year ()
  "返回年份"
  (substr (itoa (fix (getvar "CDATE"))) 1 4)
  )
(defun-q timer:begin ()
  "计时器开始"
  (if (> (@:acadver) 20.1)
      (setq *timer* (getvar "millisecs"))
    (setq *timer* (getvar "TDUSRTIMER"))
    ))
(defun-q timer:end (time p / usetime) ;计时器结束
  "计时器结束。time 开始时间 p 是否打印。"
  (if (not time)(setq time *timer*))
  (if (> (@:acadver) 20.1)
      (setq usetime (-  (getvar "millisecs") time))
    (setq usetime (* 86400000(- (getvar "TDUSRTIMER") time))))
  (if p (print(strcat"use time: "(rtos usttime 2 0)"micro second.")))
  usetime
  )
(defun-q datetime:leap-yearp (year)
  "判断某个是否为闰年。"
  (if (= 0 (mod year 100))
      (if (= 0 (mod year 400))
	  T
	  nil)
      (if (= 0 (mod year 4))
	  T
	  nil)))
  
(defun-q datetime:mktime (lst / )
  ;;        '(0 31 28 31  30  31  30  31  31  30  31 30  31))
  "计算某一时间(列表)到1970年01月01日经过的秒数,适合转换vl-file-systime的结果"
  "Timestamp"
  "(datetime:mktime (vl-file-systime (findfile \"acad.pgp\")))"
  (setq days-of-month
	(if (datetime:leap-yearp (nth 0 lst))
	    '(0 31 60 91 121 152 182 213 244 274 305 335 366)
	    '(0 31 59 90 120 151 181 212 243 273 304 334 365)))
  (+ (* 60.
	(+
	 (* 60.
	    (+
	     (* 24.
		(+ (* (- (nth 0 lst) 1970.) 365.) ;; 年差经过的天数
		   (fix (/ (- (nth 0 lst) 1970) 4)) ;; 补闰
		   (nth (1- (nth 1 lst)) days-of-month) ;;月份之前的天数
		   (nth 3 lst)))  ;; 当月经过的天数
	     (nth 4 lst)
	     (- 8))) ;; 小时数减时区
	 (nth 5 lst) ))
     (nth 6 lst)))
(defun-q datetime:mktime1900 (timestamp)
  "unix timestamp 转 到1900年01月01日经过的秒数."
  "real"
  "(datetime:mktime1900 (datetime:mktime (vl-file-systime (findfile \"acad.pgp\"))))"
  (+ (* 22089.0 100000.) 88800.0 timestamp)
  )
