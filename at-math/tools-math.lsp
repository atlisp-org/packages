;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vitaltools -- 唯他工具集
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DWG文件中的单行文本的数学运算
(@:define-config '@math:layer-of-zone  "柱" "表示区域的多段线所在的图层")
;;; 数字求和
;;;过滤非数字
(defun filter-num(str / tmp)
  (if (setq tmp (car (vl-remove-if-not 'string:numberp (string:auto-split str))))
      (atof tmp)))
(@:add-menu "数学" "∑求和" "(@m:text-sum)")
(defun @m:text-sum ( / s1 si% ti% ename total )
  (@::prompt 
   "求多个数字单行文本的和。支持纯单行文本和天正文本。")
  (prompt "请选择一个或多个数字单行文本")
  (setq s1 (pickset:to-list (ssget '((0 . "text,tch_text")))))
  (setq total (apply '+ (mapcar '(lambda(x)(filter-num(entity:getdxf x 1))) s1)))
  (print (setq @m:*result* total))
  (@m:draw)
  (princ)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;; 数字求积
(@:add-menu "数学" "∏求积" "(@m:text-mul)")
(defun @m:text-mul ( / s1  ti% ename total )
  (@::prompt 
   "求多个数字单行文本的积。支持单行文本和天正文本。")
  (prompt "请选择一个或多个数字单行文本")
  (setq s1 (pickset:to-list (ssget '((0 . "text,tch_text")))))

  (setq total (apply '* (mapcar '(lambda(x)(filter-num (entity:getdxf x 1))) s1)))
  (print (setq @m:*result* total))
  (@m:draw)
  (princ)
  )
;;;;;;;;;;;;;;;;;;;;;;;;
;;; 数字求积
(@:add-menu "数学" "算术均值" "(@m:avg)")
(defun @m:avg ( / s1  ti% ename total )
  (@::prompt 
   "求多个数字单行文本的算术均值。支持单行文本和天正文本。")
  (prompt "请选择一个或多个数字单行文本")
  (setq s1 (pickset:to-list (ssget '((0 . "text,tch_text")))))
  (setq total (apply '+ (mapcar '(lambda(x)(filter-num (entity:getdxf x 1))) s1)))
  (print (setq @m:*result* (/ total (length s1))))
  (@m:draw)
  (princ)
  )
(defun @:probe-int (str)
  "分析一个字符串数值，如果是整数则取整。"
  (if (= 0 (atoi (last (@:string-to-list str "."))))
      (car (@:string-to-list str "."))
      str))
	     
	     
;;;;;;;;;;;;;;;;;;;;;;;
;;文本数据筛选
(@:add-menu "数学" "数据筛选" "(@m:sx)")
(defun @m:sx ( / minnum1 s1 ti% ename e )
  (@::prompt
   "进行筛选操作,然后输入要筛除的最大数值,完成后选择的文字颜色区分")
  (setq minnum1 (getint "请输入要筛分的数值:"))
  (setq s1 (ssget '((0 . "text"))))

  (mapcar '(lambda (x / str)
	    (setq str (entity:getdxf x 1))
	    (if (string:numberp str)
		(if (> (filter-num str) minnum1)
		    (entity:putdxf x 62 141)
		    (entity:putdxf x 62 1))))
	  (pickset:to-list s1))
  (princ))

;;"批量改数值(对所选数值加或减一定值)
(@:add-menu "数学" "批量运算" "(@:multi-text-cal)")
(defun @:multi-text-cal( / cal-symble num  s1 ti% fn )
  (@::prompt
   "批量改数值,将数字式单行文本 + - * / 一个数值.")
  (initget 1 "+ - * /")
  (setq cal-symble (getkword "请输入运算符 (+ - * /): "))
  (setq s1 (pickset:to-list (ssget '((0 . "text")))))
  (setq num (getreal "请输入要进行批量加减乘除的数值:"))
  (setq ti% 0)
  (mapcar '(lambda (x)
	    (setq fn
	     (@:probe-int (rtos (apply (read cal-symble)
				       (list (atof (entity:getdxf (entget x) 1)) num))
				2 3 )))
	    (entmod (subst (cons 1 fn) (assoc 1 (entget x)) (entget x))))
	  s1)
  (princ)
  )

(@:add-menu "数学" "批量取整" "(@:fix-number)")
(defun @:fix-number( / num  s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "批量取整"
  (setq s1 (ssget '((0 . "text"))))
  (setq num 1)
  (setq ti% 0)
  (if (/= s1 nil)
      (progn
        (while 
            (<= 
             ti% 
             (- (sslength s1) 1)
             )
          (setq ename(ssname s1 ti%))
          (setq p11 nil)			;将p11置空
          (setq e (entget ename))		;取实体表e
          (setq fn (rtos (* (atof (cdr (assoc 1 e))) num) 2 0))
          (setq fn1 (cons 1 fn))
          (setq e (subst fn1 (assoc 1 e) e))
          
          (entmod e)
          (setq ti%(+ 1 ti%))
          )  ;end while
        ) ;endprogn s1/=nil
    )  ;endif  s1 /= nil
  (setq s1 nil)
  (princ)
  )
  
(@:add-menu "数学" "插编号1~20" "(@:insert-numbers)")
(defun @:insert-numbers (/ mulu-path fp% mulu% i% insert-point% page-number% zhuanye ml-ruler)
  (push-var nil)
  (@::prompt "插入一序列编号,行距 800 。配合批量加减功能可方便的编辑序号。")
  (setq insert-point% (getpoint "请输入编号插入起始点："))
  (setq i% 0)(setq page-number% 1)
  (setvar "osmode" 0)
  (setvar "autosnap" 0)
  (while (< i% 20) 
    ;;(setq mulu% (@:string-to-list mulu-s% ","))
    (entity:make-text (itoa page-number%)
		      (polar insert-point% (* 1.5 pi) (* i% 800))
		      350 0 0.72 0 "LB")
    (setq page-number% (1+ page-number%))
    (setq i% (1+ i%)))
  (pop-var)
  (princ))

(@:add-menu "数学" "多列运算" "(@m:column-cal)")
(defun @m:sort-by-x (ss-lst)
  (vl-sort ss-lst '(lambda (x y)
		    (> (car (entity:getdxf x 10))
		     (car (entity:getdxf y 10))))))
(defun @m:sort-by-y (ss-lst)
  (vl-sort ss-lst '(lambda (e1 e2)
		    (> (cadr (entity:getdxf e1 10))
		     (cadr (entity:getdxf e2 10))))))
(defun @m:column-cal (/ cal-symble number-lst ss i% res-matrix)
  (initget 1 "+ - * /")
  (setq cal-symble (getkword "请输入运算符 (+ - * /): "))
  (setq number-lst '())
  (setq i% 0)
  (prompt (strcat "请选择第 " (itoa (1+ i%)) " 列:"))
  (while (setq ss (ssget '((0 . "text"))))
    (if number-lst
	(setq number-lst
	      (append number-lst (list
				  (@m:sort-by-y (pickset:to-list ss)))))
	(setq number-lst (list  (@m:sort-by-y (pickset:to-list ss)))))
    (setq i% (1+ i%))
    (prompt (strcat "请选择第 " (itoa (1+ i%)) " 列:"))
    )
  (setq res-matrix '())
  (foreach matrix number-lst
	   (if res-matrix 
	       (setq res-matrix
		     (mapcar (read cal-symble) res-matrix 
			     (mapcar
			      '(lambda (x) (filter-num (entity:getdxf x 1)))
			       matrix)))
	       (setq res-matrix
		     (mapcar '(lambda (x) (filter-num (entity:getdxf x 1)))
			     matrix)))
	   )
  ;; 写图
  (mapcar '(lambda (x y)
	    (entity:make-text
	     (rtos x 2 2)
	     (polar (entity:getdxf y 10) 0 4000)
	     (entity:getdxf y 40)
	     0 0.8 0 "RB"))
	  res-matrix (last number-lst))
  )

(defun @math:select-number (/ str-fw entlst)
  (if (null amax) (setq amax 2.6))
  (if (null amin) (setq amin 1.6))
  (if (/= "" (setq str-fw (getstring t (strcat "请输入数字范围(可用 , ~ 及空格分隔两个数)< " (rtos amin 2 2)"~"(rtos amax 2 2)" >: "))))
      (progn
	(setq fw (vl-remove-if '(lambda(x)(equal 0.0 x 1e-8)) (mapcar 'atof (string:auto-split str-fw))))
	(setq amin (apply 'min fw))
	(setq amax (apply 'max fw))))
  (prompt "请选择文字对象:")
  (setq entlst
	(pickset:to-list (ssget '((0 . "*text")))))
  (setq entlst
	(vl-remove-if-not '(lambda(x / flag)
			     (setq flag nil)
			     (foreach num (mapcar 'atof (string:auto-split (entity:getdxf x 1)))
				      (if (<= amin num amax) (setq flag T)))
			     flag)
			  entlst))
  (sssetfirst nil (pickset:from-list entlst))
  (pickset:from-list entlst)
  )
