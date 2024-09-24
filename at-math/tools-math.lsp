;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vitaltools -- Ψ�����߼�
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: ���� AutoLisp/VisualLisp �����Ļ�ͼ���߼�
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DWG�ļ��еĵ����ı�����ѧ����
(@:define-config '@math:layer-of-zone  "��" "��ʾ����Ķ�������ڵ�ͼ��")
;;; �������
;;;���˷�����
(defun filter-num(str / tmp)
  (if (setq tmp (car (vl-remove-if-not 'string:numberp (string:auto-split str))))
      (atof tmp)))
(@:add-menu "��ѧ" "�����" "(@m:text-sum)")
(defun @m:text-sum ( / s1 si% ti% ename total )
  (@:help 
   "�������ֵ����ı��ĺ͡�֧�ִ������ı��������ı���")
  (prompt "��ѡ��һ���������ֵ����ı�")
  (setq s1 (pickset:to-list (ssget '((0 . "text,tch_text")))))
  (setq total (apply '+ (mapcar '(lambda(x)(filter-num(entity:getdxf x 1))) s1)))
  (print (setq @m:*result* total))
  (@m:draw)
  (princ)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;; �������
(@:add-menu "��ѧ" "�����" "(@m:text-mul)")
(defun @m:text-mul ( / s1  ti% ename total )
  (@:help 
   "�������ֵ����ı��Ļ���֧�ֵ����ı��������ı���")
  (prompt "��ѡ��һ���������ֵ����ı�")
  (setq s1 (pickset:to-list (ssget '((0 . "text,tch_text")))))

  (setq total (apply '* (mapcar '(lambda(x)(filter-num (entity:getdxf x 1))) s1)))
  (print (setq @m:*result* total))
  (@m:draw)
  (princ)
  )
;;;;;;;;;;;;;;;;;;;;;;;;
;;; �������
(@:add-menu "��ѧ" "������ֵ" "(@m:avg)")
(defun @m:avg ( / s1  ti% ename total )
  (@:help 
   "�������ֵ����ı���������ֵ��֧�ֵ����ı��������ı���")
  (prompt "��ѡ��һ���������ֵ����ı�")
  (setq s1 (pickset:to-list (ssget '((0 . "text,tch_text")))))
  (setq total (apply '+ (mapcar '(lambda(x)(filter-num (entity:getdxf x 1))) s1)))
  (print (setq @m:*result* (/ total (length s1))))
  (@m:draw)
  (princ)
  )
(defun @:probe-int (str)
  "����һ���ַ�����ֵ�������������ȡ����"
  (if (= 0 (atoi (last (@:string-to-list str "."))))
      (car (@:string-to-list str "."))
      str))
	     
	     
;;;;;;;;;;;;;;;;;;;;;;;
;;�ı�����ɸѡ
(@:add-menu "��ѧ" "����ɸѡ" "(@m:sx)")
(defun @m:sx ( / minnum1 s1 ti% ename e )
  (@:help
   "����ɸѡ����,Ȼ������Ҫɸ���������ֵ,��ɺ�ѡ���������ɫ����")
  (setq minnum1 (getint "������Ҫɸ�ֵ���ֵ:"))
  (setq s1 (ssget '((0 . "text"))))

  (mapcar '(lambda (x / str)
	    (setq str (entity:getdxf x 1))
	    (if (string:numberp str)
		(if (> (filter-num str) minnum1)
		    (entity:putdxf x 62 141)
		    (entity:putdxf x 62 1))))
	  (pickset:to-list s1))
  (princ))

;;"��������ֵ(����ѡ��ֵ�ӻ��һ��ֵ)
(@:add-menu "��ѧ" "��������" "(@:multi-text-cal)")
(defun @:multi-text-cal( / cal-symble num  s1 ti% fn )
  (@:help
   "��������ֵ,������ʽ�����ı� + - * / һ����ֵ.")
  (initget 1 "+ - * /")
  (setq cal-symble (getkword "����������� (+ - * /): "))
  (setq s1 (pickset:to-list (ssget '((0 . "text")))))
  (setq num (getreal "������Ҫ���������Ӽ��˳�����ֵ:"))
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

(@:add-menu "��ѧ" "����ȡ��" "(@:fix-number)")
(defun @:fix-number( / num  s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "����ȡ��"
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
          (setq p11 nil)			;��p11�ÿ�
          (setq e (entget ename))		;ȡʵ���e
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
  
(@:add-menu "��ѧ" "����1~20" "(@:insert-numbers)")
(defun @:insert-numbers (/ mulu-path fp% mulu% i% insert-point% page-number% zhuanye ml-ruler)
  (push-var nil)
  (@:help "����һ���б��,�о� 800 ����������Ӽ����ܿɷ���ı༭��š�")
  (setq insert-point% (getpoint "�������Ų�����ʼ�㣺"))
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

(@:add-menu "��ѧ" "��������" "(@m:column-cal)")
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
  (setq cal-symble (getkword "����������� (+ - * /): "))
  (setq number-lst '())
  (setq i% 0)
  (prompt (strcat "��ѡ��� " (itoa (1+ i%)) " ��:"))
  (while (setq ss (ssget '((0 . "text"))))
    (if number-lst
	(setq number-lst
	      (append number-lst (list
				  (@m:sort-by-y (pickset:to-list ss)))))
	(setq number-lst (list  (@m:sort-by-y (pickset:to-list ss)))))
    (setq i% (1+ i%))
    (prompt (strcat "��ѡ��� " (itoa (1+ i%)) " ��:"))
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
  ;; дͼ
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
  (if (/= "" (setq str-fw (getstring t (strcat "���������ַ�Χ(���� , ~ ���ո�ָ�������)< " (rtos amin 2 2)"~"(rtos amax 2 2)" >: "))))
      (progn
	(setq fw (vl-remove-if '(lambda(x)(equal 0.0 x 1e-8)) (mapcar 'atof (string:auto-split str-fw))))
	(setq amin (apply 'min fw))
	(setq amax (apply 'max fw))))
  (prompt "��ѡ�����ֶ���:")
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
