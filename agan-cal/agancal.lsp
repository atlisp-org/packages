;���ʼ�����(����:ag��agg��ag���޸�)-2021.2.24
(setq GL-precision 3)  ;С������λ����Ĭ��3λ���Լ��ɸ�����Ҫ�޸�
(setq GL-texthight 400)  ;�������ָ߶�400���Լ��ɸ�����Ҫ�޸�

;����Ϊ��ݼ���c:��Ϊ��ݼ���;�ź�Ϊע�ͣ����Լ��ɸ�����Ҫ�޸�
(defun c:ag() (vl-cmdf "agg")(princ));��������ag,���޸�
(defun c:ad() (GL:jiafa)) ;��
(defun c:a2() (GL:jianfa)) ;��
(defun c:a3() (GL:chengfa)) ;��
(defun c:a4() (GL:chufa)) ;��
(defun c:sq() (c:shiqutxt)) ;ʰȡ
(defun c:css() (command "GC:agcalcss")) ;����ʽ

;����Ϊ��ݼ������㲢�����������Լ��ɸ�����Ҫ�޸�
(defun c:ys() (if (GL:jiafa)(GL:crjswz GL-result))(princ)) ;��
(defun c:y2() (if (GL:jianfa)(GL:crjswz GL-result))(princ)) ;��
(defun c:y3() (if (GL:chengfa)(GL:crjswz GL-result))(princ)) ;��
(defun c:y4() (if (GL:chufa)(GL:crjswz GL-result))(princ)) ;��

;����Ϊ��ݼ������㲢������ʽ�����Լ��ɸ�����Ҫ�޸�
(defun c:dd() (if (GL:jiafa)(GL:crjswz GL-formula))(princ)) ;��
(defun c:d2() (if (GL:jianfa)(GL:crjswz GL-formula))(princ)) ;��
(defun c:d3() (if (GL:chengfa)(GL:crjswz GL-formula))(princ)) ;��
(defun c:d4() (if (GL:chufa)(GL:crjswz GL-formula))(princ)) ;��

;�������������ݾ�����Ҫ�޸�(��һ��lisp��̻�������)������
(vl-load-com)
;; (setq cadpath (vla-get-Path (vlax-get-acad-object))) ;��ȡCAD��װ·��
(setq vers (substr (getvar "acadver") 1 2))
(if (< (atoi vers) 19)
    (command "netload" (strcat (@:package-path "agan-cal") "AganCal18.dll")) ;DLL�ļ�λ�ã����޸�
    (command "netload" (strcat (@:package-path "agan-cal") "AganCal.dll"))) ;DLL�ļ�λ�ã����޸�
(GC:agrecord (strcat (@:package-path "agan-cal") "��ʷ��¼.txt")) ;��ʷ��¼txt�ļ�λ�ã����޸�

(command "cal")(command)     ;�ȵ���cal���������cal�����޷�ʹ��
(setq GL-result "")          ;��������ȫ�ֱ���
(setq GL-formula "")         ;��ʽ

;c#����lisp����
(defun c:GL-ad() (GL:jiafa)) ;��
(defun c:GL-a2() (GL:jianfa)) ;��
(defun c:GL-a3() (GL:chengfa)) ;��
(defun c:GL-a4() (GL:chufa)) ;��

(defun GL:jiafa ()(GL:Galculate (GL:GetNumber "+")))
(defun GL:jianfa ()(GL:Galculate (GL:GetNumber "-")))
(defun GL:chengfa ()(GL:Galculate (GL:GetNumber "*")))
(defun GL:chufa ()(GL:Galculate (GL:GetNumber "/")))

;ѡ������
(defun GL:GetNumber (ope / *error* en en-lst lst n num pt ss ss_data)
  (defun *error* (x) ;������
    (UnHighLight en-lst)
    (setvar "ErrNo" 0)
    (princ x)
  )
  (setvar "ErrNo" 0)
  (setq en-lst (list))
  (while (/= (getvar "ErrNo") 52)
    (if (setq ss (ssget ":S" '((0 . "*TEXT,DIMENSION,INSERT,ATTDEF,ACAD_TABLE,TCH_ELEVATION,TCH_SPACE,TCH_DRAWINGNAME")))) ; ʰȡ���֡��ߴ��ע�������֡�CAD�����������ߡ������ͼ��
    (progn
	(if (= (caar (setq ss_data (ssnamex ss 0))) 1)
	  (progn ;��ѡʱ
	    (setq pt (trans (cadr (last (car ss_data))) 0 1)
		      en (car (nentselp pt))
		      en-lst (reverse en-lst);����
		      en-lst (cons en en-lst)
		      en-lst (MJ:delsame en-lst)
		      en-lst (reverse en-lst);����
		      n (itoa (length en-lst))
	    )
	    (redraw en 3)
	    (princ (strcat "\n��ʰȡ " n " ������" (GL:Formula en-lst ope)))
	  )
	  (progn ;��ѡʱ
	    (setq lst (ss->lst ss)
		      lst (reverse lst)
		      en-lst (append en-lst lst)
		      en-lst (MJ:delsame en-lst)
		      n (itoa (length en-lst))
	    )
	    (HighLight lst)
	    (princ (strcat "\n��ʰȡ" n "������" (GL:Formula en-lst ope)))
	  )
	)
	))
  )
  (if en-lst
    (progn
      (setq Num (GL:Formula en-lst ope))
      (UnHighLight en-lst)
    )
  )
  (setvar "ErrNo" 0)
  Num
)

;��ȡ��ʽ
(defun GL:Formula (lst ope / ed en ent i num regex text tn)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0)            ;�����Դ�Сд
  (vlax-put-property regex "Global" 1)                ;ȫ��ƥ�䣬������ֻƥ���һ��
  (setq i 0 Num "")
  (repeat (length lst)
    (setq en (nth i lst)
          ed (entget en)
          ent (cdr (assoc 0 ed))
    )
    (cond
      ((wcmatch ent "MTEXT") (setq text (cdr (assoc 1 ed))) (setq text (mtext2text text)));��������
      ((wcmatch ent "*TEXT,TCH_ELEVATION,TCH_DRAWINGNAME") (setq text (cdr (assoc 1 ed))));���֡���������ߡ�ͼ��
      ((wcmatch ent "TCH_SPACE") (setq text (cdr (assoc 41 ed))));�������
      ((wcmatch ent "DIMENSION") (setq text (cdr (assoc 42 ed))));�ߴ��ע(����ֵ)
      ((wcmatch ent "ATTDEF") (setq text (cdr (assoc 2 ed))));������(��ȡ����ǡ� ����2)
    )
    (if text
     (progn
      (vlax-put-property regex "Pattern" "[^0-9\\+\\-\\*\\/\\.\\(\\)\\=]") ;ƥ�����ֺ������
      (setq text (vlax-invoke-method regex "Replace" text ""))
      (if (/= text "")
       (if (= Num "");���ʽ������ 
        (if (wcmatch text "*`+*,*`-*,*`**,*`/*")
         (setq Num (strcat"(" text ")") TN nil)
         (setq Num text TN nil)
        )
        (if (wcmatch text "*`+*,*`-*,*`**,*`/*")
          (setq Num (strcat Num ope "(" text ")"))
          (setq Num (strcat Num ope text))
        )
       )
      )
      (setq text "")
      )
     )
    (setq i (1+ i))
  )
  Num
)

;����
(defun GL:Galculate (Num / regex)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0)            ;�����Դ�Сд
  (vlax-put-property regex "Global" 1)                ;ȫ��ƥ�䣬������ֻƥ���һ��
  (if (and num (/= num ""))
   (progn
    (if (setq GL-result (cal (strcat Num "*" "1.0"))) ;*1.0������ת��ΪС��������ֻ�ܽ���-32768��32767֮��
      (progn
	    (setq GL-result (rtos GL-result 2 GL-precision))
	    (if (wcmatch GL-result "*.*") ;ȥ��С��������0
	      (progn
	        (vlax-put-property regex "Pattern" ".0+?$") ;ȥ��.������0
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	        (vlax-put-property regex "Pattern" "[.]$") ;�����һλ��.��ȥ��
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	    ))
	    (setq GL-formula (strcat Num "=" GL-result))
	    (if (= (car (GC:agform T)) "True")
	      (progn
            (GC:agjsjg GL-result Num) ;c#������������ʾ���
            (princ (strcat "\n���ʽ��" Num "=" GL-result "  >>>>��������" GL-result))
          )
          (princ (strcat "\n���ʽ��" Num "=" GL-result "  >>>>��������" GL-result  "       !!!!!����������δ�������޷�������ʷ��¼!!!!!"))
        )
      )
      (progn
        (setq GL-result "")
        (setq GL-formula "")
        (alert (strcat Num " ���ʽ����" "\n\n������̫�󳬳����㷶Χ" "\n(���������2147483647��-2147483648 ֮��>"))
    )))
    (progn
      (setq GL-result "")
      (setq GL-formula "")
  ))
  (princ)
)

;c#����
(defun GCGalculate (str / jg num regex)
  (setq regex (vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0)            ;�����Դ�Сд
  (vlax-put-property regex "Global" 1)                ;ȫ��ƥ�䣬������ֻƥ���һ��
  (vlax-put-property regex "Pattern" "[^0-9\\+\\-\\*\\/\\.\\(\\)\\=]")
  (setq str (vlax-invoke-method regex "Replace" str ""))
  (if str
   (progn
     (setq jg (cal (strcat "(" str ")*1.0"))) ;���ʽ*1.0������ת��ΪС��������ֻ�ܽ���-32768��32767֮��,�������������س���
     (setq GL-result (rtos jg 2 GL-precision))
	    (if (wcmatch GL-result "*.*") ;ȥ��С��������0
	      (progn
	        (vlax-put-property regex "Pattern" ".0+?$") ;ȥ��.������0
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	        (vlax-put-property regex "Pattern" "[.]$") ;�����һλ��.��ȥ��
	        (setq GL-result (vlax-invoke-method regex "Replace" GL-result ""))
	    ))
     (GC:agjsjg GL-result str) ;c#������������ʾ���
     (princ (strcat "\n���ʽ��" str "=" GL-result "  >>>>��������" GL-result))
   )
   (alert (strcat Num " ���ʽ����" "\n\n������̫�󳬳����㷶Χ" "\n(���������2147483647��-2147483648 ֮��>"))
  )
  (princ)
)

;ʰȡ���ֺ��������C#����
(defun c:shiqutxt (/ num)
  (setq num (GL:GetNumber "+"))
  (GC:agjsxs num) ;c#������������ʾ
  (princ (princ (strcat "\n���ʽ��" num)))
  (princ)
)

;���Զ���
(defun HighLight (en-lst / en i)
  (setq i 0)
  (repeat (length en-lst)
    (setq en (nth i en-lst))
    (redraw en 3)
    (setq i (1+ i))
  )
)

;ȡ�����Զ���
(defun UnHighLight (en-lst / en i)
  (setq i 0)
  (repeat (length en-lst)
    (setq en (nth i en-lst))
    (redraw en 4)
    (setq i (1+ i))
  )
)

;ѡ��->ͼԪ�б� By caiqs --��лbbs.mjtd.com����
(defun ss->lst (ss / retu)
  (setq retu (apply 'append (ssnamex ss)))
  (setq retu (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) retu))
)

;ɾ��������ͬͼԪ--��лbbs.mjtd.com����
(defun MJ:delsame (L)
  (if L
    (cons (car L) (MJ:delsame (vl-remove (car L) (cdr L))))
  )
)

;��ȡ��������,ȥ�����ø�ʽ����--����bbs.mjtd.com
(defun mtext2text (MTextString / regex s)
  (setq regex(vlax-create-object "Vbscript.RegExp")) ;����������ʽ�ؼ�
  (vlax-put-property regex "IgnoreCase" 0) ;�����Դ�Сд
  (vlax-put-property regex "Global" 1) ;ƥ�䷽ʽ��ȫ����ƥ��
  (setq s MTextString)
     ;�滻\\�ַ�
  (vlax-put-property regex "Pattern" "\\\\\\\\")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 1)))
     ;�滻\{�ַ�
  (vlax-put-property regex "Pattern" "\\\\{")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 2)))
     ;�滻\}�ַ�
  (vlax-put-property regex "Pattern" "\\\\}")
  (setq s(vlax-invoke-method  regex "Replace" s (chr 3)))
     ;ɾ������������ʽ
  (vlax-put-property regex "Pattern" "\\\\pi(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���Ʊ����ʽ
  (vlax-put-property regex "Pattern" "\\\\pt(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���ѵ���ʽ
  (vlax-put-property regex "Pattern" "\\\\S(.[^;]*)(\\^|#|\\\\)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����塢��ɫ���ָߡ��־ࡢ��б���ֿ������ʽ
  (vlax-put-property regex "Pattern" "(\\\\F|\\\\f|\\\\C|\\\\H|\\\\\T|\\\\Q|\\\\W|\\\\A)(.[^;]*);")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ���»��ߡ�ɾ���߸�ʽ
  (vlax-put-property regex "Pattern" "(\\\\L|\\\\O|\\\\l|\\\\o)")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ������Ͽո��ʽ
  (vlax-put-property regex "Pattern" "\\\\~")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����з���ʽ
  (vlax-put-property regex "Pattern" "\\\\P")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ�����з���ʽ(���Shift+Enter��ʽ)
  (vlax-put-property regex "Pattern" "\n")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     ;ɾ��{}
  (vlax-put-property regex "Pattern" "({|})")
  (setq s(vlax-invoke-method  regex "Replace" s ""))
     
     ;�滻��\\,\{,\}�ַ�
  (vlax-put-property regex "Pattern" "\\x01")
  (setq s(vlax-invoke-method  regex "Replace" s "\\"))
  (vlax-put-property regex "Pattern" "\\x02")
  (setq s(vlax-invoke-method  regex "Replace" s "{"))
  (vlax-put-property regex "Pattern" "\\x03")
  (setq s(vlax-invoke-method  regex "Replace" s "}"))
     
  (vlax-release-object regex)
  s
)

;��������-��̬����
(defun GL:crjswz (GL-result / boolean code en endate motion pt pt2 text-jiaodu TorN)
  (if (/= GL-result "")
    (progn
      (setq pt (cadr (grread 1)));ȡ�õ�ǰ�������
      (entmake (list
          '(0 . "TEXT")
           (cons 1 GL-result)
           (cons 10 pt)
           (cons 40 GL-texthight) ;���ָ߶�
           )
      )
      (setq en (entlast))
      (if en ;��̬����
        (progn
                (setq TorN t)
                (setq endate (entget en))
				(princ "\n��ȡλ�û�[ת90��(A)/�Ҽ��˳�]")
				(setq boolean t)
				(setq text-jiaodu 0)
				(while boolean
					 (setq motion (grread T 8));grread ��������һ�������е�һ��Ԫ��˵���������͵Ĵ��룬�ڶ���Ԫ�ؼȿ������������ֿ����ǵ�
					 (setq code (car motion)) ;grread���һ��Ԫ���������͵Ĵ���
					 (setq pt2 (cadr motion)) ;grread��ڶ���Ԫ�� �϶�ģʽ����
					 (cond
					  ((= code 5)   ;����϶�ģʽ
						 (entmod (setq endate (subst (cons 10 (trans pt2 1 0)) (assoc 10 endate) endate)));��̬����������
					  )
					  ((= code 3)   ;������I����
					  (setq boolean nil)
					  )
					  ((= code 11)
					   (setq boolean nil)
					   (entdel en)
					   (setq TorN nil)
					  )
					  ((= code 25)
					   (setq boolean nil)
					   (entdel en)
					   (setq TorN nil)
					  )
					  ((equal motion '(2 32))
					   (setq boolean nil)
					  )
					  ((equal motion '(2 13))
					   (setq boolean nil)
					  )
					  ((equal motion '(2 27))
					   (setq boolean nil)
					   (entdel en)
					   (setq TorN nil)
					  )
					  ((equal motion '(2 65))
						 (setq text-jiaodu (+ text-jiaodu (/ pi 2)))
						 (entmod (setq endate (subst (cons 50 text-jiaodu) (assoc 50 endate) endate)));��̬�����ֽǶ�
					  )
					  ((equal motion '(2 97))
						 (setq text-jiaodu (+ text-jiaodu (/ pi 2)))
						 (entmod (setq endate (subst (cons 50 text-jiaodu) (assoc 50 endate) endate)));��̬�����ֽǶ�
					  )
					 )
                );end while
  ))))
  (princ)
)
