;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ����ʹ�ÿ������� dev-tools �Զ������ĳ���Դ�ļ� 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ���������� 'at-structure:first ���� Ӧ�ð� at-structure �� ��һ�������� first 
;;(@:define-config 'at-structure:first "���������� at-structure:first ��ֵ" "������������;˵����")
;; (@:get-config 'at-structure:first) ;; ��ȡ���ö���ֵ
;; (@:set-config 'at-structure:first  "�����ֵ") ;; �������ö���ֵ
;; ��ϵͳ����Ӳ˵� 
(@:add-menu "�ṹ����" "��̬�����" "(at-structure:query-steelbar)" )
(@:add-menu "�ṹ����" "��ֽ����" "(at-structure:menu-get-area)" )
(defun at-structure:menu-get-area (/ steelbar-str)
  (@:help "ѡ�иֽ��ַ����ĵ����ı����� %%1328@100,2%%13220+3%%13222 �ȣ����ظֽ������")
  (setq steelbar-str (string:parse-by-lst (cdr (assoc 1 (entget (car (entsel))))) '(";" "��")))
  (foreach x steelbar-str
	   (format t "�ֽ����: ~d"
		   (at-structure:get-steel-area x))))

(defun at-structure:get-steel-area (steelbar-str / steelbar-lst steelbar-to-area gujin-zhishu)
  "����ֽ����ֵõ��ֽ����"
  "Real number"
  (defun steel-to-area (steelbar-str / nxd)
    (setq nxd (string:parse-by-lst steelbar-str '("%%132" "%%130" "%%131"))) ;;�ֽ����
    (if (= 2 (length nxd )) ; nxd = (list ����  ֱ��)
	(cons * (list (if (= "" (car nxd)) 1.0 (atof (car nxd))) 0.25 pi (atof (cadr nxd)) (atof (cadr nxd))))))
  (setq steelbar-str (vl-string-left-trim "GN BTXY&:" steelbar-str)) 
  (if (vl-string-search "@" steelbar-str)
      (progn;; ����/���/ǽ��
	(setq steelbar-lst  (string:parse-by-lst steelbar-str '("@"))) ;; ������
	(setq gujin-steel (string:parse-by-lst (car steelbar-lst) '("/")))
	(if (setq gujin-zhishu 
		  (cadr (string:parse-by-lst (cadr steelbar-lst) '("(" ")"))))
	    (setq gujin-zhishu (atoi gujin-zhishu))
	    (setq gujin-zhishu 1))
	    
	(eval (cons *
		    (cons
		     (cons /
			   (cons (cons +
				       (vl-remove nil (mapcar 'steel-to-area gujin-steel)))
				 (cons (length gujin-steel) (cons (atof (cadr steelbar-lst)) (cons 0.001  nil)))))
		     (cons gujin-zhishu nil)))))
      (progn ;; �ݽ�
	;; ȥ����������
	(setq steelbar-str (string:subst-all "" ")" (string:subst-all "" "(" steelbar-str)))
	(eval (cons +  (vl-remove nil (mapcar 'steel-to-area (string:parse-by-lst steelbar-str '("+" "/"))))))))
  )

(defun at-structure:query-steelbar()
  "��̬��ֽ������"
  (ui:dynquery '(lambda(x)
		 (cond
		   ((or (= name "TEXT")(= name "TCH_TEXT"))
		    (setq lst
			  (mapcar '(lambda (x / area )
				    (if (> (setq area (at-structure:get-steel-area x)) 0)
					(format nil "�ֽ����: ~d" area )
					"�Ǹֽ�����"))
				  (string:parse-by-lst (entity:getdxf ent 1) '(";" "��"))))
		    (setq lst (vl-remove nil lst)))
		   (T (setq lst (list "������" name )))
		   )
		 lst)
  ))
