(defun @lab:summary-data (/ tbl-summary get-data)
  (@::prompt (strcat "����ͼ��ͼ�����ı���Ϣ\n�γɱ��"))
  (defun get-data (ent / data box mt)
    (if ent
	(progn
	  (setq data (block:get-attributes ent))
	  (setq box (entity:getbox ent 0))
	  (@:cmd "zoom" "w" (car box)(cadr box))
	  (setq mt (pickset:to-list
		    (ssget "w" (car box)(cadr box)
			   '((0 . "mtext")))))
	  (if mt
	      (progn
		(setq data-mt
		      (mapcar 'car
			      (vl-remove-if '(lambda(x)(cdr x))
					    (apply 'append
						   (mapcar '(lambda(x)
							      (text:parse-mtext
							       (text:get-mtext x)))
							   mt)))))
		(foreach
		 txt data-mt
		 (foreach handle '("����""���ϳߴ�""���洦��""����")
			  (if (setq res (member handle
						(mapcar '(lambda(x)
							   (vl-string-trim " " x))
							(string:parse-by-lst txt '("��""��""��")))))
			      (setq data (cons (cons handle
						     (cadr res)) 
					       data))))))
	    )
	  data
	  )))
  (setq mapsheet (car (entsel)))
  (prompt "��ѡ��Ҫ��ȡ���ݵ�ͼ��")
  (if (and (setq ss-tk (ssget (list '(0 . "insert")
				    (assoc 2 (entget mapsheet)))))
	   (setq tbl-data (mapcar 'get-data (pickset:to-list ss-tk)))
	   (setq tbl-header '("���ϱ���" "ͼֽ���" "��Ԫ����/�㲿������" "����""���ϳߴ�""���洦��""����" "��ע"))
	   (setq tbl-data (mapcar '(lambda(x / data)
				     (foreach hd tbl-header
					      (if (assoc hd x)
						  (setq data (cons (cdr (assoc hd x)) data))
						(setq data (cons "" data)))
					      )
				     (reverse data))
				  tbl-data))
	   (listp tbl-data)
	   (> (length tbl-data) 0))
      (table:make (getpoint "�����������λ��:") "���" tbl-header (reverse tbl-data)))
  )

