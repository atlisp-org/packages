(defun @lab:named-first-layout-from-filename(/ *error* dir% dwgs)
  (@::prompt "�����޸�ĳһ�ļ����µ� dwg �ļ��ĵ�һ����������Ϊ�ļ�����")
  (defun *error* (msg)
    (if (and
	 (and (= 'vla-object (type doc%)))
	 (null (vlax-erased-p doc%)))
	(progn
	  (vla-close doc%)
	  (vlax-release-object doc%)
	  (vla-quit acadapp)
	  ))
    (mapcar '(lambda(obj)
	       (if(and (= 'vla-object (type obj))
		       (null (vlax-erased-p obj)))
		   (vlax-release-object obj)))
	    (list dwgdocs% acadapp))
    (@:*error* msg))
  (setq dir% (system:get-folder "��ѡ��Ҫ������ļ��У�"))
  (if (and dir%
	   (setq dwgs (vl-directory-files dir% "*.dwg" 1))
	   (setq acadapp (vlax-get-or-create-object "AutoCAD.Application"))
	   (setq dwgdocs% (vla-get-documents acadapp)))
      (progn
	(foreach file% dwgs
		 (princ(strcat "����dwg�ļ� " file% "... "))
		 
		 (vla-open dwgdocs% (strcat dir% "\\" file%))
		 (setq doc% (vla-item dwgdocs% file%))
		 (if doc%
		     (progn
		       ;;ʵ��
		       (if (/= (vla-get-name (vla-item (vla-get-layouts doc%) 0))
			       "Model")
			   (vla-put-name 
			    (vla-item (vla-get-layouts doc%) 0)
			    (vl-filename-base file%))
			 (if (/= (vla-get-name (vla-item (vla-get-layouts doc%) 1))
				 "Model")
			     (vla-put-name 
			      (vla-item (vla-get-layouts doc%) 1)
			      (vl-filename-base file%))
			   ))
		       ;;����
		       (vla-save doc% )
		       (vla-close doc%)
		       (princ "OK!\n"))
		   (princ "Faile!\n")
		 ))
	(vla-quit acadapp)
	(mapcar 'vlax-release-object (list doc% dwgdocs% acadapp)))
    (alert "Error: δѡ���κ��ļ��С����ܴ� autocad ������")
    ))
