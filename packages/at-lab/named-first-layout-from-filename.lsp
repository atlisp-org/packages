(defun @lab:named-first-layout-from-filename(/ *error* dir% dwgs)
  (@:help "批量修改某一文件夹下的 dwg 文件的第一个布局名称为文件名。")
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
  (setq dir% (system:get-folder "请选择要处理的文件夹："))
  (if (and dir%
	   (setq dwgs (vl-directory-files dir% "*.dwg" 1))
	   (setq acadapp (vlax-get-or-create-object "AutoCAD.Application"))
	   (setq dwgdocs% (vla-get-documents acadapp)))
      (progn
	(foreach file% dwgs
		 (princ(strcat "处理dwg文件 " file% "... "))
		 
		 (vla-open dwgdocs% (strcat dir% "\\" file%))
		 (setq doc% (vla-item dwgdocs% file%))
		 (if doc%
		     (progn
		       ;;实操
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
		       ;;保存
		       (vla-save doc% )
		       (vla-close doc%)
		       (princ "OK!\n"))
		   (princ "Faile!\n")
		 ))
	(vla-quit acadapp)
	(mapcar 'vlax-release-object (list doc% dwgdocs% acadapp)))
    (alert "Error: 未选中任何文件夹。或不能打开 autocad 副本。")
    ))
