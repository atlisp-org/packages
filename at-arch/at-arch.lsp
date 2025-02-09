;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ����ʹ�ÿ������� dev-tools �Զ������ĳ���Դ�ļ� 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ���������� 'at-arch:first ���� Ӧ�ð� at-arch �� ��һ�������� first 
(@:define-config '@arch:parking-blk "��λ*,��е��λ*" "��λͼ����")
(@:define-config '@arch:parking-number-order "Yx" "�������ʽ")

;; (@:get-config 'at-arch:first) ;; ��ȡ���ö���ֵ
;; (@:set-config 'at-arch:first  "�����ֵ") ;; �������ö���ֵ
;; ��ϵͳ����Ӳ˵� 
(@:add-menus
 '("@����"
   ("��������" "(at-arch:setup)" )
   ("���복λ" "(at-arch:insert-parking)" )
   ("�������ϰ���λ" "(at-arch:insert-accparking)" )
   ("�����е��λ" "(at-arch:insert-machineparking)" )
   
   ("��λ���" "(at-arch:parking-numbering)" )))

(defun at-arch:setup (/ res)
   (setq @::tmp-search-str "@arch")
   (@::edit-config-dialog))

(defun at-arch:insert-parking ()
  (setq downfile "at-arch/��λ.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "�������������dwg�ļ�, ���Ժ�"))(sleep 5))
    (ui:dyndraw
     (block:insert "��λ" "D:/design/standard" '(0 0 0)0 1)
     '(0 0 0))))
(defun at-arch:insert-accparking ()
  (setq downfile "at-arch/��λ-���ϰ�.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "�������������dwg�ļ�, ���Ժ�"))(sleep 5))
    (ui:dyndraw
     (block:insert "��λ-���ϰ�" "D:/design/standard" '(0 0 0)0 1)
     '(0 0 0))))
(defun at-arch:insert-machineparking ()
  (setq downfile "at-arch/��е��λ.dwg")
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "�������������dwg�ļ�, ���Ժ�"))(sleep 5))
    (ui:dyndraw
     (block:insert "��е��λ" "D:/design/standard" '(0 0 0)0 1)
     '(0 0 0))))

  
(defun at-arch:parking-numbering (/ parkings inputint atts)
  ;; ���²���Ϊ��Ϊʵ��ĳһ��������д�Ĵ��롣
  (@::prompt "���ѡҪ���б�ŵĳ�λ")
  (setq parkings
	(pickset:sort
	 (pickset:to-list  (block:ssget  nil (@::get-config '@arch:parking-blk) nil))
	 (@::get-config '@arch:parking-number-order) 
	 (mapcar '@::scale '(8 8))))

  ;; ���

  (if (null parking-curr-number)
      (setq parking-curr-number 0))
  (if (setq inputint  (getint (strcat "��������ʼ��<"(itoa (1+ parking-curr-number))">:")))
      (setq parking-curr-number (1- inputint)))
  
  (foreach park% parkings
	   (setq atts (list:sort (block:get-attributes park%)
				 '(lambda (x y)
				    (< (car x)(car y)))))
	   (foreach att atts
		    (if (wcmatch (car att) "NUMBER*")
			(block:set-attributes
			 park%
			 (list
			  (cons
			   (car att)
			   (itoa (setq parking-curr-number (1+ parking-curr-number)))))))))
  (princ)
  )
