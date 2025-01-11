;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ����ʹ�ÿ������� dev-tools �Զ������ĳ���Դ�ļ� 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ���������� 'at-number:first ���� Ӧ�ð� at-number �� ��һ�������� first 
(@:define-config '@number:layer "number"  "�����������ͼ��")
(@:define-config '@number:order "yx"  "��ŵ�λ��˳��")
(@:define-config '@number:fontsize 2.5  "��ŵ������С��ʵ��ֵΪ��ֵ����@lisp��ͼ������")
;; (@:define-config '@number:layer "number"  "�����������ͼ��")
;; ��ϵͳ����Ӳ˵� 
(@:add-menus
 '("��Ź���"
   ("�������" (at-number:setup))
   ("ͼ�α��" (at-number:number-entity))
   ("ɾ�����" (at-number:delete-number))

   ))
(defun at-number:setup (/ res)
  (setq @::tmp-search-str "@number")
  (@::edit-config-dialog))
(defun at-number:number-entity ()
  (@::prompt '("��ѡ�е�ͼ�ν��б�ţ�������ѡ������ͼ�Σ���ִ�иù��ܡ�"
	     ))
  (@::prompt "��ѡ��Ҫ��ŵ�ͼ��")
  (if (null (tblsearch "layer" (@::get-config '@number:layer)))
      (layer:make (@::get-config '@number:layer) 1 nil nil))
  (if (null (setq ents (cadr (ssgetfirst))))
      (setq ents (ssget)))
  
  (setq ents
	(pickset:sort-by-box
	 (pickset:to-list ents)
	 (@::get-config '@number:order) (mapcar '@::scale  '(1e-2 1e-2))))
  
  (setq i 0)
  (mapcar '(lambda(x)
	    (entity:putdxf 
	     (entity:make-text (itoa (setq i (1+ i)))
	      (point:centroid (entity:getbox x 0))
	      (@::scale (@::get-config '@number:fontsize))
	      0 0.72 0 "MM")
	     8 (@::get-config '@number:layer)
	     ))
	  ents)
  (princ)
  )
(defun at-number:delete-number ()
  (setq
   ents
   (vl-remove-if-not
    '(lambda(x)
      (string:numberp (entity:getdxf x 1)))
    (pickset:to-list
     (ssget (list '(0 . "text")
		  (cons 8 (@::get-config '@number:layer)))))))
  (mapcar 'entdel ents)
  (princ))
