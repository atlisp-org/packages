(@:add-menu "ͳ��" "ͼ����" "(@:stat-block-by-name)")
(@:add-menu "ͳ��" "����" "(@:menu-stat-block-by-attribute)")
(@:add-menu "ͳ��" "�����ı�" "(@:menu-stat-text)")
(@:add-menu "ͳ��" "ͼԪͳ��" "(@math:stat-entity-gui)")
(@:add-menu "ͳ��" "ͼԪ��ɫ" "(@math:stat-color)")
(@:add-menu "ͳ��" "�����豸" "(@stat:telec-equip)")
(@:add-menu "ͳ��" "--" "--")
(@:add-menu "ͳ��" "������" "(stat:print)")
(@:add-menu "ͳ��" "���ƽ��" "(stat:draw)")
;;(@:add-menu "ͳ��" "������" "(@:stat-block-by-attribute)")
(defun @:stat-block-by-name ()
  "ͳ��ѡ�п�Ŀ�����������"
  (setq @:tmp-stat-result  ;; ͳ�ƽ��
	(stat:stat         ;; ͳ�ƺ���
	 (mapcar (function
		  (lambda (x) ;; ������������ѡ���б���ȡ��Ҫͳ�Ƶ��
		   (block:get-effectivename x))) ;; ʾ��: ��ͼԪȡ��ͼ����
		 (pickset:to-list (ssget '((0 . "insert")))))))) ;; Ҫͳ�Ƶ�ͼԪ��

(defun @:stat-block-by-attribute (attribute-name block-name)
  "ͳ��ѡ�е�ָ�������е�ĳһ���Ե�ֵ��������"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (if (= block-name (block:get-effectivename x))
		       (cdr (assoc attribute-name (block:get-attributes x)))
		       )))
		 (pickset:to-list (ssget '((0 . "insert"))))))))

(defun @:menu-stat-block-by-attribute (/ blk-name attribute-name)
  (setq blk-name (getstring "������Ҫͳ�ƵĿ�����:"))
  (setq attribute-name (getstring "������Ҫ����ͳ�ƵĿ����Ե�����:"))
  (@:stat-block-by-attribute attribute-name blk-name))

(defun @:stat-entity (stat-item ssfilter)
  "stat-item: ͳ����Ŀ(dxf �������ͼ�� Ϊ8 ); ssfilter ѡ�񼯹���"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (cdr (assoc stat-item (entget x)))))
		 (pickset:to-list (ssget ssfilter))))))

(defun @math:stat-color (/ ents)
  "stat-color: ͳ��ʵ�����ɫ"
  (prompt "��ѡ��ͼԪ:")
  (setq ents (pickset:to-list (ssget )))
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		    (entity:get-color x)))
		 ents))))

(defun @math:stat-entity-gui (/ name dxf)
  (@:help "������Ҫ��ͳ����Ŀͳ��ѡ�е�ѡ�񼯡�")
  (setq dxf nil)
  (setq name (ui:select "��ѡ��Ҫͳ�Ƶ���" (mapcar 'cdr dxf-common)))
  (foreach n% dxf-common
	   (if (= name (cdr n%))
	       (setq dxf (car n%))))
  (if dxf 
      (@:stat-entity dxf nil)))

(defun @:menu-stat-text (/ ssfilter)
  (setq strfilter (getstring "�������ı�ͨ���(ʾ�� GBZ* ?BZ* LL*): "))
  (if (/= "" strfilter)
      (setq ssfilter (list  '(0 . "TEXT") (cons 1  strfilter)))
      (setq ssfilter (list  '(0 . "TEXT"))))
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (cdr (assoc 1 (entget x)))))
		 (pickset:to-list (ssget ssfilter))))))

(defun @stat:telec-equip ()
  (@:help (strcat 
	   "ͳ�Ƶ����豸��������������������������豸�顣"))
  (setq @:tmp-stat-result  ;; ͳ�ƽ��
	(stat:stat         ;; ͳ�ƺ���
	 (mapcar (function
		  (lambda (x) ;; ������������ѡ���б���ȡ��Ҫͳ�Ƶ��
		    (car
		     (string:to-list 
		      (block:get-effectivename x)
		      "(")
		     ))) ;; ʾ��: ��ͼԪȡ��ͼ����
		 (pickset:to-list (ssget '((0 . "insert")(2 . "$equip*")))))))) ;; Ҫͳ�Ƶ�ͼԪ��