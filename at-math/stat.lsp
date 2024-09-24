(@:add-menu "ͳ��" "ͼ����" "(@:stat-block-by-name)")
(@:add-menu "ͳ��" "������" "(@:menu-stat-block-by-attribute)")
(@:add-menu "ͳ��" "��̬������" "(@:menu-stat-block-by-properties)")
(@:add-menu "ͳ��" "�����ı�" "(@:menu-stat-text)")
(@:add-menu "ͳ��" "��������" '(at-math:sumtxt))
(@:add-menu "ͳ��" "ͼԪͳ��" "(@math:stat-entity-gui)")
(@:add-menu "ͳ��" "ͼԪ��ɫ" "(@math:stat-color)")
(@:add-menu "ͳ��" "�����豸" "(@stat:telec-equip)")
(@:add-menu "ͳ��" "���򰴲�" "(@stat:region-by-layer)")
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
		 (pickset:to-list (ssget '((0 . "insert"))))))) ;; Ҫͳ�Ƶ�ͼԪ��
  (if @:tmp-stat-result (stat:draw))
  )

(defun @:stat-block-by-attribute (attribute-name block-name)
  "ͳ��ѡ�е�ָ�������е�ĳһ���Ե�ֵ��������"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (if (wcmatch (block:get-effectivename x) block-name)
		       (cdr (assoc attribute-name (block:get-attributes x)))
		       )))
		 (pickset:to-list (ssget '((0 . "insert")))))))
  (if @:tmp-stat-result (stat:draw))
  )

(defun @:menu-stat-block-by-attribute (/ blk-name attribute-name)
  (setq blk-name (getstring "������Ҫͳ�ƵĿ�����:"))
  (setq attribute-name (getstring "������Ҫ����ͳ�ƵĿ����Ե�����:"))
  (@:stat-block-by-attribute attribute-name blk-name))

(defun @:stat-block-by-properties (prop-name block-name)
  "ͳ��ѡ�е�ָ�������е�ĳһ��̬�����Ե�ֵ��������"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (if (wcmatch (block:get-effectivename x) block-name)
		       (cdr (assoc prop-name (block:get-dynamic-properties x)))
		       )))
		 (pickset:to-list (ssget '((0 . "insert")))))))
  (if @:tmp-stat-result (stat:draw)))

(defun @:menu-stat-block-by-properties (/ blk-name prop-name)
  (setq blk-name (getstring "������Ҫͳ�ƵĿ�����:"))
  (setq prop-name (getstring "������Ҫ����ͳ�ƵĶ�̬�����Ե�����:"))
  (@:stat-block-by-properties prop-name blk-name))


(defun @:stat-entity (stat-item ssfilter)
  "stat-item: ͳ����Ŀ(dxf �������ͼ�� Ϊ8 ); ssfilter ѡ�񼯹���"
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (cdr (assoc stat-item (entget x)))))
		 (pickset:to-list (ssget ssfilter)))))
  (if @:tmp-stat-result (stat:draw)))

(defun @math:stat-color (/ ents)
  "stat-color: ͳ��ʵ�����ɫ"
  (prompt "��ѡ��ͼԪ:")
  (setq ents (pickset:to-list (ssget )))
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		    (entity:get-color x)))
		 ents)))
  (if @:tmp-stat-result (stat:draw)))

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
      (setq ssfilter (list  '(0 . "TEXT,TCH_TEXT") (cons 1  strfilter)))
      (setq ssfilter (list  '(0 . "TEXT,TCH_TEXT"))))
  (setq @:tmp-stat-result
	(stat:stat
	 (mapcar (function
		  (lambda (x)
		   (cdr (assoc 1 (entget x)))))
		 (pickset:to-list (ssget ssfilter)))))
  (if @:tmp-stat-result (stat:draw)))

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
		 (pickset:to-list (ssget '((0 . "insert")(2 . "*$equip*")))))))
  (if @:tmp-stat-result (stat:draw)))
(if (null dxf-common)
    (setq dxf-common
	  '((0 . "ͼԪ����")
	    (8 . "ͼ��")
	    (6 . "����")
	    (62 . "��ɫ��")
	    (48 . "���ͱ���")
	    (40 . "�뾶"))))
      
(defun @stat:region-by-layer ()
  (@:help (strcat "����ͬ��ͼ��ͳ������������"))
  (setq @:tmp-stat-result
	(stat:classify
	 (mapcar (function
		  (lambda (x)
		   (cons
		    (entity:getdxf x 8)
		    (vla-get-area (e2o x)))
		   ))
		 (pickset:to-list (ssget '((0 . "region"))))
		 ))))
