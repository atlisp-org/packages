(@:add-menu "���ʹ���" "���ʼ�����" '(c:ag))
(defun agan-cal:init(/ fp)
  (if (not (findfile (strcat @::*prefix* "packages/agan-cal/��ʷ��¼.txt")))
      (progn
	(setq fp (open (strcat @::*prefix* "packages/agan-cal/��ʷ��¼.txt")"w"))
	(close fp))))
(agan-cal:init)
