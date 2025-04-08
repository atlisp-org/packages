(@:add-menu "�ⲿ����" "Everything" "(everything:start)" )
(defpackage :everything 
  (:use :cl)
  (:export :open)
  )
(defun everything:start ( / app )
  ;; ��ִ���ļ�·��
  (setq app "bin\\everything.exe")
  (if (null (findfile app))
      ;; ����ѹ����
      (@:down-and-unzip "archives/everything.zip" "bin"))
  ;;�����ⲿ��ִ�г���
  (if (findfile app)
      (progn
	(setvar "cmdecho" 0)
	(command "start-bg" (strcat  @:*prefix* app ))
	(setvar "cmdecho" 1)))
  (princ)
  )
