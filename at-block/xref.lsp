(@:add-menus 
  '("����ղ���"
    ("���չ��" (@block:xref-layer))
    ("����ѡ��" (@block:xfr))
    ("��������" (@block:czcz))
    ("ж��ѡ��" (@block:xfx))
    ("ж������" (@block:xfxa))
    ("����ѡ��" (@block:xfd))
    ("��������" (@block:xfda))
    ("��ѡ��" (@block:xfb))
    ("������" (@block:bdcz))
    ("������ʾ" (@block:help-xref-hk))))
(defun @block:xref-layer (/ xrefs) 
  (@::prompt "���ⲿ��������ͬһ��ͼ�㣬�Ա���������ͼ�����������н������á�")
  (setq xrefs (pickset:to-list (ssget "x" '((0 . "insert")))))
  ;; ȥ��������տ�
  (setq xrefs (vl-remove-if-not 
                '(lambda (x) 
                   (and 
                     (assoc 1 (tblsearch "block" (entity:getdxf x 2)))
                     (findfile 
                       (cdr (assoc 1 (tblsearch "block" (entity:getdxf x 2)))))))
                xrefs))
  ;;���� xrefs �������е��ⲿ����ͼԪ�ˡ�
  (layer:lock (@:get-config '@block:xref-layer) nil)
  (mapcar 
    '(lambda (xref%) 
       ;; ��������ڲ�������ͼ������
       (if (null (tblsearch "layer" (@:get-config '@block:xref-layer))) 
         (layer:make (@:get-config '@block:xref-layer) nil nil nil))
       ;;�������������յ�ͼ������ͬ���޸�
       (if (/= (@:get-config '@block:xref-layer) (entity:getdxf xref% 8)) 
         (entity:putdxf xref% 8 (@:get-config '@block:xref-layer))))
    xrefs)
  (layer:lock (@:get-config '@block:xref-layer) t))
;;XFR        ����ѡ��
;;CZCZ        ��������
;;XFX        ж��ѡ��
;;XFXA        ж������
;;XFD        ����ѡ��
;;XFDA        ��������
;;XFB        ��ѡ��
;;BDCZ        ������
(defun @block:help-xref-hk () 
  (alert 
    (strcat "\n����ѡ�������ļ� ���XFR" "\n���¼��������ⲿ���� ���CZCZ" "\nж��ѡ�������ļ� ���XFX" 
            "\nж�������ⲿ���� ���XFXA" "\n����ѡ�������ļ� ���XFD" "\n���������ⲿ���� ���XFDA" "\n��ѡ�������ļ� ���XFB" 
            "\n�������ⲿ���� ���BDCZ")))

(Defun @block:XFR ()  ;���塰����ѡ�������ļ�������
  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������

  (princ "\nѡ��Ҫ��������Ĳ����ļ�:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_������
        (command "-xref" "R" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat ���ӳ��ַ���
      ) ;end repeat
      (prompt "\n�Ѿ����ص��ļ�Ϊ:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:CZCZ ()  ;���塰���¼��������ⲿ���ա�����
  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������
  (command "-xref" "R" "*")
  (princ))
(Defun @block:XFX ()  ;���塰ж��ѡ�������ļ�������

  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������

  (princ "\nѡ��Ҫж�ص��ⲿ���ն���:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_������
        (command "-xref" "U" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat ���ӳ��ַ���
      ) ;end repeat
      (prompt "\n��ж�ص��ⲿ�����ļ���:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:XFXA ()  ;���塰ж�������ⲿ���ա�����
  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������
  (command "-xref" "U" "*")
  (princ))
(Defun @block:XFD ()  ;���塰����ѡ�������ļ�������

  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������

  (princ "\nѡ��Ҫ������ⲿ���ն���:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_������
        (command "-xref" "Detach" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat ���ӳ��ַ���
      ) ;end repeat
      (prompt "\n�Ѳ�����ⲿ�����ļ�Ϊ:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:XFDA ()  ;���塰���������ⲿ���ա�����
  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������
  (command "-xref" "Detach" "*")
  (princ))
(Defun @block:XFB ()  ;���塰��ѡ�������ļ�������

  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������

  (princ "\nѡ��Ҫ�󶨵��ⲿ���ն���:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (setq oldBT (getvar "BINDTYPE"))
      (setq BT (if (null BT) oldBT BT))
      (setq BT_tmp (getstring 
                     (strcat "���������[��(N)/����(Y)]<" (itoa BT) ">: ")))
      (if (null BT_tmp) (setq BT_tmp BT))
      (setq BT (atoi BT_tmp))
      (setvar "BINDTYPE" BT)
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_������
        (command "-xref" "Bind" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat ���ӳ��ַ���
      ) ;end repeat
      (setvar "BINDTYPE" oldBT)
      (prompt "\n�Ѱ󶨵��ⲿ�����ļ�Ϊ:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:BDCZ ()  ;���塰�������ⲿ���ա�����
  (setvar "cmdecho" 0) ;����ִ�й��̲�������ʾ������
  (setq oldBT (getvar "BINDTYPE"))
  (setq BT (if (null BT) oldBT BT))
  (setq BT_tmp (getstring 
                 (strcat "���������[��(0)/����(1)]<" (itoa BT) ">: ")))
  (if (null BT_tmp) (setq BT_tmp BT))
  (setq BT (atoi BT_tmp))
  (setvar "BINDTYPE" BT)
  (command "-xref" "Bind" "*")
  (setvar "BINDTYPE" oldBT)
  (princ))

(@:define-hotkey "xfr" "(@block:xfr)")
(@:define-hotkey "czcz" "(@block:czcz)")
(@:define-hotkey "xfx" "(@block:xfx)")
(@:define-hotkey "xfxa" "(@block:xfxa)")
(@:define-hotkey "xfd" "(@block:xfd)")
(@:define-hotkey "xfda" "(@block:xfda)")
(@:define-hotkey "xfb" "(@block:xfb)")
(@:define-hotkey "bdcz" "(@block:bdcz)")