;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vitaltools -- Ψ�����߼�
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: ���� AutoLisp/VisualLisp �����Ļ�ͼ���߼�
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; �ı�����
;;; �Ű棬�����Ķ��뷽ʽ�����Ա��ı���
(@:define-config '@text:fonts "tssdeng.shx,tssdchn.shx" "������ʽ standard �������ļ�")
(@:define-config '@text:color 10 "��ʱ���ߵ���ɫ�ţ�ÿ�α仯��������")
(@:define-config '@text:temp-layer "@temp@" "��ʱ���ߵ�ͼ��")

(defun @text:setup (/ res)
   (setq @::tmp-search-str "@text")
  (@::edit-config-dialog))
(defun @:set-fonts (/ fonts)
  (setq fonts (@:string-to-list (@:get-config '@text:fonts) ","))
  (if (and (findfile (car fonts)) (findfile (last fonts)))
      (command "-style" "Standard" (@:get-config '@text:fonts)
	       0 0.72 0 "n" "n" "n")))
(defun @text:multi-text-align (/ A ALL B C L M N P VAL X0 XYZ_NEW Y Y0 Z)
  (@::prompt 
   "�Զ���������еĵ����ı������.�����о��������С�")
  (setq a (ssget (list (cons 0 "text"))))
  (setq n (sslength a))
  (setq all nil)
  (setq m 0)
  (while (< m n)
    (setq all (append all (list (entget (ssname a m)))))
    (setq m (1+ m))
    )
  ;; �����㷨
  (setq l 0)
  (setq m 1)
  (while (< l n)
    (setq b (nth l all))
    (while (< m n)
      (setq c (nth m all))
      (if (> (nth 2 (assoc '10 c)) (nth 2 (assoc '10 b)))
          (progn
            (setq all (subst 'aa (nth l all) all))
            (setq all (subst 'bb (nth m all) all))
            (setq all (subst c 'aa all))
            (setq all (subst b 'bb all))
            (setq b c)
            )
          )
      (setq m (1+ m))
      )
    (setq l (1+ l))
    (setq m (1+ l))
    )

  (setq val (getdist "\n�оࣺ"))
  (setq p (getpoint "\n���еĲ���㣺"))
  (setq x0 (car p))
  (setq y0 (cadr p))

  (setq m 0)
  (while (< m n)
    (setq b (nth m all))
    (setq y (- y0 (* m val)))
    (setq z (nth 3 (assoc '10 b)))
    (setq xyz_new (list '10 x0 y z))
    (setq b (subst (cons '72 0) (assoc '72 b) b))
    (setq b (subst (cons '73 0) (assoc '73 b) b))
    (setq b (subst xyz_new (assoc '10 b) b))
    (entmod b)
    (setq m (1+ m))
    )
  )
(defun @text:insert-time( / ctextstyle cl )
  "����ʱ�����"
  (push-var nil)
					;(setvar "textstyle" "vitalhz350")
  (setq pt0 (getpoint "\n��ָ������λ�õ� :"))
  (setq date0 (menucmd "M=$(edtime,$(getvar,date),YYYY.MO.DD hh:mm:ss)"))
					;(setvar "textsize" 250)
  (setq cl (getvar "clayer"))
  (if (/= (tblsearch "layer" "org" ) nil )
      (setvar "clayer" "org")
      (progn
	(command "-layer" "n" "org" "")
	(command "-layer" "c" "t" "210,180,60" "org" "")
	)
      )
  (entity:make-text date0 pt0 350 0 0.8 0 13)
  (pop-var)
  (princ)
  )

(defun @text:justify( / s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "���������ı����뷽ʽ(�ı���λ�ò���)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;�������TEXT��ѡ��
  (command "justifytext" s1 "")
;;;  (setq ti% 0)
;;;   (if (/= s1 nil)
;;;       (progn
;;; 	(while 
;;; 	    (<= 
;;; 	     ti% 
;;; 	     (- (sslength s1) 1)
;;; 	     )
;;; 	  (setq ename(ssname s1 ti%))
;;; 	  (setq p11 nil)			;��p11�ÿ�
;;; 	  (setq e (entget ename))		;ȡʵ���e
;;; 	  (setq p11 (assoc  11 e))	;ȡ�����
;;; 	  (setq p10 (assoc 10 e))	;ȡ��ʼ���
;;; 	  (setq f (assoc 10 e))	;����
;;; 	  (setq en e)
;;; 	  (if (= (cdr f) 0) (setq p11n (cons 11 (cdr p10))))
;;; 
;;; 	  (if (/= (cdr f ) 0)
;;; 	      (progn
;;; 		(setq p10n (cons 10 (cdr p11)))	;�������λ������ʼ�����p10n
;;; 		(setq en (subst p10n p10 en))	;���µ���ʼ��λ�����ɵ���ʼ ���
;;; 		)
;;; 	    )
;;; 	  (setq fn 0)
;;; 	  (setq fn (cons 72 fn))			;(72 . 0) Ϊ����
;;; 	  (setq fn1 (assoc 72 en))		;ȡ�ɵĶ��뷽ʽ
;;; 	  (setq e (subst fn fn1 e))		;�����ɵĶ��뷽ʽΪ����
;;; 
;;; 	  (entmod e)
;;; 	  (setq ti%(+ 1 ti%))
;;; 	  )  ;end while
;;; 	) ;endprogn s1/=nil
;;;     )  ;endif  s1 /= nil
;;;   (setq s1 nil)
  (princ)
  )

(defun @text:justifytext-left(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "���������ı������(�ı���λ�ò���)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;�������TEXT��ѡ��
  (command "justifytext" s1 "" "L" )
  )
(defun @text:justifytext-right(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "���������ı��Ҷ���(�ı���λ�ò���)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;�������TEXT��ѡ��
  (command "justifytext" s1 "" "r" )
  )

(defun @text:justifytext-middle(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1)
  "���������ı��ж���(�ı���λ�ò���)"
  (setq s1 (ssget  '((0 . "TEXT"))))		;�������TEXT��ѡ��
  (command "justifytext" s1 "" "m" )
  )
(defun @text:a2t(/ s1 ename ti% p11 p10 f en p11n p10n fn fn1 attribtext)
  (@::prompt "������ת��Ϊ�����ı�.")
  (setq  s1  (ssget  '((0 . "ATTDEF"))))		;�������TEXT��ѡ��
  (setq attribtext '((0 . "TEXT") (100 . "AcDbEntity") (67 . 0) (410 . "Model") (8 . "0") (100 . "AcDbText") )  ) 
  (setq ti% 0)
  (if (/= s1 nil)
      (progn
	(while 
	    (<= 
	     ti% 
	     (- (sslength s1) 1)
	     )
	  (setq ename(ssname s1 ti%))
	  (setq p11 nil)			;��p11�ÿ�
	  (setq e (entget ename))		;ȡʵ���e
	  (setq en e)
	  ;;	(setq fn (cons 0 "TEXT"))  ; (72 . 0) Ϊ����
	  ;;	(setq fn1 (assoc 0 en))	   ; ȡ�ɵĶ��뷽ʽ
	  ;;	(setq e (subst fn fn1 e))  ; �����ɵĶ��뷽ʽΪ����
	  (setq e1 attribtext)
	  (setq e1 (append e1 (list (cons 1 (cdr (assoc 2 en))) (assoc 10 en) (assoc 11 en) (assoc 7 en) (assoc 50 en) (assoc 71 en) (assoc 72 en) (assoc 73 en) (assoc 210 en) (assoc 40 en) (assoc 41 en))))
	  ;;;(princ e1)
	  (entmake e1)
	  (entdel ename)
	  (setq ti%(+ 1 ti%))
	  )  ;end while
	) ;endprogn s1/=nil
      )  ;endif  s1 /= nil
  (setq s1 nil)
  (princ)
  )

;;�������������Ͻ�Ϊ����ת��������
;;����<<��̬�޸ĵ������ֿ�ȷ�Χ>>�޸Ķ���   
;;�޸ģ��·�
(defun @text:to-mtext( / #height #layer #pnt1 #read #string #text #textwidth Textwidth dl e1 e2 ell ename entext i ss text y #Pnt2)
  (setq ss (ssget '((0 . "TEXT"))) 
        i  0 
        dl nil
	Textwidth 0
	);setq
					;(setq #Pnt1 (getpoint "\�������Ͻǲ����: "))
  (if ss
      (progn 
        (repeat (sslength ss)
                (setq ename (ssname ss i)
		      box (getbox ename)
                      ell    (entget ename)
                      y      (caddr (assoc 10 ell))
                      text   (cdr (assoc 1 ell))
                      #Height   (cdr (assoc 40 ell))
		      #textwidth (* #Height (cdr (assoc 41 ell)))
                      Textwidth  (max Textwidth (- (caadr box)(cadr (assoc 10 ell))))
		      #textstyle (cdr (assoc 7 ell))
                      #Layer   (cdr (assoc 8 ell))
                      i      (1+ i)
                      );setq
                (setq dl (append dl (list (list y text ename))))
                );repeat
        (setq dl    (vl-sort dl (function (lambda (e1 e2) (> (car e1) (car e2)))))
              i     1
              );setq
        (setq #Pnt2 (getbox (caddar dl)) #Pnt1 (list (caar #Pnt2)(cadadr #Pnt2) 0.0))
	(setq text  (cadr (nth 0 dl)))
        (repeat (- (length dl) 1)
                (setq text (strcat text "\\P" (cadr (nth i dl))))
                (setq i (1+ i))
                );repeat
        (command "erase" ss "")
        );progn
      (princ "\nδѡ���κ����֣�")
      );if
  (setq #String text)
  (setq #Text (@text:MText #Pnt1 #String Textwidth #Layer 1 #Height #textstyle)) 
					;(setq enText (vlax-vla-object->ename #Text))
					;(command "explode" enText)   
  (redraw)
  (princ)
  )
;;;ȡ��ʵ������ο�---���أ�p1 p2��
(defun getbox (en / obj box-p1 box-p2)
  (setq obj (vlax-ename->vla-object en))
  (vla-getboundingbox obj 'box-p1 'box-p2)
  (setq box-p1 (vlax-safearray->list box-p1))
  (setq box-p2 (vlax-safearray->list box-p2))
  (list box-p1 box-p2)
  )
(defun @text:MText (#InsertionPoint #String #Width #Layer #Justification #Height  #style / #insertion #object #space)
  (or #Width (setq #Width 0))
  (or *AcadDoc*
      (setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object)))
      ) ;_ or
  (setq #Space     (if (or (eq acmodelspace
                               (vla-get-activespace *AcadDoc*)
                               ) ;_ eq
                           (eq :vlax-true (vla-get-mspace *AcadDoc*))
			   ) ;_ or
                       (vla-get-modelspace *AcadDoc*)
                       (vla-get-paperspace *AcadDoc*)
                       ) ;_ if
        #Insertion (cond
                     ((vl-consp #InsertionPoint) (vlax-3d-point #InsertionPoint))
                     ((eq (type #InsertionPoint) 'variant) #InsertionPoint)
                     (T nil)
                     ) ;_ cond
	) ;_ setq
  (setq #Object (vla-addmtext #Space #Insertion #Width #String))
  (if #Height (vla-put-Height #Object #Height) ) 
  (and #Layer
       (tblsearch "layer" #Layer)
       (vla-put-layer #Object #Layer)
       ) ;_ and
  (cond ((member #Justification (list 1 2 3 4 5 6 7 8 9))
         (vla-put-attachmentpoint #Object #Justification)
         (vla-move #Object
                   (vla-get-InsertionPoint #Object)
                   #Insertion
		   ) 
         )
	)
  (vla-put-StyleName #Object #style)
  #Object
  )

(defun @text:menu-add-prefix-or-suffix(/ res)
  (setq res (ui:input "������ǰ��׺����" '(("ǰ׺:" )("��׺:"))))
  (@text:add-prefix-suffix (cdr (assoc "ǰ׺:" res))
			   (cdr (assoc "��׺:" res))
			   (pickset:to-list (ssget '((0 . "TEXT")))))
  (princ))
    
(defun @text:add-prefix-suffix (str-prefix str-suffix lst-ent)
  (mapcar '(lambda (x)
	     (entity:putdxf x 1 
			    (strcat str-prefix (entity:getdxf x 1) str-suffix)))
	  lst-ent))

(defun @text:find-from-line(/ s1 pt-base txt ename ti% p11 p10 f en p11n p10n fn fn1 attribtext)
  (@::prompt "ѡ��һ���ı�a��ѡ����ҷ�Χ������������ͬ���ı��������ߡ�")
  (if (null layer:make)(require 'layer:*))
  (if (= 'subr (type layer:make))
      (layer:make (@:get-config '@text:temp-layer) 1 nil nil))
  (setq en1 (car (entsel "��ѡ��һ�������ı�:")))
  (setq pt-base (cdr (assoc 10 (entget en1))))
  (setq txt (cdr (assoc 1 (entget en1))))
  (if (and pt-base txt)
      (progn
	(princ "\n��ѡ����Ҫ���ҵ�����:")
	(setq s1 (ssget  (list '(0 . "text,tch_text") (cons 1 txt))))
	(mapcar
	 (function
	  (lambda (x)
	    (entity:putdxf
	     (entity:putdxf
	      (entity:make-line pt-base
				(cdr (assoc 10 (entget x))))
	      8 (@:get-config '@text:temp-layer))
	     62 (@:get-config '@text:color))))
	 (pickset:to-list s1))
	(@:set-config '@text:color
		      (* 10
			 (if (< (@:get-config '@text:color) 240)
			     (+ 10 (@:get-config '@text:color))
			   10)))))
  )

(defun @text:menu-format-number(/ int-n int-fraction res)
  (@::prompt "��ʽ���ı��е����֣�")
  (setq res (ui:input "�������ʽ�����ò���"
		      '(("����λ��" 2)
			("С��λ��" 0)
			("����ַ�" "0"))))
  (prompt "��ѡ��Ҫ������ı�")
  (setq txts (pickset:to-list (ssget '((0 . "text")))))
  (mapcar
   '(lambda(x)
      (entity:putdxf
       x 1
       (string:from-list
	(mapcar
	 '(lambda (y)
	    (if (string:numberp y)
		(string:number-format
		 (vl-string-left-trim "0 " y)
		 (cdr (assoc "����λ��" res))
		 (cdr (assoc "С��λ��" res))
		 (cdr (assoc "����ַ�" res))
		 )
	      y)
	    )
	 (string:auto-split (entity:getdxf x 1)))
	""
	)))
   txts)
  )
