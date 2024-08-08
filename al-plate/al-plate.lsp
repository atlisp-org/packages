(@::define-config 'al-plate:al-layer "A-����" "����������ͼ��")
(@::define-config 'al-plate:thikness-layer "A-�����" "����������ͼ��")
(@::define-config 'al-plate:thikness 0.5 "������ֵ")
(@::define-config 'al-plate:length 200 "���������Ƴ���")
(@::define-config 'al-plate:symmetry 1 "�������Ƿ�ת�ǹ�����0: ��ת�ǣ�1:һ��ת�ǣ�2:����ת��")
(@::define-config 'al-plate:symmetry 1 "�������Ƿ�Գ�")


(@::add-menus
 '("����"
   ("��������" (al-plate:setup))
   ("����չ��" (al-plate:draw1))
   ))

(defun al-plate:setup ()
  (setq @::tmp-search-str "al-plate")
  (@::edit-config-dialog))

(defun al-plate:draw-expand (lst)
  (setq s 0)
  (setq lst-diso
	(mapcar '(lambda(x)
		  (+ s x)
		  (setq s (+ s x)))
		lst))
  (setq pts (cons '(0 0 0) (mapcar '(lambda(x)
				     (list x 0 0))
				   lst-diso)))
  (setq ents
	(cons
	 ;;����
	 (entity:putdxf
		    (entity:make-lwpolyline
		     pts nil 0 0 0)
		    8 "A-չ����")
		   (mapcar '(lambda(x)
			     (entity:putdxf
			      (entity:make-line
			       x
			       (polar x (* 0.5 pi) (@::get-config 'al-plate:length)))
			      8  "A-������")
			     )
			   pts)))
  (ui:dyndraw ents '(0 0 0))
  )
(defun al-plate:draw1 ()
  (@:prompt "ѡ������")
  (if (setq lwpl (car (pickset:to-list (ssget ":S:E" (list '(0 . "lwpolyline")
							   (cons 8 (@::get-config 'al-plate:al-layer)))
					      ))))
      (progn
	(setq pts (curve:get-points lwpl))
	(setq pt1 (car pts))
	(setq pts (cdr pts))
	(setq pt2 (car pts))
	(setq pts (cdr pts))
	(setq flag-start 0)
	(setq segs nil)
	(while (setq pt3 (car pts))
	  ;; �ж����ۻ�����
	  (setq flag-end
		(if (ssget "f"
			   (list
			    (setq pt-mid (point:mid pt1 pt2))
			    (polar pt-mid (angle pt2 pt3)  2))
			   (list '(0 . "line,lwpolyline")
			     (cons 8 (@::get-config 'al-plate:thikness-layer))))
		    (@::get-config 'al-plate:thikness) 0.0))
	  (setq segs
		(cons
		 (- (distance  pt1 pt2)
		    flag-start
		    flag-end)
		 segs))
	  (setq flag-start flag-end)
	  (setq pt1 pt2)
	  (setq pt2 pt3)
	  (setq pts (cdr  pts))
	  )
	(setq segs
	      (cons
	       (- (distance  pt1 pt2)
		  flag-start)
	       segs))
	(princ segs)
	;;����
	(al-plate:draw-expand segs)
	)))

