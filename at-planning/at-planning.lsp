;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ����ʹ�ÿ������� dev-tools �Զ������ĳ���Դ�ļ� 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ���������� 'at-planning:first ���� Ӧ�ð� at-planning �� ��һ�������� first 
(@:define-config '@planning:land-layer "�õغ���" "�õغ���ͼ�㡣")
(@:define-config '@planning:building-layer "��������" "��������������ͼ�㡣")
(@:define-config '@planning:green-layer "�̵���" "�̵�ͼ�㡣")
(@:define-config '@planning:road-layer "��·" "��·ͼ�㡣")
(@:define-config '@planning:openspace-layer "�����ռ�" "���п����ռ�ͼ�㡣")
(@:define-config '@planning:square-layer "���й㳡" "���й㳡ͼ�㡣")
(@:define-config '@planning:parking "*��λ" "����ͳ��ͣ��λ��ͼ����")

;; ��ϵͳ����Ӳ˵� 
(@:add-menus
 '("�滮"
   ("ָ����Χ"(at-planning:set-range))
   ("�õ����"(at-planning:land-area))
   ("�̵����" (at-planning:area-of-green))
   ("������" (at-planning:hatch-zone))
   ("��λ�ۼ�" (at-planning:reduction-green))
   ("���̵���" (at-planning:greening-rate))
   ("��������" (at-planning:input))
   ("����ָ��" (at-planning:make-index))
   ))
(defun at-planning:set-range ()
  (@::help "ָ��Ҫ����ľ��η�Χ")
  (@::prompt "��ָ�����㷶Χ")
  (setq at-planning:range-pt1
	(getpoint "���½ǵ�:"))
  (setq at-planning:range-pt2
	(getcorner at-planning:range-pt1 "���Ͻǵ�:"))

  (if (or (null at-planning:range-pt1)
	  (null at-planning:range-pt2))
      (at-planning:set-range)
      )
  )
(defun at-planning:zoom ()
  (if (or (null at-planning:range-pt1)
	  (null at-planning:range-pt2))
      (at-planning:set-range)
      )
  (vla-zoomwindow *ACAD* (point:to-ax at-planning:range-pt1)
		  (point:to-ax  at-planning:range-pt2)))
(defun at-planning:sum (lst)
  (cond
    ((atom lst)
     lst)
    ((listp lst)
     (apply '+ lst))))

(defun at-planning:calc (/ sum-area land-curve green-cruve)
  "�������ָ��"
  (at-planning:zoom)
  ;;�ؿ����
  (setq land-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
			  (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:land-layer))
				))))
  (setq @planning:*land-area* (mapcar '(lambda(x)(vla-get-area (e2o x))) land-curve))
  (if (> (at-planning:sum @planning:*land-area*) 0)
      (progn
	;; �̵����
	(setq green-curve (pickset:to-list
			   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
				  (list (cons 0 "lwpolyline")
					(cons 8 (@:get-config '@planning:green-layer))
					))))
	(setq @planning:*greening-area* (mapcar '(lambda(x)(vla-get-area (e2o x))) green-curve))
	(setq @planning:*green-all-area* (at-planning:sum @planning:*greening-area*))
	
	;; ����λ�ۼ�
	(setq lst-car (pickset:to-list
                       (ssget"w" at-planning:range-pt1 at-planning:range-pt2
			     (list (cons 0 "insert")
				   (cons 2 "����λ")
				   ))))
	(setq @planning:*green-reduction-area* (* -1 0.6 2.4 5.3 (length lst-car)))
	;; �̵���
	(setq @planning:*greening-rate*
	      (/ (+ @planning:*green-all-area*
		    @planning:*green-reduction-area*
		    )
		 (at-planning:sum @planning:*land-area*))
	      )
	;; �����������
	(setq building-curve (pickset:to-list
			 (ssget"w" at-planning:range-pt1 at-planning:range-pt2
			       (list (cons 0 "lwpolyline")
				     (cons 8 (@:get-config '@planning:building-layer))
				     ))))
	(setq @planning:*building-floor1-area* (mapcar '(lambda(x)(vla-get-area (e2o x))) building-curve))
	;; ��λ
	(setq @planning:*parking-overground*
	      (length
	       (pickset:to-list
                (ssget"w" at-planning:range-pt1 at-planning:range-pt2
		      (list (cons 0 "insert")
			    (cons 2 (@:get-config '@planning:parking))
			    )))))
	)))
(defun at-planning:land-area (/ sum-area)
  (@::help "���ɸ��õصؿ����������������")
  (@:prompt "��ѡ���õغ��߱պ�����:")
  (at-planning:zoom)
  (setq lst-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
			  (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:land-layer))
				))))
  (setq sum-area 0)
  (foreach curve lst-curve
	   (setq sum-area (+ sum-area (vla-get-area (e2o curve))))
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@::draw-scale))
             0
             0.72
             0
             "mm")
            8
            (@:get-config '@planning:land-layer)
	    ))
  ;; (setq @planning:*land-area* sum-area)
  (setq @m:*result* sum-area)
  (@m:draw)
  (princ))
(defun at-planning:area-of-green (/ sum-area)
  (@::help "���ɸ��̵صؿ����������������")
  (at-planning:zoom)
  (if (setq ss-tmp (ssget  "w" at-planning:range-pt1 at-planning:range-pt2
		  (list (cons 0 "text")
				  (cons 8 (@:get-config '@planning:green-layer))
				  )))
      (pickset:erase ss-tmp))
  (@:prompt "��ѡ���̵رպ�����:")
  (setq lst-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
		    (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:green-layer))
				))))
  (setq sum-area 0)
  (foreach curve lst-curve
	   (setq sum-area (+ sum-area (vla-get-area (e2o curve))))
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@::draw-scale))
             0
             0.72
             0
             "mm")
            8
            (@:get-config '@planning:green-layer)
	    ))
  ;; (setq @planning:*green-all-area* sum-area)
  (setq @m:*result* sum-area)
  (@m:draw)
  (princ))
(defun at-planning:hatch-zone ()
  (@::help "������佨����̵ء��㳡�������ռ�ؿ�")
  ;; (@:prompt "��ѡ���̵رպ�����:")
  (at-planning:zoom)
  (foreach hatchlayer
	   (list
	    (@:get-config '@planning:green-layer)
	    (@:get-config '@planning:building-layer)
	    (@:get-config '@planning:openspace-layer)
	    (@:get-config '@planning:square-layer))
	   (if hatchlayer
	       (progn
		 (if (setq toerase (ssget "w" at-planning:range-pt1 at-planning:range-pt2
					  (list (cons 0 "hatch")
						(cons 8 hatchlayer)
						)))
		     (pickset:erase toerase))
		 (setq lst-curve (pickset:to-list
				  (ssget
				   "w" at-planning:range-pt1 at-planning:range-pt2
				   (list (cons 0 "lwpolyline")
					 (cons 8 hatchlayer)
					 ))))
		 (setvar "hpname" "solid")
		 (setvar "hpcolor" ".")
		 (setvar "hplayer" (car (string:to-list hatchlayer",")))
		 (foreach curve lst-curve
			  (command "-hatch" "s" curve "" "")
			  ))))
  (if (setq sshatch (ssget  "w" at-planning:range-pt1 at-planning:range-pt2 '((0 . "hatch"))))
      (command  "_.draworder" sshatch ""  "B")
      )
  (princ))
(defun at-planning:reduction-green ()
  (@::help '("��������̵�����λ�ۼ������"
	     "��λ�ߴ� 2.4x5.3m2����60%�ۼ���"))
  (@:prompt "��ѡ������λ:")
  (setq lst-car (pickset:to-list
                 (ssget "w" at-planning:range-pt1 at-planning:range-pt2
			(list (cons 0 "insert")
			      (cons 2 "����λ")
			      ))))
  ;; (setq @planning:*green-reduction-area*  (* -1 0.6 2.4 5.3 (length lst-car)))
  (@:prompt (strcat "��������λ " (itoa (length lst-car))" ��"))
  (setq @m:*result*  (* -1 0.6 2.4 5.3 (length lst-car)))
  (@m:draw)
  (princ))

(defun at-planning:greening-rate ()
  (@::help "�����̵���")
  (if (and (numberp @planning:*land-area*)
	   (numberp @planning:*green-all-area*)
	   (numberp @planning:*green-reduction-area*))
      (progn
	(setq @m:*result*
	      (/ (+ @planning:*green-all-area*
		    @planning:*green-reduction-area*
		    )
		 @planning:*land-area*))
	(@m:draw))
      (@::prompt "���Ƚ��еؿ鼰�̵ؼ��㡣"))
  (princ))
(defun at-planning:area-of-building (/ sum-area)
  (@::help "���ɸ��������׵���������������")
  (at-planning:zoom)
  (pickset:erase (ssget "w" at-planning:range-pt1 at-planning:range-pt2
		  (list (cons 0 "text")
				  (cons 8 (@:get-config '@planning:building-layer))
				  )))
  (@:prompt "��ѡ�������ױպ�����:")
  (setq lst-curve (pickset:to-list
                   (ssget "w" at-planning:range-pt1 at-planning:range-pt2
		    (list (cons 0 "lwpolyline")
				(cons 8 (@:get-config '@planning:building-layer))
				))))
  (setq sum-area 0)
  (foreach curve lst-curve
	   (setq sum-area (+ sum-area (vla-get-area (e2o curve))))
           (entity:putdxf
            (entity:make-text
             (rtos (vla-get-area (e2o curve)) 2 3)
             (point:2d->3d (point:centroid (curve:get-points curve)))
             (* 2.5 (@:get-config '@::draw-scale))
             0
             0.72
             0
             "mm")
            8
            (@:get-config '@planning:building-layer)
	    ))
  ;;(setq @planning:*building-floor1-area* sum-area)
  (setq @m:*result* sum-area)
  (@m:draw)
  (princ))
(defun at-planning:make-index ()
  (if (or (or (null  @planning:*building-all-area*)
	      (zerop @planning:*building-all-area*))
	  (or (null  @planning:*building-overground-area*)
	      (zerop @planning:*building-overground-area*))
	  (null  @planning:*building-underground-area*)
	  (or (null @planning:*plot-area*)
	      (zerop @planning:*plot-area*))
	  (null @planning:*parking-underground*))
      (at-planning:input))
  (if (at-planning:calc)
      (ui:dyndraw (table:make
		   '(0 0 0)
		   "��Ҫ��������ָ��һ����"
		   '("��Ŀ����""��λ""����""��ע")
		   (append
		    (list
		     (list "�ܽ����õ�" "�O" (at-planning:sum  @planning:*land-area*) ""))
		    (if (> (length @planning:*land-area*)1)
			(progn
			  (setq i 0)
			  (mapcar '(lambda(x)
				    (list (strcat "����:  �ؿ�"(itoa (setq i (1+ i))))
				     "�O"
				     x
				     ""))
				  @planning:*land-area*)))
		    (list
		     (list "�ܽ������" "�O" @planning:*building-all-area* "")
		     (list "���У����Ͻ������" "�O" @planning:*building-overground-area* "")
		     (list "���У����½������" "�O" @planning:*building-underground-area* "")
		     (list "�������" "�O" @planning:*plot-area* "")
		     (list "�����������" "�O" (at-planning:sum @planning:*building-floor1-area*) "")
		     (list "�̵����" "�O" (+ @planning:*green-all-area*
					      @planning:*green-reduction-area*)
			   "")
		     (list "�����ܶ�" "%" (strcat (@:to-string(* 100 (/ (at-planning:sum @planning:*building-floor1-area*) (at-planning:sum @planning:*land-area*))))"%") "��35%")
		     (list "�ݻ���" "��O/ha"
			   (/ @planning:*plot-area*
			      (at-planning:sum @planning:*land-area*)
			      )
			   "��2.0")
		     (list "�̵���" "%" (strcat (@:to-string (* 100 @planning:*greening-rate*))"%") "��20%")
		     (list "�ǻ�����ͣ��λ" "�O" "" "")
		     (list "�ܳ�λ" "��" (itoa (+ @planning:*parking-overground*
						  @planning:*parking-underground*))
			   "")
		     (list "���У�����" "��" (itoa @planning:*parking-overground*) "")
		     (list "���У�����" "��" (itoa @planning:*parking-underground*) "")
		     )))
		  '(0 0 0))
      
      ))
(defun at-planning:input (/  res)
  (@::help "��Ҫ�ֶ�������ͼ���޷����������")
  (setq res 
	(ui:input
	 "��������"
	 (list (list "���������" (if @planning:*building-all-area* @planning:*building-all-area* 0.0))
	       (list "���Ͻ������" (if @planning:*building-overground-area*  @planning:*building-overground-area* 0.0))
	       (list "���½������" (if @planning:*building-underground-area*   @planning:*building-underground-area* 0.0))
	       (list  "�������" (if @planning:*plot-area* @planning:*plot-area*  0.0))
	       (list "���³�λ��" (if @planning:*parking-underground* @planning:*parking-underground* 0)))))
  (setq @planning:*building-all-area* (cdr (assoc "���������" res)))
  (setq @planning:*building-overground-area* (cdr (assoc "���Ͻ������" res)))
  (setq @planning:*building-underground-area* (cdr (assoc "���½������" res)))
  (setq @planning:*plot-area* (cdr (assoc "�������" res)))
  (setq @planning:*parking-underground* (cdr (assoc "���³�λ��" res)))
  )
  
