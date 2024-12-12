(defun @text:string-to-table ( / dcl-fp strs)
  (@::prompt "�����������ı������ɷָ������Ʊ��")
  (defun align-str (n str / flag)
    (if (null str)(setq str ""))
    (setq flag nil)
    (while (< (string:bytelength str) n)
      (if flag
	  (setq str (strcat " " str))
	(setq str (strcat str " ")))
      (setq flag (not flag)))
    str)
  (setq ss
	(pickset:sort
	 (pickset:to-list (ssget '((0 . "text"))))
	 "Yx" 0.01
	 ))
  (setq lst-str (mapcar '(lambda(x)(entity:getdxf x 1)) ss))
  (setq strs lst-str)
  ;;(mapcar '(lambda(x)(string:parse-by-lst x '(":" " " "("")"))) lst-str)
  (dcl:dialog "example")
  
  (progn
    (dcl:begin-cluster "column" "")
    (write-line (strcat ":text{label=\""
			"˵��:��ͬ�ķָ���֮���� &&& �Ͽ���ɾ���ַ�����ɾ���ִ���ǰ����Ҫ���ַ���"
			"\";}"
			)
		dcl-fp)
    (write-line (strcat ":edit_box{key=\"sepa\";label=\"�ָ���:\";}"
			)
		dcl-fp)
    (write-line (strcat ":edit_box{key=\"trim\";label=\"ɾ���ַ�:\";}"
			)
		dcl-fp)
    (dcl:hr 0.08)
    (setq i 0)
    (setq width (+ 30 (apply 'max (mapcar 'string:bytelength lst-str))))
    (foreach str lst-str
	     (write-line (strcat ":text{key=\"str" (itoa (setq i (1+ i)))"\";label=\""
				 str "\";width="(itoa width)";}")
			 dcl-fp))
    (dcl:hr 0.08)
    (dcl:end-cluster)
    )
  (dcl:dialog-end-ok-cancel)
  (dcl:new "example")
  ;; MVCNIS
  (defun parse-str (str)
    (vl-remove ""
	       (mapcar '(lambda(x)(vl-string-trim (get_tile "trim") x))
		       (string:parse-by-lst str (string:to-list (get_tile "sepa") "&&")))
	       ))
  (defun update-strs (/ lens j)
    (setq strs (mapcar 'parse-str lst-str))
    (setq lens nil)(setq j 0)
    (repeat (apply 'max (mapcar 'length strs))
	    (setq lens
		  (cons 
		   (apply 'max (mapcar '(lambda(x) (if (nth j x)(string:bytelength (nth j x)) 0)) strs))
		   lens))
	    (setq j (1+ j)))
    (setq lens (reverse lens))
    (mapcar '(lambda(x)(mapcar '(lambda(y z)(align-str y z)) lens x))  strs)
    (setq i 0)
    (foreach str strs
	     (set_tile (strcat "str" (itoa (setq i (1+ i))))
		       (string:from-lst (mapcar '(lambda(y z)(align-str y z)) lens str) " | "))))
  
  (action_tile "sepa" "(update-strs)")
  (action_tile "trim" "(update-strs)")
  (set_tile "title" "�ı�ת���")
  (dcl:show)
  (setq @:tmp-result strs)
  
  (table:make
   (getpoint "�������:")
   "�ָ����"
   (mapcar 'chr
	   (list:range (ascii "A")
		       (+ (ascii "A")
			  (apply 'max (mapcar 'length strs))
			  -1)
		       1))
   strs)
  )
