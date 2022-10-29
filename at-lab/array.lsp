(@:define-config '@lab:disx 300 "X向间距，梅花布置时应为列距的2倍")
(@:define-config '@lab:disy 300 "Y向间距，梅花布置时应为列距的2倍")
(@:define-config '@lab:r  "10,50" "半径。以逗号分为不同的圆")
(defun array-circle (pt-begin pt-end disx disy r)
  "pt-begin 左下起点; pt-end 右上终点;disx,disy X Y向的间距,r半径(可以是列表)"
(entity:make-circle 
 (append  ;; 梅花
  (apply
   'append
   (mapcar
    '(lambda(x)
       (mapcar
	'(lambda(y)
	   (list x y))
	(list:range (cadr pt-begin)(cadr pt-end) disy)));;Y向的起点，终点，距离
    (list:range (car pt-begin)(car pt-end) disx)));;X向的起点，终点，距离
  (apply
   'append
   (mapcar
    '(lambda(x)
       (mapcar
	'(lambda(y)
	   (list x y))
	(list:range (+ (* 0.5 disy)(cadr pt-begin))(cadr pt-end) disy)));;Y向的起点，终点，距离
    (list:range  (+ (* 0.5 disx)(car pt-begin))(car pt-end) disx))));;X向的起点，终点，距离
 r) ;; 半径表
)
(defun @lab:array-circle (/ pt-start pt-end disx disy r res)
  (setq res
	(ui:input
	 "请输入圆阵参数"
	 (list
	  (list "disx" (@:get-config '@lab:disx) "X向间距，梅花布置时应为列距的2倍")
	  (list "disy"  (@:get-config '@lab:disy) "Y向间距，梅花布置时应为行距的2倍")
	  (list "r"  (@:get-config '@lab:r)  "半径。以逗号分为不同的圆"))))
  (@:set-config  '@lab:disx (cdr (assoc "disx" res)))
  (@:set-config  '@lab:disy (cdr (assoc "disy" res)))
  (@:set-config  '@lab:r (cdr (assoc "r" res)))
  (setq pt-start (getpoint (@:speak "请输入左下角:")))
  (setq pt-end (getcorner pt-start (@:speak "请输入右上角:")))
  (array-circle pt-start pt-end
		(cdr (assoc "disx" res))
		(cdr (assoc "disy" res))
		(mapcar
		 'atof
		 (string:to-list(cdr (assoc "r" res)) ","))))
