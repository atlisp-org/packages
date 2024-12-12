(defun @text:locate-overlay-text(/ txts box)
  (@::prompt "高亮显示有重叠的文字。")
  (if(null (setq txts (pickset:to-list (ssget '((0 . "text"))))))
     (setq txts (pickset:to-list (ssget "x" '((0 . "text"))))))
  ;; 粗过滤
  (setq txts (vl-remove-if
	      '(lambda(x / box)
		(setq box (entity:getbox x 0))
		(= 1 (sslength (ssget "c" (car box)(cadr box) '((0 . "text"))))))
	      txts))
  (setq txts (vl-remove-if
	      '(lambda(x )
		(= 1 (sslength (ssget "cp" (text:box x) '((0 . "text"))))))
	      txts))
  (if txts
      (progn
	(setq txts (pickset:from-list txts))
	(setq corner (pickset:getbox txts 10))
	(command "zoom" "w" (car corner) (cadr corner))
	;;(sssetfirst nil (ssadd (ssname txts 0)))
	(sssetfirst nil txts))
      (@:prompt "没有发现重叠文字")
      ))
(defun @text:delete-overlay-sametext(/ txts box sames same1)
  (@::prompt "删除内容相同且重叠的文字。")
  (if(null (setq txts (pickset:to-list (ssget '((0 . "*text"))))))
     (setq txts (pickset:to-list (ssget "x" '((0 . "*text"))))))
  ;; 粗过滤
  ;; (setq txts (vl-remove-if
  ;; 	      '(lambda(x / box)
  ;; 		(setq box (entity:getbox x 0))
  ;; 		(= 1 (sslength (ssget "c" (car box)(cadr box) '((0 . "*text"))))))
  ;; 	      txts))
  ;; (setq txts (vl-remove-if
  ;; 	      '(lambda(x )
  ;; 		(= 1 (sslength (ssget "cp" (text:box x) '((0 . "*text"))))))
  ;; 	      txts))
  (setq sames nil)
  (while (and (setq txt% (car txts))
	      (setq sames1
		    (pickset:to-list
		     (ssget "cp"
			    (text:box txt%)
			    (list (cons 0 "*TEXT")
				  (cons 1
					(entity:getdxf txt% 1)))))))
    (if (and sames1 (> (length sames1) 1))
	(setq sames (append sames (vl-remove txt% sames1)))
	)
    (setq txts (vl-remove-if '(lambda(x)(member x sames1)) txts))
    )
  (if (and sames)
      (mapcar 'vla-delete (mapcar 'e2o sames))
      (@:prompt "没有发现相同的重叠文字。")
      ))

(defun @text:handle-overlay-text (/ txts box ots box1 box2 tbox1 tbox2 w1 w2 h1 h2 o1 o2 dis)
  ;; 避让原则：竖向避水平，水平向右错。
  (@::prompt "文字避让，处理重叠的单行文本,避让成功设为绿色，不成功设为黄色(颜色索引号为50)。")
  (setq txts (pickset:to-list (ssget  '((0 . "text")))))
  ;;去除不重叠的文字
  ;; 粗过滤
  (setq txts (vl-remove-if
	      '(lambda(x / box)
		(setq box (entity:getbox x 0))
		(= 1 (sslength (ssget "c" (car box)(cadr box) '((0 . "text"))))))
	      txts))
  (setq txts (vl-remove-if
	      '(lambda(x)
		(= 1 (sslength
		      (ssget "cp" (text:box x) '((0 . "text"))))))
	      txts))
  (if (entity:getdxf (car txts) 40)
      (setq txts (pickset:sort txts "xy" (* 0.01 (entity:getdxf (car txts) 40)))))
  (foreach
   txt  txts
   (setq ots (vl-remove txt (pickset:to-list (ssget "cp" (text:box txt) '((0 . "text"))))))
   (foreach
    ot ots
    ;; 比较 ot 与 txt的位置关系，确定避让规则
    (if  (and
	  (setq box1 (text:box txt))
	  (setq box2 (text:box ot))
	  (setq r1 (entity:getdxf  txt 50))
	  (setq r2 (entity:getdxf ot 50))
	  (setq w1  (distance (car box1)(cadr box1)))
	  (setq w2  (distance (car box2)(cadr box2)))
	  (setq h1  (distance (car box1)(last box1)))
	  (setq h2  (distance (car box2)(last box2)))
	  (setq o1 (point:centroid box1))
	  (setq o2 (point:centroid box2)))
    (cond
      ;; 两者平行且共线
      ((and (equal r1 r2 0.5)
	    (or (equal r1 (angle o1 o2) 0.2)
		(equal r1 (angle o2 o1) 0.2)))
       (setq dis (+ (* 0.2 h1) (* 0.5 (- (* 0.5 (+ w1 w2)) (abs (- (car o2)(car o1)))))))
       (vla-move (e2o txt)  (point:to-ax o1)(point:to-ax (polar o1 (+ r1 (if (> (cos (angle o1 o2)) 0) pi 0)) dis)))
       (vla-move (e2o ot)  (point:to-ax o2)(point:to-ax (polar o2 (+ r2 (if (> (cos (angle o1 o2)) 0) 0 pi))  dis)))
       (entupd txt)(entupd ot)
       (if (<= (pickset:length (ssget "cp" (text:box txt) '((0 . "text")))) 1)
	   (entity:putdxf txt 62 3)
	   (entity:putdxf txt 62 50)
	   )
       )
      ;; 两者平行不共线
      ((and (equal r1 r2 0.5)
	    (null (and (equal r1 (angle o1 o2) 0.2)
		       (equal r1 (angle o2 o1) 0.2))))
       (setq dis (+ (* 0.2 h1) (* 0.5 (- (* 0.5 (+ h1 h2)) (abs (- (cadr o2)(cadr o1)))))))
       (vla-move (e2o txt)  (point:to-ax o1) (point:to-ax (polar o1 (+ r1 (* 0.5 pi) (if (> (sin (angle o1 o2)) 0) pi 0)) dis)))
       (vla-move (e2o ot)  (point:to-ax o2)(point:to-ax (polar o2 (+ r2 (* 0.5 pi) (if (> (sin (angle o1 o2)) 0) 0 pi)) dis)))
       (entupd txt)(entupd ot)
       (if (<= (pickset:length (ssget "cp" (text:box txt) '((0 . "text")))) 1)
	   (entity:putdxf txt 62 3)
	   (entity:putdxf txt 62 50)
	   )
       )
      ;; 两者垂直；动Y向
      ((null (equal r1 r2 0.5))
       (if (< (abs (sin r1))(abs(sin r2)))
	   (progn
	     (setq dis (-(+ (* 0.7 h1) (* 0.5 w2)) (abs (- (cadr o2)(cadr o1)))))
	     (vla-move (e2o ot) (point:to-ax o2)
		       (point:to-ax
			(polar o2 (+ r2
				     (if (>
					  (sin (- (angle o1 o2) r1))
					  0)
					 0 pi))
			       dis)))
	     (entupd ot)
	     )
       ))
      ))
    ;; 成功设绿,不成功设黄
    (if (and (setq i (pickset:length (ssget "cp" (text:box ot) '((0 . "text")))))
	     (<= i 1))
	(entity:putdxf ot 62 3)
	(entity:putdxf ot 62 50)
	)
    )))
(defun @text:locate-overline-text(/ txts box)
  (@::prompt "高亮显示压线的单行文本。")
  (if(null (setq txts (pickset:to-list (ssget '((0 . "text"))))))
     (setq txts (pickset:to-list (ssget "x" '((0 . "text"))))))
  ;; 粗过滤
  (setq txts (vl-remove-if
	      '(lambda(x / box)
		(setq box (entity:getbox x 0))
		(null (ssget "c" (car box)(cadr box) '((0 . "*line,insert")))))
	      txts))
  (setq txts (vl-remove-if
	      '(lambda(x )
		(null (ssget "cp" (text:box x) '((0 . "*line,insert")))))
	      txts))
  (if txts
      (progn
	(setq txts (pickset:from-list txts))
	(setq corner (pickset:getbox txts 10))
	(command "zoom" "w" (car corner) (cadr corner))
	;;(sssetfirst nil (ssadd (ssname txts 0)))
	(sssetfirst nil txts))
      (@:prompt "没有发现压线的文字")
  ))
