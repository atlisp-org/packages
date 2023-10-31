(defun @text:handle-overlay-text ()
  ;; 避让原则：竖向避水平，水平向右错。
  (@:help "文字避让，处理重叠的单行文本")
  (setq txts (pickset:to-list (ssget  '((0 . "text")))))
  ;;去除不重叠的文字
  (setq txts (vl-remove-if-not
	      '(lambda(x / box)
		(setq box (entity:getbox x 0))
		(ssget "c" (car box)(cadr box)'((0 . "text"))))
	      txts))
  (setq txts (pickset:sort txts "xy" (* 0.01(entity:getdxf (car txts) 40))))
  (foreach
   txt  txts
   (setq box (entity:getbox txt 0))
   (setq ots (vl-remove txt (pickset:to-list (ssget "c" (car box)(cadr box)'((0 . "text"))))))
   (foreach
    ot ots
    ;; 比较 ot 与 txt的位置关系，确定避让规则
    (setq box1 (entity:getbox txt 0)
	  box2(entity:getbox ot 0))
    (setq r1 (entity:getdxf  txt 50)
	  r2 (entity:getdxf ot 50))
    (setq tbox1 (textbox (entget txt))
	  tbox2 (textbox  (entget ot)))
    (setq w1  (- (car (cadr tbox1))(car (car tbox1)))
	  w2  (- (car (cadr tbox2))(car (car tbox2)))
	  h1  (- (cadr (cadr tbox1))(cadr (car tbox1)))
	  h2  (- (cadr (cadr tbox2))(cadr (car tbox2))))
    (setq o1 (point:centroid box1)
	  o2 (point:centroid box2))
    (cond
      ;; 两者平行且共线
      ((and (equal r1 r2 0.5)
	    (or (equal r1 (angle o1 o2) 0.2)
		(equal r1 (angle o2 o1) 0.2)))
       (setq dis (+ (* 0.2 h1) (* 0.5 (- (* 0.5 (+ w1 w2)) (abs (- (car o2)(car o1)))))))
       (vla-move (e2o txt)  (point:to-ax o1)(point:to-ax (polar o1 (+ r1 (if (> (cos (angle o1 o2)) 0) pi 0)) dis)))
       (entity:putdxf txt 62 2)
       (vla-move (e2o ot)  (point:to-ax o2)(point:to-ax (polar o2 (+ r2 (if (> (cos (angle o1 o2)) 0) 0 pi))  dis)))
       (entity:putdxf ot 62 3)
       )
      ;; 两者平行不共线
      ((and (equal r1 r2 0.5)
	    (null (and (equal r1 (angle o1 o2) 0.2)
		       (equal r1 (angle o2 o1) 0.2))))
       (setq dis (+ (* 0.2 h1) (* 0.5 (- (* 0.5 (+ h1 h2)) (abs (- (cadr o2)(cadr o1)))))))
       (vla-move (e2o txt)  (point:to-ax o1) (point:to-ax (polar o1 (+ r1 (* 0.5 pi) (if (> (sin (angle o1 o2)) 0) pi 0)) dis)))
       (entity:putdxf txt 62 2)
       (vla-move (e2o ot)  (point:to-ax o2)(point:to-ax (polar o2 (+ r2 (* 0.5 pi) (if (> (sin (angle o1 o2)) 0) 0 pi)) dis)))
       (entity:putdxf ot 62 3)
       )
      ;; 两者垂直；动Y向
      ((null (equal r1 r2 0.5))
       (if (< r1 r2)
	   (progn
	     (setq dis (-(+ (* 0.7 h1) (* 0.5 w2)) (abs (- (cadr o2)(cadr o1)))))
	     (vla-move (e2o ot)  (point:to-ax o2)(point:to-ax (polar o2 (+ r2 (if (> (sin (angle o1 o2)) 0) 0 pi)) dis)))
	     (entity:putdxf ot 62 3)
	     )
       ))
      ))))
