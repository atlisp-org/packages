(@:define-config '@dim:dimarc-gap 1000 "弧长标注到弧线的距离")
(defun at-dim:dimarc (pt-cen pt1 pt2 pt-arc)
  "标注弧长"
  (vla-adddimarc  *ms*
	       (point:to-ax  pt-cen)
	       (point:to-ax  pt1)
	       (point:to-ax  pt2)
	       (point:to-ax pt-arc)))
(defun at-dim:menu-dimarc-div ()
  (@:help "等分标注圆弧")
  (setq arc (car(pickset:to-list (ssget ":S:E" '((0 . "ARC"))))))
  (setq n (getint (@:speak "请输入要等分的数量:")))
  (setq ang-s (entity:getdxf arc 50))
  (setq arc-r (entity:getdxf arc 40))
  (setq pt-c (entity:getdxf arc 10))
  (setq pt-s (polar
	      pt-c
	      (entity:getdxf arc 50)
	      (entity:getdxf arc 40)))
  (setq pt-e (polar
	      pt-c
	      (entity:getdxf arc 51)
	      (entity:getdxf arc 40)))
  ;;dimarc
  (setq ang-all
	(- (entity:getdxf arc 51)
	   (entity:getdxf arc 50)))
  (if (< ang-all 0)
      (setq ang-all (+ pi ang-all)))
  (setq ang-per (/ ang-all n))
  (setq i 0)
  (repeat n
	  (at-dim:dimarc
	   pt-c
	   pt-s
	   (setq pt-step
		 (polar pt-c
			(m:fix-angle
			 (setq ang-s (+ ang-per ang-s)))
			arc-r))
	   (polar pt-c
		  (m:fix-angle
		   (- ang-s (* 0.5 ang-per)))
		  (+ arc-r
		     (@:get-config '@dim:dimarc-gap))))
	  (setq pt-s pt-step)))
	      
