;; 生成构件图
(defun prefabricated-building:getwalls ()
  "选择墙构件"
  (pickset:to-list
   (ssget '((0 . "insert")
	    (2 . "QN##0,QW##0")
	    ))))
(defun prefabricated-building:beamflag (wall / ptb rot pl pr flag-lw flag-rw w)
  ;; 设置梁标志
  (setq w (cond
	    ((wcmatch (entity:getdxf wall 2) "Q@600")
	     295)
	    ((wcmatch (entity:getdxf wall 2) "Q@750")
	     370)))
  (setq ptb (entity:getdxf wall 10))
  (setq rot (entity:getdxf wall 50))
  (setq pr (polar ptb rot w))
  (setq pl (polar ptb (+ pi rot) w))
  (setq flag-lw
	(ssget "F"
	       (list
		(polar pl
		       (- rot (* 0.5 pi))
		       80)
		(polar pl
		       (+ rot (* 0.5 pi))
		       80))
	       '((0 . "*POLYLINE")
		 (43 . 160))))
  (setq flag-rw
	(ssget "F"
	       (list
		(polar pr
		       (- rot (* 0.5 pi))
		       80)
		(polar pr
		       (+ rot (* 0.5 pi))
		       80))
	       '((0 . "*POLYLINE")
		 (43 . 160))))
  (cond
    ((and flag-lw flag-rw)
     (block:set-attributes wall
			   '(("BEAM" . "D"))))
    ((and flag-lw (null flag-rw))
     (block:set-attributes wall
			   '(("BEAM" . "L"))))
    ((and (null flag-lw) flag-rw)
     (block:set-attributes wall
			   '(("BEAM" . "R"))))
    ((and (null flag-lw) (null flag-rw))
     (block:set-attributes wall
			   '(("BEAM" . "")))))
  )

(defun prefabricated-building:gen-beamflag ()
  (mapcar 'prefabricated-building:beamflag (prefabricated-building:getwalls))
  )
(defun prefabricated-building:gen-order  ()
  "墙构件编号"
  (setq walls (prefabricated-building:getwalls))
  ;;排序
  (setq walls (pickset:sort walls
			    "yx" 80))
  (setq i 0)
  (foreach
   wall walls
   (block:set-attributes wall
			 (list (cons 
				"ORDER"
				(itoa (setq i (1+ i))))))))

;; 电气留盒分析
(defun prefabricated-building:gen-elecdata (wall)
  "返回墙构件上的电气列表"
  (setq w (cond
	    ((wcmatch (entity:getdxf wall 2) "Q@600")
	     300)
	    ((wcmatch (entity:getdxf wall 2) "Q@750")
	     375)))
  (setq ptb (entity:getdxf wall 10))
  (setq rot (entity:getdxf wall 50))
  (setq pl (polar ptb (+ pi rot) w))
  (setq pr (polar ptb rot w))
  (setq a-eqs (pickset:to-list
	     (ssget "CP"
		    (list 
		     (polar pr (- rot (* 0.5 pi)) 80)
		     (polar pl (- rot (* 0.5 pi)) 80)
		     (polar pl (- rot (* 0.5 pi)) 180)
		     (polar pr (- rot (* 0.5 pi)) 180))
		    '((2 . "$equip$*")))))
  (setq b-eqs (pickset:to-list
	     (ssget "CP"
		    (list 
		     (polar pr (+ rot (* 0.5 pi)) 80)
		     (polar pl (+ rot (* 0.5 pi)) 80)
		     (polar pl (+ rot (* 0.5 pi)) 180)
		     (polar pr (+ rot (* 0.5 pi)) 180))
		    '((2 . "$equip$*")))))
  (list a-eqs b-eqs)
  )

(defun prefabricated-building:elec-h (elecbox / d)
  "求线盒安装高度"
  (setq d 120) ;;地面做法厚度
  (setq a (cdr (assoc "A"(block:get-attributes elecbox))))
  (cond
    ((wcmatch a "*开关") (+ d  1300))
    ((wcmatch a "*空调*")
     (cond
       ((="G" (cdr (assoc "$TEXT$"(block:get-attributes elecbox)))) (+ d 300))
       ((="C" (cdr (assoc "$TEXT$"(block:get-attributes elecbox)))) (+ d 1250))
       ((="Y" (cdr (assoc "$TEXT$"(block:get-attributes elecbox)))) (+ d 2200))
       ((="Z" (cdr (assoc "$TEXT$"(block:get-attributes elecbox)))) (+ d 600))
       (t (+ d 1800))))
    ((wcmatch a "*窗帘*") (- 3080 44))
    ((wcmatch a "*保护*") (+ d 1500))
    ((and (not (wcmatch a "*空调*,*窗帘*"))
	  (wcmatch a "*插座"))
     (if (wcmatch a "D*") (+ d 300)
	 (+ d 700)))
    ((wcmatch a "*交接线,*配电箱") (+ d 1500))
    (t (+ d 700))
    ))
(defun prefabricated-building:m-gen-elecdata ()
  (setq res
	(prefabricated-building:gen-elecdata(car(entsel)))))

(defun prefabricated-building:draw-elec (wall pt-base AorB)
  "在墙立面图上绘制电盒，AorB: t为 A"
  (setq elecs (prefabricated-building:gen-elecdata wall))
  (setq w (cond
	    ((wcmatch (entity:getdxf wall 2) "Q@600")
	     300)
	    ((wcmatch (entity:getdxf wall 2) "Q@750")
	     375)))
  (setq pt-wall (entity:getdxf wall 10))
  (setq pt-wall-l (polar pt-wall
			 (+ pi (entity:getdxf wall 50))
			 w))
  (setq pt-wall-r (polar pt-wall
			 (entity:getdxf wall 50)
			 w))
  (if (setq elec (if aorb (car elecs)(cadr elecs)))
      (foreach
       elec% elec
       (setq pt-i
	     (inters pt-wall-l
		     pt-wall-r
		     (entity:getdxf elec% 10)
		     (polar (entity:getdxf elec% 10)
			    (+ (* 0.5 pi)(entity:getdxf wall 50))
			    100)
		     nil))
       ;; 到墙构件的水平距离
       (setq dis-w (distance pt-wall-l pt-i))
       (if (< dis-w 44)(setq dis-w 44))
       (if (> dis-w (- (* 2 w) 44)) (setq dis-w (- (* 2 w) 44)))
       ;; 竖直距
       (setq dis-h (prefabricated-building:elec-h elec%))
       (setq pt-center	(polar (polar pt-base 0 (if aorb dis-w
				    (- (* 2 w) dis-w)
				    ))
			       (* 0.5 pi) dis-h))
       ;;安装盒
       (cond
	 ((wcmatch (cdr (assoc "A" (block:get-attributes elec%)))
		   "*配电箱")
	  (entity:make-rectangle
	   (polar pt-center pi 200)
	   (polar (polar pt-center 0 200) (* 0.5 pi) 500)
	   ))
	 ((wcmatch (cdr (assoc "A" (block:get-attributes elec%)))
		   "*交接线")
	  (entity:make-rectangle
	   (polar pt-center pi 175)
	   (polar (polar pt-center 0 175) (* 0.5 pi) 300)
	   ))
	 (t
	  (entity:make-rectangle
	   (polar pt-center (* 1.25 pi) (* 1.414 44))
	   (polar pt-center (* 0.25 pi) (* 1.414 44))
	   )))
       ;;  TODO 电箱
       
       ;;接线盒
       (setq pt-center2	(polar (polar pt-base 0 (if aorb dis-w
						    (- (* 2 w) dis-w)
						    ))
			       (* 0.5 pi)
			       (if (< dis-h 450)
				   44
				   (- 3080 44))))
       (entity:make-rectangle
	(polar pt-center2 (* 1.25 pi) (* 1.414 44))
	(polar pt-center2 (* 0.25 pi) (* 1.414 44))
	)
       (entity:make-lwpolyline
	(list pt-center pt-center2)
	nil 30 0 0)
       ))
  )

(defun prefabricated-building:draw-wall (wall pt-base)
  (setq w (cond
	    ((wcmatch (entity:getdxf wall 2) "Q@600")
	     300)
	    ((wcmatch (entity:getdxf wall 2) "Q@750")
	     375)))
  ;; (entity:make-text
  ;;  (strcat "构件号: "
  ;; 	   (cdr(assoc "ORDER" (block:get-attributes wall)))
  ;; 	   " , 类型: " (entity:getdxf wall 2)
  ;; 	   )
  ;;  (polar pt-base 4.25 2930)
  ;;  250 0 0.8 0 "LB")
  ;; 图框
  
  (setq tk
	(vla-insertblock *MS* (point:to-ax (polar pt-base 5.42698 4535))
			 "图框-图集" 0.2 0.2 0.2 0))
  ;; (block:insert "图框-图集" "" (polar pt-base 5.42698 4535) 0 0.2))
  ;; (vla-update (e2o tk))
  (block:set-attributes tk (list (cons "图名" (strcat "构件号: "
   						      (cdr(assoc "ORDER" (block:get-attributes wall)))
   						      " , 类型: " (entity:getdxf wall 2)))))
  
  (vla-insertblock *MS* (point:to-ax pt-base)
			 "GJ-elec-dim" 1.0 1.0 1.0 0)
  ;; A
  ;; 绘外形
  (entity:make-text
   "A面"
   (polar pt-base (* 1.5 pi) 500)
   250 0 0.8 0 "LB")
  (entity:make-lwpolyline
   (list pt-base
	 (setq pt-r (polar pt-base 0 (* 2 w)))
	 (setq pt-rh  (polar pt-r (* 0.5 pi) 3080))
	 (setq pt-lh (polar pt-base (* 0.5 pi) 3080)))
   nil 0 1 0)
  ;; 绘梁花
  (cond
    ((= "D" (cdr(assoc "BEAM" (block:get-attributes wall))))
     (entity:make-lwpolyline
      (list pt-lh
	    (setq pt-l2 (polar pt-lh 0 160))
	    (polar pt-l2 (* 1.5 pi) 380)
	    (polar pt-lh (* 1.5 pi) 380))
      nil 0 1 0)
     (entity:make-lwpolyline
      (list pt-rh
	    (setq pt-r2 (polar pt-rh pi 160))
	    (polar pt-r2 (* 1.5 pi) 380)
	    (polar pt-rh (* 1.5 pi) 380))
      nil 0 1 0))
    ((= "L" (cdr(assoc "BEAM" (block:get-attributes wall))))
     (entity:make-lwpolyline
      (list pt-lh
	    (setq pt-l2 (polar pt-lh 0 160))
	    (polar pt-l2 (* 1.5 pi) 380)
	    (polar pt-lh (* 1.5 pi) 380))
      nil 0 1 0))
    ((= "R" (cdr(assoc "BEAM" (block:get-attributes wall))))
     (entity:make-lwpolyline
      (list pt-rh
	    (setq pt-r2 (polar pt-rh pi 160))
	    (polar pt-r2 (* 1.5 pi) 380)
	    (polar pt-rh (* 1.5 pi) 380))
      nil 0 1 0)))
  ;; 电气A
  (prefabricated-building:draw-elec wall pt-base t)
  ;; B
  ;; 绘外形
  (setq pt-base (polar pt-base 0 1000))
  (entity:make-text
   "B面"
   (polar pt-base (* 1.5 pi) 500)
   250 0 0.8 0 "LB")
  (entity:make-lwpolyline
   (list pt-base
	 (setq pt-r (polar pt-base 0 (* 2 w)))
	 (setq pt-rh  (polar pt-r (* 0.5 pi) 3080))
	 (setq pt-lh (polar pt-base (* 0.5 pi) 3080)))
   nil 0 1 0)
  ;; 绘梁花
  (cond
    ((= "D" (cdr(assoc "BEAM" (block:get-attributes wall))))
     (entity:make-lwpolyline
      (list pt-lh
	    (setq pt-l2 (polar pt-lh 0 160))
	    (polar pt-l2 (* 1.5 pi) 380)
	    (polar pt-lh (* 1.5 pi) 380))
      nil 0 1 0)
     (entity:make-lwpolyline
      (list pt-rh
	    (setq pt-r2 (polar pt-rh pi 160))
	    (polar pt-r2 (* 1.5 pi) 380)
	    (polar pt-rh (* 1.5 pi) 380))
      nil 0 1 0))
    ((= "R" (cdr(assoc "BEAM" (block:get-attributes wall))))
     (entity:make-lwpolyline
      (list pt-lh
	    (setq pt-l2 (polar pt-lh 0 160))
	    (polar pt-l2 (* 1.5 pi) 380)
	    (polar pt-lh (* 1.5 pi) 380))
      nil 0 1 0))
    ((= "L" (cdr(assoc "BEAM" (block:get-attributes wall))))
     (entity:make-lwpolyline
      (list pt-rh
	    (setq pt-r2 (polar pt-rh pi 160))
	    (polar pt-r2 (* 1.5 pi) 380)
	    (polar pt-rh (* 1.5 pi) 380))
      nil 0 1 0)))
  ;; 电气B
  (prefabricated-building:draw-elec wall pt-base nil)
  )
(defun prefabricated-building:draw-walls  ()
  (setq walls (prefabricated-building:getwalls))
  ;;排序
  (setq walls (pickset:sort walls
			    "yx" 80))
  (setq i 0)
  (setq pt1 (getpoint  "绘制起点:"))
  (foreach
   wall walls
   (prefabricated-building:draw-wall wall
				     (polar pt1 0 (* (setq i (1+ i)) 12000)))
   
   ))
