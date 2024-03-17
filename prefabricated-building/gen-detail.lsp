;; 生成构件图
(defun prefabricated-building:getwalls ()
  (pickset:to-list
   (ssget '((0 . "insert")
	    (2 . "QN##0,QW##0")
	    ))))
(defun prefabricated-building:beamflag (wall / ptb rot pl pr flag-lw flag-rw w)
  ;; 600
  (setq w (cond
	    ((wcmatch (entity:getdxf wall 2) "Q@600")
	     300)
	    ((wcmatch (entity:getdxf wall 2) "Q@750")
	     375)))
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
  "返回列表，线盒类型(开关、普插、空插、其它) 水平相对位置，竖向相对位置，A或B面"
  ;; 600
  ;; A面
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


(defun prefabricated-building:m-gen-elecdata ()
  (setq res
	(prefabricated-building:gen-elecdata(car(entsel)))))
