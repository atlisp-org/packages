(defun at-dim:menu-zbbz (/ pt-b  pt-c  txtx txty txth line th unit1)
  (@:help "标注选定点的坐标及标高")
  (if (and (/= "" (@:get-config '@dim:layer))
	   (null(tblsearch "layer"  (@:get-config '@dim:layer))))
      (layer:make  (@:get-config '@dim:layer) 2 nil nil))
      
  (setq pt-b (getpoint "选定坐标点:"))
  (setq th (* (@:get-config '@dim:textheight)(@:get-config '@dim:scale)))
  (cond
    ((= (@:get-config '@dim:draw-units)(@:get-config '@dim:text-units))
     (setq unit1 1.0))
    ((and (= (@:get-config '@dim:draw-units) "mm")
	  (= (@:get-config '@dim:text-units) "m"))
     (setq unit1 0.001))
    ((and (= (@:get-config '@dim:draw-units) "m")
	  (= (@:get-config '@dim:text-units) "mm"))
     (setq unit1 1000))
    (t (setq unit1 1.0))
    )

    
  (setq ss (ssget pt-b))
  ;;relative position
  (setq ang-rel (angle (point:centroid (pickset:getbox ss 0)) pt-b))
  ;;绘制
  (if(= 0 (@:get-config '@dim:coordinate-position))
     (cond
       ((< ang-rel (* 0.5 pi))
	(setq txt-ang 0)
	(setq pt-c (polar pt-b (* 0.25 pi) (* th 4))))
       ((< (* 0.5 pi) ang-rel pi)
	(setq txt-ang pi)
	(setq pt-c (polar pt-b (* 0.75 pi) (* th 4))))
       ((< pi ang-rel (* 1.5 pi))
	(setq txt-ang pi)	
	(setq pt-c (polar pt-b (* 1.25 pi) (* th 4))))
       ((< (* 1.5 pi) ang-rel (* 2 pi))
	(setq txt-ang 0)
	(setq pt-c (polar pt-b (* 1.75 pi) (* th 4)))))
     (progn
       (setq pt-c (getpoint pt-b "文字位置:"))
       (setq txt-ang (getangle pt-c "文字方向:"))
       ))
  (cond
    ((<= (* 0.75 pi) txt-ang (* 1.01 pi))
     (setq txt-ang1 (+ txt-ang pi))
     )
    ((< (* 1.01 pi) txt-ang (* 1.5 pi))
     (setq txt-ang1 (- txt-ang  pi))
     )
    (t (setq txt-ang1 txt-ang)
       )
    )
  (if (= txt-ang txt-ang1)
      (setq txtdq "LB")
      (setq txtdq "RB"))
  (if(> (strlen (@:get-config '@dim:prefix))1)
     (progn
       (setq prefix-x (substr (@:get-config '@dim:prefix) 1 1))
       (setq prefix-y (substr (@:get-config '@dim:prefix) 2 1)))
     (progn
       (setq prefix-x "X")
       (setq prefix-y "Y"))
     )
  (setq strx (strcat prefix-x "=" (rtos(* unit1 (car pt-b)) 2 (@:get-config '@dim:units)))
	stry (strcat prefix-y "=" (rtos (* unit1(cadr pt-b)) 2 (@:get-config '@dim:units)))
	strh
	(if (= 1 (@:get-config '@dim:elevation))
	    (strcat "H=" (rtos (* unit1(caddr pt-b)) 2 (@:get-config '@dim:units)))))

  
  (if (= 1 (@:get-config '@dim:coordinate-type))
      (progn
	(setq txtx
	      (entity:make-text strx
				pt-c
				th
				txt-ang1 0.8 0 txtdq))
	(setq txty
	(entity:make-text stry
			  (polar pt-c  (+ txt-ang1 (* 1.5 pi)) (* 1.2 th))
			  th
			  txt-ang1 0.8 0 txtdq))
	(setq ents (list txtx txty))
	(if (= 1 (@:get-config '@dim:elevation))
	    (progn
	      (setq txth
		    (entity:make-text strh (polar pt-c (+ txt-ang1 (* 1.5 pi)) (* 2.4 th)) th txt-ang1 0.8 0 txtdq))
	      (setq ents (cons txth ents))))
	(setq text-maxwidth
	(apply 'max
	       (mapcar '(lambda(x / box)
			 (setq box (text:box x))
			 (distance (car box)(cadr box)))
		       ents)))
	;;调整为左对齐
	(if (= 2 (entity:getdxf txtx 72))
      (mapcar '(lambda(x)
		(entity:putdxf
		 (entity:putdxf x 11
		  (polar 
		   (entity:getdxf x 11)
		   (angle (entity:getdxf x 11)
			  (entity:getdxf x 10))
		   text-maxwidth))
		 72 0))
	      ents))
	(setq line
	(entity:make-lwpolyline
	 (list pt-b
	       pt-c
	       (polar pt-c txt-ang text-maxwidth))
	 nil 0 0 0))
	(entity:putdxf line 38 (caddr pt-b))
	(setq ents (cons line ents))
	(if (/= "" (@:get-config '@dim:layer))
	    (mapcar '(lambda (x)
		      (entity:putdxf x 8 (@:get-config '@dim:layer)))
		    ents))
	;; 编组
	(group:make ents (strcat "XY" (@:timestamp)))
	)
      (progn ;;引线
	;;(if (= "RB"  txtdq) ;;修正 txt 的点
	;;    (setq pt-c (polar pt-c pi  (distance pt-b pt-c))))
	(vla-put-ScaleFactor
	 (e2o(entity:make-multileader (list pt-b pt-c)
				 (string:from-list
				  (vl-remove nil
					     (list strx stry strh))
				  "\\P")))
	 (* 10 (@:get-config '@dim:scale)))
	)
      ))
