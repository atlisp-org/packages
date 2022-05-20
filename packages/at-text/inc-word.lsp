
;;;字母数字动态递增
;;;点哪个就加哪个
;;;luyu9635 2012.11.15深圳
(defun @text:inc-word (/ z a ad ab str str2 str1 dxf adx len xlen loop i su zors p1 p2 pt box npt xpt pt10 zc pjz do)
  (if (or (setq a (entsel "\n选择数字或字母:"))
	  ;;(ssget (cadr a) '((0 . "*TEXT")))
	  )
      (progn
	(defun dxf (zm ent)(entity:getdxf ent zm))
      (setq ab(car a)
	    ad (dxf  1 ab )
	    pt10 (dxf 10 ab)
	    h(dxf 40 ab)
	    pt (cadr a)) 
      (setq box(textbox (cdr(entget(car a)))))
      (setq xpt(car pt))
      (setq zc (- (car (cadr box)) (car (car box))))
      (setq len	 (strlen ad) i 1 xlen len )
      (repeat len
	(if(member (substr ad i 1) '("'" "," "." "1"))(setq len(- len 0.5) j 2))
        (setq i(1+ i))
	)
       (setq pjz (/ zc len) pt0(car pt10) npt pt0 i  1)
       (if(= j 2)(setq p2 (/ pjz 2)))
       (repeat xlen
          (setq str(substr ad i 1))
	     (if(member str '("'" "," "." "1"))(setq npt(+ npt p2))(setq npt (+ npt pjz) ))
              (if (<= pt0  xpt npt)  (setq zors str  n i))
              (setq pt0 npt i (1+ i))
            )
      (cond( (= n 1)
	       (setq str1 "" str(ascii zors) str2(substr ad (1+ n)))
	    )
	   ( (= n xlen)
	     (setq str1(substr ad 1 (- xlen 1)) str(ascii zors) str2 "")
	    )
	  (t
	   (setq str1(substr ad 1 (- n 1)) str(ascii zors) str2(substr ad (1+ n)))
	   )
	   )
      (cond 
	    ((or (wcmatch zors  "~*[~.0-9]*" )(wcmatch zors "*[a-zA-Z]"))
               (setq p1 (getpoint "\n选择基点:") loop T)
	   (while loop
            (setq str(1+ str) tmp(strcat str1 (chr str) str2))
            (vla-copy (vlax-ename->vla-object ab))
	       (setq  adx (entget (entlast)))
	       (setq adx (subst (cons 1 tmp) (assoc 1 adx) adx))
	       (entmod adx)
	       (command ".move" (entlast) "" "non" p1 "non" pause)
               (if (= 0 (distance p1 (getvar "lastpoint")))
		    (progn (setq loop nil)(entdel(entlast)))
                    (progn (setq ab(entlast) p1(getvar "lastpoint")))
                )
	     )
	   )
      )
    )
  )  
 
  (princ)
)

