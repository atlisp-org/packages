;;; 湿空气参数计算库

;;; 摄氏温标转化为开式温标
(defun psy-c->k (tt) (+ tt 273.15))
;;; 开氏温标转化为摄式温标
(defun psy-k->c (tt) (- tt 273.15))
;;; 干球温度(K)求饱和水蒸气分压Pws (Pa) 
;;;_$ (psy-db->pws (psy-c->k -1))
;;;562.823
;;;_$ (psy-db->pws (psy-c->k 10))
;;;1228.04
;;;_$ (psy-db->pws (psy-c->k 50))
;;;12350.3
;;;_$ (psy-db->pws (psy-c->k 105))
;;;120908.0
;;;_$ (psy-db->pws (psy-c->k 180))
;;;1.00289e+006
(defun psy-db->pws (db / a b c d)
  (cond ((and (>= db 213.15) (< db 273.15))
         (setq a -7.297593707E-06
               b 5.397420727E-03
               c 2.069880620E+01
               d -6.042275128E+03
         )
        )
        ((and (>= db 273.15) (< db 322.15))
         (setq a 1.255001965E-05
               b -1.923595289E-02
               c 2.705101899E+01
               d -6.344011577E+03
         )
        )
        ((and (>= db 322.15) (< db 373.15))
         (setq a 1.246732157E-05
               b -1.915465806E-02
               c 2.702388315E+01
               d -6.340941639E+03
         )
        )
        ((and (>= db 373.15) (< db 423.15))
         (setq a 1.204507646E-05
               b -1.866650553E-02
               c 2.683629403E+01
               d -6.316972063E+03
         )
        )
        ((and (>= db 423.15) (< db 473.15))
         (setq a 1.069730183E-05
               b -1.698965754E-02
               c 2.614073298E+01
               d -6.220781230E+03
         )
        )
  )
  (setq a (+ (* a db db) (* b db) c (/ d db)))
  (* (exp a) 1000.)
)
;;; 饱和水蒸气分压Pws (Pa) 求干球温度(K)
(defun psy-pws->db (pws / b e f g h k)
  (cond ((and (>= pws 1) (< pws 611))
         (setq e 1.004926534E-03
               f 1.392917633E-03
               g 2.815151574E-01
               h 7.311621119E+00
               k 2.125893734E+02
         )
        )
        ((and (>= pws 611) (< pws 12350))
         (setq e 5.031062503E-03
               f -8.826779380E-02
               g 1.243688446E+00
               h 3.388534296E+00
               k 2.150077993E+02
         )
        )
        ((and (>= pws 12350) (< pws 101420))
         (setq e 1.209512517E-02
               f -3.545542105E-01
               g 5.020860328E+00
               h -2.050301050E+01
               k 2.718585432E+02
         )
        )
        ((and (>= pws 101420) (< pws 476207))
         (setq e 2.467291016E-02
               f -9.367112883E-01
               g 1.514142334E+01
               h -9.882417501E+01
               k 4.995092948E+02
         )
        )
        ((and (>= pws 476207) (< pws 1555099))
         (setq e 2.748402484E-02
               f -1.068661307E+00
               g 1.742964692E+01
               h -1.161208532E+02
               k 5.472618120E+02
         )
        )
  )
  (setq b (log pws))
  (+ (* e (expt b 4)) (* f (expt b 3)) (* g b b) (* h b) k)
)
;;;(defun test (/ db r)
;;;  (setq db 213.15)
;;;  (while (< db 473.15)
;;;    (setq r (cons (cons db (abs (- (psy-pws->db (psy-db->pws db)) db))) r))
;;;    (setq db (+ 1. db))
;;;  )
;;;  r
;;;)
;;;(vl-sort (test) '(lambda (e1 e2) (> (abs (cdr e1))(abs (cdr e2)))))
;;;((213.15 . 0.0109878) (214.15 . 0.00653634) (221.15 . 0.00517123) (222.15 . 0.00512335)..

;;; 通过饱和水蒸气分压(Pa)及相对湿度水蒸气分压(Pa)
(defun psy-rh->pw (pws rh)
  (* pws rh)
)
;;; 由饱和水蒸气分压Pws(Pa)及水蒸气分压Pw(Pa)计算相对湿度RH
(defun psy-pw->rh (pws pw)
  (/ pw pws)
)

(setq $psy-ap 101325.)

;;; 由水蒸气分压(Pa)计算含湿量(g/kg)
;;; (psy-pw->w (psy-rh->pw (psy-db->pws (psy-c->k 26.)) 0.6))
;;; 12.6388
(defun psy-pw->w (pw)
   (/ (* 0.62198 pw) (- $psy-ap pw) 1e-3) 
)
;;; 由含湿量(g/kg)计算水蒸气分压(Pa)
;;; (psy-w->pw 12.6388)
;;; 2017.95
(defun psy-w->pw (d)
  (/ (* $psy-ap (/ d 1000.)) (+ 0.62198 (/ d 1000.)))
)
;;; 干球温度(摄氏C)含湿量(g/kg)求焓(kJ/kg)
;;;_$ (psy-get-enthalpy (psy-c->k 26.) (psy-pw->w (psy-rh->pw (psy-db->pws (psy-c->k 26.)) 0.6)))
;;;58.3587
(defun psy-get-enthalpy (db w /)
  (setq db (psy-k->c db))
  (+ (* 1.006 db) (* 0.001 w (+ 2501 (* 1.805 db))))
)
;;;由温度t(in C)和焓h计算含湿量d(W)
;;;inline double CMoistAir::Pw2W(double t, double h) {return (h - 1.006L * t)/ (2501 + 1.805 * t);}

;;; 含湿量(g/kg)饱和湿空气露点温度
;;; (psy-k->c (psy-get-dewpoint 12.6388))
;;; 17.6392
(defun psy-get-dewpoint (w)
  (psy-pws->db (psy-w->pw w))
)
;;; 干球温度(K)水蒸气分压(Pa)求湿空气密度(kg/m3)
;;;_$ (psy-get-density (psy-c->k 26.) (psy-rh->pw (psy-db->pws (psy-c->k 26.)) 0.6))
;;;1.16981
(defun psy-get-density (db pw)
  (- (* 0.00348 (/ $psy-ap db)) (* 0.00132 (/ pw db)))
)
;;; 干球温度(K)饱和焓(kJ/kg)
;;;_$ (psy-get-enthalpysaturation (psy-c->K 26))
;;;80.5642
(defun psy-get-enthalpysaturation (db)
  (psy-get-enthalpy (psy-k->c db) (psy-pw->w (psy-db->pws db)))
)

;;;	[in]	干球温度DB in K
;;;	[in]	含湿量W
;;;	[in]	焓h
;;;	[out]	湿球温度WB in K
;;; (psy-get-wb (psy-c->k 26.) (psy-get-enthalpy 26. (psy-pw->w (psy-rh->pw (psy-db->pws (psy-c->k 26.)) 0.6))) (psy-pw->w (psy-rh->pw (psy-db->pws (psy-c->k 26.)) 0.6)) )
(defun psy-get-wb (twb2 h w / )
  (setq twb1 213.15
        twb  (/ (+ twb1 twb2) 2.0)
        ws   0
        hs   0
        hd   0
        i    0
  )
  (while (< i 1e2)
    (setq i  (1+ i)
          ws (psy-pw->w (psy-db->pws twb))
          hs (psy-get-enthalpy (psy-k->c twb) ws)
          hd (* 4.186 (- ws w) twb 1e-3)
    )
    (cond ((or (equal hs (+ h hd) 1e-3)
               (equal twb twb1 1e-3)
               (equal twb twb2 1e-3)
           )
           (setq i 1e2)
          )
          ((> hs (+ h hd))
           (setq twb2 twb
                 twb  (/ (+ twb twb1) 2.0)
           )
          )
          (t
           (setq twb1 twb
                 twb  (/ (+ twb twb2) 2.0)
           )
          )
    )
  )
  twb
)

;; (test 26. .6)
(defun test (db rh / h pw pws w)
  (setq
    pws (psy-db->pws (psy-c->k db))
    pw  (psy-rh->pw pws rh)
    w   (psy-pw->w pw)
    h   (psy-get-enthalpy (psy-c->k db) w)
  )
  (list db
        (psy-k->c (psy-get-wb (psy-c->k db) h w))
        (psy-k->c (psy-get-dewpoint w))
        rh
        h
        w
        pw
        pws
        (psy-get-density (psy-c->k db) pw)
  )
;;;  (list db
;;;        (psy-get-density (psy-c->k db) pw)
;;;        pws
;;;        w
;;;        h
;;;  )
)
;; (p-list->file (mapcar 'vl-princ-to-string (psy-saturation-points)) "a.txt")
(defun psy-saturation-points (/ db h pws r w)
  (setq db -60.)
  (while (< (setq db (1+ db)) 100)
    (setq pws (psy-db->pws (psy-c->k db))
          w   (psy-pw->w pws)
          h   (psy-get-enthalpy (psy-c->k db) w)
    )
    (setq r (cons (list db pws w h) r))
  )
  (reverse r)
)
;;;	[in]	焓h	kJ/kg
;;;	[in]	含湿量W	g/kg
;;;	[out]	干球温度DB in C
;;; (hw->db 58.3587 12.6388)
(defun hw->db (h w)
  (/ (- h (* 0.001 2501 w)) (+ 1.01 (* 0.001 1.84 w)))
)


;;;	[in]	焓h	kJ/kg
;;;	[out]	干空气中的饱和水蒸气分压Pws
(defun satenthalpy2pw (h / pws1 pws2 pws htry i)
  (setq pws1 1
        pws2 1555098
        pws  (/ (+ pws1 pws2) 2.0)
        htry 0
        i    0
  )
  (while (< i looplimit)
    (setq i (1+ i))
    (if (>= i looplimit)
      (alert "err: ")
    )
    (setq
      htry (psy-get-enthalpy (psy-pws->db pws) (/ (psy-pw->w pws) 1000.0))
    )
    (cond ((<= (abs (- htry h)) epsilon) (setq i looplimit))
          ((> htry h)
           (setq pws2 pws
                 pws  (/ (+ pws pws1) 2.0)
           )
          )
          (t
           (setq pws1 pws
                 pws  (/ (+ pws pws2) 2.0)
           )
          )
    )                                   ;endcond
  )
  pws
)                                       ;endfun




;;;	[in]	饱和水蒸气分压Pws
;;;	[out]	饱和焓h in kJ/kg
;;;double CMoistAir::GetEnthalpyByPws(double Pws)
;;;{
;;;	psy-pws->db(Pws)
;;;	return psy-get-enthalpy(DB, psy-pw->w(psy-db->pws(DB)));
;;;}




;;;	[in]	露点温度DP in K
;;;	[out]	水蒸气分压Pw in Pa
(defun dp2pw (dp /)
  (setq dp (psy-k->c dp))
  (if (< dp 0)
    (progn                              ;0.37*x^2+7.0322*x-60.45-DP			ln(Pq)=x
      (setq pq (/
                 (+ -7.0322
                    (expt (- (* 7.0322 7.0322) (* 4 0.37 (- -60.45 dp))) 0.5)
                 )
                 (* 2 0.37)
               )
            pq (exp pq)
      )
    )                                   ;endthen
    (progn                              ;1.1689*x^2-1.8726*x-35.957-DP			ln(Pq)=x
      (setq pq (/
                 (+ 1.8726
                    (expt (- (* -1.8726 -1.8726) (* 4 1.1689 (- -35.957 dp)))
                          0.5
                    )
                 )
                 (* 2 1.1689)
               )
            pq (exp pq)
      )
    )                                   ;endelse
  )                                     ;endif
)                                       ;endfun DP2Pws