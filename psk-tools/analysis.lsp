
;;; 直线相互连接形成之拓朴结构的相关操作 2018-4-1
;;; 2021-2-17

;; (setq bst (psk-comps-buildbst (psk-comps-ssget) 0.1))
;; (setq bst (psk-comps-buildbst (psk-comps-ssget) 0.1))
;; (p-bst-find (getpoint) bst 0.1)
;; (p-bst-find (getpoint) (psk-comps-buildbst (psk-comps-ssget) 0.1) 0.1)
;; (<图元名: 7ff415f1e150> <图元名: 7ff415f1af10>)
(defun psk-comps-buildbst (comps tolerance / pcs ports)
  (foreach comp	comps
    (if	(setq ports (psk-comp-getports comp))
      (progn
	(setq ports (mapcar 'psk-port-pos ports))
	(foreach p ports
	  (setq pcs (cons (list p (psk-comp-getename comp)) pcs))
	)
      )
    )
  )

  (p-bst-build (p-sort-points pcs tolerance))
)
;;
(defun psk-comp-nextp (res bst ename p / ports nextps r)
  (setq	ports (psk-comp-getports ename)
	ports (psk-ports-sort ports p)
  )

  (if (not (ssmemb ename $psk-comps-walked))
    (progn
      (setq res		      (cons (p-dxf ename 5) res)
	    $psk-comps-walked (ssadd ename $psk-comps-walked)
	    nextps	      (cdr (mapcar 'cadr ports))
      )
;;;      (redraw ename 3)

      ;; TODO:
      (if (not (p-xdata-exist ename "PSK-EQUIP"))
	(cond ((= 1 (length nextps))
	       (setq res (psk-comp-next res bst (car nextps)))
	      )
	      ((< 1 (length nextps))
	       (foreach	p nextps
		 (setq r (cons (psk-comp-next '() bst p) r))
	       )
	       (setq res (cons r res))
	      )
	)
      )
    )
  )
  res
)
;;
(defun psk-comp-next (res bst p / ename enames nextps r r1)
  (setq	enames (p-bst-find p bst 0.1)
	enames (vl-remove-if
		 '(lambda (e) (ssmemb e $psk-comps-walked))
		 enames
	       )
  )
  (cond
    ;; 只有一个后续管件
    ((= 1 (length enames))
     (setq res (psk-comp-nextp res bst (car enames) p))
    )
    ;; 多个后续管件
    ((< 1 (length enames))
     ;; 优先遍历管道
;;;     (setq enames (vl-sort enames
;;;			   '(lambda (e1 e2)
;;;			      (if (p-xdata-get e1 "PSK-PATH")
;;;				t
;;;			      )
;;;			    )
;;;		  )
;;;     )
     (foreach ename enames
       (setq r (cons (psk-comp-nextp '() bst ename p) r))
     )
     (setq res (cons r res))
    )
  )
  res
)
;; 建立路径图
;; (psk-comp-buildmap (psk-comps-fromviewport) (getpoint))
;; ((("92D" "92F") ("922" "921" "923" "91B" "91C" "91A")) "919")
(defun psk-comp-buildmap (comps p / bst)
  (setq	bst		  (psk-comps-buildbst comps 0.1)
	$psk-comps-walked (ssadd)
  )
  (psk-comp-next '() bst p)
)
;;

;; (p-list-flat '((("92D" "92F") ("922" "921" "923" "91B" "91C" "91A")) "919"))
;; ("919" "92F" "92D" "91A" "91C" "91B" "923" "921" "922")
(defun p-list-flat (lst / r)
  (foreach e lst
    (if	(atom e)
      (setq r (cons e r))
      (if (vl-consp e)
	(setq r (append r (p-list-flat e)))
      )
    )
  )
  r
)
;; (psk-map-result '((("92D" "92F") ("922" "921" "923" "91B" "91C" "91A")) "919"))
(defun psk-map-result (map /)
  (p-enames->ss
    (mapcar (function handent) (p-list-flat map))
  )
)
;;

(defun psk-map-enum-inner (map root /)
  (setq map (reverse map))
  (while map
    (cond
      ((atom (car map))
       (setq root (append root (list (car map))))
       (if (null (cdr map))
	 (setq psk-map-routers (cons root psk-map-routers))
       )
      )
      ((vl-consp (car map))
       (psk-map-enum-inner (car map) root)
      )
    )
    (setq map (cdr map))
  )
  root
)
;; 生成各分支(从指定起点到各末端点)的所有路径
;; (psk-map-enum '((("92D" "92F") ("922" "921" "923" "91B" "91C" "91A")) "919"))
;; (("919" "922" "921" "923" "91B" "91C" "91A") ("919" "92D" "92F"))
;; (psk-map-enum '((((((("83B") ("83A")) "83C" "800") ((("814") ("811")) "82A" "801")) "802" "7FF") nil) "891" "888"))
;; (psk-map-enum (psk-comp-buildmap (getpoint)))
(defun psk-map-enum (map / psk-map-routers)
  (psk-map-enum-inner map '())
  psk-map-routers
)
(defun psk-branch-enum (p / pname routers)
  (getstring "\n确保支管视图范围内可见, 按任意键继续:")
  (princ "\n正在计算...")
  (p-timer-start)
  (setq	routers
;;;         (psk-map-enum (psk-comp-buildmap (psk-comps-ssget) p))
        (psk-map-enum (psk-comp-buildmap (psk-comps-fromviewport) p))
;;;	pname	(p-getkword1
;;;		  "选择要累计的属性"
;;;		  '((".PRL" "管道阻力")
;;;		   )
;;;		  ".PRL"
;;;		)
	pname	".PRL"
  )
  (setq	routers	(mapcar
		  (function (lambda (e)
			      (setq e (vl-remove-if 'null e)
				    e (mapcar 'handent e)
			      )
			      (cons (psk-path-total2 e pname) e)
			    )
		  )
		  routers
		)
	routers	(vl-sort routers
			 (function (lambda (e1 e2) (< (car e1) (car e2))))
		)
  )
  (princ (strcat
	   "\r计算完成 "
	   (rtos (/ (p-timer-stop) 1000.) 2 1)
	   " s"
	 )
  )
  routers
)
;;; 遍历所有支管
(defun psk-show-branchs	(p / c pname routers ss)

  (setq	routers	(psk-branch-enum p)
	c	0
  )
  (foreach router routers
    (setq ss (ssadd))
    (foreach e (cdr router)
      (if e
	(setq ss (ssadd e ss))
      )
    )
    (sssetfirst ss ss)

    (getstring
      (strcat "\n("
	      (itoa (setq c (1+ c)))
	      " / "
	      (itoa (length routers))
	      ") 沿程阻力 = "
	      ;;(rtos (/ (car router) 1e4) 2 2)
	      (rtos (car router) 2 2)
	      "Pa 总长 = "
	      (rtos (/ (psk-path-totallength (cdr router)) 1e3) 2 2)
	      "m 下一个:"
      )
    )
    (setq ss nil)
    (sssetfirst nil nil)
  )
  (princ)
)
(defun psk-show-largestbranch (p / c pname routers ss)
  (if p
    (progn
      (setq routers (psk-branch-enum p)
	    router  (last routers)
      )

      (setq ss (ssadd))
      (foreach e (cdr router)
	(if e
	  (setq ss (ssadd e ss))
	)
      )
      (sssetfirst ss ss)

      (princ
	(strcat	"\n沿程阻力 = "
		(rtos (car router) 2 2)
		"Pa 总长 = "
		(rtos (/ (psk-path-totallength (cdr router)) 1e3) 2 2)
		"m"
	)
      )
    )
  )
)
;; (psk-path-sum '((("9F6" "9F3") ("9F9")) "9F0" "9FC") 100.)
;; (psk-path-sum (psk-comp-buildmap (getpoint)) (getreal "输入末端流量"))
;;;(defun psk-path-sum (map fs / v)
;;;  (foreach node	map
;;;    (cond ((null node)
;;;	  )
;;;	  ((vl-consp node)
;;;	   (if (null v)
;;;	     (setq v 0.)
;;;	   )
;;;	   (setq v (+ v (psk-path-sum node fs)))
;;;	  )
;;;	  ((atom node)
;;;	   (if (null v)
;;;	     (setq v fs)
;;;	   )
;;;	   (psk-comp-set (handent node) (list (cons "FLR" v)))
;;;	  )
;;;    )
;;;  )
;;;  v
;;;)
;;;(psk-path-sum2 (psk-comp-buildmap (getpoint)) "CLD")
(defun psk-path-sum2 (map pname / v)
  (foreach node	map
    (cond ((null node)
	  )
	  ((vl-consp node)
	   (if (null v)
	     (setq v 0.)
	   )
	   (setq v (+ v (psk-path-sum2 node pname)))
	  )
	  ((atom node)
;;;	   (or v
;;;	       (setq v (psk-comp-get (handent node) pname))
;;;	       (setq v 0)
;;;	   )
	   (if (null v)
	     (setq v (psk-comp-get (handent node) pname))
	   )
	   (psk-comp-set (handent node) (list (cons pname v)))
	  )
    )
  )
  v
)
;;
;;;(defun c:hh (/ en)
;;;;;;  (princ (p-dxf (car (entsel)) 5))
;;;  (setq en (car (entsel)))
;;;  (princ (psk-comp-get en "FLR"))
;;;  (princ)
;;;)

(defun psk-path-pickend	(/ line p)
  (if (and (setq line (entsel "选择分支起点:"))
      )
    (progn
      (setq p	 (trans (cadr line) 1 0)
	    line (car line)
	    p	 (cdr (p-line-getendnear line p))
      )
      (list line p)
    )
  )
)

(setq $psk-total-key "CLD")

(defun psk-path-total (/ p)
  (if (and ;;(setq comps (psk-comps-ssget))
           (setq p (psk-path-pickend))
	   (getstring "\n确保支管视图范围内可见, 按任意键继续:")
	   (setq comps (psk-comps-fromviewport2 (p-dxf (car p) 8)))
	   (setq $psk-total-key
		  (p-getkword1
		    "选择要累计的属性"
		    '(("FLR" "流量")
		      ("CLD" "冷负荷")
		      ("HLD" "热负荷")
		     )
		    $psk-total-key
		  )
	   )
      )
    (progn
      (psk-path-sum2
	(psk-comp-buildmap comps (cadr p))
	$psk-total-key
      )

      (princ
	(strcat	"\n累计结果: "
		(rtos (psk-comp-get (car p) $psk-total-key) 2 2)
	)
      )
    )
  )
)
(defun psk-totalandcalc	(/ map p ss)
  (if (and (setq comps
;;;                  (psk-comps-ssgetpipe)
		  (ssget '((-3 ("PSK-PATH,PSK-PART,PSK-PARTSET"))))
                ;; (psk-comps-fromviewport)
                 )
           ;;(setq p (psk-path-pickend))
           )
    (progn
;;;      (setq map	(psk-comp-buildmap comps (cadr p))
;;;	    ss	(psk-map-result map)
;;;      )
;;;      (psk-path-sum2 map "CLD")
      (psk-batch-cld->flr1 comps)
      (psk-batch-flr->dn1 comps t)
    )
  )
)
(defun psk-path-total2 (paths pname / v v1)
  (setq v 0.)
  (foreach en paths
    (setq v1 (psk-comp-get en pname))
    (if	(null v1)
      (setq v1 0.)
    )
    (setq v (+ v v1))
  )
  v
)
(defun psk-path-totallength (paths / v)
  (setq v 0.)
  (foreach en paths
    (if	(= "LINE" (p-dxf en 0))
      (setq v (+ v (distance (p-dxf en 10) (p-dxf en 11))))
    )
  )
  v
)

;; 根据冷负荷kW计算流量m3/s
;; density - kg/m3
;; dt - C

;;;(psk-path-calc-flr (car (entsel)) 5 990)
;;;(defun psk-path-calc-flr (path dt density / cld flr)
;;;  (setq	cld (psk-comp-get path "CLD")
;;;	flr (psk-cld->flr cld 4.1868 dt density)
;;;  )
;;;  (psk-comp-set path (list (cons "FLR" flr)))
;;;)

;; kw kj/kg*C C kg/m3
;; (psk-cld->flr 40. 4.1868 5. 990.)
;; 0.00193007
(defun psk-cld->flr (cld hc dt density /)
  (/ cld hc dt density (/ 1 3600.))
)


(defun p-lookup	(k lst / r)
  (while lst
    (if	(<= k (caar lst))
      (setq r	(cadar lst)
	    lst	nil
      )
    )
    (setq lst (cdr lst))
  )
  r
)
;;;_$ (p-lookup3 320 $psk-steelductwallthick nil)
;;;(320 0.5 0.5 0.75 2.0)
;;;_$ (p-lookup3 320 $psk-steelductwallthick '("D" "P1" "P2" "P3" "P4"))
;;;(("D" . 320) ("P1" . 0.5) ("P2" . 0.5) ("P3" . 0.75) ("P4" . 2.0))
(defun p-lookup3 (k lst header / r)
  (while lst
    (if (<= k (caar lst))
      (setq r   (car lst)
            lst nil
      )
    )
    (setq lst (cdr lst))
  )
  (if header
    (mapcar 'cons header r)
    r
  )
)
;;;_$ (p-lookup1 202 '(120 160 200 250 320 400 500 630 800 1000 1250 1600 2000 2200 2500 3000))
;;;250
(defun p-lookup1 (k lst / r)
  (while lst
    (if	(<= k (car lst))
      (setq r	(car lst)
	    lst	nil
      )
    )
    (setq lst (cdr lst))
  )
  r
)
;; 根据负荷选冷凝水管径
(defun psk-cld->cddn (/ cld dn ss)
  (if (and (setq ss (psk-ss-getpipe)))
    (progn
      (foreach en (p-ss->enames ss)
	(setq cld (psk-comp-get en "CLD")
	      dn  (p-lookup cld $psk-clddn)
	)
	(psk-comp-set
	  en
	  (list
	    (cons "DN" dn)
	  )
	)
      )
    )
  )
)
;; 根据负荷计算多联机冷媒管径
(defun psk-cld->coppersize (/ cld dn ss)
  (if (and (setq ss (psk-ss-getpipe)))
    (progn
      (foreach en (p-ss->enames ss)
	(setq cld (psk-comp-get en "CLD")
	      dn  (p-lookup cld $psk-cldcopperpipesize)
	)
	(psk-comp-set
	  en
	  (list
	    (cons "REFS" dn)
	  )
	)
      )
    )
  )
)
;; 根据流量圆管内径计算流速m/s
;; m3/h mm
;;;_$ (psk-flr->flv 50 50.)
;;;7.07355
(defun psk-flr->flv (flr di /)
  (/ flr 3600. (* di di pi 0.25 1e-6))
)

;; 根据流量圆管内径表计算DN内径及流速
;; (psk-flr->dndjflv 50 $psk-pipe-calctable)
;; (100 100 1.76839)
;; (DN - mm DJ - mm  FLV - m/s)
(defun psk-flr->dndjflv	(flr dns / flv row r)
  (while dns
    (setq row (car dns))

    (if	(< (setq flv (psk-flr->flv flr (cadr row))) (caddr row))
      (setq r	(list (car row) (cadr row) flv)
	    dns	nil
      )
    )

    (setq dns (cdr dns))
  )
  r
)
;; 根据流量DN计算DN内径及流速
(defun psk-flrdn->djflv	(flr dn dns / flv row r)
  (while dns
    (setq row (car dns))

    (if	(equal dn (car row) 1e-3)
      (setq r	(list (car row) (cadr row) (psk-flr->flv flr (cadr row)))
	    dns	nil
      )
    )

    (setq dns (cdr dns))
  )
  r
)





(defun psk-batch-cld->flr1 (ss / cld den dt flr ss)
  (if (and ss
           (setq dt (p-edit-value "\n输入温差" 5.))
      )
    (progn
      (setq den 990.)
      (foreach en (p-ss->enames ss)
        (if (and (setq cld (psk-comp-get en "CLD"))
                 (not (equal cld 0.0 1e-6))
                 (setq flr (psk-cld->flr cld 4.1868 dt den))
            )

          (psk-comp-set
            en
            (list
              (cons "FLR" flr)
            )
          )
          (princ "\n未指定冷负荷CLD属性")
        )
      )
    )
  )
)


(defun psk-batch-cld->flr (/ cld den dt flr ss)
  (psk-batch-cld->flr1
    (psk-ss-getpipepath)
  )
)


;; 水管阻力计算
;;;(psk-batch-flr->dn t)
(defun psk-batch-flr->dn1 (ss setdn / den dn flr k mu prl ss)
  (if ss
    (progn
      (foreach en (p-ss->enames ss)
        (setq flr (psk-comp-get en "FLR")
              den 990.
              k   0.0005
              mu  6.03E-06
        )

        (if (and flr (/= flr 0.0))
          (progn
            (if setdn
              (setq dn  (psk-flr->dndjflv flr $psk-pipe-calctable)
                    prl (psk-pm (caddr dn) (/ (cadr dn) 1000.) mu k den)
              )
              (setq dn  (psk-flrdn->djflv
                          flr
                          (psk-comp-get en "DN")
                          $psk-pipe-calctable
                        )
                    prl (psk-pm (caddr dn) (/ (cadr dn) 1000.) mu k den)
              )
            )
            (psk-comp-set
              en
              (list (cons "DN" (car dn))
                    (cons "FLR" flr)
                    (cons ".FLV" (caddr dn))
                    (cons ".PM" (caddr prl))
                    ;; Pa/m
                    (cons ".PRL"
                          (* 1e-3
                             (caddr prl)
                             (distance (p-dxf en 10) (p-dxf en 11))
                          )
                    )
                    ;; Pa
              )
            )
          )
        )
      )
    )
  )
)

(defun psk-batch-flr->dn (setdn / den dn flr k mu prl ss)
  (psk-batch-flr->dn1 (psk-ss-getpipepath) setdn)
)


(defun psk-flr->w (flr hmax v /)
  (/ flr 3600. v hmax 1e-3)
)
(defun psk-flr->ductrectflv (flr w h /)
  (/ flr 3600. w h 1e-6)
)
;; 计算风管高度
(defun psk-batch-flr->w	(/ flr flv hmax prl ss v w)
  (if (and (setq ss (psk-ss-getductpath))
	   (setq hmax (p-edit-value "风管限高" 320.))
	   (setq v (p-edit-value "最大风速" 8.))
      )
    (progn
      (foreach en (p-ss->enames ss)
	(if (and (setq flr (psk-comp-get en "FLR"))
		 (setq w (psk-flr->w flr hmax v))
	    )
	  (progn
	    (setq w   (* 1000. w)
		  w   (p-lookup1 w $psk-ductrect-sizes)
		  flv (psk-flr->ductrectflv flr w hmax)
		  prl (psk-cmh->prl flv w hmax 1.2 0.15e-3 15.06E-06)
	    )

	    (psk-comp-set
	      en
	      (list
		(cons "W" w)
		(cons "H" hmax)
		(cons ".FLV" flv)
		(cons ".PM" (caddr prl))
		(cons ".PRL"
		      (* 1e-3
			 (caddr prl)
			 (distance (p-dxf en 10) (p-dxf en 11))
		      )
		)
	      )
	    )
	  )
	)
      )
    )
  )
)
(defun psk-cmh->prl (flv a b den k mu /)
  (psk-pm flv (/ (* 2. a b) (+ a b) 1e3) mu k den)
)
;; 风管阻力计算 (psk-batch-cmh->prl)
(defun psk-batch-cmh->prl (/ a b den flr flv k mu prl ss)
  (if (and (setq ss (psk-ss-getduct)))
    (progn
      (foreach en (p-ss->enames ss)
	(if (= "LINE" (p-dxf en 0))
	  (progn
	    (if	(and
		  (setq flr (psk-comp-get en "FLR"))
		  (setq a (psk-comp-get en "W"))
		  (setq b (psk-comp-get en "H"))
		)
	      (progn
		(setq
		  flv (psk-flr->ductrectflv flr a b)
		  prl (psk-cmh->prl flv a b 1.2 0.15e-3 15.06E-06)
		)
		(psk-comp-set
		  en
		  (list
		    (cons ".FLV" flv)
		    (cons ".PM" (caddr prl))
		    ;; Pa/m
		    (cons ".PRL"
			  (* 1e-3
			     (caddr prl)
			     (distance (p-dxf en 10) (p-dxf en 11))
			  )
		    )
		    ;; Pa
		  )
		)
	      )
	      (princ "\n属性W H FLR中一个或多个未定义")
	    )
	  )

	  (progn
	    (setq flr  (psk-comp-get en "FLR")
		  flr  (/ flr 3600.)
		  port (psk-comp-getports en)
		  a    (car (last (car port)))
		  b    (cadr (last (car port)))
		  ff   (psk-comp-get en "FRCF")
		  flv  (/ flr a b 1e-6)
		  den  1.2
		  k    0.15e-3
		  mu   15.06E-06
		  prl  (psk-pm flv (/ (* 2. a b) (+ a b) 1e3) mu k den)
	    )
	    (if	(null ff)
	      (setq ff 0.5)
	    )
	    (psk-comp-set
	      en
	      (list
		(cons ".FLV" flv)
		(cons "FRCF" ff)
		;; Pa/m
		(cons ".PRL" (* ff (cadr prl)))
		;; Pa
	      )
	    )
	  )
	)
      )
    )
  )
)
;;;_$ (p-interpolation 22.5 $psk-kinematic-viscosity)
;;;9.535e-007
;;;_$ (p-interpolation 101 $psk-kinematic-viscosity)
;;;nil
;;;_$ (p-interpolation -1 $psk-kinematic-viscosity)
;;;nil
(defun p-interpolation (k lst / k1 k2 r v1 v2)
  (while lst
    (if	(equal k (setq k1 (car (car lst))) 1e-6)
      (setq r	(cadr (car lst))
	    lst	nil
      )
      (if (and (> k (setq k1 (car (car lst))))
	       (< k (setq k2 (car (cadr lst))))
	  )
	(setq v1  (cadr (car lst))
	      v2  (cadr (cadr lst))
	      r	  (+ v1 (* (- v2 v1) (/ (- k k1) (- k2 k1) 1.)))
	      lst nil
	)
      )
    )

    (setq lst (cdr lst))
  )
  r
)
;; 计算沿程阻力系数
;;;v - m/s
;;;d - m
;;;mu - m2/s
;;;k - mm
;;;ro - kg/m3
;; (psk-lmd-colebrook 1. 0.050 6.03E-06 0.0005)
;; 0.0439964
;; (psk-lmd-colebrook 7. 0.390 15.06E-06 0.15e-3)
(defun psk-lmd-colebrook (v d mu k / lmd lmdtry re)
  (setq	re     (/ (* v d) mu)
	lmdtry 1.
  )

  (while (> (abs (- (setq lmd (/
				1.
				(* -2.
				   (/ (log
					(+ (/ k 3.71 d) (/ 2.5 re lmdtry))
				      )
				      (log 10.)
				   )
				)
			      )
		    )
		    lmdtry
		 )
	    )
	    1e-6
	 )
    (setq lmdtry lmd)
  )
  (* lmd lmd)
)
;; (psk-pm 1.0 0.05 6.03E-06 0.0005 990.)
;; (0.0679421 499.1 2825.82)
;; (阻力系数m 动压kg/m*s2  比摩阻Pa/m)
(defun psk-pm (v d mu k den / lmd pd pm)
  (setq	pd  (* v v den 0.5)
	lmd (psk-lmd-colebrook v d mu k)
	pm  (* (/ lmd d 1.0) pd)
  )
  (list lmd pd pm)
)



;;;(psk-analysis-export (psk-comps-ssget) '(".TYPE" "SERV" "W" "H" "D" "DN" "CLD" "FLR" ".FLV" ".PM" ".PRL"))
(defun psk-analysis-export (comps fields / r)
  (foreach comp comps
    (setq r
           (cons (p-string-connect
                   (p-stringfy
                     (cons (p-dxf (psk-comp-getename comp) 5)
                           (p-get comp fields)
                     )
                   )
                   "\t"
                 )
                 r
           )
    )
  )

  (setq r (cons (p-string-connect (p-stringfy (cons "ID" fields)) "\t") r))

  (p-list->file r "analysis.txt")
)