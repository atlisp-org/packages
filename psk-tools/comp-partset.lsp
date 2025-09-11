

;;;(psk-keypack-getitems '((".TYPE" . "AXIAL-FAN-GROUP") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300) ("L1" . 500)))
;;;(("DUCT-STOR" ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300))
;;;  ("AXIAL-FAN" ("L" . 500) ("D" . 320))
;;;  ("DUCT-STOR" ("FLIP" . 1) ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300))
;;;)
(defun psk-keypack-getitems
       (kpack / data e key keydef keyparam mapper param r)
  ;; '(("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300) ("L1" . 500))
  (setq def (psk-keypack-getdef kpack))

  (foreach key (p-get def "PACK")
    (setq ;; '("W" "H" "D" "L")
	  keyparam (mapcar 'car (psk-part-getparam (car key)))

	  key	   (cons (cons ".TYPE" (car key)) (cdr key))

	  ;; 处理属性名称映射
	  ;; '("W" "H" "D" "L1")
	  data	   (mapcar '(lambda (e)
			      (if (setq mapper (p-get key e))
				mapper
				e
			      )
			    )
			   keyparam
		   )
	  ;; 取得数值
	  data	   (p-get (cdr kpack) data)
	  key	   (p-set key (mapcar 'cons keyparam data))
    )

    (setq r (cons key r))
  )
  (reverse r)
)

;; (psk-keypack-getname '((".TYPE" . "AXIAL-FAN-GROUP")))
(defun psk-keypack-getname (kpack)
  (p-get kpack ".TYPE")
)
;; (psk-keypack-getdef '((".TYPE" . "AXIAL-FAN-GROUP")))
(defun psk-keypack-getdef (kpack)
  (p-get $psk-keypacks (psk-keypack-getname kpack))
)
;; (psk-keypack-getparam '((".TYPE" . "AXIAL-FAN-GROUP")))
(defun psk-keypack-getparam (kpack)
  (p-get (psk-keypack-getdef kpack) "PARAM")
)

;; (psk-keypack-draw '((".TYPE" . "AXIAL-FAN-GROUP") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300) ("L1" . 500)))
;;; (psk-keypack-draw (car(entsel)))
(defun psk-keypack-draw	(kpack)
  (if (= 'ename (type kpack))
    (setq kpack (psk-comp-load kpack))
  )
  (setq	r		 (p-dxf (psk-comp-getename kpack) '(10 50))
	$psk-block-base	 (car r)
	$psk-block-angle (cadr r)
  )

  (foreach key (psk-keypack-getitems kpack)
    ;; 为反向绘制的管件准备环境
    (if	(= 1 (p-get key "FLIP"))
      (setq $psk-block-base  (polar $psk-block-base
				    $psk-block-angle
				    (p-get key "L")
			     )
	    $psk-block-angle (+ $psk-block-angle pi)
      )
    )

    (psk-part-draw key)

    (if	(= 1 (p-get key "FLIP"))
      ;; 如果前一个管件为反向绘制，在此将方向翻转到正确的方向
      (setq $psk-block-angle (- $psk-block-angle pi))
      ;; 将原点移动到下一个将要绘制的管件
      (setq $psk-block-base
	     (polar $psk-block-base
		    $psk-block-angle
		    (p-get key "L")
	     )
      )
    )
  )
)
;;;
;;;(defun c:tt ()
;;;  (p-commandrun '(psk-keypack-draw))
;;;)


;; (psk-keypack-getlength '((".TYPE" . "AXIAL-FAN-GROUP") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300) ("L1" . 500)))
;;; 获取管件包对路径的占用长度
(defun psk-keypack-getlength (kpack / len)
  (setq len 0.)
  (foreach key (psk-keypack-getitems kpack)
    (setq len (+ len (p-get key "L")))
  )
  len
)
;;;
;;;
;;;(defun d-partsrc-make ()
;;;  (if (and
;;;	(setq ss (ssget ":E:S" '((0 . "LINE") (-3 ("MYPROPS_ROUTER")))))
;;;	(setq en (ssnamex ss 0)
;;;	      p	 (last (last (car en)))
;;;	      en (cadar en)
;;;	)
;;;      )
;;;    (progn
;;;      (setq p (vlax-curve-getclosestpointto en p t)
;;;	    a (p-line-getangle en)
;;;      )
;;;      (command "_.-INSERT"
;;;	       (strcat $psk-install-path "\\blocks\\sym-partsrc.dwg")
;;;	       "NON"
;;;	       p
;;;	       $PSK-iden-scale
;;;	       $PSK-iden-scale
;;;	       (p-rad->deg a)
;;;      )
;;;      (setq partsrc (entlast))
;;;      (command "_.BREAK" en "NON" p "@")
;;;      (setq l2 (entlast))
;;;      (p-entmod partsrc (assoc 8 (entget en)))




;;;(defun c:iv (/ ent)
;;;  (while (null ent)
;;;    (setq ent (entsel "\n选择要插入管件的路径:"))
;;;  )
;;;  (p-startundomark)
;;;
;;;  (setq	dn   (p-get-routerproperty (car ent) "DN")
;;;	id   (strcat "VA-FL-GLOBAL-16-" (rtos dn 2 0))
;;;	name (p-make-block-with-funcs
;;;	       "*U"
;;;	       '(0 0)
;;;	       'd-user-draw-global-valve-up
;;;	       (p-get-params
;;;		 '(flangediameter length handwheeldiameter)
;;;		 (p-xprop-get-from-csvfile
;;;		   (psk-get-filename "/config/sizes/GlobalValve J41T-16.csv")
;;;		   id
;;;		   '(flangediameter length handwheeldiameter)
;;;		   '$PSK-csvfile-cache
;;;		 )
;;;	       )
;;;	     )
;;;  )
;;;  (psk-set-customlayerbyid (getvar 'clayer))
;;;  (setq
;;;    en (p-make-insert
;;;	 name
;;;	 (setq p (vlax-curve-getclosestpointto (car ent) (cadr ent)))
;;;	 1.
;;;	 1.
;;;	 1.
;;;	 (p-line-getangle (car ent))
;;;       )
;;;  )
;;;  (p-xprop-set en "MYPROPS_PART" (list (cons "KEY" id)))
;;;  (command "_.BREAK" (car ent) "NON" p "@")
;;;
;;;  (p-endundomark)
;;;  (princ)
;;;)
;; 创建一个占用一段管道的管件包
;; (psk-keypack-createinpath (psk-paths-pick 1) "AXIAL-FAN-GROUP")
(defun psk-keypack-createinpath	(pick name / a e en keypack len	p p1 p2
				 param path ports prop
				)
  (setq	pick (car pick)
	p    (cadr pick)
	path (car pick)
  )

  (setq	keypack	(p-get $psk-keypacks name)
	desc	(p-get keypack "PARAM")
	param	(mapcar 'car desc)
	;; 将路径属性传递给管件包对象
	prop	(p-get1 path param)
	prop	(p-set prop '(("D" . 300.) ("L" . 300.) ("L1" . 300.)))
  )

  ;; 提示输入创建属性
  (setq	prop	(propertybag-edit
		  prop
		  desc
		  '$psk-sel-path-create
		  t
		)
	prop	(mapcar	'(lambda (e)
			   (if (= 'str (type (cdr e)))
			     (cons (car e) (atof (cdr e)))
			     e
			   )
			 )
			prop
		)
	keypack	(cons (cons ".TYPE" name) prop)
  )


  (setq	len   (psk-keypack-getlength keypack)
	ports (psk-path-getports path)
	ports (psk-ports-sort ports p)
	a     (psk-port-angle (cadr ports))
	p1    (polar p a len)
  )

  (setq en (psk-line-break (psk-comp-getename path) p p1))

  (setq	fit (psk-comp-create-inner
	      p
	      a
	      'p-make-line
	      (list p (polar p 0. len))
              (list (list "0"
                          '(0 0 0)
                          (polar '(0 0) a -1)
                          0
                    )
                    (list "1"
                          (mapcar '- p1 p)
                          (polar '(0 0) a 1)
                          0
                    )
              )
	      (append (list
;;;                        '(".TYPE" . "KEYPACK")
                        (p-get1 path "SERV")
                      )
		      keypack
	      )
              "PSK-PARTSET"
	    )
  )

  (if en
    (psk-comp-redraw (psk-comp-load en))
  )
  (psk-comp-redraw path)
  (psk-comp-redraw (psk-comp-load fit))
)
;;


;; (psk-keypack-set (car (entsel)) nil)
(defun psk-keypack-set (en prop /)

  (setq	kpack (psk-comp-load en)
  )

  ;; 提示输入创建属性
  (setq	kpack (propertybag-edit
		kpack
		$property-desction
		'$psk-sel-path-create
		t
	      )
	kpack (mapcar '(lambda (e)
			 (if (= 'str (type (cdr e)))
			   (cons (car e) (atof (cdr e)))
			   e
			 )
		       )
		      kpack
	      )
	len   (psk-keypack-getlength keypack)
  )

)