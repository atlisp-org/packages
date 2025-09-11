;;; 布置管道路径
(defun psk-editor-createpath (/ bst p path paths rt ss)
  (if (/= 0 (psk-createvalue-prompt))
  (while (/= "" (car paths))
    (if	(null paths)
      ;; 指定起点时的提示信息
      (progn (initget "C R W")
	     (setq p (getpoint
		       "\n指定起点或 [设置(C)/相对(R)/长宽互换(W)]:"
		     )
	     )
      )
      ;; 指定下一点时的提示信息
      (progn (initget "C S P R U")
	     (setq p
		    (getpoint
		      (trans (cadar paths) 0 1)
		      "\n指定下一个点或 [设置(C)/重新指定起点(S)/坐标(P)/相对(R)/后退(U)]:"
		    )
	     )
      )
    )
    (cond
      ((null p)
       (setq paths (cons "" paths)) ;_ 将结束绘制
      )
      ((= p "C") (psk-createvalue-prompt))
;;;      ((= p "G")
;;;       (psk-fetch-prop)
;;;      )
      ((= p "W")
       (psk-swap-w-h)
       (princ (strcat "W="
		      (rtos (p-get $psk-path-createvalue "W") 2 0)
		      ", H="
		      (rtos (p-get $psk-path-createvalue "H") 2 0)
	      )
       )
      )
;;;      ((= p "F")
;;;       (if rt
;;;	 (progn
;;;	   (setq $property-profile-name (strcase rt))
;;;	   (d-set-property-profile)
;;;	 )
;;;       )
;;;      )
      ((= p "S") (setq paths nil))
      ((= p "U")
       (setq paths (cdr paths)
	     p	   (car paths)
       )
       (command "._UNDO" "B")
      )
      ;; 用户指定了一个有效的点
      ((or (= p "P") (= p "R") (and p (listp p)))
       (cond
;;;	 ((= p "C")
;;;	  (setq	p (car (car paths))
;;;		p (dlg_xyzcreate_show p)
;;;	  )
;;;	 )
	 ((= p "R")
	  (if (setq p (getpoint "\n指定参考点:"))
	    (setq p (getpoint p "\n指定相对位置:"))
	  )
	 )
       )

       (if (and p (listp p))
	 (progn
	   (setq p (trans p 1 0))
	   (if (null paths)
	     ;; 指定第一个点
	     (progn
;;;	   (psk-createvalue-prompt)
;;;               (if (null bst)
               ;; 在指定第一个点时生成路径表及绘图列表，后续操作复用该两表以提高大图时操作速度
               ;; 操作过程中缩小平移窗口时可能造成绘图结果不准确（绘图该删未删）
               (setq $psk-drafts nil)
;;;               (if (= $psk-autoredraw "Y")
;;;                 (progn
;;;                   (princ "\n缓存对象数据...")
;;;                   (p-timer-start)
;;;
;;;                   (setq bst         (psk-comps-buildbst
;;;                                       (psk-paths-fromviewport)
;;;                                       0.1
;;;                                     )
;;;                         $psk-drafts (psk-drafts-fromviewport)
;;;                   )
;;;                   (princ (strcat "\r缓存完成, 耗时 "
;;;                                  (rtos (/ (p-timer-stop) 1000.) 2 2)
;;;                                  " s"
;;;                          )
;;;                   )
;;;                   (setq rt (p-bst-find p bst 0.1))
;;;                 )
;;;               )
               
	       (if
		 (and rt
		      (= 1 (length rt))
		 )
		  ;; 和已有路径开放管口相连
		  (setq	paths
			 (cons (list (psk-comp-load (car rt)) p) paths)
		  )
		  (setq paths (cons (list nil p) paths))
	       )
	     )
	     ;; 指定第2+点
	     (if
	       (wcmatch (p-get $psk-path-createvalue ".TYPE") "DUCT*")
		(progn
		  ;; 创建管道
		  (command "._UNDO" "M")

		  (psk-set-customlayerbyid
		    (strcat (psk-path-getservice $psk-path-createvalue)
			    "-CL"
		    )
		  )

		  (setq	path  (psk-path-create
				(cadar paths)
				p
				$psk-path-createvalue
			      )
			paths (cons (list path p) paths)
		  )
                  (if (and (= 2 (length paths))
			   (= $psk-autoredraw "Y")
		      )
		    (psk-comp-redraw1 path)
		  )

		  (if
		    (and (>= (length paths) 2)
			 (vl-consp (caar paths))
			 (vl-consp (caadr paths))
		    )
       		     (psk-paths-connect
		       (list (car paths)
			     (list (caadr paths)
				   (psk-port-pos
				     (cadr
				       (psk-ports-sort
					 (psk-comp-getports (caadr paths))
					 (cadadr paths)
				       )
				     )
				   )
			     )
		       )
		       (= $PSK-AUTOREDRAW "Y")
		     )
		  )
		)
		(progn
		  (setq
		    path (psk-editor-createpipe-inner
			   (psk-path-getservice $psk-path-createvalue)
			   (cadar paths)
			   p
			   $psk-path-createvalue
			 )
		  )
		  (setq paths (cons (list path p) paths))
		)
	     )
	   )
	 )
       )
      )
    )
  ))
  (princ)
)
;;


(defun psk-swap-w-h (/ w h)
  (if (= "DUCT-RECT" (p-get $psk-path-createvalue ".TYPE"))
    (setq w (p-get $psk-path-createvalue "W")
	  h (p-get $psk-path-createvalue "H")
	  $psk-path-createvalue
	   (p-set $psk-path-createvalue (cons "W" h))
	  $psk-path-createvalue
	   (p-set $psk-path-createvalue (cons "H" w))
    )
  )
)
(setq $psk-createpipe-serv "CHS")

(defun psk-editor-createpipe-inner (serv p1 p2 prop / tp)

  (if (= serv "REF")
    (setq tp "REFPIPE")
    (setq tp "PIPE")
  )
  (psk-set-customlayerbyid
    (strcat serv "-PIPE")
  )
  (psk-path-create
    p1
    p2
    (p-set prop
	   (list (cons ".TYPE" tp)
		 (cons "SERV" serv)
	   )
    )
  )
)

(defun psk-editor-createpipe (/ p1 p2 prop tp)
  (if
    (and
      (setq $psk-createpipe-serv
	     (p-getkword1
	       "选择服务介质"
;;;	       $psk-services
	       (mapcar
		 (function
		   (lambda (e)
		     (list (car e) (strcat (caddr e) " " (cadr e)))
		   )
		 )
		 $psk-services
	       )
	       $psk-createpipe-serv
	     )
      )
      (setq p1 (getpoint "\n指定起点: "))
      (setq p2 (getpoint p1 "\n指定终点: "))
    )

     (psk-editor-createpipe-inner
       $psk-createpipe-serv
       (trans p1 1 0)
       (trans p2 1 0)
       nil
     )
  )
)
;;; (psk-create-pipegroup '("CHS" "CHR" "CD") 300)
;;; (psk-create-pipegroup '("REF" "CD") 300)
(defun psk-create-pipegroup (servs offset / a p1 p2)
  (if
    (and
      (setq p1 (getpoint "\n指定起点: "))
      (setq p2 (getpoint p1 "\n指定终点: "))
    )
     (progn
       (setq p1	(trans p1 1 0)
	     p2	(trans p2 1 0)
	     a	(+ $pi/2 (angle p1 p2))
       )
       (foreach	serv servs
	 (psk-editor-createpipe-inner serv p1 p2 nil)
	 (setq p1 (polar p1 a offset)
	       p2 (polar p2 a offset)
	 )
       )
     )
  )
)
;;;(psk-editor-createduct-inner "EA" "DUCT-RECT")
;;;(psk-editor-createduct-inner "EA" "DUCT-ROUND")
(defun psk-editor-createduct-inner (serv tp / p1 p2)
  (if
    (and
      (setq p1 (getpoint "\n指定起点: "))
      (setq p2 (getpoint p1 "\n指定终点: "))
    )
     (progn
       (psk-set-customlayerbyid
	 (strcat serv "-CL")
       )
       (psk-path-create
	 (trans p1 1 0)
	 (trans p2 1 0)
	 (list (cons ".TYPE" tp)
	       (cons "SERV" serv)
	 )
       )
     )
  )
)
(setq $psk-createduct-serv "EA")
(defun psk-editor-createduct (/)
  (setq	$psk-createduct-serv
	 (p-getkword1
	   "选择服务介质"
	   $psk-services-duct
	   $psk-createduct-serv
	 )
  )
  (psk-editor-createduct-inner
    $psk-createduct-serv
    "DUCT-RECT"
  )
)
(defun psk-editor-createductr (/)
  (setq	$psk-createduct-serv
	 (p-getkword1
	   "选择服务介质"
	   $psk-services-duct
	   $psk-createduct-serv
	 )
  )
  (psk-editor-createduct-inner
    $psk-createduct-serv
    "DUCT-ROUND"
  )
)
(defun psk-editor-changeprop (/ comp pname ss value)
  (if
    (and
;;;      (setq ss (psk-comps-ssget1))
      (setq ss (ssget '((0 . "LINE"))))
      (setq pname (p-getkword1
		    "选择要修改的属性"
		    '(("FLR" "流量")
		      ("CLD" "冷负荷")
		      ("HLD" "热负荷")
		      ("TD" "温差")
		      ("DN" "公称直径")
		      ("SERV" "服务介质")
		      (".TYPE" "管道类型")
		      ("FRCF" "局部阻力系数")
		     )
		    "CLD"
		  )
      )
      (setq value (p-edit-value
		    "\n输入参数值"
		    (cond ((= pname "SERV")
			   "CS"
			  )
			  ((= pname ".TYPE")
			   "PIPE"
			  )
			  (t
			   5.0
			  )
		    )
		  )
      )
    )
     (progn
       (foreach	en (p-ss->enames ss)
	 (setq comp (psk-comp-load en))
	 (psk-comp-set en (list (cons pname value)))
	 (if
	   (and	(= pname "SERV")
		(/= value (p-get comp "SERV"))
	   )
	    (progn
	      (psk-set-customlayerbyid
		(if (wcmatch (p-get comp ".TYPE") "DUCT*")
		  (strcat value "-CL")
		  (strcat value "-PIPE")
		)
	      )
	      (vla-put-layer
		(p-ensure-object en)
		$addnew-layer
	      )
	    )
	 )
       )
     )
  )
)

;; 将指定直线设置为管口
(defun c:port (/ en serv) 
  (if 
    (and 
      (setq en (entsel "\n选择接管:"))
;;;      (setq serv (p-getkword1 
;;;                   "选择服务介质"
;;;                   $psk-services
;;;                   "CHS"
;;;                 )
;;;      )
      (setq serv (getstring 
                   "选择服务介质"
                   )
      )
    )
    (p-xprop-set 
      (car en)
      "PSK-PORT"
      (list (cons "SERV" serv))
    )
;;;    (p-xdata-set
;;;      (car en)
;;;      "PSK-PORT"
;;;      (list (cons 1010 '(0 0 0))
;;;	    (cons 1013 (polar '(0 0) a -1.))
;;;	    (psk-port-packsize 0)
;;;	    (cons 1013 (polar '(0 0) a len))
;;;	    (cons 1013 (polar '(0 0) a 1.))
;;;	    (psk-port-packsize 0)
;;;      )
;;;    )
  )
)
;;(p-get-ports (car (entsel)))
(defun p-get-ports (en / name r sys)
  (setq	name (p-dxf en 2)
	r    (p-get $p-ports-cache name)
  )
  (if (null r)
    (progn
      (vlax-for	e (p-get-block name)
	(setq e (p-ensure-ename e))
	(if (setq sys (p-xprop-get e "PSK-PORT" "SERV"))
	  (setq r (cons (cons sys (p-dxf e '(10 11))) r))
	)
      )
      (setq $p-ports-cache (cons (cons name r) $p-ports-cache))
    )
  )
  r
)
(setq $p-ports-cache nil)
;; 获取块实例中管口直线起终点
;; (p-get-portsforinsert (car (entsel)))
(defun p-get-portsforinsert (en / geom p ports tr)
  (defun tr (e / p1 p2)
    (setq p1 (p-block-trans (cadr e) geom)
	  p2 (p-block-trans (caddr e) geom)
    )
    (list (car e)
	  (mapcar '- p1 p)
	  (p-vector-from2p p2 p1)
	  0
    )
  )

  (setq	geom (p-refgeom en)
	p    (p-dxf en 10)
  )
  (if (setq ports (p-get-ports en))
    (setq ports (mapcar (function tr) ports))
  )
)
(defun c:recreateports ()
  (if (and (setq ss (ssget '((0 . "INSERT") (-3 ("PSK-EQUIP"))))))
    (progn
      (foreach en (p-ss->enames ss)
	(psk-ports-store en (p-get-portsforinsert en))
      )
    )
  )
)
;;; 自动连接管口与干管
(defun psk-connect-ports (/	a     dxf   en	  ints	line  n
			  p	p1    p2    p3	  p4	port  ports
			  ss	offset	    remain	sys
			 )
  (princ "选择块\n")
  (if (and (setq ss (ssget '((0 . "INSERT") (-3 ("PSK-PORT"))))))
    (progn
      (foreach en (p-ss->enames ss)
	(setq ports (append ports (psk-fit-getports en)))
      )
      (setq offset 150.)

      (while
	(and ports
	     (setq line (entsel "选择主管:"))
	)
	 (setq p    (trans (cadr line) 1 0)
	       line (car line)
	       a    (p-line-getangle line)
	       p    (cdr (p-line-getendnear line p))
	       dxf  (entget line '("*"))
	       dxf  (vl-remove-if '(lambda (e) (member e '(5 10 11))) dxf)
	       sys  (p-xprop-get line "PSK-PATH" "SERV")
	 )

	 ;; 绘制每个到设备的支管
	 (foreach port ports
	   (if (= sys (car port))
	     (progn
	       (setq p1	  (psk-port-pos port)
		     p3	  (polar p1 (psk-port-angle port) offset)
		     p4	  (vlax-curve-getclosestpointto line p3 t)
		     ints (cons (cons p4 (distance p4 p)) ints)
	       )
	       (entmake
		 (append dxf
			 (list (cons 10 p1) (cons 11 p3))
		 )
	       )
	       (entmake
		 (append dxf
			 (list (cons 10 p3) (cons 11 p4))
		 )
	       )

	       (setq
		 ints (vl-sort ints
			       '(lambda (e1 e2) (< (cdr e1) (cdr e2)))
		      )
	       )
	     )
	     (progn
	       (setq remain (cons port remain))
	     )
	   )
	 )

	 ;; 干管与每个支管交点处打断
	 (entdel line)
	 (while	ints
	   (if (> (distance p (caar ints)) 1.)
	     (entmake
	       (append dxf
		       (list (cons 10 p) (cons 11 (caar ints)))
	       )
	     )
	   )
	   (setq p    (caar ints)
		 ints (cdr ints)
	   )
	 )
	 (setq ports  remain
	       remain nil
	       offset (+ 150. offset)
	 )
      )
    )
  )
)
;;;
;;;_$ (get-near-number 12700 '(7100 12500 14000) 0.1 0)
;;;12500
;;; 返回'(7100 12500 14000)列表中最接近12700的数，允许返回的数比指定的数小 10 % 或大 0 %
;;;(defun get-near-number (n nl lt gt / l g r)
;;;  (setq	l (* n (- 1.0 lt))
;;;	g (* n (+ 1.0 gt))
;;;  )
;;;  (while nl
;;;    (if	(and (>= (car nl) l) (<= (car nl) g))
;;;      (setq r  (car nl)
;;;	    nl nil
;;;      )
;;;    )
;;;    (setq nl (cdr nl))
;;;  )
;;;  r
;;;)


;;;(defun d-insert-router (p func params layer props / en name old rs)
;;;  (setq	old (p-make-setenv
;;;	      (list "0" 0 "ByBlock" -2 nil p)
;;;	    )
;;;  )
;;;
;;;  (setq name (p-make-block "*U" func params))
;;;
;;;  (p-make-setenv
;;;    (list layer
;;;	  256
;;;	  "ByLayer"
;;;	  -1
;;;    )
;;;  )
;;;
;;;  (setq en (p-make-insert name p 1. 1. 1. 0.))
;;;  (p-set-routerproperty en props)
;;;  (p-make-setenv old)
;;;)



;;;(defun d-remove-router (/)
;;;
;;;
;;;  (while (setq td (ssget ":E:S" '((-3 ("PSK-FIT")))))
;;;    (setq ss (ssget "W"
;;;		    (trans (getvar "EXTMIN") 0 1)
;;;		    (trans (getvar "EXTMAX") 0 1)
;;;		    '((-3 ("PSK-FIT")))
;;;	     )
;;;    )
;;;
;;;    (setq bst (p-build-bstforlines ss $PSK-router-fuzz))
;;;
;;;    (setq td (ssname td 0))
;;;    (setq p (cdr (assoc 10 (entget td))))
;;;    (setq rs (p-find-bst p bst $PSK-router-fuzz))
;;;
;;;    (setq p (cdr (assoc 11 (entget td))))
;;;    (setq rs (append rs (p-find-bst p bst $PSK-router-fuzz)))
;;;
;;;    (setq rs (vl-remove-if '(lambda (e) (equal e td)) rs))
;;;
;;;    (setq p (p-line-getinters (car rs) (last rs)))
;;;
;;;    (if	p
;;;      (progn
;;;	(entdel td)
;;;	(d-set-lineendpoint (car rs) p p)
;;;	(d-set-lineendpoint (last rs) p p)
;;;      )
;;;    )
;;;  )
;;;)





;;
(setq $psk-alignmode "C")
(defun psk-editor-getalignmode (/ r)
  (initget "L R C")
  (if
    (setq
      r	(getkword
	  (strcat "\n选择偏心管件对齐方式 [左(L)/居中(C)/右(R)]: <"
		  $psk-alignmode
		  ">"
	  )
	)
    )
     (setq $psk-alignmode r)
  )
  $psk-alignmode
)
;;




;;; BOOKMARK 变量处理辅助函数
(defun psk-propbox-change (name /)
  (propbox-update
    (psk-get-createvalue name)
    (psk-get-desc name)
  )
)
;;;_$ (psk-get-createvalue "PIPE")
;;;((".TYPE" . "PIPE") ("SERV" . "CHS") ("DN" . 100.0) ("ERF" . 0.8) ("EL") ("AL"))
;;;_$ (psk-get-createvalue "DUCT-RECT")
;;;((".TYPE" . "DUCT-RECT") ("SERV" . "SE") ("W" . 500) ("H" . 320) ("ERF" . 0.8) ("EL") ("AL"))
(defun psk-get-createvalue (tp / e r v)
  (p-get $psk-path-createvaluelast tp)
;;;  (setq r                         (p-get $psk-path-createvaluelast tp) 
;;;        r                         (mapcar (function list) r) 
;;;        $psk-path-createvaluelast (p-set 
;;;                                    $psk-path-createvaluelast
;;;                                    (cons ".TYPE" tp)
;;;                                  )
;;;        r                         (mapcar 
;;;                                    (function 
;;;                                      (lambda (e) 
;;;                                        (if 
;;;                                          (setq v (p-get 
;;;                                                    $psk-path-createvaluelast
;;;                                                    (car e)
;;;                                                  )
;;;                                          )
;;;                                          (cons (car e) v)
;;;                                          e
;;;                                        )
;;;                                      )
;;;                                    )
;;;                                    r
;;;                                  )
;;;  )
;;;  (if (wcmatch tp "DUCT*") 
;;;    (p-set 
;;;      r
;;;      (cons "SERV" (p-get $psk-path-createvaluelast "SERVD"))
;;;    )
;;;    r
;;;  )
)
;;(psk-get-desc "PIPE")
;;(psk-get-desc "DUCT-RECT")
(defun psk-get-desc (tp / descs)
  (setq descs (mapcar 'car (psk-get-createvalue tp)))

  (if descs
    (setq
      descs (p-get1 $psk-prop-def descs)
      descs (mapcar
              (function
                (lambda (e)
                  (cond
                    ((wcmatch (car e) "W,H")
                     (append e (list $psk-ductrect-sizes))
                    )
                    ((wcmatch (car e) "D")
                     (append e (list $psk-ductround-sizes))
                    )
                    ((wcmatch (car e) "DN")
                     (append e (list $psk-pipe-sizes))
                    )
                    ((wcmatch (car e) "ERF")
                     (append e (list '(0.6 0.8 1.0 1.5 2.0)))
                    )
                    ((wcmatch (car e) "AL")
                     (append e
                             (list '(("" "")
                                     ("T" "顶")
                                     ("C" "中心")
                                     ("B" "底")
                                    )
                             )
                     )
                    )
                    ((wcmatch (car e) ".FLV")
                     (append
                       e
                       (list
                         nil
                         (cond
                           ((= tp "DUCT-RECT")
                            "(/ {FLR} {H} {W} 3600. 1e-6)"
                           )
                           ((= tp "DUCT-ROUND")
                            "(/ {FLR} 3600. (/ (* pi {D} {D}) 4.e6))"
                           )
                           ((= tp "PIPE")
                            "(/ {FLR} 3600. (/ (* pi {DN} {DN}) 4.e6))"
                           )
                         )
                       )
                     )
                    )
                    ((wcmatch (car e) ".TYPE")
                     (append e
                             (list
                               '(("DUCT-RECT"
                                  "矩形风管"
                                  "(psk-propbox-change \"DUCT-RECT\")"
                                 )
                                 ("DUCT-ROUND"
                                  "圆形风管"
                                  "(psk-propbox-change \"DUCT-ROUND\")"
                                 )
                                 ("PIPE"
                                  "水管"
                                  "(psk-propbox-change \"PIPE\")"
                                 )
                                )
                             )
                     )
                    )
                    ((wcmatch (car e) "SERV")
                     (append
		       e
		       (if (and	tp
				(wcmatch tp "DUCT*")
			   )
;;;                               (list $psk-services-duct)
;;;                               (list $psk-services)
			 (list (mapcar
				 (function
				   (lambda (e) (list (car e) (strcat (cadr e) " "(caddr e))))
				 )
				 $psk-services-duct
			       )
			 )
			 (list (mapcar
				 (function
				   (lambda (e) (list (car e) (strcat (cadr e) " "(caddr e))))
				 )
				 $psk-services
			       )
			 )
		       )
		     )
                    )
                    ((wcmatch (car e) "MAT")
                     (append e
                             '((("" "")
                                ("GS" "镀锌钢板")
                                ("SS" "不锈钢板")
                                ("AL" "铝板")
                                ("UPVC" "硬聚氯乙烯")
                                ("PMMA" "有机玻璃钢")
                                ("FRP" "无机玻璃钢")
                               )
                              )
                     )
                    )
                    (t
                     e
                    )
                  )
                )
              )
              descs
            )
    )
  )
  descs
)


(defun psk-fetch-prop (/ comp r)
  (if (setq r (psk-paths-pick 1))
    (progn (setq comp  (caar r)
		 props (cdr comp)
	   )
	   (propertybag-init)
    )
  )
)

(defun propertybag-edit1 (props	    _desc     _selkey	returnwhole
			  /	    _props    _changes	_cursel
			  _options  id	      rt
			 )
  (pdb-dlg-show
    (psk-get-filename "propertyedit.dcl")
    "psk_createpath"
    ;; set tile
    (lambda ()
      (propertybag-init)
      (action_tile "FETCH" "(done_dialog 4)")
      (action_tile "DUCTV" "(done_dialog 5)")
    )
    ;; on dialog result
    (lambda (rt)
      (if (= rt 4)
	(psk-fetch-prop)
      )

      (if (= rt 5)
        (progn
          (setq comp (p-set _props _changes))
          (if (psk-path-getservice comp)
            (progn
              (if (wcmatch (psk-comp-gettype comp) "DUCT*")
                (psk-set-customlayerbyid
                  (strcat (psk-path-getservice comp) "-DUCT")
                )
                (psk-set-customlayerbyid
                  (strcat (psk-path-getservice comp) "-PIPE")
                )
              )
            )
          )
          
          (setq old (p-setvars (list (cons 'clayer $addnew-layer))))
          (if (wcmatch (psk-comp-gettype comp) "DUCT*")
            (psk-create-ductvert
              (if (wcmatch (p-get comp "SERV") "SA,OA,RS,PS,MA,S,X,JY,XB,S(B)")
                "SAD-V"
                "RAD-V"
              )
              (getpoint "\n指定插入点:")
              (p-get comp "W")
              (p-get comp "H")
            )
            (psk-create-ductvert
              "PIPE-V"
              (getpoint "\n指定插入点:")
              (p-get comp "DN")
              (p-get comp "DN")
            )
          )
          (p-setvars old)
        )
      )
      
      (list rt (propertybag-result rt))
    )
  )
)




(defun psk-createvalue-prompt (/ change k)
  (setq	$psk-path-createvalue
	 (psk-get-createvalue $psk-path-createtype)
  )
  (setq	change (propertybag-edit1
		 $psk-path-createvalue
		 (psk-get-desc $psk-path-createtype)
		 '$psk-sel-path-create
		 t
	       )
  )

  (if (/= 0 (car change))
    (if	(setq change (cadr change))
      (progn
	(if (setq k (p-get change ".TYPE"))
	  (progn
	    (setq $psk-path-createtype
		   k
		  $psk-path-createvalue change
;;;		  $psk-path-createvalue
;;;		   (psk-get-createvalue $psk-path-createtype)
	    )
	  )
	)
;;;	(setq $psk-path-createvalue
;;;	       (p-set $psk-path-createvalue change)
;;;	)

	;; 保存历史记录方便下次使用
	(setq $psk-path-createvaluelast
	       (p-set
		 $psk-path-createvaluelast
		 (list (cons $psk-path-createtype $psk-path-createvalue))
	       )
	)

	(vlax-ldata-put
	  "PSK"
	  "CREATEVALUELAST"
	  $psk-path-createvaluelast
	)
	$psk-path-createvalue
      )
    )
    0 ;_ 用户取消了对话框
  )
)
;;

(defun psk-comps-modify	(/ comps r rt serv v)
  (if (and (setq comps (psk-comps-ssget)))
    (progn
      (foreach comp comps
	(setq comp (cdr comp)) ;_ 去掉-1 实体名
	(if (null r)
	  (setq r comp)
	  (progn
	    (foreach e comp
	      (setq v (p-get r (car e)))
	      (if (null v)
		(setq r (p-set r e))
		(if (not (equal v (cdr e)))
		  (setq r (p-set r (cons (car e) "*多种*")))
		)
	      )
	    )
	  )
	)
      )
      (if (= "*多种*" (p-get r ".TYPE"))
	(princ "\n暂不支持不同类别管件批量编辑")
	(if
	  (/= 0
	      (setq rt (propertybag-edit
			 r
			 (if (= "PART" (psk-comp-getname (car comps)))
                           (psk-get-desc "PART")
                           (psk-get-desc (p-get r ".TYPE"))
                           )
			 nil
			 nil
		       )
	      )
	  )
	   (progn
	     (foreach comp comps
               ;; 控制类型修改将控制线放置到相应的图层中
               (if
                 (and (setq serv (p-get rt "SERV"))
                      (/= serv (p-get comp "SERV"))
                 )

                  (progn
                    (psk-set-customlayerbyid
                      (if
                        (wcmatch (p-get comp ".TYPE") "DUCT*")
                         (strcat serv "-CL")
                         (strcat serv "-PIPE")
                      )
                    )
                    (vla-put-layer
                      (p-ensure-object (psk-comp-getename comp))
                      $addnew-layer
                    )
                  )
               )
               ;; D DN W 属性设置额外处理TODO
               (psk-comp-set (psk-comp-getename comp) rt)

               (if
                 (and (setq v (psk-comp-gettype comp))
                      (wcmatch v "DUCT-RECT,DUCT-ROUND")
                 )
                  (progn
                    (setq $psk-drafts (psk-drafts-fromviewport))
                    (psk-comp-redraw1
                      (psk-comp-load (psk-comp-getename comp))
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
;;