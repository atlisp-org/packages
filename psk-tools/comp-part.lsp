

;; (setq key (psk-part-create "DUCT-STOR" '(("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300))))
;;;(defun psk-part-create (en key /)
;;;  (cons (cons ".TYPE" name) prop)
;;;)

(defun psk-part-getdef (name)
  (p-get $psk-keys name)
)

;;;_$ (psk-part-gettype (psk-part-getdef "SS"))
;;;"ATTACH"
(defun psk-part-gettype (keydef)
  (p-get keydef ".TYPE")
)

;;;_$ (psk-part-getdrawfunc (psk-part-getdef "SS"))
;;;PSK-DRAW-SINGLESHUTTER
(defun psk-part-getdrawer (keydef)
  (p-get keydef "DRAWER")
)

;;;_$ (psk-part-getdrawparam (psk-part-getdef "SS"))
;;;("A" "B")
(defun psk-part-getdrawparam (keydef)
  (cadr (p-get keydef "DRAWER"))
)

(defun psk-part-getparam (keydef)
  (mapcar 'car (p-get keydef "PARAM"))
)

;; (psk-part-getkeytemplate (psk-part-getdef key))
(defun psk-part-getkeytemplate (keydef)
  (p-get keydef "KEY")
)

;; (psk-part-evalkey '((".TYPE" . "DUCT-STOR") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300)))
(defun psk-part-evalkey (key / keydef templ)
  (setq keydef (psk-part-getdef (p-get key ".TYPE"))
        templ  (psk-part-getkeytemplate keydef)
  )
  (p-template-eval templ key)
)


;; (setq $psk-block-base '(0 0) $psk-block-angle 0.)
;; (psk-part-draw '((".TYPE" . "DUCT-STOR") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300)))
(defun psk-part-draw (key / e func keydef param)
;;;  (psk-set-customlayerbyid
;;;    (strcat (p-get key "SERV") "-DUCT")
;;;  )

  (setq def (psk-part-getdef (p-get key ".TYPE")))

  (if (null def)
    (setq def (p-get $psk-pipe-parts (p-get key ".TYPE")))
  )

  (setq templ    (p-get def "KEY")
        sizefile (p-get def "SIZEFILE")
        drawer   (car (p-get def "DRAWER"))
        func     (car drawer)
        param    (cdr drawer)
  )

  (if (and templ sizefile)
    (setq key (append key (psk-csvfile-get sizefile (p-template-eval templ key))))
  )

  ;; ("W" 50) 处理 字符进行求值 数字不处理
  (setq param (mapcar '(lambda (e)
                         (if (= 'str (type e))
                           (p-get key e)
                           (if (= 'sym (type e))
                             (vl-symbol-value e)
                             e
                           )
                         )
                       )
                      param
              )
  )

  (if (vl-catch-all-error-p (vl-catch-all-apply func param))
    (princ (strcat "\nerror in psk-part-draw "
                   (vl-princ-to-string (cons func param))
           )
    )
  )
;;;  (p-xdata-set en "PSK-DRAWID" (list uid))
;;;  (setq $addnew-xdata nil)
)
;;
(defun psk-part-drawblock (key / name)
  (setq $psk-block-base  '(0 0)
        $psk-block-angle 0.
  )
  (p-make-block "*U" 'psk-part-draw (list key))
)
;; (psk-part-createblock (getpoint) 0. '((".TYPE" . "DUCT-STOR") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300)))
;; (psk-part-createblock (getpoint) 0. '((".TYPE" . "DS") ("A" . 500) ("B" . 250)))
;; (psk-part-createblock (getpoint) 0. '((".TYPE" . "AXIAL-FAN") ("D" . 320) ("L" . 320)))
(defun psk-part-createblock (p a key ktype en / name r)
  (setq r (p-make-setenv $addnew-default))

  (setq name (psk-part-drawblock key))

  (p-make-setenv r)

  ;; 创建新块或修改已有块
  (if (null en)
    (setq en (p-make-insert name p 1. 1. 1. a))
    (p-entmod en (cons 2 name))
  )

  (cond ((= "ATTACH" ktype)
         (psk-ports-store
           en
           (list (list "0" '(0 0 0) '(0 0 1) 0)
           )
         )
        )
        ((= "INLINE" ktype)
         (setq len (p-get key "L"))
         (psk-ports-store
           en
           (list (list "0" '(0 0 0) (polar '(0 0) a -1.) 0)
                 (list "1" (polar '(0 0) a len) (polar '(0 0) a 1.) 0)
           )
         )
        )
        ((= "END" ktype)
         (psk-ports-store
           en
           (list (list "0" '(0 0 0) (polar '(0 0) a 1.) 0)
           )
         )
        )
  )

  (p-xprop-set en "PSK-PART" key)
  en
)
;;


;;; 在路径上插入实体部件(管件或管件包)
;;;(defun d-insert-pack (router p func params / en name old rs)
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
;;;
;;;  (command "_.BREAK" en p "@")
;;;  (p-xprop-set en "MYPROPS_PACK" props)
;;;  (p-make-setenv old)
;;;)

;;;_$ (psk-part-getnames)
;;;("DUCT-STOR" "AXIAL-FAN" "FDC-70" "CAP" "DS" "FL-PL")
(defun psk-part-getnames ()
  (mapcar 'car $psk-keys)
)
;;;_$ (psk-part-gettext "DS")
;;;"双层百叶风口"
(defun psk-part-gettext (name)
  (p-get (p-get $psk-keys name) "DESC")
)
;;;_$ (psk-part-getparam "DS")
;;;"双层百叶风口"
(defun psk-part-getparam (name)
  (p-get (p-get $psk-keys name) "PARAM")
)
;;;_$ (psk-part-getdesc "DS")
;;;((".TYPE"...) ("A" 1070 "风口长度") ("B" 1070 "风口宽度"))
(defun psk-part-getdesc (name)
  (append (list
            (list ".TYPE"
                  1000
                  "件号"
                  "NAME\n"
                  (mapcar
                    (function
                      (lambda (e)
                        (list e
                              (psk-part-gettext e)
                              (strcat "(psk-part-change \"" e "\")")
                        )
                      )
                    )
                    (psk-part-getnames)
                  )
            )
          )
          (psk-part-getparam name)
  )
)
(defun psk-part-getcreatevalues (name / r)
  (if (setq r (p-get $psk-part-lastvalues name))
    r
    nil
  )
)
(defun psk-part-change (name /)
  (propbox-update
    (psk-part-getcreatevalues name)
    (psk-part-getdesc name)
  )
)
(setq $psk-part-lastname   (vlax-ldata-get "PSK" "$psk-part-LASTNAME")
      $psk-part-lastvalues (vlax-ldata-get "PSK" "$psk-part-LASTVALUES")
)
(if (null $psk-part-lastname)
  (setq $psk-part-lastname "DS")
)
(defun psk-part-createvalue-prompt (/ change)
  (setq change (propertybag-edit
                 (psk-part-getcreatevalues $psk-part-lastname)
                 (psk-part-getdesc $psk-part-lastname)
                 nil
                 t
               )
  )

  (if (/= 0 change)
    (if change
      (progn
        (setq $psk-part-lastname   (p-get change ".TYPE")
              $psk-part-lastvalues (p-set $psk-part-lastvalues
                                          (cons $psk-part-lastname change)
                                   )
        )
        (vlax-ldata-put
          "PSK"
          "$psk-part-LASTNAME"
          $psk-part-lastname
        )
        (vlax-ldata-put
          "PSK"
          "$psk-part-LASTVALUES"
          $psk-part-lastvalues
        )
        change
      )
    )
  )
)
;; (psk-selectd '("A" "B"))
;;;_$ (psk-selectd '(("A" . "A desc..") ("B" . "B desc..")))
;;;("B" . "B desc..")
(defun psk-selectd (lst selkey / cursel id r)
  (if
    (and (>= (setq id (load_dialog (psk-get-filename "\\propertyedit.dcl")))
             0
         )
         (new_dialog "psk_selectbox" id)
    )
     (progn
       (start_list "LIST")
       (mapcar (function (lambda (e)
                           (if (atom e)
                             (add_list e)
                             (add_list (cdr e))
                           )
                         )
               )
               lst
       )
       (end_list)

       (if selkey
         (setq cursel (vl-position (assoc selkey lst) lst))
         (setq cursel 0)
       )
       (set_tile "LIST" (itoa cursel))
       (action_tile "LIST" "(setq cursel (atoi $value)) (if (= 4 $reason) (done_dialog 1))")
       (action_tile "OK" "(done_dialog 1)")

       (setq r (start_dialog))
       (unload_dialog id)

       (if (= r 1)
         (nth cursel lst)
       )
     )
  )
)
(defun psk-prop-prompt (prop desc / r)
  (setq
    prop (mapcar (function (lambda (e)
                             (if (null (p-get prop (car e)))
                               (cons (car e)
                                     (cond ((= 1040 (cadr e)) 0.)
                                           ((= 1070 (cadr e)) 0)
                                     )
                               )
                               (p-get1 prop (car e))
                             )
                           )
                 )
                 desc
         )
  )
  (foreach v desc
    (initget 6) ;_ >=0
    (setq r (p-edit-value (strcat "\n" (caddr v)) (p-get prop (car v))))
    (setq prop (p-set1 prop (car v) r))
  )

  prop
)
(defun psk-posangle-prompt (path p / a do line p2 ports)
  (setq line (psk-comp-getename path)
        p    (p-line-closestpoint line p t)
        do   t
  )

  (while do
    (initget "N")
    (setq p2 (getpoint p "\n指定管件方向或 [选择最近管端(N)]:"))

    (cond
      ((= "N" p2)
       (setq ports (psk-path-getports path)
             ports (psk-ports-sort ports p)
             a     (psk-port-angle (car ports))
             p     (psk-port-pos (car ports))
             do    nil
       )
      )
      (p2
       (setq p2 (p-line-closestpoint line p2 t)
             a  (angle p p2)
             do nil
       )
      )
    )
  )
  (list p a)
)
(defun psk-part-create (/ keydef p param paramnames path prop tp)
  (if (and (setq path (psk-paths-pick 1))
           (setq
             ;; (("DUCT-STOR" . "方圆变径") ...)
             $psk-part-lastname
              (car
                (psk-selectd
                  (mapcar
                    (function
                      (lambda (e) (cons (car e) (p-get (cdr e) "DESC")))
                    )
                    $psk-keys
                  )
                  $psk-part-lastname
                )
              )
           )
      )
    (progn
      (setq keydef     (p-get $psk-keys $psk-part-lastname)
            tp         (p-get keydef "TYPE")
            param      (p-get keydef "PARAM")
            paramnames (mapcar 'car param)

            ;; 将路径属性传递给管件包对象
            prop       (psk-part-getcreatevalues $psk-part-lastname)
      )

      (foreach e (p-get1 (caar path) paramnames)
        (if (cdr e)
          (setq prop (p-set prop e))
        )
      )

      (setq prop                 (psk-prop-prompt prop param)
            $psk-part-lastvalues (p-set $psk-part-lastvalues
                                        (cons $psk-part-lastname prop)
                                 )
      )
      (vlax-ldata-put
        "PSK"
        "$psk-part-LASTNAME"
        $psk-part-lastname
      )
      (vlax-ldata-put
        "PSK"
        "$psk-part-LASTVALUES"
        $psk-part-lastvalues
      )

      (setq prop (cons (cons ".TYPE" $psk-part-lastname) prop))
      (psk-set-customlayerbyid
        (strcat (psk-path-getservice (caar path)) "-CPNT")
      )

      (cond ((= "ATTACH" tp)
             (psk-path-createattach (caar path) (cadar path) prop)
            )
            ((= "INLINE" tp)
             (setq p (psk-posangle-prompt (caar path) (cadar path)))

             (psk-path-createinline (caar path) (car p) (cadr p) prop)
            )
            ((= "END" tp)
             (psk-path-createend (caar path) (cadar path) prop)
            )
      )
    )
  )

  (princ)
)
;;
(defun c:iv ()
  (p-commandrun '(psk-part-create))
)


;;;(defun psk-part-remove (/ key)
;;;
;;;  (while (null
;;;	   (setq key (ssget ":E:S" '((0 . "INSERT") (-3 ("PSK-PART")))))
;;;	 )
;;;  )
;;;
;;;  (setq key (ssname path 0))




;;;  (if (and (setq path (psk-paths-pick 1))
;;;	   (setq prop (psk-part-createvalue-prompt))
;;;;;;	   (setq p (getpoint "指定插入点"))
;;;      )
;;;    (psk-path-createattach
;;;      (caar path)
;;;      (p-line-closestpoint
;;;	(psk-comp-getename (caar path))
;;;	(cadar path)
;;;	t
;;;      )
;;;      prop
;;;    )
;;;  )

;;;  (princ)
;;;)
;; (psk-part-set (car (entsel)) '("A" . 1000))
(defun psk-part-set (en prop / a p)
  (setq prop (p-set (p-xprop-getall en "PSK-PART") prop)
        p    (p-dxf en 10)
        a    (p-dxf en 50)
  )

  (psk-part-createblock
    p
    a
    prop
    (psk-part-gettype (psk-part-getdef (p-get prop ".TYPE")))
    en
  )
)
;;


;;;(psk-part-evalkey '((".TYPE" . "FL-PL") ("PN" . "2.5") ("DN" . 100)))

;;;(defun psk-csv-readline	(file)
;;;  (vl-string-trim " \t\n," (read-line file))
;;;)
;;;_$ (p-string-tokenize "OD:0-FlangeDiameter" ":-")
;;;("OD" "0" "FlangeDiameter")
;;;_$ (p-string-tokenize "BoltArrangeDiameter" ":-")
;;;("BoltArrangeDiameter")
;; (psk-part-fromcsvfile "D:/Profile/desktop/dd3/bin/config/sizes/Flange PN.csv" "FL-PL-2.5-700")
;;;(defun psk-part-fromcsvfile (filename key / e e1 e2 file head line r wc)
;;;  (if (setq file (open filename "r"))
;;;    (progn
;;;      (setq head (psk-csv-readline file)
;;;	    head (p-string-tokenize head ",")
;;;	    head (mapcar '(lambda (e) (p-string-tokenize e ":-")) head)
;;;	    line (psk-csv-readline file)
;;;	    wc	 (strcat key "*")
;;;      )
;;;
;;;      (while line
;;;	(if (wcmatch line wc)
;;;	  (setq	line (p-string-tokenize line ",")
;;;		r    (mapcar '(lambda (e1 e2)
;;;				(cons (car e1)
;;;				      (cond ((= "0" (cadr e1))
;;;					     (atoi e2)
;;;					    )
;;;					    ((= "1" (cadr e1))
;;;					     (atof e2)
;;;					    )
;;;					    (t
;;;					     e2
;;;					    )
;;;				      )
;;;				)
;;;			      )
;;;			     head
;;;			     line
;;;		     )
;;;		line nil
;;;	  )
;;;	  (setq line (psk-csv-readline file))
;;;	)
;;;      )
;;;      (close file)
;;;    )
;;;    (princ
;;;      (strcat "\nError (psk-part-fromcsvfile): 无法打开文件 \""
;;;	      filename
;;;	      "\""
;;;      )
;;;    )
;;;  )
;;;  r
;;;)


;; (psk-draw-key '((".TYPE" . "FL-PL") ("PN" . "2.5") ("DN" . 100.0)))
(defun psk-draw-key (key / def drawer func param size sizefile templ)
  (setq def      (p-get $psk-pipe-parts (p-get key ".TYPE"))
        templ    (p-get def "KEY")
        sizefile (p-get def "SIZEFILE")
        drawer   (car (p-get def "DRAWER"))
        size     (psk-csvfile-get sizefile (p-template-eval templ key))
        func     (car drawer)
        param    (p-get size (cdr drawer))
  )

  (if (vl-catch-all-error-p (vl-catch-all-apply func param))
    (princ (strcat "\nerror in psk-part-draw "
                   (vl-princ-to-string (cons func param))
           )
    )
  )
)

(defun c:piv (/)
  (if (and (setq path (psk-paths-pick 1))
           (setq key (car (psk-selectd
                            (mapcar
                              (function
                                (lambda (e) (cons (car e) (p-get (cdr e) "DESC")))
                              )
                              $psk-pipe-parts
                            )
                            nil
                          )
                     )
           )
      )
    (progn
      (setq def      (p-get $psk-pipe-parts key)
            tp       (p-get def "TYPE")
            templ    (p-get def "KEY")
            sizefile (p-get def "SIZEFILE")
            name     (p-template-eval templ (caar path))
            prop     (list (cons ".TYPE" key))
      )
      (if (and templ sizefile)
        (setq prop (append prop (psk-csvfile-get sizefile name)))
      )


      (psk-set-customlayerbyid
        (strcat (psk-path-getservice (caar path)) "-IDEN")
      )

      (cond ((= "ATTACH" tp)
             (psk-path-createattach (caar path) (cadar path) prop)
            )
            ((= "INLINE" tp)
             (setq p (psk-posangle-prompt (caar path) (cadar path)))

             (psk-path-createinline (caar path) (car p) (cadr p) prop)
            )
            ((= "END" tp)
             (psk-path-createend (caar path) (cadar path) prop)
            )
      )
    )
  )
)