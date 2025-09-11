;;;(defun c:tt (/ id rt)
;;;  (setq id (load_dialog (psk-get-filename "catelogExplor.dcl")))
;;;  (if (and (>= id 0)
;;;           (new_dialog "catelogExplor" id)
;;;      )
;;;    (progn
;;;
;;;      (setq rows
;;;             (p-csvfile-read
;;;               (psk-get-filename
;;;                 "\\catelog\\vrf\\daikin\\多联机空调室内机（大金）.csv"
;;;               )
;;;             )
;;;            rows (mapcar '(lambda (e) (p-string-connect e "\t")) rows)
;;;      )
;;;
;;;      ;; 填充属性框
;;;      (start_list "KEYLIST")
;;;      (mapcar 'add_list rows)
;;;      (end_list)
;;;
;;;      (set_tile "KEYLIST" "0")
;;;
;;;
;;;      (action_tile "KEYLIST" "")
;;;
;;;      (action_tile "OK" "(done_dialog 1)")
;;;
;;;      (setq rt (start_dialog))
;;;      (unload_dialog id)
;;;    )
;;;  )
;;;)


(defun psk-catelogexplor-showsld (sldfile / h w)
  (if (null (findfile sldfile))
    (princ (strcat "文件\"" sldfile "\"不存在\n"))
    (progn (setq w (dimx_tile "IMG")
                 h (dimy_tile "IMG")
           )
           (start_image "IMG")
           (fill_image 0 0 w h 0)
           (slide_image 5 5 (- w 10) (- h 10) sldfile)
           (end_image)
    )
  )
)


(if (null $psk-selectcatelog)
  (setq $psk-selectcatelog 0)
)
(if (null $psk-selectkey)
  (setq $psk-selectkey "")
)

(defun c:eq (/ catelogfiles)
  (setq catelogfiles (p-file-search (psk-get-filename "\\catelog") "*.csv")
        catelogfiles (mapcar
                       (function
                         (lambda (e)
                           (cons (vl-filename-base e)
                                 (strcat (vl-filename-directory e) "\\")
                           )
                         )
                       )
                       catelogfiles
                     )
  )
  (pdb-dlg-show
    (psk-get-filename "catelogExplor.dcl")
    "catelogExplor2"
    ;; set tile
    (lambda ()
      (pdb-list-fill
        "CATELOG"
        (mapcar (function car) catelogfiles)
      )
      (pdb-list-select "CATELOG" $psk-selectcatelog)
      (psk-oncateloglist $psk-selectcatelog)

      (action_tile "CATELOG" "(psk-oncateloglist (atoi $VALUE))")
      (action_tile
        "KEYLIST"
        "(psk-onkeylist (atoi $VALUE) $REASON)"
      )
      (action_tile "OK" "(done_dialog 4)")
    )
    ;; on dialog result
    (lambda (rt)
      (if (= rt 4)
        (psk-insert)
      )
      0
    )
  )
)
;;




;; (psk-getportdef "003" '(("002,003,004,006,008,010,012,014"
;;;   ("CHS" (0 0 0) (1 0 0) 0)
;;;   ("CHR" (0 100 0) (1 0 0) 0)
;;;   ("CD" (0 -100 0) (1 0 0) 0)
;;; )
;;;)
;;;)
(defun psk-getportdef (key lst / r)
  (while lst
    (if (wcmatch key (caar lst))
      (setq r   (cdar lst)
            lst nil
      )
    )
    (setq lst (cdr lst))
  )
  r
)
;;


(defun psk-insert (/ name p)
  (setvar 'cmdecho 0)
  (setvar 'clayer (psk-set-customlayerbyid "EQUIP"))
  (while (setq p (getpoint))
    (command "._insert"
             (strcat (cdr catelogfile)
                     (setq name (p-get selectkeyitem "DWG"))
                     ".dwg"
             )
             "NON"
             p
             1.
             1.
             pause
    )
    (setq en (entlast))
;;;    (if (findfile (strcat (cdr catelogfile) "ports.lsp"))
;;;      (if (setq ports (psk-getportdef name (p-lisp-load (strcat (cdr catelogfile) "ports.lsp"))))
;;;        (psk-ports-store en ports)
;;;        (princ "\n未找到管口定义")
;;;      )
;;;    )
    (p-xprop-set
      en
      "PSK-EQUIP"
      (list (cons "NAME" (p-get selectkeyitem "NAME"))
            (cons "CLD" (atof (p-get selectkeyitem "CLD")))
      )
    )
    (psk-ports-store en (p-get-portsforinsert en))
;;;    (p-osnap-disable)
    ;; TODO: UCS下插入管口位置方向出错
;;;    (command "._rotate" en "" "NON" p "NON" pause)
;;;    (p-osnap-disable)
  )
  (setvar 'cmdecho 1)
)


(defun psk-showkey (item)
  (start_list "PROPLIST")
  (mapcar (function add_list)
          (mapcar (function (lambda (e) (strcat (car e) "\t" (cdr e))))
                  item
          )
  )
  (end_list)
)
(defun psk-oncateloglist (value)
;;;  (princ (strcat "\npsk-onlist," value))
  (setq $psk-selectcatelog value
        catelogfile        (nth $psk-selectcatelog catelogfiles)
        rows               (p-csvfile-read
                             (strcat (cdr catelogfile) (car catelogfile) ".csv")
                           )
  )

  (if (or (null selectkey)
          (>= selectkey (length (cdr rows)))
      )
    (setq selectkey 0)
  )
  (pdb-list-fill "KEYLIST" (p-csvread-keys rows))
  (pdb-list-select "KEYLIST" selectkey)
  (psk-onkeylist selectkey -1)
)
(defun psk-onkeylist (value reason)
  ;;  (princ (strcat "\npsk-onkeylist" value "," (itoa reason)))
  (setq selectkey value)
  (setq key (car (nth value (cdr rows))))
  (setq selectkeyitem (p-csvread-get rows key))
  (psk-showkey selectkeyitem)
  (psk-catelogexplor-showsld
    (strcat (cdr catelogfile)
            (p-get selectkeyitem "DWG")
            ".sld"
    )
  )
  (if (= reason 4)
    (psk-insert)
  )
)


(setq $psk-selectcatelog 2)