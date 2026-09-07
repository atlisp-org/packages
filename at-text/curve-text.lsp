;;; curve-text.lsp - 沿曲线放置和对齐文本
;;; 整合 Lee Mac 的 CurveText v1.4 和 AlignTextToCurve v1.2
;;; 重构至 @text: 命名空间
;;;
;;; 功能：
;;;   c:curvetext       - 沿曲线放置文本（逐字符旋转）
;;;   c:curvetext-align - 动态对齐文本到曲线（支持Text/MText）
;;;
;;; Copyright 2012-2013 Lee Mac  www.lee-mac.com
;;; Refactored for @lisp/at-text package

;;----------------------------------------------------------------------;;
;;                      CurveText v1.4 (重构)                          ;;
;;----------------------------------------------------------------------;;
(defun c:curvetext ( / sel str obj )
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))))
            (princ "\n当前图层已锁定。")
        )
        (   (and
                (setq sel
                    (@text:selection-or-text "\n指定或选择文本字符串: "
                        (function
                            (lambda ( x ) (wcmatch (cdr (assoc 0 (entget x))) "*TEXT,ATTRIB"))
                        )
                    )
                )
                (or
                    (and
                        (eq 'STR (type sel))
                        (setq str sel)
                    )
                    (setq str (cdr (assoc 1 (entget sel))))
                )
                (setq obj (@text:select-if "\n选择曲线: " '@text:curve-object-p))
            )
            (@text:curve-text str obj)
        )
    )
    (princ)
)

(defun @text:curve-text ( str ent / *error* 3pi/2 a1 a2 a3 acdoc acspc df di dr g1 g2 gr in ln lst ms obj p1 p2 p3 pi/2 ts )

    (defun *error* ( msg )
        (foreach obj lst
            (if
                (and
                    (not (vlax-erased-p obj))
                    (vlax-write-enabled-p obj)
                )
                (vla-delete obj)
            )
        )
        (if (null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )

    (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
          acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'paperspace 'modelspace))
    )
    (or *offset* (setq *offset* 0.0))
    (or *spacin* (setq *spacin* 1.1))

    (setq ts
        (/  (getvar 'textsize)
            (if (@text:is-annotative (getvar 'textstyle))
                (cond ((getvar 'cannoscalevalue)) ( 1.0 ))
                1.0
            )
        )
    )

    (setq lst
        (mapcar
            (function
                (lambda ( c )
                    (setq obj (vla-addtext acspc (chr c) (vlax-3D-point (getvar 'VIEWCTR)) ts))
                    (vla-put-alignment obj acalignmentmiddlecenter)
                    obj
                )
            )
            (vl-string->list str)
        )
    )

    (setq ms    (princ "\n放置文本: [+/-] 偏移, [</>] 间距")
          ln    (- (/ (1+ (strlen str)) 2.0))
          pi/2  (/ pi 2.0)
          3pi/2 (/ (* 3.0 pi) 2.0)
    )
    (while
        (progn
            (setq gr (grread t 15 0)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (or (= 05 g1) (= 03 g1))
                    (setq p1 (trans g2 1 0)
                          p2 (vlax-curve-getclosestpointto ent p1)
                          a1 (angle p2 p1)
                          di (vlax-curve-getdistatpoint ent p2)
                          dr (angle '(0.0 0.0 0.0) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatpoint ent p2)))
                          df (- a1 dr)
                          in ln
                          a2 (cond
                                 (   (and (> dr pi/2) (<= dr pi))
                                     (- pi)
                                 )
                                 (   (and (> dr pi) (<= dr 3pi/2))
                                     pi
                                 )
                                 (   0.0   )
                             )
                    )
                    (foreach obj
                        (if (and (< pi/2 dr) (<= dr 3pi/2))
                            (reverse lst)
                            lst
                        )
                        (if (setq p3 (vlax-curve-getPointatDist ent (+ di (* (setq in (1+ in)) *spacin* ts))))
                            (progn
                                (setq a3 (angle '(0. 0. 0.) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatpoint ent p3))))
                                (vla-put-TextAlignmentPoint obj
                                    (vlax-3D-point (polar p3 (+ a3 df) (* ts *offset*)))
                                )
                                (vla-put-rotation obj (+ a2 a3))
                            )
                        )
                    )
                    (= 05 g1)
                )
                (   (= 25 g1)
                    nil
                )
                (   (= 02 g1)
                    (cond
                        (   (member g2 '(13 32))
                            nil
                        )
                        (   (member g2 '(43 61))
                            (setq *offset* (+ *offset* 0.1))
                        )
                        (   (member g2 '(45 95))
                            (setq *offset* (- *offset* 0.1))
                        )
                        (   (member g2 '(46 62))
                            (setq *spacin* (+ *spacin* 0.05))
                        )
                        (   (member g2 '(44 60))
                            (setq *spacin* (- *spacin* 0.05))
                        )
                        (   (princ (strcat "\n无效按键。" ms))
                        )
                    )
                )
                (   t  )
            )
        )        
    )
    
    (if (< 15.0 (atof (getvar 'ACADVER)))
        (vla-appenditems (vla-add (vla-get-groups acdoc) "*")
            (vlax-make-variant
                (vlax-safearray-fill
                    (vlax-make-safearray vlax-vbObject (cons 0 (1- (length lst))))
                    lst
                )
            )
        )
    )
    (princ)
)

(defun @text:curve-object-p ( ent )
    (null
        (vl-catch-all-error-p
            (vl-catch-all-apply 'vlax-curve-getEndParam (list ent))
        )
    )
)

(defun @text:select-if ( msg pred )
    (
        (lambda ( f / e )
            (while
                (progn (setvar 'ERRNO 0) (setq e (car (entsel msg)))
                    (cond
                        (   (= 7 (getvar 'ERRNO))
                            (princ "\n未选中，请重试。")
                        )
                        (   (eq 'ENAME (type e))
                            (if (and f (null (f e)))
                                (princ "\n无效对象。")
                            )
                        )
                    )
                )
            )
            e
        )
        (eval pred)
    )
)

(defun @text:is-annotative ( style / obj xdt )
    (and
        (setq obj (tblobjname "STYLE" style))
        (setq xdt (cadr (assoc -3 (entget obj '("AcadAnnotative")))))
        (= 1 (cdr (assoc 1070 (reverse xdt))))
    )
)

(defun @text:selection-or-text ( msg pred / en g1 g2 gr result )
    (setq pred (eval pred))
    
    (if msg
        (princ msg)
        (princ (setq msg "\n选择对象或输入文本: "))
    )
    (setq result "")

    (while
        (progn
            (setq gr (grread t 13 2)
                  g1 (car  gr)
                  g2 (cadr gr)
            )
            (cond
                (   (= 03 g1)
                    (if (setq en (car (nentselp g2)))
                        (if (pred en)
                            (not (setq result en))
                            (princ (strcat "\n所选对象无效。" msg))
                        )
                        (princ (strcat "\n未选中，请重试。" msg))
                    )
                )
                (   (= 02 g1)
                    (cond
                        (   (< 31 g2 127)
                            (setq result (strcat result (princ (chr g2))))
                        )
                        (   (= 13 g2)
                            nil
                        )
                        (   (= 08 g2)
                            (if (< 0 (strlen result))
                                (progn
                                    (setq result (substr result 1 (1- (strlen result))))
                                    (princ (vl-list->string '(8 32 8)))
                                )
                            )
                            t
                        )
                        (   t   )
                    )
                )
                (   (= 25 g1)
                    nil
                )
                (   t   )
            )
        )
    )
    result
)

;;----------------------------------------------------------------------;;
;;                   AlignTextToCurve v1.2 (重构)                      ;;
;;----------------------------------------------------------------------;;

(setq @text:curve-align:version "1.2")

;;----------------------------------------------------------------------;;

(defun c:curvetext-align

    (
        /
        *error*
        ang
        bak
        cfg
        dcl def dis
        ent enx
        gr1 gr2
        hgt
        jus
        mat msg mtp
        nrm
        off
        pi2 prn prp pt1 pt2
        red rot
        sav sel str sym
        tmp txt typ
        uxa
    )

    (defun *error* ( msg )
        (if
            (and
                (= 'list (type def))
                (= 'str  (type cfg))
                (findfile cfg)
            )
            (@text:curve-align:writeconfig cfg (mapcar 'eval (mapcar 'car def)))
        )
        (if
            (and
                (= 'vla-object (type txt))
                (not (vlax-erased-p txt))
                (vlax-write-enabled-p txt)
            )
            (if (= 'list (type prp))
                (foreach x prp
                    (if (vlax-property-available-p txt (car x) t)
                        (vl-catch-all-apply 'vlax-put-property (cons txt x))
                    )
                )
                (vl-catch-all-apply 'vla-delete (list txt))
            )
        )
        (if
            (and
                (= 'list  (type mat))
                (= 'ename (type ent))
                (entget ent)
            )
            (entdel ent)
        )
        (@text:curve-align:endundo (@text:curve-align:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )

    (@text:curve-align:startundo (@text:curve-align:acdoc))
    (cond
        (   (or (@text:curve-align:layerlocked (getvar 'clayer))
                (@text:curve-align:layerlocked "0")
            )
            (princ "\n当前图层或图层 \"0\" 已锁定。")
        )
        (   (null (vl-file-directory-p (setq sav (@text:curve-align:savepath))))
            (princ "\n保存路径无效。")
        )
        (   (progn
                (setq def
                   '(
                        (typ . "txt")
                        (jus . "Middle-Center")
                        (off . 1.0)
                        (rot . 0.0)
                        (red . t)
                        (bak . nil)
                        (mtp . nil)
                    )
                )
                (setq cfg (strcat sav "\\LMAC_ATC_V" (vl-string-translate "." "-" @text:curve-align:version) ".cfg")
                      dcl (strcat sav "\\LMAC_ATC_V" (vl-string-translate "." "-" @text:curve-align:version) ".dcl")
                )
                (if (not (findfile cfg))
                    (@text:curve-align:writeconfig cfg (mapcar 'cdr def))
                )
                (@text:curve-align:readconfig cfg (setq sym (mapcar 'car def)))

                (while
                    (progn
                        (setvar 'errno 0)
                        (initget "New Settings Exit")
                        (setq sel (entsel "\n选择要对齐的文本 [新建/设置] <退出>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\n未选中，请重试。")
                            )
                            (   (= 'list (type sel))
                                (setq ent (car sel)
                                      enx (entget ent)
                                )
                                (cond
                                    (   (not (wcmatch (cdr (assoc 0 enx)) "TEXT,MTEXT"))
                                        (princ "\n对象必须是 Text 或 MText。")
                                    )
                                    (   (@text:curve-align:layerlocked (cdr (assoc 8 enx)))
                                        (princ "\n对象所在图层已锁定。")
                                    )
                                    (   t
                                        (setq txt (vlax-ename->vla-object ent)
                                              prp (@text:curve-align:getproperties txt)
                                        )
                                        nil
                                    )
                                )
                            )
                            (   (= "Exit" sel)
                                nil
                            )
                            (   (= "Settings" sel)
                                (mapcar 'set sym (@text:curve-align:settings dcl (mapcar 'eval sym)))
                            )
                            (   (= "New" sel)
                                (= "" (vl-string-trim " \t\n" (setq str (getstring t "\n指定文本 <选择>: "))))
                            )
                        )
                    )
                )
                (not
                    (or (= 'vla-object (type txt))
                        (and (= 'str (type str)) (/= "" (vl-string-trim " \t\n" str)))
                    )
                )
            )
            (@text:curve-align:writeconfig cfg (mapcar 'eval sym))
        )
        (   (progn
                (while
                    (progn
                        (setvar 'errno 0)
                        (setq sel (nentselp "\n选择对齐文本的曲线 <退出>: "))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\n未选中，请重试。")
                            )
                            (   (= 'ename (type (car sel)))
                                (if
                                    (not
                                        (or (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                                            (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getendparam (list (car sel)))))
                                        )
                                    )
                                    (princ "\n所选对象无效。")
                                )
                            )
                        )
                    )
                )
                (null sel)
            )
        )
        (   (not
                (or
                    (and
                        (setq mat (caddr sel))
                        (setq ent (@text:curve-align:copynested (car sel) mat))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\n无法重新创建嵌套实体。")
        )
        (   t
            (if (null txt)
                (if (= "txt" typ)
                    (progn
                        (setq txt
                            (vla-addtext
                                (vlax-get-property (@text:curve-align:acdoc)
                                    (if (= 1 (getvar 'cvport))
                                        'paperspace
                                        'modelspace
                                    )
                                )
                                str
                                (vlax-3D-point (trans (cadr sel) 1 0))
                                (@text:curve-align:styleheight (getvar 'textstyle))
                            )
                        )
                        (vla-put-alignment txt
                            (eval
                                (cadr
                                    (assoc jus
                                       '(
                                            ("Left"          acalignmentleft)
                                            ("Center"        acalignmentcenter)
                                            ("Right"         acalignmentright)
                                            ("Middle"        acalignmentmiddle)
                                            ("Top-Left"      acalignmenttopleft)
                                            ("Top-Center"    acalignmenttopcenter)
                                            ("Top-Right"     acalignmenttopright)
                                            ("Middle-Left"   acalignmentmiddleleft)
                                            ("Middle-Center" acalignmentmiddlecenter)
                                            ("Middle-Right"  acalignmentmiddleright)
                                            ("Bottom-Left"   acalignmentbottomleft)
                                            ("Bottom-Center" acalignmentbottomcenter)
                                            ("Bottom-Right"  acalignmentbottomright)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (progn
                        (setq txt
                            (vla-addmtext
                                (vlax-get-property (@text:curve-align:acdoc)
                                    (if (= 1 (getvar 'cvport))
                                        'paperspace
                                        'modelspace
                                    )
                                )
                                (vlax-3D-point (trans (cadr sel) 1 0))
                                (   (lambda ( box ) (- (caadr box) (caar box)))
                                    (textbox
                                        (list
                                            (cons 01 (strcat str "."))
                                            (cons 40 (@text:curve-align:styleheight (getvar 'textstyle)))
                                            (cons 07 (getvar 'textstyle))
                                        )
                                    )
                                )
                                str
                            )
                        )
                        (vla-put-attachmentpoint txt
                            (eval
                                (cadr
                                    (assoc jus
                                       '(
                                            ("Top-Left"      acattachmentpointtopleft)
                                            ("Top-Center"    acattachmentpointtopcenter)
                                            ("Top-Right"     acattachmentpointtopright)
                                            ("Middle-Left"   acattachmentpointmiddleleft)
                                            ("Middle-Center" acattachmentpointmiddlecenter)
                                            ("Middle-Right"  acattachmentpointmiddleright)
                                            ("Bottom-Left"   acattachmentpointbottomleft)
                                            ("Bottom-Center" acattachmentpointbottomcenter)
                                            ("Bottom-Right"  acattachmentpointbottomright)
                                        )
                                    )
                                )
                            )
                        )
                        (vla-put-height txt (@text:curve-align:styleheight (getvar 'textstyle)))
                        (if bak (vla-put-backgroundfill txt :vlax-true))
                    )
                )
            )
            (if
                (and
                    (= "AcDbText" (vla-get-objectname txt))
                    (/= acalignmentleft (vla-get-alignment txt))
                )
                (setq prn 'textalignmentpoint)
                (setq prn 'insertionpoint)
            )
            (setq hgt (vla-get-height txt)
                  pi2 (/ pi -2.0)
                  nrm (trans '(0.0 0.0 1.0) 1 0 t)
                  uxa (if (= "AcDbText" (vla-get-objectname txt)) (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 nrm t)) 0.0)
                  msg (strcat "\n[+/-] for [O]ffset | [</>] for [R]otation | Readabilit[y] |"
                          (if (= "AcDbMText" (vla-get-objectname txt))
                              " [B]ackground Mask | <[E]xit>: "
                              " <[E]xit>: "
                          )
                      )
            )
            (princ msg)
            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (or (= 5 gr1) (= 3 gr1))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointto ent pt2)
                            )
                            (if (not (equal pt1 pt2 1e-8))
                                (progn
                                    (setq dis (/ (* hgt off) (distance pt1 pt2))
                                          ang (+ (angle (trans pt1 0 1) gr2) uxa rot pi2)
                                    )
                                    (vlax-put-property txt prn (vlax-3D-point (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt1 pt2)))
                                    (vla-put-rotation  txt (if red (@text:curve-align:readable ang) ang))
                                )
                            )
                            (cond
                                (   (= 5 gr1))
                                (   mtp
                                    (setq txt (vla-copy txt)
                                          prp nil
                                    )
                                    t
                                )
                            )
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq off (+ off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq off (- off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq rot (+ rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq rot (- rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    (*error* nil)
                                    nil
                                )
                                (   (member gr2 '(089 121))
                                    (if (setq red (not red))
                                        (princ "\n<文本可读性已启用>")
                                        (princ "\n<文本可读性已禁用>")
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(066 098))
                                    (if (= "AcDbMText" (vla-get-objectname txt))
                                        (progn
                                            (vlax-put txt 'backgroundfill (~ (vlax-get txt 'backgroundfill)))
                                            (if (setq bak (= -1 (vlax-get txt 'backgroundfill)))
                                                (princ "\n<背景遮罩已开启>")
                                                (princ "\n<背景遮罩已关闭>")
                                            )
                                        )
                                        (princ "\n背景遮罩仅适用于 MText。")
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\n指定旋转角度 <" (angtos rot) ">: ")))
                                        (setq rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\n指定偏移 <" (rtos (* hgt off)) ">: ")))
                                        (setq off (/ tmp hgt))
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(11 25))
                            (*error* nil)
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if mat (entdel ent))
            (@text:curve-align:writeconfig cfg (mapcar 'eval sym))
        )
    )
    (@text:curve-align:endundo (@text:curve-align:acdoc))
    (princ)
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:readable ( a )
    (   (lambda ( a )
            (if (and (< (* pi 0.5) a) (<= a (* pi 1.5)))
                (@text:curve-align:readable (+ a pi))
                a
            )
        )
        (rem (+ a pi pi) (+ pi pi))
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:styleheight ( sty / tmp )
    (if (zerop (setq tmp (cdr (assoc 40 (tblsearch "style" sty)))))
        (setq tmp (getvar 'textsize))
    )
    (if (@text:curve-align:annotative-p sty)
        (/ tmp (cond ((getvar 'cannoscalevalue)) (1.0)))
        tmp
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:annotative-p ( sty )
    (and
        (setq sty (tblobjname "style" sty))
        (setq sty (cadr (assoc -3 (entget sty '("AcadAnnotative")))))
        (= 1 (cdr (assoc 1070 (reverse sty))))
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:copynested ( ent mat / enx tmp )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (@text:curve-align:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (@text:curve-align:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (setq tmp (cdr (assoc 330 (entget (@text:curve-align:entmakex enx)))))
        )
        (setq tmp (@text:curve-align:entmakex enx))
    )
    (if tmp (vla-transformby (vlax-ename->vla-object tmp) (vlax-tmatrix mat)))
    tmp
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
                (function
                    (lambda ( x )
                        (or (member (car x) '(005 006 008 039 048 062 102 370))
                            (= 'ename (type (cdr x)))
                        )
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:getproperties ( obj )
    (vl-remove nil
        (mapcar
            (function
                (lambda ( prp )
                    (if (vlax-property-available-p obj prp t)
                        (list prp (vlax-get-property obj prp))
                    )
                )
            )
           '(
                insertionpoint
                textalignmentpoint
                backgroundfill
                rotation
            )
        )
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:settings ( dcl lst / *error* alg bak dch jus mtp off off:str red rot rot:str typ typ:fun )

    (defun *error* ( msg )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )
    
    (cond
        (   (not (@text:curve-align:writedcl dcl))
            (princ "\n无法写入 DCL 文件。")
        )
        (   (<= (setq dch (load_dialog dcl)) 0)
            (princ "\n无法加载 DCL 文件。")
        )
        (   (not (new_dialog "atc" dch))
            (princ "\n无法加载程序对话框。")
        )
        (   t
            (mapcar 'set '(typ jus off rot red bak mtp) lst)
            
            (set_tile typ "1")
            (
                (setq typ:fun
                    (lambda ( typ )
                        (setq alg (@text:curve-align:justlist typ))
                        (set_tile "jus"
                            (itoa
                                (cond
                                    (   (vl-position jus alg))
                                    (   (setq jus (car alg)) 0)
                                )
                            )
                        )
                        (if (= "mtx" typ)
                            (mode_tile "bak" 0)
                            (mode_tile "bak" 1)
                        )
                    )
                )
                typ
            )
            (action_tile "jus" "(setq jus (nth (atoi $value) alg))")
            (action_tile "txt" "(typ:fun (setq typ $key))")
            (action_tile "mtx" "(typ:fun (setq typ $key))")

            (set_tile    "off"  (setq off:str (rtos off)))
            (action_tile "off" "(setq off:str $value)")

            (set_tile    "rot"  (setq rot:str (angtos rot)))
            (action_tile "rot" "(setq rot:str $value)")

            (foreach key '("red" "bak" "mtp")
                (set_tile    key (if (eval (read key)) "1" "0"))
                (action_tile key (strcat "(setq " key " (= \"1\" $value))"))
            )
            (action_tile "accept"
                (vl-prin1-to-string
                   '(cond
                        (   (not (distof off:str))
                            (alert "\nOffset Factor must be numerical.")
                            (mode_tile "off" 2)
                        )
                        (   (not (angtof rot:str))
                            (alert "\nText Rotation must be numerical.")
                            (mode_tile "rot" 2)
                        )
                        (   (setq off (distof off:str)
                                  rot (angtof rot:str)
                            )
                            (done_dialog 1)
                        )
                    )
                )
            )
         
            (if (= 1 (start_dialog))
                (setq lst (list typ jus off rot red bak mtp))
            )
        )
    )
    (if (< 0 dch)
        (unload_dialog dch)
    )
    lst
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:justlist ( typ / lst )
    (start_list "jus")
    (foreach itm
        (setq lst
            (append
                (if (= "txt" typ)
                   '(
                        "Left"
                        "Center"
                        "Right"
                        "Middle"
                    )
                )
               '(
                    "Top-Left"
                    "Top-Center"
                    "Top-Right"
                    "Middle-Left"
                    "Middle-Center"
                    "Middle-Right"
                    "Bottom-Left"
                    "Bottom-Center"
                    "Bottom-Right"
                )
            )
        )
        (add_list itm)
    )
    (end_list)
    lst
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:layerlocked ( lay / def )
    (and
        (setq def (tblsearch "layer" lay))
        (= 4 (logand 4 (cdr (assoc 70 def))))
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:writedcl ( dcl / des )
    (cond
        (   (findfile dcl))
        (   (setq des (open dcl "w"))
            (foreach x
               '(
                    "edt : edit_box"
                    "{"
                    "    edit_width = 8;"
                    "    edit_limit = 10;"
                    "    alignment = left;"
                    "}"
                    "atc : dialog"
                    "{"
                    "    label = \"设置\";"
                    "    spacer;"
                    "    : text"
                    "    {"
                    "        label = \"新文本对象类型:\";"
                    "    }"
                    "    : radio_row"
                    "    {"
                    "        alignment = centered;"
                    "        fixed_width = true;"
                    "        : radio_button"
                    "        {"
                    "            key = \"txt\";"
                    "            label = \"Text\";"
                    "        }"
                    "        : radio_button"
                    "        {"
                    "            key = \"mtx\";"
                    "            label = \"MText\";"
                    "        }"
                    "    }"
                    "    spacer;"
                    "    : text"
                    "    {"
                    "        label = \"新文本对齐方式:\";"
                    "    }"
                    "    : popup_list"
                    "    {"
                    "        key = \"jus\";"
                    "    }"
                    "    spacer;"
                    "    : edt"
                    "    {"
                    "        key = \"off\";"
                    "        label = \"偏移因子:\";"
                    "    }"
                    "    : edt"
                    "    {"
                    "        key = \"rot\";"
                    "        label = \"文本旋转角度:\";"
                    "    }"
                    "    spacer;"
                    "    : toggle"
                    "    {"
                    "        key = \"red\";"
                    "        label = \"保持文本可读性\";"
                    "    }"
                    "    : toggle"
                    "    {"
                    "        key = \"bak\";"
                    "        label = \"MText 背景遮罩\";"
                    "    }"
                    "    : toggle"
                    "    {"
                    "        key = \"mtp\";"
                    "        label = \"多重文本模式\";"
                    "    }"
                    "    spacer;"
                    "    ok_cancel;"
                    "}"
                )
                (write-line x des)
            )
            (setq des (close des))
            (while (not (findfile dcl)))
            dcl
        )
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:writeconfig ( cfg lst / _tostring des )
 
    (defun _tostring ( x / dim )
        (cond
            (   (= 'int (type x))
                (itoa x)
            )
            (   (= 'real (type x))
                (setq dim (getvar 'dimzin))
                (setvar 'dimzin 0)
                (setq x (rtos x 2 8))
                (setvar 'dimzin dim)
                x
            )
            (   (vl-prin1-to-string x))
        )
    )
    
    (if (setq des (open cfg "w"))
        (progn
            (foreach x lst (write-line (_tostring x) des))
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:readconfig ( cfg lst / des itm )
    (if
        (and
            (setq cfg (findfile cfg))
            (setq des (open cfg "r"))
        )
        (progn
            (foreach sym lst
                (if (setq itm (read-line des))
                    (set  sym (read itm))
                )
            )
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:savepath ( / tmp )
    (if (setq tmp (getvar 'roamablerootprefix))
        (strcat (@text:curve-align:fixdir tmp) "\\Support")
        (@text:curve-align:fixdir (getvar 'tempprefix))
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:startundo ( doc )
    (@text:curve-align:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun @text:curve-align:acdoc nil
    (eval (list 'defun '@text:curve-align:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (@text:curve-align:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ "\n:: curve-text.lsp | CurveText 1.4 + AlignTextToCurve 1.2 | @lisp/at-text ::")
(princ "\n:: 命令 \"curvetext\" 或 \"curvetext-align\" 启动 ::")
(princ)
