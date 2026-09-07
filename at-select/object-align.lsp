;;; ========================================================================
;;; @select:obj-align - 对象沿曲线对齐工具
;;; 动态将选择的对象对齐到指定曲线，支持偏移、旋转、连续复制等功能。
;;; 原版: Lee Mac ObjectAlign v1.7 (www.lee-mac.com)
;;; 重构: @select: 命名空间
;;; ========================================================================

(defun c:obj-align

    (
        /
        *error*
        bb1 bb2 blk bnm bpt
        def dis
        ent
        fac
        gr1 gr2
        idx inc
        llp lst
        mat msg
        obj ocs oss
        pi2 pt1 pt2 pt3 pt4
        sel
        tma tmp trm
        urp uxa
        vec 
    )

    (defun *error* ( msg )
        (if (and (= 'list (type trm)) (= 'ename (type ent)) (entget ent))
            (entdel ent)
        )
        (if (and (= 'vla-object (type blk)) (not (vlax-erased-p blk)))
            (vl-catch-all-apply 'vla-delete (list blk))
        )
        (if (and (= 'vla-object (type def)) (not (vlax-erased-p def)))
            (vl-catch-all-apply 'vla-delete (list def))
        )
        (foreach obj lst
            (if (not (vlax-erased-p obj))
                (vl-catch-all-apply 'vla-delete (list obj))
            )
        )
        (@select:align:endundo (@select:align:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )
    
    (@select:align:startundo (@select:align:acdoc))
    (if (null @select:align|rot) (setq @select:align|rot 0.0))
    (if (null @select:align|off) (setq @select:align|off 0.0))
    
    (cond
        (   (or (@select:align:layerlocked (getvar 'clayer))
                (@select:align:layerlocked "0")
            )
            (princ "\n当前图层或图层\"0\"已锁定 - 请解锁这些图层后再使用本工具。")
        )
        (   (null (setq oss (@select:align:ssget "\n选择要对齐的对象: " '("_:L" ((0 . "~VIEWPORT"))))))
            (princ "\n*取消*")
        )
        (   (progn
                (setq bpt (getpoint "\n指定基点 <中心>: "))
                (while
                    (progn
                        (setvar 'errno 0)
                        (setq sel (nentselp "\n选择对齐目标曲线 <退出>: "))
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
                (while (/= 5 (car (setq pt1 (grread t 13 1)))))
                (null sel)
            )
        )
        (   (not
                (or
                    (and
                        (setq trm (caddr sel))
                        (setq ent (@select:align:copynested (car sel) trm))
                    )
                    (and
                        (= "VERTEX" (cdr (assoc 0 (entget (car sel)))))
                        (setq ent (cdr (assoc 330 (entget (car sel)))))
                    )
                    (setq ent (car sel))
                )
            )
            (princ "\n无法重建嵌套实体。")
        )
        (   (progn
                (setq ocs (trans '(0 0 1) 1 0 t)
                      uxa (angle '(0.0 0.0) (trans (getvar 'ucsxdir) 0 ocs t))
                      mat (@select:align:mxm
                              (list
                                  (list (cos uxa)     (sin uxa) 0.0)
                                  (list (- (sin uxa)) (cos uxa) 0.0)
                                 '(0.0 0.0 1.0)
                              )
                              (mapcar '(lambda ( a ) (trans a ocs 0 t))
                                 '(
                                      (1.0 0.0 0.0)
                                      (0.0 1.0 0.0)
                                      (0.0 0.0 1.0)
                                  )
                              )
                          )
                      vec (mapcar '- (@select:align:mxv mat (trans '(0.0 0.0 0.0) ocs 0)))
                      tma (vlax-tmatrix (append (mapcar 'append mat (mapcar 'list vec)) '((0.0 0.0 0.0 1.0))))
                )
                (repeat (setq idx (sslength oss))
                    (setq idx (1- idx)
                          obj (vla-copy (vlax-ename->vla-object (ssname oss idx)))
                          lst (cons obj lst)
                    )
                    (vla-transformby obj tma)
                    (if (and (vlax-method-applicable-p obj 'getboundingbox)
                             (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                        )
                        (setq bb1 (cons (vlax-safearray->list llp) bb1)
                              bb2 (cons (vlax-safearray->list urp) bb2)
                        )
                    )
                    (vla-put-visible obj :vlax-false)
                )
                (not (and bb1 bb2))
            )
            (*error* nil)
            (princ "\n无法计算所选对象的包围盒。")
        )
        (   t
            (setq bb1 (apply 'mapcar (cons 'min bb1))
                  bb2 (apply 'mapcar (cons 'max bb2))
                  bpt (cond ( bpt (mapcar '+ (@select:align:mxv mat (trans bpt 1 0)) vec)) ((mapcar '(lambda ( a b ) (/ (+ a b) 2.0)) bb1 bb2)))
                  fac (/ (- (cadr bb2) (cadr bb1)) 2.0)
                  pi2 (/ pi -2.0)
                  inc 0
            )
            (if (equal 0.0 fac 1e-8)
                (if (equal bb1 bb2 1e-8)
                    (setq fac 1.0)
                    (setq fac (/ (- (car bb2) (car bb1)) 2.0))
                )
            )
            (while (tblsearch "block" (setq bnm (strcat "$tmp" (itoa (setq inc (1+ inc)))))))
            (foreach obj lst (vla-put-visible obj :vlax-true))
            (vla-copyobjects (@select:align:acdoc)
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbobject (cons 0 (1- (length lst))))
                        lst
                    )
                )
                (setq def (vla-add (vla-get-blocks (@select:align:acdoc)) (vlax-3D-point bpt) bnm))
            )
            (foreach obj lst (vla-delete obj))
            (setq lst nil
                  blk
                (vla-insertblock
                    (vlax-get-property (@select:align:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                    (vlax-3D-point (trans (cadr pt1) 1 0))
                    bnm 1.0 1.0 1.0 0.0
                )
            )
            (vla-put-layer  blk "0")
            (vla-put-normal blk (vlax-3D-point ocs))
            (setq msg (princ "\n[+/-] 调整偏移 | [</>] 调整旋转 | [M] 连续复制 | <[E] 退出>: "))

            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (member gr1 '(3 5))
                            (setq pt2 (trans gr2 1 0)
                                  pt1 (vlax-curve-getclosestpointtoprojection ent pt2 ocs)
                                  pt3 (@select:align:2d (trans pt1 0 ocs))
                                  pt4 (@select:align:2d (trans pt2 0 ocs))
                            )
                            (if (not (equal pt3 pt4 1e-8))
                                (progn
                                    (setq dis (/ (* fac @select:align|off) (distance pt3 pt4)))
                                    (vla-put-insertionpoint blk
                                        (vlax-3D-point
                                            (trans
                                                (append
                                                    (mapcar '(lambda ( a b ) (+ a (* (- b a) dis))) pt3 pt4)
                                                    (list (caddr (trans pt1 0 ocs)))
                                                )
                                                ocs 0
                                            )
                                        )
                                    )
                                    (vla-put-rotation blk (+ (angle (trans pt1 0 ocs) (trans gr2 1 ocs)) @select:align|rot pi2))
                                )
                            )
                            (cond
                                (   (= 5 gr1))
                                (   (progn (vla-explode blk) @select:align|mtp))
                            )
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (member gr2 '(043 061))
                                    (setq @select:align|off (+ @select:align|off 0.1))
                                )
                                (   (member gr2 '(045 095))
                                    (setq @select:align|off (- @select:align|off 0.1))
                                )
                                (   (member gr2 '(044 060))
                                    (setq @select:align|rot (+ @select:align|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(046 062))
                                    (setq @select:align|rot (- @select:align|rot (/ pi 4.0)))
                                )
                                (   (member gr2 '(013 032 069 101))
                                    nil
                                )
                                (   (member gr2 '(082 114))
                                    (if (setq tmp (getangle (strcat "\n指定旋转角度 <" (angtos @select:align|rot) ">: ")))
                                        (setq @select:align|rot tmp)
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(079 111))
                                    (if (setq tmp (getdist (strcat "\n指定偏移距离 <" (rtos (* fac @select:align|off)) ">: ")))
                                        (setq @select:align|off (/ tmp fac))
                                    )
                                    (princ msg)
                                )
                                (   (member gr2 '(077 109))
                                    (if (setq @select:align|mtp (not @select:align|mtp))
                                        (princ "\n<连续复制 已开启>")
                                        (princ "\n<连续复制 已关闭>")
                                    )
                                    (princ msg)
                                )
                                (   t   )
                            )
                        )
                        (   (member gr1 '(011 025))
                            nil
                        )
                        (   t   )
                    )
                )
            )
            (if trm (entdel ent))
            (vla-delete  blk)
            (vla-delete  def)
            (@select:align:endundo (@select:align:acdoc))
        )
    )
    (princ)
)

;;; ========================================================================

(defun @select:align:2d ( x ) (list (car x) (cadr x)))

;;; ========================================================================

(defun @select:align:layerlocked ( lay / def )
    (and
        (setq def (tblsearch "layer" lay))
        (= 4 (logand 4 (cdr (assoc 70 def))))
    )
)

;;; ========================================================================

(defun @select:align:copynested ( ent mat / enx tmp )
    (if (= 1 (cdr (assoc 66 (setq enx (entget ent)))))
        (progn
            (@select:align:entmakex enx)
            (setq ent (entnext ent)
                  enx (entget  ent)
            )
            (while (/= "SEQEND" (cdr (assoc 0 enx)))
                (@select:align:entmakex enx)
                (setq ent (entnext ent)
                      enx (entget  ent)
                )
            )
            (setq tmp (cdr (assoc 330 (entget (@select:align:entmakex enx)))))
        )
        (setq tmp (@select:align:entmakex enx))
    )
    (if tmp (vla-transformby (vlax-ename->vla-object tmp) (vlax-tmatrix mat)))
    tmp
)

;;; ========================================================================

(defun @select:align:entmakex ( enx )
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

;;; ========================================================================

(defun @select:align:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;;; ========================================================================

(defun @select:align:startundo ( doc )
    (@select:align:endundo doc)
    (vla-startundomark doc)
)

;;; ========================================================================

(defun @select:align:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;; ========================================================================

(defun @select:align:acdoc nil
    (eval (list 'defun '@select:align:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (@select:align:acdoc)
)

;;; ========================================================================
;;; 矩阵工具函数
;;; ========================================================================

;; 矩阵转置 - Doug Wilson
;; 参数: m - nxn 矩阵

(defun @select:align:trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; 矩阵乘矩阵 - Vladimir Nesterovsky
;; 参数: m,n - nxn 矩阵

(defun @select:align:mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (@select:align:mxv a r)) m)) (@select:align:trp n))
)

;; 矩阵乘向量 - Vladimir Nesterovsky
;; 参数: m - nxn 矩阵, v - n维向量

(defun @select:align:mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)
    
;;; ========================================================================

(vl-load-com)
(princ
    (strcat
        "\n:: @select:obj-align | ObjectAlign v1.7 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2010")
        " www.lee-mac.com ::"
        "\n:: \u547d\u4ee4 \"obj-align\" \u8c03\u7528 ::"
    )
)
(princ)

;;; ========================================================================
;;;                             End of File                              ;;
;;; ========================================================================
