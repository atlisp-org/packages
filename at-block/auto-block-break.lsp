;;; -----------------------------------------------------------------------
;;; auto-block-break.lsp — 自动块打断（AutoBlockBreak）
;;;
;;; 功能：在指定点插入图块，并自动将周围几何对象修剪到图块的矩形轮廓内。
;;;       可选自动旋转图块以对齐穿过插入点的曲线对象。
;;;       支持对已有图块进行修剪（单个/批量）。
;;;
;;; 命令：
;;;   AUTO-BLOCK-BREAK         — 插入新块并修剪周围几何
;;;   AUTO-BLOCK-BREAK-EXPLODE — 选择已有块并修剪周围几何
;;;   AUTO-BLOCK-BREAK-SELECT  — 选择多个已有块批量修剪
;;;
;;; 重构自 Lee Mac AutoBlockBreak v1.9
;;; Original: https://www.lee-mac.com
;;; Copyright (c) 2010 Lee Mac - www.lee-mac.com
;;; -----------------------------------------------------------------------

(defun c:auto-block-break ( / *error* blk obj ins sel tmp )

    (defun *error* ( msg )
        (@block:abb:endundo (@block:abb:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )

    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
            (princ "\n当前图层已锁定。")
        )
        (   (progn
                (while
                    (not
                        (progn
                            (setvar 'errno 0)
                            (initget "Browse Name Rotation")
                            (princ (strcat "\nAutomatic Block Rotation: " (getenv "LMac\\ABBRotation")))
                            (setq sel
                                (entsel
                                    (strcat "\n选择图块 [浏览/名称/旋转]"
                                        (if (= "" (setq blk (getvar 'insname)))
                                            ": "
                                            (strcat " <" blk ">: ")
                                        )
                                    )
                                )
                            )
                            (cond
                                (   (= 7 (getvar 'errno))
                                    (prompt "\n未选中，请重试。")
                                )
                                (   (null sel)
                                    (not (if (= "" blk) (setq blk nil)))
                                )
                                (   (= "Rotation" sel)
                                    (initget "ON OFF")
                                    (setenv "LMac\\ABBRotation"
                                        (cond
                                            (   (getkword
                                                    (strcat "\n自动块旋转 [开/关] <"
                                                        (getenv "LMac\\ABBRotation") ">: "
                                                    )
                                                )
                                            )
                                            (   (getenv "LMac\\ABBRotation")   )
                                        )
                                    )
                                    nil
                                )
                                (   (= "Name" sel)
                                    (while
                                        (not
                                            (or
                                                (= ""
                                                    (setq tmp
                                                        (getstring t
                                                            (strcat "\n指定块名称"
                                                                (if (= "" blk) ": " (strcat " <" blk ">: "))
                                                            )
                                                        )
                                                    )
                                                )
                                                (tblsearch "block" tmp)
                                            )
                                        )
                                        (princ (strcat "\n块 \"" tmp "\" 在当前图形中未定义。"))
                                    )
                                    (cond
                                        (   (/= "" tmp) (setq blk tmp))
                                        (   (/= "" blk))
                                    )
                                )
                                (   (= "Browse" sel)
                                    (setq blk (getfiled "选择图块" "" "dwg" 16))
                                )
                                (   (listp sel)
                                    (if (/= "INSERT" (cdr (assoc 0 (entget (car sel)))))
                                        (prompt "\n对象必须是图块。")
                                        (setq obj (vla-copy (vlax-ename->vla-object (car sel)))
                                              blk (@block:abb:blockname obj)
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (not (or blk obj))
            )
        )
        (   (setq ins (getpoint (strcat "\n指定 " (vl-filename-base blk) " 块的插入点: ")))
            (@block:abb:startundo (@block:abb:acdoc))
            (if (null obj)
                (setq obj
                    (vla-insertblock
                        (vlax-get-property (@block:abb:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                        (vlax-3D-point (trans ins 1 0))
                        blk
                        1.0
                        1.0
                        1.0
                        (angle '(0.0 0.0 0.0) (trans (getvar 'ucsxdir) 0 (trans '(0.0 0.0 1.0) 1 0 t) t))
                    )
                )
            )
            (if blk (setvar 'insname (vl-filename-base blk)))
            (vla-put-insertionpoint obj (vlax-3D-point (trans ins 1 0)))
            (@block:abb:autoblockbreak (vlax-vla-object->ename obj) (= "ON" (getenv "LMac\\ABBRotation")))
            (@block:abb:endundo (@block:abb:acdoc))
        )
    )
    (princ)
)

;;; ======== 修剪已有块（单个） ========

(defun c:auto-block-break-explode ( / *error* enx sel )

    (defun *error* ( msg )
        (@block:abb:endundo (@block:abb:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )
    
    (while
        (progn
            (setvar 'errno 0)
            (initget "Rotation")
            (princ (strcat "\nAutomatic Block Rotation: " (getenv "LMac\\ABBRotation")))
            (setq sel (entsel "\n选择要修剪的图块 [旋转]: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\n未选中，请重试。")
                )
                (   (= "Rotation" sel)
                    (initget "ON OFF")
                    (setenv "LMac\\ABBRotation"
                        (cond
                            (   (getkword
                                    (strcat "\n自动块旋转 [开/关] <"
                                        (getenv "LMac\\ABBRotation") ">: "
                                    )
                                )
                            )
                            (   (getenv "LMac\\ABBRotation")   )
                        )
                    )
                )
                (   (= 'ename (type (car sel)))
                    (cond
                        (   (/= "INSERT" (cdr (assoc 0 (setq enx (entget (car sel))))))
                            (princ "\n对象必须是图块。")
                        )
                        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (cdr (assoc 8 enx)))))))
                            (princ "\n选中的图块位于已锁定的图层上。")
                        )
                        (   t
                            (@block:abb:startundo (@block:abb:acdoc))
                            (@block:abb:autoblockbreak (car sel) (= "ON" (getenv "LMac\\ABBRotation")))
                            (@block:abb:endundo   (@block:abb:acdoc))
                        )
                    )
                    t
                )
            )
        )
    )
    (princ)
)

;;; ======== 批量选择修剪 ========

(defun c:auto-block-break-select ( / *error* inc rot sel )

    (defun *error* ( msg )
        (@block:abb:endundo (@block:abb:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )
    
    (setq rot (= "ON" (getenv "LMac\\ABBRotation")))
    (if (setq sel (ssget "_:L" '((0 . "INSERT"))))
        (progn
            (@block:abb:startundo (@block:abb:acdoc))
            (repeat (setq inc (sslength sel))
                (@block:abb:autoblockbreak (ssname sel (setq inc (1- inc))) rot)
            )
            (@block:abb:endundo (@block:abb:acdoc))
        )
    )
    (princ)
)


;;; ======== 核心子函数：自动块打断 ========

(defun @block:abb:autoblockbreak ( ent rot / *error* _furthestapart ang bbx brk cmd crv di1 di2 enx idx ins int lst ply sel tmp )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type ply)) (vlax-write-enabled-p ply))
            (vla-delete ply)
        )
        (if (= 'int (type cmd))
            (setvar 'cmdecho cmd)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )

    (defun _furthestapart ( lst / dis mxd out pt1 pt2 )
        (setq mxd 0.0)
        (while (setq pt1 (car lst))
            (foreach pt2 (setq lst (cdr lst))
                (if (< mxd (setq dis (distance pt1 pt2)))
                    (setq mxd dis
                          out (list pt1 pt2)
                    )
                )
            )
        )
        out
    )
  
    (if (and (= 'ename (type ent))
             (setq enx (entget ent))
             (= "INSERT" (cdr (assoc 0 enx)))
        )
        (progn
            (if
                (and
                    rot
                    (setq bbx (@block:abb:blockboundingbox (vlax-ename->vla-object ent)))
                    (setq sel
                        (ssget "_C"
                            (trans (car   bbx) 0 1)
                            (trans (caddr bbx) 0 1)
                           '((0 . "ARC,ELLIPSE,CIRCLE,LINE,XLINE,SPLINE,*POLYLINE"))
                        )
                    )
                    (progn
                        (setq ins (trans (cdr (assoc 10 enx)) ent 0)
                              crv (ssname sel (1- (sslength sel)))
                              di1 (distance ins (vlax-curve-getclosestpointto crv ins))
                        )
                        (repeat (setq idx (1- (sslength sel)))
                            (setq tmp (ssname sel (setq idx (1- idx))))
                            (if (< (setq di2 (distance ins (vlax-curve-getclosestpointto tmp ins))) di1)
                                (setq di1 di2
                                      crv tmp
                                )
                            )
                        )
                        (< di1 1e-4)
                    )
                    (setq par (vlax-curve-getparamatpoint crv (vlax-curve-getclosestpointto crv ins)))
                    (cond
                        (   (equal par (vlax-curve-getendparam crv) 1e-8)
                            (setq  par (vlax-curve-getparamatdist crv (- (vlax-curve-getdistatparam crv par) 1e-3)))
                        )
                        (   (equal par (vlax-curve-getstartparam crv) 1e-8)
                            (setq  par (vlax-curve-getparamatdist crv (+ (vlax-curve-getdistatparam crv par) 1e-3)))
                        )
                        (   t   )
                    )
                    (setq der (vlax-curve-getfirstderiv crv par))
                    (setq ang (angle '(0.0 0.0 0.0) (trans der 0 (cdr (assoc 210 (entget crv))))))
                    (or (<= ang (/ pi 2.0))
                        (< (/ (* 3.0 pi) 2.0) ang)
                        (setq ang (+ ang pi))
                    )
                )
                (vla-put-rotation (vlax-ename->vla-object ent) ang) ;; VL used to account for attributes
            )
            (if
                (and
                    (setq bbx (@block:abb:blockboundingbox (vlax-ename->vla-object ent)))
                    (setq sel
                        (ssget "_C"
                            (trans (car   bbx) 0 1)
                            (trans (caddr bbx) 0 1)
                           '((0 . "ARC,ELLIPSE,CIRCLE,LINE,XLINE,SPLINE,*POLYLINE"))
                        )
                    )
                )
                (progn
                    (setq ply
                        (vlax-ename->vla-object
                            (entmakex
                                (append
                                    (list
                                       '(000 . "LWPOLYLINE")
                                       '(100 . "AcDbEntity")
                                       '(100 . "AcDbPolyline")
                                       '(090 . 4)
                                       '(070 . 1)
                                        (cons 38 (cadddr (assoc 10 enx)))
                                    )
                                    (mapcar '(lambda ( p ) (mapcar '+ (cons 10 (trans p 0 ent)) '(0 0 0))) bbx)
                                    (list (assoc 210 enx))
                                )
                            )
                        )
                    )
                    (repeat (setq idx (sslength sel))
                        (setq ent (ssname sel (setq idx (1- idx))))
                        (if (setq int (@block:abb:intersections (vlax-ename->vla-object ent) ply acextendthisentity))
                            (setq lst (cons (cons ent int) lst))
                        )
                    )
                    (vla-delete ply)
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (foreach int lst
                        (if (setq brk (_furthestapart (cdr int)))
                            (command
                                "_.break" (list  (car int) (trans (car brk) 0 1)) "_F"
                                "_non"    (trans (car  brk) 0 1)
                                "_non"    (trans (cadr brk) 0 1)
                            )
                        )
                    )
                    (setvar 'cmdecho cmd)
                )
            )
        )
    )
    (princ)
)

;; Intersections  -  Lee Mac
;; Returns a list of all points of intersection between two objects.
;; obj1,obj2 - [vla] VLA-Objects with intersectwith method applicable
;; mode      - [int] acextendoption enum of intersectwith method
;; Returns: [lst] List of 3D WCS intersection points, else nil

(defun @block:abb:intersections ( obj1 obj2 mode / l r )
    (setq l (vlax-invoke obj1 'intersectwith obj2 mode))
    (repeat (/ (length l) 3)
        (setq r (cons (list (car l) (cadr l) (caddr l)) r)
              l (cdddr l)
        )
    )
    (reverse r)
)

;;-------------------=={ Block BoundingBox }==----------------;;
;;                                                            ;;
;;  Returns a point list describing a rectangular frame       ;;
;;  bounding all geometry of a supplied block reference.      ;;
;;  Excludes Text, MText & Attribute Definitions.             ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  blk - VLA Block Reference Object                          ;;
;;------------------------------------------------------------;;
;;  Returns: WCS Point list describing boundingbox of block   ;;
;;------------------------------------------------------------;;

(defun @block:abb:blockboundingbox ( blk / bnm llp lst urp )
    (setq bnm (strcase (vla-get-name blk)))
    (cond
        (   (setq lst (cdr (assoc bnm @block:abb:blockboundingbox:cache))))
        (   (progn
                (vlax-for obj (vla-item (@block:abb:acblk) bnm)
                    (cond
                        (   (= "AcDbBlockReference" (vla-get-objectname obj))
                            (setq lst (append lst (@block:abb:blockboundingbox obj)))
                        )
                        (   (and
                                (= :vlax-true (vla-get-visible obj))
                                (not (wcmatch (vla-get-objectname obj) "AcDbAttributeDefinition,AcDb*Text"))
                                (vlax-method-applicable-p obj 'getboundingbox)
                                (= :vlax-false (vla-get-freeze (vla-item (@block:abb:aclyr) (vla-get-layer obj))))
                                (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
                            )
                            (setq lst (vl-list* (vlax-safearray->list llp) (vlax-safearray->list urp) lst))
                        )
                    )
                )
                lst
            )
            (setq lst (mapcar '(lambda ( fun ) (apply 'mapcar (cons fun lst))) '(min max)))
            (setq lst
                (list
                    (car lst)
                    (list (caadr lst) (cadar lst))
                    (cadr lst)
                    (list (caar lst) (cadadr lst))
                )
            )
            (setq @block:abb:blockboundingbox:cache (cons (cons bnm lst) @block:abb:blockboundingbox:cache))
        )
    )
    (apply
        (function
            (lambda ( m v )
                (mapcar (function (lambda ( p ) (mapcar '+ (mxv m p) v))) lst)
            )
        )
        (refgeom (vlax-vla-object->ename blk))
    )
)

;;; 参考几何变换 (gile)
;;; 返回变换矩阵和插入点偏移

(defun @block:abb:refgeom ( ent / ang ang mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
)

;;; 矩阵×向量 - Vladimir Nesterovsky

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;;; 矩阵转置 - Doug Wilson

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;;; 矩阵×矩阵 - Vladimir Nesterovsky

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;;; 块名称 - Lee Mac
;;; 返回块引用的真实名称
                        
(defun @block:abb:blockname ( obj )
    (if (vlax-property-available-p obj 'effectivename)
        (defun @block:abb:blockname ( obj ) (vla-get-effectivename obj))
        (defun @block:abb:blockname ( obj ) (vla-get-name obj))
    )
    (@block:abb:blockname obj)
)

;;; 开始撤销组 - Lee Mac

(defun @block:abb:startundo ( doc )
    (@block:abb:endundo doc)
    (vla-startundomark doc)
)

;;; 结束撤销组 - Lee Mac

(defun @block:abb:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;; 活动文档 - Lee Mac

(defun @block:abb:acdoc nil
    (eval (list 'defun '@block:abb:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (@block:abb:acdoc)
)

;;; 块集合 - Lee Mac

(defun @block:abb:acblk nil
    (eval (list 'defun '@block:abb:acblk 'nil (vla-get-blocks (@block:abb:acdoc))))
    (@block:abb:acblk)
)

;;; 图层集合 - Lee Mac

(defun @block:abb:aclyr nil
    (eval (list 'defun '@block:abb:aclyr 'nil (vla-get-layers (@block:abb:acdoc))))
    (@block:abb:aclyr)
)


(if (null (getenv "LMac\\ABBRotation"))
    (setenv "LMac\\ABBRotation" "ON")
)


(vl-load-com)
(princ
    (strcat
        "\n:: auto-block-break.lsp | 重构自 AutoBlockBreak v1.9 | "
        "Copyright (c) 2010 Lee Mac www.lee-mac.com ::"
        "\n:: AUTO-BLOCK-BREAK - 插入并修剪 | AUTO-BLOCK-BREAK-EXPLODE/AUTO-BLOCK-BREAK-SELECT - 修剪已有块 ::"
    )
)
(princ)

;;; -----------------------------------------------------------------------
;;;                             End of File
;;; -----------------------------------------------------------------------