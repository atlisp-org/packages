;;------------------------=={ Chain Selection }==-----------------------;;
;;                                                                      ;;
;;  本程序提示用户选择一个对象，并生成所有与选中对象共享端点的             ;;
;;  "链式选择集"。                                                       ;;
;;                                                                      ;;
;;  详细说明：当提示选择时，用户可以选择直线、圆弧或椭圆弧、              ;;
;;  开放的轻量多段线或样条曲线，或二维多段线。程序将返回所有端点          ;;
;;  与已选对象端点重合的对象的选择集，包括与选择集中任何对象端点           ;;
;;  重合的对象，形成一个"链式"选择集。                                    ;;
;;                                                                      ;;
;;  本版本排除位于冻结、锁定或关闭图层上的对象。                         ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2012  -  www.lee-mac.com              ;;
;;  Refactored: @lisp/at-select, 2024                                   ;;
;;----------------------------------------------------------------------;;

(defun c:chain-sel ( / df en ex fl fz in l1 l2 s1 s2 sf vl )

    (setq fz 1e-8) ;; 点比较容差
    
    (while (setq df (tblnext "layer" (not df)))
        (if
            (or
                (minusp (cdr (assoc 62 df)))
                (< 0 (logand 5 (cdr (assoc 70 df))))
            )
            (setq ex (cons (cons 8 (@select:chain-escape-wildcards (cdr (assoc 2 df)))) ex))
        )
    )
    
    (setq sf
        (append
            (list
               '(-4 . "<OR")
                   '(0 . "LINE,ARC")
                   '(-4 . "<AND")
                       '(0 . "LWPOLYLINE,SPLINE")
                       '(-4 . "<NOT")
                           '(-4 . "&=")
                           '(70 . 1)
                       '(-4 . "NOT>")
                   '(-4 . "AND>")
                   '(-4 . "<AND")
                       '(0 . "POLYLINE")
                       '(-4 . "<NOT")
                           '(-4 . "&")
                           '(70 . 89)
                       '(-4 . "NOT>")
                       '(-4 . "AND>")
                   '(-4 . "<AND")
                       '(0 . "ELLIPSE")
                       '(-4 . "<OR")
                           '(-4 . "<>")
                           '(41 . 0.0)
                           '(-4 . "<>")
                            (cons 42 (+ pi pi))
                       '(-4 . "OR>")
                   '(-4 . "AND>")
               '(-4 . "OR>")
                (if (= 1 (getvar 'cvport))
                    (cons 410 (getvar 'ctab))
                   '(410 . "Model")
                )
            )
            (if ex
                (append
                   '(
                        (-4 . "<NOT")
                        (-4 . "<OR")
                    )
                    ex
                   '(
                        (-4 . "OR>")
                        (-4 . "NOT>")
                    )
                )
            )
        )
    )
    
    (if (setq s1 (ssget "_X" sf))
        (if (setq en (ssget "_+.:E:S" sf))
            (progn
                (setq s2 (ssadd)
                      en (ssname en 0)
                      l1 (list (vlax-curve-getstartpoint en) (vlax-curve-getendpoint en))
                )
                (repeat (setq in (sslength s1))
                    (setq en (ssname s1 (setq in (1- in)))
                          vl (cons (list (vlax-curve-getstartpoint en) (vlax-curve-getendpoint en) en) vl)
                    )
                )
                (while
                    (progn
                        (foreach v vl
                            (if (vl-some '(lambda ( p ) (or (equal (car v) p fz) (equal (cadr v) p fz))) l1)
                                (setq s2 (ssadd (caddr v) s2)
                                      l1 (vl-list* (car v) (cadr v) l1)
                                      fl t
                                )
                                (setq l2 (cons v l2))
                            )
                        )
                        fl
                    )
                    (setq vl l2 l2 nil fl nil)
                )
            )
        )
        (princ "\n未找到有效对象。")
    )
    (sssetfirst nil s2)
    (princ)
)

;; 转义通配符 - Lee Mac
;; 转义字符串中的通配符特殊字符

(defun @select:chain-escape-wildcards ( str )
    (vl-list->string
        (apply 'append
            (mapcar
               '(lambda ( c )
                    (if (member c '(35 64 46 42 63 126 91 93 45 44))
                        (list 96 c)
                        (list c)
                    )
                )
                (vl-string->list str)
            )
        )
    )
)
        
;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: chain-sel.lsp | Chain Selection | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2012")
        " www.lee-mac.com ::"
        "\n:: 命令 \"chain-sel\" 启动链式选择 ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;
