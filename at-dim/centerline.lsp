;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @dim:centerline - 圆弧/圆/椭圆中心线工具
;;; 基于 Lee Mac Centerline v1.2
;;; MIT License - Copyright 2013 Lee Mac
;;; 重构为 @dim: 命名空间
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 创建圆弧/圆/椭圆的中心线
;;; 命令: centerline
(defun c:centerline ( / ang dis ent pt1 pt2 )
    (if
        (and
            (progn
                (while
                    (progn (setvar 'errno 0)
                        (setq ent (car (entsel "\n选择圆弧、圆或椭圆: ")))
                        (cond
                            (   (= 7 (getvar 'errno))
                                (princ "\n未选中，请重试。")
                            )
                            (   (= 'ename (type ent))
                                (if (not (wcmatch (cdr (assoc 0 (entget ent))) "CIRCLE,ARC,ELLIPSE"))
                                    (princ "\n无效对象。")
                                )
                            )
                        )
                    )
                )
                (= 'ename (type ent))
            )
            (setq pt1 (trans (cdr (assoc 10 (entget ent))) ent 1))
            (setq pt2 (getpoint "\n指定中心线长度和方向: " pt1))
            (setq ang (angle pt1 pt2)
                  dis (distance pt1 pt2)
            )
        )
        (repeat 2
            (entmake
                (list
                   '(0 . "LINE")
                    (cons 10 (trans (polar pt1 ang dis) 1 0))
                    (cons 11 (trans (polar pt1 ang (- dis)) 1 0))
                )
            )
            (setq ang (+ ang (/ pi 2.0))
                  dis (- dis)
            )
        )
    )
    (princ)
)
(princ)
