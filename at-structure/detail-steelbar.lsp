;;多段线批量偏移 - caoyin
;; 根据要求自行设定缺省值
;; $OFFSETTO-DIST$  - 偏移距离   (100)
;; $OFFSETTO-ERASE$ - 删除源     (nil=否   |  T=是    )
;; $OFFSETTO-DIR$   - 偏移方向   (nil=内   |  T=外    )
;; $OFFSETTO-LAYER$ - 图层       (nil=源   |  T=当前层)
;; $OFFSETTO-PLMOD$ - 多段线模式 (nil=连续 |  T=独立  )


(defun at-structure:detail-steelbar (/ *ERROR* GET-PLINE-VERTEXS:BULGES ADD-2P-PLINE OFFSET-PLINES SS DOC ZIN DST)
  (defun *ERROR* (M)
    (if ZIN (setvar 'DIMZIN ZIN))
    )
  (defun GET-PLINE-VERTEXS:BULGES (ENX / P)
    (if (and (setq P (assoc 10 ENX))
	     (setq ENX (member P ENX))
	     )
	(cons (cons (cdr (assoc 42 ENX)) (cdr P))
	      (GET-PLINE-VERTEXS:BULGES (cdr ENX))
	      )
	)
    )
  (defun ADD-2P-PLINE (P1 P2 B LAY NOR)
    (entmake (list '(0 . "LWPOLYLINE")        '(100 . "AcDbEntity")
                   (cons 8 LAY)               '(100 . "AcDbPolyline")
                   '(90  . 2)                 '(43 . 20)
                   (cons 10 (trans P1 0 NOR)) (cons 42 B)
                   (cons 10 (trans P2 0 NOR)) (cons 210 NOR)
		   )
	     )
    )
  (defun OFFSET-PLINES (SS D DEL DIR LAY MOD / E D1 PTS X V ENX NOR)
    (if (setq E (ssname SS 0))
	(progn
          (setq D1  D
		PTS (GET-PLINE-VERTEXS:BULGES (entget E))
		)
	  (if (equal (cdar PTS) (setq X (cdr (last PTS))) 1E-8)
	      (setq X (nth (- (length PTS) 2) PTS))
	      )
	  (setq V (mapcar (function (lambda (A B) (- (/ (+ A B) 2.0) A))) (cdar PTS) X))
	  (if (> (car (trans V 0 (vlax-curve-getFirstDeriv E 0))) 0)
	      (setq D (- D))
	      )
	  (if DIR (setq D (- D)))
	  (setq X (vlax-Invoke (vlax-ename->vla-object E) 'offset D))
	  (if DEL (entdel E))
	  (if MOD
	      (foreach O X
		       (setq ENX (entget (vlax-vla-object->ename O))
			     NOR (cdr (assoc 210 ENX))
			     PTS (GET-PLINE-VERTEXS:BULGES ENX)
			     )
		       (mapcar (function (lambda (P1 P2)
                                 (Add-2P-Pline (cdr P1) (cdr P2) (car P1) LAY NOR)
				 )
					 )
			       PTS (cdr PTS)
			       )
		       (vla-delete O)
		       )
              (if LAY (foreach O X (vla-put-layer O LAY)))
              )
          (OFFSET-PLINES (ssdel E SS) D1 DEL DIR LAY MOD)
	  )
	)
    )
  (if (setq SS (ssget '((0 . "LWPOLYLINE") (-4 . ">=") (90 . 3))))
      (progn
	(setq DOC (vla-get-ActiveDocument (vlax-get-acad-object))
              ZIN (getvar 'DIMZIN)
	      )
	(vla-EndUndoMark DOC)
	(or $OFFSETTO-DIST$
            (setq $OFFSETTO-DIST$ 100)
	    )
	(while
            (progn
              (setvar 'DIMZIN 0)
              (princ (strcat "\n当前设置: 删除源="
                             (if $OFFSETTO-ERASE$ "是" "否")
                             " 偏移方向="
                             (if $OFFSETTO-DIR$ "外" "内")
                             " 图层="
                             (if $OFFSETTO-LAYER$ "当前层" "源")
                             " 新对象段="
                             (if $OFFSETTO-PLMOD$ "独立" "连续")
			     )
		     )
              (initget 6 "Erase Dir Layer Mode")
              (setq DST (getdist (strcat
				  "\n指定偏移距离或 [删除源(E)/偏移方向(D)/图层(L)/新对象段(M)] <"
				  (rtos $OFFSETTO-DIST$) ">: "
				  )
				 )
		    )
              (cond
		((numberp DST)
		 (not (setq $OFFSETTO-DIST$ DST))
		 )
		((= DST "Erase")
		 (setq $OFFSETTO-ERASE$ (not $OFFSETTO-ERASE$))
		 T
		 )
		((= DST "Dir")
		 (setq $OFFSETTO-DIR$ (not $OFFSETTO-DIR$))
		 T
		 )
		((= DST "Layer")
		 (setq $OFFSETTO-LAYER$ (not $OFFSETTO-LAYER$))
		 T
		 )
		((= DST "Mode")
		 (setq $OFFSETTO-PLMOD$ (not $OFFSETTO-PLMOD$))
		 T
		 )
		((not DST)
		 (not (setq DST $OFFSETTO-DIST$))
		 )
		)
              )
	  )
	(OFFSET-PLINES SS DST $OFFSETTO-ERASE$
                       $OFFSETTO-DIR$
                       (if $OFFSETTO-LAYER$ (getvar 'CLAYER))
                       $OFFSETTO-PLMOD$
		       )
	(while (= (logand 8 (getvar 'UNDOCTL)) 8)
          (vla-EndUndoMark DOC)
	  )
	(setvar 'DIMZIN ZIN)
	)
      )
  )
