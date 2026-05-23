;;;2016-9-30
;;;2021-2-4

;;; 路径的代理对象为LINE或CIRCLE，用于生成管道类部件，如水管、风管等
;;; 路径对象有两个可以实现自由伸缩的管口组成 由LINE对象的 10 11组码表示


(defun psk-path-getservice (path)
  (p-get path "SERV")
)
;;;(psk-path-create
;;;  (setq p (getpoint))
;;;  (getpoint p)
;;;  '( (".TYPE" . "DUCT-RECT") ("SERV" . "OA") ("W" . 500) ("H" . 320) ("ERF" . 0.8))
;;;)
;; ((-1 . <图元名: 7ff4e823f070>) (".CLASS" . "PATH") (".TYPE" . "DUCT-RECT") ("SERV" . "OA") ("W" . 500) ("H" . 320) ("ERF" . 0.8))
(defun psk-path-create (p1 p2 prop / comp line)
  (setq line (p-make-line p1 p2))

  (p-xprop-set-inner
    line
    "PSK-PATH"
    prop
    (mapcar (function (lambda (e) (cons (car e) (cadr e)))) $psk-prop-def)
  )
  (append (list (cons -1 line) (cons ".CLASS" "PATH")) prop)
)
;; (psk-path-getports (car (entsel)))
;; ((10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0) (11 (45632.5 13384.1 0.0) (1.0 0.0) 0.0))
(defun psk-path-getports (path / p1 p2)
  (if (/= 'ename (type path))
    (setq path (psk-comp-getename path))
  )
  (setq p1 (p-dxf path '(10 11))
        p2 (cadr p1)
        p1 (car p1)
  )
  (list (list 10 p1 (polar '(0 0) (angle p2 p1) 1.) 0.)
        (list 11 p2 (polar '(0 0) (angle p1 p2) 1.) 0.)
  )
)

(defun psk-path-getlength (path /)
  (if (/= 'ename (type path))
    (setq path (psk-comp-getename path))
  )

  (apply 'distance (p-dxf path '(10 11)))
)
;; 将路径靠近指定点的端点移到新的位置，返回后续移动量及需要移动的点
;; (psk-path-moveport (car (entsel)) (trans (getpoint) 1 0) (trans (getpoint) 1 0) t)
;; ((0.0 -172.645 0.0) (2714.66 2453.51 0.0) ...)
(defun psk-path-moveport (path p np resize / np2 offset p2 ports rp)
  (if (/= 'ename (type path))
    (setq path (psk-comp-getename path))
  )

  (setq ports (psk-ports-sort (psk-path-getports path) p))

  (if resize
    (progn
      ;; 允许伸缩管道
      ;; 对移动点到新点，对后续点仅应用非轴向的偏移分量
      (setq offset (mapcar '-
                           np
                           (vlax-curve-getclosestpointto
                             (p-ensure-object path)
                             np
                             t
                           )
                   )
      )

      (setq p2  (psk-port-pos (cadr ports))
            np2 (mapcar '+ offset p2)
      )
      (p-entmod
        path
        (list
          (cons (psk-port-id (car ports)) np)
          (cons (psk-port-id (cadr ports)) np2)
        )
      )

      (if (not (equal '(0. 0. 0.) offset 0.1))
        (setq rp (list offset p2))
      )
    )
    (progn
      ;; 将管道做为刚性整体处理
      (setq offset (mapcar '- np (psk-port-pos (car ports))))

      (if (not (equal '(0. 0. 0.) offset 0.1))
        (progn
          (setq p2  (psk-port-pos (cadr ports))
                np2 (mapcar '+ offset p2)
                rp  (list offset p2)
          )
          (p-entmod path
                    (list
                      (cons (psk-port-id (car ports)) np)
                      (cons (psk-port-id (cadr ports)) np2)
                    )
          )
        )
      )
    )
  )
  rp
)




;; (psk-path-getportsize (car (entsel)))
(defun psk-path-getportsize (path / tp)
  (if (= 'ename (type path))
    (setq path (psk-comp-load path))
  )
  (setq tp (p-get path ".TYPE"))
  (cond
    ((= tp "DUCT-RECT")
     (list (p-get path "W") (p-get path "H"))
    )
    ((= tp "DUCT-ROUND") (p-get path "D"))
    ((= tp "PIPE") (p-get path "DN"))
  )
)

;; utils
;; (psk-paths-pick 2)
(defun psk-paths-pick (limit / path paths r)
;;;  (while (setq path (ssget ":E:S" '((0 . "LINE") (-3 ("PSK-FIT")))))
  (princ "\n")
  (while (and
           (not (initget "  "))
           (< (length paths) limit)
           (/= "" (setq r (entsel "选择路径:")))
         )
    (if (and r
             (setq path (psk-comp-load (car r)))
             (not (member path (mapcar 'car paths)))
        )
      (progn
        (setq paths (cons (list path (p-line-closestpoint (car r) (trans (cadr r) 1 0) nil)) paths))
        (redraw (psk-comp-getename path) 3)
      )
    )
    (princ (strcat "\r选中了 " (itoa (length paths)) " 个对象, "))
  )

  (mapcar '(lambda (e) (redraw (psk-comp-getename (car e)) 4)) paths)
  (reverse paths)
)
;; (psk-line-keepend (car (entsel)) (getpoint) (getpoint))
;; 将直线在p点处分断，保留pkeep所在的部分
;; 返回一个点表 靠前的是需要移动到p的点
(defun psk-line-keepend (line p pkeep / a e ps)
  (setq p  (p-line-closestpoint line p t)
        ps (p-dxf line '(10 11))
        a  (angle p pkeep)
  )

  (mapcar
    'cdr
    (vl-sort
      (mapcar 'cons
              (mapcar '(lambda (e) (p-angle-include e a))
                      (mapcar '(lambda (e) (angle p e))
                              ps
                      )
              )
              ps
      )
      '(lambda (e1 e2)
         (if (equal (car e1) (car e2) 1e-3)
           (< (distance p (cdr e1)) (distance p (cdr e2)))
           (> (car e1) (car e2))
         )
       )
    )
  )
)
;;



(setq $psk-reducer-length 700)

;; (psk-paths-connect (psk-paths-pick 4) t)
(defun psk-paths-connect
       (paths draw / a1 a2 a3 a4 al d d1 d2 d3 d4 e erf lines p p1 p2 picks ports1 r)
  (setq
    picks       (mapcar 'cadr paths)
    paths       (mapcar 'car paths)
    lines       (mapcar 'psk-comp-getename paths)
;;;    picks (mapcar '(lambda (line p)
;;;		     (p-line-closestpoint line p nil)
;;;		   )
;;;		  lines
;;;		  picks
;;;	  )
    $psk-drafts (psk-drafts-fromviewport)
  )
  (cond
    ((= 2 (length paths))
     (if (p-line-parallel (car lines) (cadr lines))
       ;; 生成异径
       (progn
         (setq ports1 (psk-comp-getports (car paths))
               p      (p-line-closestpoint
                        (car lines)
                        (mapcar '(lambda (e) (/ e 2.)) (mapcar '+ (car picks) (cadr picks)))
                        t
                      )
               ports1 (psk-ports-sort ports1 p)
         )
         (setq r (psk-create-reducer
                   p
;;;                   (setq $psk-reducer-length (p-edit-value "输入异径长度" $psk-reducer-length))
		   (setq $psk-reducer-length (p-getdist "输入异径长度" $psk-reducer-length (trans p 0 1)))
                   (psk-port-angle (car ports1))
                   (psk-path-getportsize (car paths))
                   (psk-path-getportsize (last paths))
                   (setq al (psk-editor-getalignmode))
                   (list
                     (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-RED"))
                     (cons "SERV" (p-get (car paths) "SERV"))
                     (cons "AL" al)
                   )
                 )
         )
         (psk-path-moveport
           (car paths)
           (car (psk-line-keepend (car lines) p (car picks)))
           (cadr r)
           t
         )
         (psk-path-moveport
           (last paths)
           (car (psk-line-keepend (last lines) p (last picks)))
           (caddr r)
           t
         )
       )
       ;; 生成弯头
       (progn
         (setq p1  (p-dxf (car lines) '(10 11))
               p2  (p-dxf (last lines) '(10 11))
               p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

               a1  (angle p (car picks))
               a2  (angle p (cadr picks))

               d   (psk-path-getportsize (car paths))
               erf (p-get (car paths) "ERF")
         )

         (setq r (psk-create-elbow
                   p
                   a1
                   a2
                   d
                   erf
                   (list
                     (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-EL"))
                     (p-get1 (car paths) "SERV")
                     (cons "ERF" erf)
                   )
                 )
         )

         (psk-path-moveport
           (car paths)
           (car (psk-line-keepend (car lines) p (car picks)))
           (cadr r)
           t
         )
         (psk-path-moveport
           (last paths)
           (car (psk-line-keepend (last lines) p (last picks)))
           (caddr r)
           t
         )
       )
     )
    )
    ((= 3 (length paths))
     (cond
       ;; 燕尾三通
       ((and (equal $pi/2
                    (p-angle-include
                      (p-line-getangle (car lines))
                      (p-line-getangle (cadr lines))
                    )
                    $psk-angle-tolerance
             )
             (equal $pi/2
                    (p-angle-include
                      (p-line-getangle (car lines))
                      (p-line-getangle (caddr lines))
                    )
                    $psk-angle-tolerance
             )
        )
        (setq p1  (p-dxf (car lines) '(10 11))
              p2  (p-dxf (last lines) '(10 11))
              p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

              a1  (angle p (car picks))
              a2  (angle p (cadr picks))
              a3  (angle p (caddr picks))

              d1  (psk-path-getportsize (car paths))
              d2  (psk-path-getportsize (cadr paths))
              d3  (psk-path-getportsize (caddr paths))
              erf (p-get (car paths) "ERF")
        )
        (setq r (psk-create-tee
                  p
                  a1
                  a2
                  a3
                  d1
                  d2
                  d3
                  erf
                  (list
                    (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-TEE"))
                    (cons "SERV" (p-get (car paths) "SERV"))
                    (cons "ERF" erf)
                  )
                )
        )
        (psk-path-moveport
          (car paths)
          (car (psk-line-keepend (car lines) p (car picks)))
          (cadr r)
          t
        )
        (psk-path-moveport
          (cadr paths)
          (car (psk-line-keepend (cadr lines) p (cadr picks)))
          (caddr r)
          t
        )
        (psk-path-moveport
          (caddr paths)
          (car (psk-line-keepend (caddr lines) p (caddr picks)))
          (cadddr r)
          t
        )
       )
       ;; 分支三通
       ((and (p-line-parallel (car lines) (cadr lines))
             (equal $pi/2
                    (p-angle-include
                      (p-line-getangle (car lines))
                      (p-line-getangle (caddr lines))
                    )
                    $psk-angle-tolerance ;; TODO: config toler param
             )
        )
        (setq
          p1  (p-dxf (car lines) '(10 11))
          p2  (p-dxf (last lines) '(10 11))
          p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

          a1  (angle p (car picks))
          a3  (angle p (caddr picks))

          erf (p-get (car paths) "ERF")

          d1  (psk-path-getportsize (car paths))
          d2  (psk-path-getportsize (cadr paths))
          d3  (psk-path-getportsize (caddr paths))
        )

        (if (not (equal d1 d2))
          (progn
            (setq al (psk-editor-getalignmode))
            (setq r (psk-create-tee2
                      p
                      a1
                      a3
                      d1
                      d2
                      d3
                      erf
                      al
                      (list
                        (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-TEES"))
                        (cons "SERV" (p-get (car paths) "SERV"))
                        (cons "ERF" erf)
                        (cons "AL" al)
                      )
                    )
            )
            (psk-path-moveport
              (car paths)
              (car (psk-line-keepend (car lines) p (car picks)))
              (cadr r)
              t
            )
            (psk-path-moveport
              (cadr paths)
              (car (psk-line-keepend (cadr lines) p (cadr picks)))
              (caddr r)
              t
            )
            (psk-path-moveport
              (caddr paths)
              (car (psk-line-keepend (caddr lines) p (caddr picks)))
              (cadddr r)
              t
            )
          )
          (progn
            ;; 主管同径
            (if (atom d1)
              (progn
                ;; 水管风格
                (setq r (psk-create-pipetee
                          p
                          a1
                          a3
                          d1
                          d3
                          (list
                            (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-TEEP"))
                            (cons "SERV" (p-get (car paths) "SERV"))
                          )
                        )
                )
                (psk-path-moveport
                  (car paths)
                  (car (psk-line-keepend (car lines) p (car picks)))
                  (cadr r)
                  t
                )
                (psk-path-moveport
                  (cadr paths)
                  (car (psk-line-keepend (cadr lines) p (cadr picks)))
                  (caddr r)
                  t
                )
                (psk-path-moveport
                  (caddr paths)
                  (car (psk-line-keepend (caddr lines) p (caddr picks))
                  )
                  (cadddr r)
                  t
                )
              )
              (progn
                ;; 风管
                (setq r (psk-create-branch
                          p
                          a1
                          a3
                          d1
                          d3
                          erf
                          (list
                            (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-BR"))
                            (cons "SERV" (p-get (car paths) "SERV"))
                            (cons "ERF" erf)
                          )
                        )
                )
                (psk-path-moveport
                  (car paths)
                  (car (psk-line-keepend (car lines) p (car picks)))
                  p
                  t
                )
                (psk-path-moveport
                  (cadr paths)
                  (car (psk-line-keepend (cadr lines) p (cadr picks)))
                  p
                  t
                )
                (psk-path-moveport
                  (caddr paths)
                  (car (psk-line-keepend (caddr lines) p (caddr picks))
                  )
                  (cadr r)
                  t
                )
              )
            )
          )
        )
       )
     )
    )
    ((= 4 (length paths))
     (if
       (and (p-line-parallel (car lines) (cadr lines))
            (equal $pi/2
                   (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (caddr lines)))
                   $psk-angle-tolerance
            )
            (equal $pi/2
                   (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (last lines)))
                   $psk-angle-tolerance
            )
       )
        (progn
          (setq p1  (p-dxf (car lines) '(10 11))
                p2  (p-dxf (last lines) '(10 11))
                p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

                a1  (angle p (car picks))
                a2  (angle p (cadr picks))
                a3  (angle p (caddr picks))
                a4  (angle p (last picks))

                d1  (psk-path-getportsize (car paths))
                d2  (psk-path-getportsize (cadr paths))
                d3  (psk-path-getportsize (caddr paths))
                d4  (psk-path-getportsize (last paths))

                erf (p-get (car paths) "ERF")
          )

          (setq r (psk-create-cross
                    p
                    a1
                    a2
                    a3
                    a4
                    d1
                    d2
                    d3
                    d4
                    erf
                    (list
                      (cons ".TYPE" (strcat (p-get (car paths) ".TYPE") "-CR"))
                      (cons "SERV" (p-get (car paths) "SERV"))
                      (cons "ERF" erf)
                    )
                  )
          )

          (psk-path-moveport
            (car paths)
            (car (psk-line-keepend (car lines) p (car picks)))
            (cadr r)
            t
          )
          (psk-path-moveport
            (cadr paths)
            (car (psk-line-keepend (cadr lines) p (cadr picks)))
            (caddr r)
            t
          )
          (psk-path-moveport
            (caddr paths)
            (car (psk-line-keepend (caddr lines) p (caddr picks)))
            (cadddr r)
            t
          )
          (psk-path-moveport
            (last paths)
            (car (psk-line-keepend (last lines) p (last picks)))
            ;; TODO:
            (last r)
            t
          )
        )
     )
    )
  )
  (if (and draw r)
    (progn
      ;; 绘制生成的管件
      (psk-comp-draw (psk-comp-load (car r)))
      ;; 绘制生成管件的所有路径
      (foreach path paths
        (psk-comp-redraw1 path)
      )
    )
  )
)
;;

;;;(setq $psk-angle-tolerance 0.02)

;; (psk-line-break (car (entsel)) (getpoint) (getpoint))
;; 如果创建了新对象 返回该对象 否则返回nil
(defun psk-line-break (line p1 p2 / a1 a2 a3 dxf en p0 p3 pts)
  (setq dxf (entget line '("*"))
        p0  (p-get dxf 10)
        p1  (p-line-closestpoint line p1 t)
        p2  (p-line-closestpoint line p2 t)
        p3  (p-get dxf 11)

        a1  (angle p0 p1)
        a2  (angle p0 p2)
        a3  (angle p0 p3)
        ;; (0.707 10.25 (200 200 0) 10)
        pts (list (list a1 (distance p0 p1) p1)
                  (list a2 (distance p0 p2) p2)
                  (list a3 (distance p0 p3) p3 11)
            )
        ;; 按距离排序
        pts (vl-sort pts '(lambda (e1 e2) (< (cadr e1) (cadr e2))))
        ;; 按方向排序
        pts (vl-sort pts
                     '(lambda (e1 e2)
                        (and (not (equal (car e1) (car e2) 1e-6))
                             (< (car e1) (car e2))
                        )
                      )
            )
  )

  ;; 打断点与直线端点存在5种关系 其中3种打断点分布在起点同一侧
  (if (and
        ;; 根据角度判断所有点分布在起点同一侧
        (equal (caar pts) (caadr pts) 1e-6)
        (equal (caar pts) (caaddr pts) 1e-6)
      )
    (if (= 11 (last (last pts)))
      ;; 情况1/3 打断点均落在直线内
      (progn
        (p-entmod line (cons 11 (caddr (car pts))))
        (setq dxf (p-unset dxf '(-1 5)))
        (entmake (p-set dxf (cons 10 (caddr (cadr pts)))))
        (setq en (entlast))
      )
      (if (/= 11 (last (car pts)))
        ;; 情况2/3 有一个打断点落在直线内，缩短直线，不创建第二条直线
        (p-entmod line (cons 11 (caddr (car pts))))
      )
      ;; 情况3/3 打断点均不在直线内（不处理）
    )
    ;; 情况1/2 两打断点在起点两侧
    (if (equal a1 a3 1e-6)
      (p-entmod line (cons 10 p1))
      (if (equal a2 a3 1e-6)
        (p-entmod line (cons 10 p2))
      )
    )
    ;; 情况2/2 两打断点在起点外（不处理）
  )
  en
)


;;;(psk-line-breakat (car (entsel)) (getpoint))
;; 如果创建了新对象 返回该对象 否则返回nil
(defun psk-line-breakat (line p2 / a1 a2 dxf en p0 p1)
  (setq dxf (entget line '("*"))
        p0  (p-get dxf 10)
        p1  (p-get dxf 11)
        p2  (p-line-closestpoint line p2 t)

        a1  (angle p0 p1)
        a2  (angle p0 p2)
  )

  ;; 如果打断点在直线端点上时不做任何处理
  (if (and (not (equal p2 p1 1e-3))
           (not (equal p2 p0 1e-3))
      )
    ;; 打断点与直线端点存在3种关系
    (if (equal (p-angle-include a1 a2) 0. 1e-6)
      (if (< (distance p0 p2) (distance p0 p1))
        ;; 1.点在直线内
        (progn
          (p-entmod line (cons 11 p2))
          (setq dxf (p-unset dxf '(-1 5)))
          (entmake (p-set dxf (cons 10 p2)))
          (setq en (entlast))
        )
        ;; 2.点在直线终点外
        (p-entmod line (cons 11 p2))
      )
      ;; 3.打断点在直线起点之前
      (p-entmod line (cons 10 p2))
    )
  )
  en
)
;; 共线直线合并 不检查共线
;;;(p-line-join (car (entsel)) (car (entsel)))
(defun p-line-join (line1 line2 / dist p1 p2 p3 p4)
  (if (not (equal line1 line2))
    (progn
      (mapcar (function set) '(p1 p2) (p-dxf line1 '(10 11)))
      (mapcar (function set) '(p3 p4) (p-dxf line2 '(10 11)))

      ;; 求合并后直线的两端点（直线最长）
      ;; 转换各点到 p1为原点 p1->p2为x轴的坐标系统上方便比较
      (setq dist (mapcar '- p2 p1)
            dist (list (list (last (trans p1 0 dist)) p1)
                       (list (last (trans p2 0 dist)) p2)
                       (list (last (trans p3 0 dist)) p3)
                       (list (last (trans p4 0 dist)) p4)
                 )
            dist (vl-sort dist
                          (function (lambda (e1 e2)
                                      (< (car e1) (car e2))
                                    )
                          )
                 )
            dist (mapcar 'cadr dist)
      )
      (p-entmod line1 (list (cons 10 (car dist)) (cons 11 (last dist))))
      (entdel line2)
    )
  )
)
;; (psk-path-createattach (caar (psk-paths-pick 1)) (getpoint "指定插入点") '(("NAME" . "DS") ("A" . 500) ("B" . 250)))
(defun psk-path-createattach (path p prop / a en line)
  (setq line (psk-comp-getename path)
        p    (p-line-closestpoint line p t)
        a    (p-line-getangle line)
        en   (psk-part-createblock p a prop "ATTACH" nil)
  )

  (psk-line-breakat line p)
)
;;

(defun psk-path-createinline (path p a prop / en len line p2)
  (setq line (psk-comp-getename path)
        en   (psk-part-createblock p a prop "INLINE" nil)
        len  (p-get prop "L")
        p2   (polar p a len)
  )
  (psk-line-break line p p2)
)
;;

(defun psk-path-createend (path p prop / a en line name ports)
  (setq line  (psk-comp-getename path)
        ports (psk-path-getports path)
        ports (psk-ports-sort ports p)
        a     (+ pi (psk-port-angle (car ports)))
        p     (psk-port-pos (car ports))
        en    (psk-part-createblock p a prop "END" nil)
  )
)


;; 批量在交点处插入分歧管
;; (psk-vrf-branchs (psk-paths-pick 2))
(defun psk-vrf-branchs (paths / a1 a2 en lines p p1 p2 p3 p4 picks)
  (setq picks (mapcar 'cadr paths)
        paths (mapcar 'car paths)
        lines (mapcar 'psk-comp-getename paths)
        p     (p-line-getinters (car lines) (cadr lines))
        a1    (angle p (car picks))
        a2    (angle p (cadr picks))
        p1    (polar p a1 500.)
        p2    (polar p1 (+ pi a1) 300.)
        p3    (polar p2 a2 100.)
        p4    (polar p3 (+ pi a1) 200.)
  )

  (psk-set-customlayerbyid "REF-PIPE")

  (command
    "._insert"
    (psk-get-filename "\\dwg\\VRF-BRANCH.dwg")
    "NON"
    (trans p1 0 1)
    1.
    1.
    (p-rad->deg
      (+ pi (angle (trans p 0 1) (trans (car picks) 0 1)))
    )
  )
  (setq en (entlast))

  (p-entmod en (cons 8 $addnew-layer))

  (psk-comp-save
    (list
      '(".TYPE" . "VRF-BRANCH")
      (cons "SERV" "REF")
    )
    en
    "PSK-PART"
  )

  (psk-ports-store
    en
    (list (list "0" '(0 0 0) (polar '(0 0 0) a1 1.) 0)
          (list "1" (mapcar '- p2 p1) (polar '(0 0 0) a1 -1.) 0)
          (list "2" (mapcar '- p3 p1) (polar '(0 0 0) a1 -1.) 0)
    )
  )

  (psk-line-break (car lines) p1 p2)

  (setq en (p-make-line p3 p4))
  (psk-comp-save
    (list
      '(".TYPE" . "REFPIPE")
      (cons "SERV" "REF")
    )
    en
    "PSK-PATH"
  )

  (psk-path-moveport
    (cadr lines)
    (car (psk-line-keepend (cadr lines) p4 (cadr picks)))
    p4
    t
  )
)


