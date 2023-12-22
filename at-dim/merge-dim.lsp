(defun getxx (x / xy xl a b bb) 
  (setq xy (entity:getdxf x 10)
        xl (entget (entity:getdxf (tblsearch "block" (entity:getdxf x 2)) -2))
        A  (angle (entity:getdxf xl 10) (entity:getdxf xl 11)))
  (cond 
    ((equal (abs (sin A)) 1 1e-6) ;;水平
     (list (cadr xy) (sin A)))
    ((equal (sin A) 0 1e-6) ;;垂直
     (list (car xy) (sin A)))
    (t
     (setq B  (+ A (* 0.5 pi))
           bb (- (cadr xy) (* (/ (sin B) (cos B)) (car xy))))
     (list bb (sin A)))))
(defun getmxy (ssent / ptx xx yy) 
  (foreach x ssent 
    (setq ptx (cons (entity:getdxf (entget x)13) ptx)
          ptx (cons (entity:getdxf (entget x) 14) ptx)))
  (setq xl (entget (entity:getdxf(tblsearch "block" (entity:getdxf(entget (car ssent))2))-2))
        A  (angle (entity:getdxf xl 10) (entity:getdxf xl 11)))
  (cond 
    ((equal SA 1 1e-6) (setq ppt (Lsort ptx 0)))
    ((equal SA 0 1e-6) (setq ppt (Lsort ptx 1)))
    (t (setq ppt (Lsort ptx 2))))
  (list (car ppt) (last ppt)))

(defun Lsort (LT i) 
  (cond 
    ((or (= i 0) (= i 2))
     (setq Lt (vl-sort LT (function (lambda (e1 e2) (< (car e1) (car e2)))))))
    ((or (= i 1) (= i 2))
     (setq Lt (vl-sort LT (function (lambda (e1 e2) (< (cadr e1) (cadr e2)))))))))

(defun @dim:merge-dim (/ ss ic xic aa bb n ent1) 
  (@:help "合并多个连续的标注为一个")
  (@:prompt "请选择需要合并的连续的标注")
  (setq ss   (ssget '((0 . "DIMENSION")))
        sumn (sslength ss)
        n    0
        xss  '()
        aa   '())
  (repeat sumn 
    (setq xss (cons (ssname ss n) xss)
          n   (1+ n))) ;;选择集改表
  (while (car xss) 
    (setq bb  '()
          bb  (cons (car xss) bb)
          ic  (getxx (entget (car xss)))
          xss (cdr xss)
          yss xss)
    (while (car yss) 
      (setq ent1 (car yss)
            xic  (getxx (entget ent1)))
      (if (apply 'and (mapcar '(lambda (x y) (equal x y 1e-5)) xic ic)) 
        (setq bb  (cons ent1 bb)
              xss (vl-remove ent1 xss)))
      (setq yss (cdr yss)))
    (setq aa (cons bb aa)))
  (setq n 0)
  (repeat (length aa) 
    (setq ent1 (nth n aa)
          n    (1+ n))
    (if (cdr ent1) 
      (progn 
        (setq ptx (getmxy ent1)
              xf  (entget (car ent1))
              nxf (subst (cons 13 (car ptx)) (assoc 13 xf) xf)
              wxf (subst (cons 14 (cadr ptx)) (assoc 14 nxf) nxf)
              xx  (cdr ent1))
        (entmod wxf)
        (while xx (entdel (car xx)) (setq xx (cdr xx))))))
  (princ))