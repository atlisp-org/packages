;; dim-leader 紧凑布局(布局 7): 两侧均衡、文字带绕点群中心居中、K 列交错、3 段正交引线
;; 依赖 dim-leader.lsp 中的几何/落图工具

(@:define-config 'dim-leader:side-distance 10.0 "布局7: 文字带距点群边界的距离")
(@:define-config 'dim-leader:leader-colors "1 3 5 2 4 6 30 40" "引线循环颜色(ACI 空格分隔, 如 1 3 5 2 4 6), 空为不配色")

(defun dim-leader:parse-colors (s / r)
  (setq r nil)
  (if (and s (= (type s) 'STR) (/= s ""))
    (setq r (vl-catch-all-apply 'read (list (strcat "(" s ")")))))
  (if (and (listp r) (vl-catch-all-error-p r))
    (setq r nil))
  (if (not (listp r)) (setq r nil))
  r)

(defun dim-leader:compact-side (g side up gap D K minX maxX yc
                                / m maxh maxw rowPitch colPitch pairs order lo hi
                                  res r c mm w h ty col tx box att ctr lane laneRank hj)
  (setq m (length g) maxh 0.0 maxw 0.0)
  (foreach mm g
    (if (> (nth 3 mm) maxh) (setq maxh (nth 3 mm)))
    (if (> (nth 2 mm) maxw) (setq maxw (nth 2 mm))))
  (setq rowPitch (+ maxh gap) colPitch (+ maxw gap))
  (setq pairs nil r 0)
  (foreach mm g
    (setq pairs (cons (cons (cadr (nth 0 mm)) r) pairs))
    (setq r (1+ r)))
  (setq pairs (vl-sort pairs (function (lambda (a b) (< (car a) (car b))))))
  (if up (setq pairs (reverse pairs)))
  (setq order (mapcar (function cdr) pairs))
  (if (= side 1)
    (setq lo (+ maxX 0.5) hi (+ maxX D -0.5))
    (setq lo (+ (- minX D) 0.5) hi (- minX 0.5)))
  (setq res nil r 0)
  (foreach c order
    (setq mm (nth c g)
          w (nth 2 mm) h (nth 3 mm) ctr (nth 0 mm)
          laneRank (if up (- m 1 r) r)
          lane (+ lo (* (/ (- hi lo) m) (+ laneRank 0.5)))
          ty (if up
               (+ yc (/ gap 2.0) (/ maxh 2.0) (* (- m 1 r) rowPitch))
               (- yc (/ gap 2.0) (/ maxh 2.0) (* (- m 1 r) rowPitch)))
          col (rem r K))
    (if (= side 1)
      (setq tx (+ maxX D (* col colPitch))
            box (list tx (+ tx w) (- ty (/ h 2.0)) (+ ty (/ h 2.0)))
            att (list (- tx (/ gap 2.0)) ty) hj 0)
      (setq tx (- minX D (* col colPitch))
            box (list (- tx w) tx (- ty (/ h 2.0)) (+ ty (/ h 2.0)))
            att (list (+ tx (/ gap 2.0)) ty) hj 2))
    (setq res (cons (list ctr (nth 1 mm) tx ty 0 hj 2
                          (dim-leader:dedupe-pts
                            (list ctr (list lane (cadr ctr)) (list lane ty) att)))
                    res))
    (setq r (1+ r)))
  (reverse res))

(defun dim-leader:layout-compact (matched obstacles
                                  / gap D K items m ctr ent dat ref w h maxw maxh
                                    n nl srt i left right grp gs med up dn side
                                    minX maxX minY maxY yc res)
  (setq gap (@:get-config 'dim-leader:gap))
  (if (not (and (numberp gap) (> gap 0.0))) (setq gap 1.0))
  (setq D (@:get-config 'dim-leader:side-distance))
  (if (not (and (numberp D) (> D 0.0))) (setq D 10.0))
  (setq K 3)
  (setq items nil maxw 0.0 maxh 0.0)
  (foreach m matched
    (setq ctr (nth 0 m) ent (nth 1 m) dat (entget ent))
    (setq ref (cdr (assoc 11 dat)))
    (if (null ref) (setq ref (cdr (assoc 10 dat))))
    (if (null ref) (setq ref ctr))
    (setq h (cdr (assoc 40 dat)))
    (if (not (and (numberp h) (> h 0.0))) (setq h 2.5))
    (setq w (dim-leader:text-fallback-width dat))
    (setq tb (vl-catch-all-apply 'textbox (list dat)))
    (if (and (listp tb) (car tb) (cadr tb) (listp (car tb)) (listp (cadr tb)))
      (setq w (abs (- (car (cadr tb)) (caar tb)))))
    (if (not (and (numberp w) (> w 0.0))) (setq w (* h 4.0)))
    (if (> w maxw) (setq maxw w))
    (if (> h maxh) (setq maxh h))
    (setq items (cons (list ctr ent w h) items)))
  (setq items (reverse items))
  (if (or (null items) (= 0.0 maxw) (= 0.0 maxh))
    (progn (setq dim-leader:*chip-error* "无可用文字尺寸") nil)
    (progn
      (setq minX 1.0e30 maxX -1.0e30 minY 1.0e30 maxY -1.0e30)
      (foreach it items
        (setq m (nth 0 it))
        (if (< (car m) minX) (setq minX (car m)))
        (if (> (car m) maxX) (setq maxX (car m)))
        (if (< (cadr m) minY) (setq minY (cadr m)))
        (if (> (cadr m) maxY) (setq maxY (cadr m))))
      (setq yc (/ (+ minY maxY) 2.0))
      (setq srt (vl-sort items (function (lambda (a b) (< (car (nth 0 a)) (car (nth 0 b)))))))
      (setq n (length srt) nl (fix (/ n 2)))
      (setq i 0 left nil right nil)
      (foreach it srt
        (if (< i nl) (setq left (cons it left)) (setq right (cons it right)))
        (setq i (1+ i)))
      (setq left (reverse left) right (reverse right))
      (setq res nil)
      (foreach pair (list (list left 2) (list right 1))
        (setq grp (car pair) side (cadr pair))
        (if grp
          (progn
            (setq gs (vl-sort grp (function (lambda (a b) (< (cadr (nth 0 a)) (cadr (nth 0 b)))))))
            (setq med (cadr (nth 0 (nth (fix (/ (length gs) 2)) gs))))
            (setq up nil dn nil)
            (foreach it gs
              (if (>= (cadr (nth 0 it)) med)
                (setq up (cons it up))
                (setq dn (cons it dn))))
            (setq dn (reverse dn))
            (setq res (append res (dim-leader:compact-side up side T gap D K minX maxX yc)))
            (setq res (append res (dim-leader:compact-side dn side nil gap D K minX maxX yc))))))
      (setq dim-leader:*chip-error* nil)
      res)))
