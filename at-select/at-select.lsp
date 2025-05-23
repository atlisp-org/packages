(@:define-config '@select:blksname "" "选择时要匹配的块名")
(@:define-config '@select:onboundary 1 "1 选择在边界上图元; 0 不选边界上的图元")
(@::define-config 'curve:similarity 0.95 "曲线相似度，0到1之间的值")
(defun @select:setup (/ res) 
  (setq @::tmp-search-str "@SELECT")
  (@::edit-config-dialog))
(defun c:ss1 (/ ss) 
  (@::prompt '("记录当前已选择的图形 为ss1。以方便其它命令使用。如果没有选择且高亮的图形，则高亮ss1"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss1 ss)
    (if ss1 
      (sssetfirst nil ss1))))
(defun c:ss2 (/ ss) 
  (@::prompt '("记录当前已选择的图形 为ss2。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss2 ss)
    (if ss2 
      (sssetfirst nil ss2))))
(defun c:ss3 (/ ss) 
  (@::prompt '("记录当前已选择的图形 为ss3。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss3 ss)
    (if ss3 
      (sssetfirst nil ss3))))
(defun c:ss4 (/ ss) 
  (@::prompt '("记录当前已选择的图形 为ss4。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss4 ss)
    (if ss4 
      (sssetfirst nil ss4))))
(defun c:ss5 (/ ss) 
  (@::prompt '("记录当前已选择的图形 为ss5。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss5 ss)
    (if ss5 
      (sssetfirst nil ss5))))
(defun boundarypath2pts (bdpath / pts) 
    "边界路径转栏选点集"
    ;; 不是多段线的处理
    (if (= 0 (boole 1 2 (cdr (assoc 92 bdpath)))) 
      (progn 
        (setq parts (list:split-by bdpath '(lambda (x) (= (car x) 72))))
        (setq parts (mapcar 
                      '(lambda (x) 
                         (cond 
                           ((= 2 (cdr (assoc 72 x)))
                            (list
                            (cons 
                              10
                              (polar 
                                (cdr (assoc 10 x))
                                (* 0.5 (+ (cdr (assoc 50 x)) (cdr (assoc 51 x))))
                                (cdr (assoc 40 x))))))
                           (t x)))
                      parts))
        (setq bdpath (apply 'append parts))))
    (setq pts (list:delsame 
                (mapcar 
                  'cdr
                  (vl-remove-if-not 
                    '(lambda (x) (or (= 10 (car x)) (= 11 (car x))))
                    bdpath))
                0.001)))
;; 自交点围栏点
(defun interself-p (pts / flag pt1 pt2 n)
  (while (and
	  (null flag)
	  (>= (length  pts) 4))
    (setq pt1 (car pts)
	  pt2 (cadr pts)
	  n 2)
    (repeat (- (length pts) 3)
	    (if (inters pt1 pt2 (nth n pts)(nth (1+ n) pts))
		(setq flag t))
	    (setq n (1+ n))
	    )
    (setq pts (cdr pts)))
  flag
  )

(defun at-select:select-blk-by-hatch (/ ha res all-outer ss-all all-inter ss-in 
                                      selopt) 
  (@::prompt '("选择一个填充，返回填充内的块。"))
  (setq selopt '("cp" "wp"))
  (if (/= 1 (@:get-config '@select:onboundary)) 
      (setq selopt (reverse selopt)))
  (setq hatchs (pickset:to-list (ssget '((0 . "hatch")))))
  (setq ss-all nil)
  (setq ss-in nil)
  (foreach hatch% hatchs
	   (setq ha (entget hatch%))
	   (setq res (cadr (list:split-by ha '(lambda (x) (= (car x) 91)))))
	   (setq res (car (list:split-by res '(lambda (x) (= (car x) 75)))))
	   (setq res (cdr (list:split-by res '(lambda (x) (= (car x) 92)))))
	   ;; (setq res (vl-sort res '(lambda (x y) (> (cdar x) (cdar y)))))
	   ;; 外部边界路径中的图元
	   (setq all-outer (vl-remove-if-not 
			    '(lambda (x) (= 1 (boole 1 1 (cdr (assoc 92 x)))))
			    res))
	   (princ (strcat (itoa (length all-outer)) "条外部边界路径"))
	   (foreach outer all-outer 
		    ;; debug (entity:make-lwpolyline (boundarypath2pts outer) nil 0 1 0)
		    (setq blk-all (pickset:to-list 
                    (ssget 
                      (car selopt)
                      (boundarypath2pts outer)
                      (append 
                       (list '(0 . "insert"))
                       (if (/= "" (@:get-config '@select:blksname)) 
                           (list 
                            (cons 2 (@:get-config '@select:blksname))))))))
		    (setq ss-all (list:union ss-all blk-all)))
	   (setq all-inter (vl-remove-if-not 
			    '(lambda (x) (= 0 (boole 1 1 (cdr (assoc 92 x)))))
			    res))
	   (princ (strcat (itoa (length all-inter)) "条内部孤岛边界路径"))
	   ;; 内部孤岛边界路径中的图元
	   (foreach inter all-inter 
		    (setq blk-in (pickset:to-list 
				  (ssget 
				   (cadr selopt)
				   (boundarypath2pts inter)
				   (append 
				    (list '(0 . "insert"))
				    (if (/= "" (@:get-config '@select:blksname)) 
					(list 
					 (cons 2 (@:get-config '@select:blksname))))))))
		    (if blk-in 
			(setq ss-in (list:union ss-in blk-in)))))
  (setq ss-res (list:difference ss-all ss-in))
  (sssetfirst nil (pickset:from-list ss-res)))

(defun @select:line-to-ss ()
  (@::prompt "绘制指向选择集的线。")
  (setq pt (getpoint))
  (mapcar 
  '(lambda (x) (entity:make-line pt (entity:getdxf x 10)))
    (vl-remove nil (pickset:to-list (cadr (ssgetfirst)))
)))
(defun at-select:select-blk-by-lwpl (/ lwpls res all-outer ss-all all-inter ss-in 
                                     selopt pts-fence) 
  (@::prompt '("选择一个单环闭合多段线，选中曲线内的块。"))
  (setq selopt '("cp" "wp"))
  (if (/= 1 (@:get-config '@select:onboundary)) 
      (setq selopt (reverse selopt)))
  (if (and (setq lwpl (car (pickset:to-list (ssget ":S" '((0 . "*polyline")(70 . 1))))))
	   (setq pts-fence (list:delsame (curve:get-points lwpl) 0.01))
	   (not (interself-p pts-fence)))
      (sssetfirst nil
		  (ssget 
		   (car selopt)
		   pts-fence
		   (append 
		    (list '(0 . "insert"))
		    (if (/= "" (@:get-config '@select:blksname)) 
			(list 
			 (cons 2 (@:get-config '@select:blksname)))))))))
  
(defun at-select:select-by-lwpl (/ lwpls res all-outer ss-all all-inter ss-in 
                                      selopt en ssfilter pts-fence) 
  (@::prompt '("选择一个单环闭合多段线，选中曲线内的图元。"))
  (setq selopt '("cp" "wp"))
  (if (/= 1 (@:get-config '@select:onboundary)) 
      (setq selopt (reverse selopt)))
  (@:prompt "选择一个单环闭合多段线:")
  (if (setq lwpl (car (pickset:to-list (ssget ":S" '((0 . "*polyline")(70 . 1))))))
      (if (and (setq pts-fence (list:delsame (curve:get-points lwpl) 0.01))
	       (not (interself-p pts-fence)))
	  (progn
	    (setq en (car (entsel"请点选要选择的图元:")))
	    (setq ssfilter
		  (if (= "INSERT" (entity:getdxf en 0))
		      (append 
		       (list '(0 . "INSERT"))
		       (list (cons 2 (entity:getdxf en 2))))
		      (append 
		       (list (cons 0 (entity:getdxf en 0))))))
	    (sssetfirst nil
			(ssget 
			 (car selopt)
			 pts-fence
			 ssfilter)))
	  (@:alert "所选的多段线不是单环的。"))
      ))
  
(defun at-select:select-by-hatch (/ ha res all-outer ss-all all-inter ss-in 
                                      selopt en ssfilter) 
  (@::prompt '("选择一个或多填充图形，再选择在填充图形内需要选中的图形。"))
  (setq selopt '("cp" "wp"))
  (if (/= 1 (@:get-config '@select:onboundary)) 
      (setq selopt (reverse selopt)))
  (@:prompt "请选择填充图形:")
  (setq hatchs (pickset:to-list (ssget '((0 . "hatch")))))
  (setq en (car (entsel"请点选要选择的图元:")))
  (setq ssfilter
	(if (= "INSERT" (entity:getdxf en 0))
	    (append 
	     (list '(0 . "INSERT"))
	     (list (cons 2 (entity:getdxf en 2))))
	    (append 
	     (list (cons 0 (entity:getdxf en 0))))))
  (setq ss-all nil)
  (setq ss-in nil)
  (foreach hatch% hatchs
	   (setq ha (entget hatch%))
	   (setq res (cadr (list:split-by ha '(lambda (x) (= (car x) 91)))))
	   (setq res (car (list:split-by res '(lambda (x) (= (car x) 75)))))
	   (setq res (cdr (list:split-by res '(lambda (x) (= (car x) 92)))))
	   ;; (setq res (vl-sort res '(lambda (x y) (> (cdar x) (cdar y)))))
	   ;; 外部边界路径中的图元
	   (setq all-outer (vl-remove-if-not 
			    '(lambda (x) (= 1 (boole 1 1 (cdr (assoc 92 x)))))
			    res))
	   (princ (strcat (itoa (length all-outer)) "条外部边界路径"))
	   (foreach outer all-outer 
		    ;; debug (entity:make-lwpolyline (boundarypath2pts outer) nil 0 1 0)
		    (setq blk-all (pickset:to-list 
                    (ssget 
                      (car selopt)
                      (boundarypath2pts outer)
		      ssfilter
		      )))
		    (setq ss-all (list:union ss-all blk-all)))
	   (setq all-inter (vl-remove-if-not 
			    '(lambda (x) (= 0 (boole 1 1 (cdr (assoc 92 x)))))
			    res))
	   (princ (strcat (itoa (length all-inter)) "条内部孤岛边界路径"))
	   ;; 内部孤岛边界路径中的图元
	   (foreach inter all-inter 
		    (setq blk-in (pickset:to-list 
				  (ssget 
				   (cadr selopt)
				   (boundarypath2pts inter)
				   ssfilter
				   )))
		    (if blk-in 
			(setq ss-in (list:union ss-in blk-in)))))
  (setq ss-res (list:difference ss-all ss-in))
  (sssetfirst nil (pickset:from-list ss-res)))
