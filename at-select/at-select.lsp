(@:define-config '@select:blksname "" "选择时要匹配的块名")
(@:define-config '@select:onboundary 1 "1 选择在边界上图元; 0 不选边界上的图元")
(defun @select:setup (/ res) 
  (setq @:tmp-search-str "@SELECT")
  (@:edit-config))
(defun c:ss1 (/ ss) 
  (@:help '("记录当前已选择的图形 为ss1。以方便其它命令使用。如果没有选择且高亮的图形，则高亮ss1"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss1 ss)
    (if ss1 
      (sssetfirst nil ss1))))
(defun c:ss2 (/ ss) 
  (@:help '("记录当前已选择的图形 为ss2。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss2 ss)
    (if ss2 
      (sssetfirst nil ss2))))
(defun c:ss3 (/ ss) 
  (@:help '("记录当前已选择的图形 为ss3。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss3 ss)
    (if ss3 
      (sssetfirst nil ss3))))
(defun c:ss4 (/ ss) 
  (@:help '("记录当前已选择的图形 为ss4。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss4 ss)
    (if ss4 
      (sssetfirst nil ss4))))
(defun c:ss5 (/ ss) 
  (@:help '("记录当前已选择的图形 为ss5。"))
  (setq ss (cadr (ssgetfirst)))
  (if ss 
    (setq ss5 ss)
    (if ss5 
      (sssetfirst nil ss5))))


(defun at-select:select-blk-by-hatch (/ ha res all-outer ss-all all-inter ss-in 
                                      selopt) 
  (@:help '("选择一个填充，返回填充内的块。"))
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
  (@:help "绘制指向选择集的线。")
  (setq pt (getpoint))
  (mapcar 
  '(lambda (x) (entity:make-line pt (entity:getdxf x 10)))
    (vl-remove nil (pickset:to-list (cadr (ssgetfirst)))
)))
