(defun @block:box-overlap? (box1 box2 tol / dx dy)
  "判断两个包围盒是否有超过容差的正面积重叠。
  仅边线/角相接(重叠量<=tol)不算重叠，用于排除并排块(如车位块边线重合)的误判。
  box1/box2: (点 点) 两个对角点，如 ((x1 y1 z1) (x2 y2 z2))，无需保证顺序。
  tol: 容差，重叠量小于等于该值视为不重叠，缺省 0.1。
  return: T 重叠，nil 不重叠。
  示例: (@block:box-overlap? '((0 0 0) (3000 6000 0)) '((3000 0 0) (6000 6000 0))) ; 并排 => nil
        (@block:box-overlap? '((0 0 0) (3000 6000 0)) '((2900 0 0) (5900 6000 0))) ; 重叠100 => T"
  (and box1 box2
       (setq tol (if (numberp tol) tol 0.1)
             dx (- (min (caadr box1) (caadr box2))
                   (max (caar box1) (caar box2)))
             dy (- (min (cadadr box1) (cadadr box2))
                   (max (cadar box1) (cadar box2))))
       (> dx tol)
       (> dy tol)))
(defun @block:overlap-pairs (blks-boxes tol / pairs rest a a-box others b)
  "从 块-包围盒 列表中找出所有包围盒相互重叠(重叠量>tol)的块对。
  blks-boxes: ((blk (p1 p3)) ...)，blk 为 ename，(p1 p3) 为包围盒两角点。
  tol: 重叠容差，见 @block:box-overlap?。
  return: ((blk1 blk2) ...) 重叠块对列表。
  示例: (@block:overlap-pairs '((e1 ((0 0 0) (3000 6000 0))) (e2 ((2900 0 0) (5900 6000 0)))) 0.1)
       => ((e1 e2))"
  (setq pairs nil
        blks-boxes
        (vl-sort blks-boxes
                 '(lambda (x y)
                   (< (caar (cadr x))
                      (caar (cadr y))))))
  ;; 按包围盒 x 最小值排序后扫描：每个块只需与 x 范围可能相交的后续块比对
  (setq rest blks-boxes)
  (while (setq a (car rest))
    (setq a-box (cadr a)
          others (cdr rest))
    (while (and (setq b (car others))
                (< (caar (cadr b))
                   (- (caadr a-box) tol)))
      (if (@block:box-overlap? a-box (cadr b) tol)
          (setq pairs (cons (list (car a) (car b)) pairs)))
      (setq others (cdr others)))
    (setq rest (cdr rest)))
  (reverse pairs))
(defun @block:overblocks (/ *error* blkname blks blkent tol blks-boxes pairs blks-overed corner)
  "定位相互重叠的同名块。
  选择一个块，找出与其包围盒存在正面积重叠(超过 @block:overlap-tol 容差)的所有同名块并选中、缩放定位。
  仅边线相接的并排块(如车位块边线重合)不算重叠。
  return: nil(重叠块被选中并缩放)。"
  (@::prompt "定位相互重叠的同名块")
  (defun *error* (msg)
    ;; 重启动处理 
    (if (= 'file (type dcl_fp))
	(close (dcl_fp)))
    (princ (strcat msg ))
    (princ))
  (prompt  "请选择一个块:")
  (while (null(and (setq blkent (ssget "_:S:E" '((0 . "insert"))))
		   (setq blkent (ssname blkent 0))))
    (@:prompt "\n未选中块，请选择一个块:"))
  (if blkent
      (progn
	(setq blks (pickset:to-list (ssget "x" '((0 . "INSERT")))))
	(setq blkname (block:get-effectivename blkent))
	(setq blks (vl-remove-if '(lambda (x) (/= blkname
					       (block:get-effectivename x)))
			       blks))
	(setq corner (pickset:getbox blks 100))
	(command "zoom" "w" (car corner) (cadr corner))

	;; 包围盒正面积重叠判定(并排块边线相接不算重叠)
	(setq tol (@:get-config '@block:overlap-tol))
	(setq blks-boxes
	      (vl-remove-if-not '(lambda (p) (cadr p))
			       (mapcar '(lambda (x) (list x (entity:getbox x 0)))
					blks)))
	(setq pairs (@block:overlap-pairs blks-boxes tol))
	(setq blks-overed nil)
	(foreach pair pairs
	       (foreach x pair
		      (if (not (member x blks-overed))
			  (setq blks-overed (cons x blks-overed)))))
	(if blks-overed
	    (progn
	      (setq corner (pickset:getbox (pickset:from-list blks-overed) 100))
	      (command "zoom" "w" (car corner) (cadr corner))
	      (sssetfirst nil (pickset:from-list blks-overed)))
	    (princ (@:speak "没有发现重叠块。"))
	    )))
  (princ))
(defun @block:overblocks2 (/ *error* blks x tol blks-boxes pairs x0 found-pairs blks-found corner)
  "定位相互重叠的块(块外围尺寸对角线长度不大于设定值)。
  框选检测范围，找出第一对包围盒存在正面积重叠(超过 @block:overlap-tol 容差)的块，缩放定位到该重叠堆并选中其中一块。
  仅边线相接的并排块(如车位块边线重合)不算重叠。
  return: nil(第一重叠堆被缩放定位并选中其中一块)。"
  (@::prompt "定位相互重叠的块(块外围尺寸对角线长度不大于设定值)")
  (defun *error* (msg)
    ;; 重启动处理 
    (if (= 'file (type dcl_fp))
	(close (dcl_fp)))
    (princ (strcat msg ))
    (princ))
  (@:prompt "请框选要检测的范围:")
  (if (null (setq blks (pickset:to-list (ssget '((0 . "INSERT"))))))
      (setq blks (pickset:to-list (ssget "x" '((0 . "INSERT"))))))
  (setq blks (vl-remove-if '(lambda (x / box)
			     (or (null (setq box (entity:getbox x 0)))
				 (> (distance (car box)(cadr box)) (@:get-config '@block:size))))
			   blks))
  ;; 包围盒正面积重叠判定(并排块边线相接不算重叠)
  (setq tol (@:get-config '@block:overlap-tol))
  (setq blks-boxes
	(vl-remove-if-not '(lambda (p) (cadr p))
			  (mapcar '(lambda (x) (list x (entity:getbox x 0)))
				blks)))
  (setq pairs (@block:overlap-pairs blks-boxes tol))
  ;; 按原顺序找第一个有重叠伙伴的块，收集其重叠堆
  (setq x0 nil)
  (foreach x blks
	 (if (and (null x0)
		  (setq found-pairs
			(vl-remove-if-not '(lambda (p) (member x p)) pairs)))
	     (progn
	       (setq x0 x)
	       (setq blks-found (list x))
	       (foreach p found-pairs
		      (foreach y p
			     (if (not (member y blks-found))
				 (setq blks-found (cons y blks-found))))))))
  (if x0
      (progn
	(setq corner (pickset:getbox blks-found 10))
	(command "zoom" "w" (car corner) (cadr corner))
	(sssetfirst nil (pickset:from-list (list x0))))
      (princ (@:speak "没有发现重叠块。"))
      )
  (princ))
