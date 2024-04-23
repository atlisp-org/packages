
(@:add-menu  "通用打印" "训练学习" "(@plot:train)")
(defun @plot:train (/ pt1 pt2 hight res *error*)
  (defun *error* (msg)
    (if fp (close fp))
    (@:*error* msg))
  (prompt "请点击图框的两个对角点")
  (setq pt1 (getpoint "左下角: "))
  (setq pt2 (getpoint pt1 "右上角: "))
  (setq hight (abs (cadr (mapcar '- pt2 pt1))))
  (setq res
	(ui:select "请确认图框的图幅及比例"
		   (mapcar (function
			    (lambda (x y)
			      (strcat y " 1:"
				(rtos x 2 7))))
			   (mapcar (function
				    (lambda (x)
				      (/ hight x)))
				   @plot:height-of-frame)
			   @plot:frame-type)))
  (setq fp (open (strcat @:*prefix-config* "frames-scale.db")"a"))
  (write-line (last (string:to-lst res ":")) fp)
  (close fp)
  (setq fp (open (strcat @:*prefix-config* "frames-ratio.db")"a"))
  (write-line
   (rtos 
    (/
     (car (mapcar '- pt2 pt1))
     (cadr (mapcar '- pt2 pt1)))
    2 7)
    fp)
  (close fp)
  (prompt "增加了一条识别规则。")
  (princ)
  )
(defun @plot:frame-recognition-by-insert (/ blks frameblks)
  ;;
  "识别外部参照或无属性块"
  (setq blks (block:list))
  (if(> (length  blks) 500)
     (progn
       (@:prompt "图块太多，跳过匿名块的识别。")
       (setq blks (vl-remove-if '(lambda(x)(wcmatch x "*`**")) blks))))
  (setq frameblks nil)
  (foreach
   blk blks
   (princ blk)
   (princ "\n")
   (foreach
    en-in-blk  (block:ent-list blk)
    (if (and (= "LWPOLYLINE" (entity:getdxf en-in-blk 0))
	     (@plot:framep(curve:get-points en-in-blk)))
	(setq frameblks (cons
			 (cons 
			  blk
			  (curve:get-points en-in-blk))
			 frameblks)))))
  frameblks
  )
  
