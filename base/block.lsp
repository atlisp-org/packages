

(defun block:get-effectivename (blk / tem blkname)
  "取得块真实名称，支持 MAC"
  (if (= 'ename (type blk))
      (progn
	(setq blkname (cdr (assoc 2 (entget blk))));;取得当前动态块名
	(if (wcmatch blkname "`**");;如果是匿名块
	    (if (and
		 (setq tem
		       (cdadr
			(assoc -3
			       (entget
				(cdr
				 (assoc 330
					(entget
					 (tblobjname "block" blkname);;根据动态块名称取得图元名
					 );;根据图元名取得实体
					)
				 )
				'("AcDbBlockRepBTag")
				)
			       )
			)
		       )
		 (setq tem (handent (cdr (assoc 1005 tem))))
		 )
		(setq blkname (cdr (assoc 2 (entget tem))))
		)
	    )
	blkname
	)
      nil))

(defun block:get-attributes (blk / lst)
  "获取块属性,返回属性名和值的点对列表。"
  (if (= 'ename (type blk))
      (if (safearray-value (setq lst (vlax-variant-value (vla-getattributes (vlax-ename->vla-object blk)))))
	  (mapcar '(lambda (x) (cons (vla-get-tagstring x) (vla-get-textstring x)))
		  (vlax-safearray->list lst)
		  )
	  )
      nil)
)

(defun block:set-attributes (blk lst / n atts)
  "设置块属性值"
  (if (= 'ename (type blk))
      (if (safearray-value (setq atts (vlax-variant-value (vla-getattributes (vlax-ename->vla-object blk)))))
	  (progn (foreach n lst
			  (mapcar '(lambda (x)
				    (if (= (strcase (car n)) (strcase (vla-get-tagstring x)))
					(vla-put-textstring x (cdr n))
					)
				    )
				  (vlax-safearray->list atts)
				  )
			  )
		 (vla-update (vlax-ename->vla-object blk))
		 )
	  )
      nil)
  )

(defun block:get-dynamic-properties (blk / oblk props)
  "获取动态块的动态属性列表：属性名，当前值，只读性，是否显示，允许值"
  (if (= 'ename (type blk))
      (progn
	(setq oblk  (vlax-ename->vla-object blk))
	;;获取动态块的属性
	(setq props (vlax-invoke oblk 'getdynamicblockproperties))
	;;获取属性名
	(list 
	 (mapcar '(lambda (x) (vlax-get x 'propertyName)) props)
	 ;;获取所有属性的当前值
	 (mapcar '(lambda (x) (vlax-get x 'Value)) props)
	 ;;获取属性是否为只读
	 (mapcar 'vla-get-readOnly props)
	 ;;获取属性是否显示
	 (mapcar 'vla-get-show props)
	 ;;获取所有属性的允许值
	 (mapcar '(lambda (x) (vlax-get x 'allowedValues)) props)
	 ))
      nil)
  )
(defun block:get-dynamic-prop-cons-name-value (blk / props n lst)
  (setq props (block:get-dynamic-properties blk))
  (setq n 0)(setq lst nil)
  (repeat (length (car props))
	  (setq lst (append lst (list (cons (nth n (car props))
					    (nth n (cadr props))))))
	  (setq n (1+ n)))
  lst)
  
(defun block:set-dynprop (blk prp val)
  "设置动态块特性值"
  (setq prp (strcase prp))
  (vl-some
   '(lambda (x)
     (if (= prp (strcase (vla-get-propertyname x)))
         (progn
           (vla-put-value
            x
            (vlax-make-variant
             val
             (vlax-variant-type (vla-get-value x))
             )
            )
           (cond (val)
                 (t)
		 )
           )
	 )
     )
   (vlax-invoke (vlax-ename->vla-object blk) 'getdynamicblockproperties)
   )
  )
(defun block:insert()
  "demo create a block containing a circle"
  ;; This example creates a block containing a circle.
  ;; It then inserts the block.
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  
  ;; Create the block
  (setq insertionPnt (vlax-3d-point 0 0 0))
  (setq blockObj (vla-Add (vla-get-Blocks doc) insertionPnt "CircleBlock"))
  
  ;; Add a circle to the block
  (setq center (vlax-3d-point 0 0 0)
        radius 1)
  (setq circleObj (vla-AddCircle blockObj center radius))
  
  ;; Insert the block
  (setq insertionPnt (vlax-3d-point 2 2 0))
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq blockRefObj (vla-InsertBlock modelSpace insertionPnt "CircleBlock" 1 1 1 0))
  
  (vla-ZoomAll acadObj)
  )
