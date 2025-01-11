;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-number:first 用于 应用包 at-number 的 第一个配置项 first 
(@:define-config '@number:layer "number"  "编号文字所在图层")
(@:define-config '@number:order "yx"  "编号的位置顺序")
;; (@:define-config '@number:layer "number"  "编号文字所在图层")
;; 向系统中添加菜单 
(@:add-menus
 '("编号工具"
   ("编号设置" (at-number:setup))
   ("图形编号" (at-number:number-entity))
   ("删除编号" (at-number:delete-number))

   ))
(defun at-number:setup (/ res)
  (setq @::tmp-search-str "@number")
  (@::edit-config-dialog))
(defun at-number:number-entity ()
  (@::help '("给选中的图形进行编号"
	     ))
  (@::prompt "请选择要编号的图形")
  (if (null (tblsearch "layer" (@::get-config '@number:layer)))
      (layer:make (@::get-config '@number:layer) 1 nil nil))
  (if (null (setq ents (cadr (ssgetfirst))))
      (setq ents (ssget)))
  
  (setq ents
	(pickset:sort-by-box
	 (pickset:to-list ents)
	 (@::get-config '@number:order) '(0 0)))
  
  (setq i 0)
  (mapcar '(lambda(x)
	    (entity:putdxf 
	     (entity:make-text (itoa (setq i (1+ i)))
	      (point:centroid (entity:getbox x 0))
	      (@::scale 2.5)
	      0 0.72 0 "MM")
	     8 (@::get-config '@number:layer)
	     ))
	  ents)
  (princ)
  )
(defun at-number:delete-number ()
  (setq
   ents
   (vl-remove-if-not
    '(lambda(x)
      (string:numberp (entity:getdxf x 1)))
    (pickset:to-list
     (ssget (list '(0 . "text")
		  (cons 8 (@::get-config '@number:layer)))))))
  (mapcar 'entdel ents)
  (princ))
