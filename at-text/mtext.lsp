(@::define-config '@text:en-style "Standard" "纯英文单行文本字体样式")
(@::define-config '@text:zh-style "HZ" "中文单行文本字体样式")
(@::define-config '@text:en-font "Arial" "英文字体")
(@::define-config '@text:zh-font "YouYuan" "中文字体")
(@::define-config '@text:mtext-width 1.0 "多行文本高宽比")
      
(defun @text:remove-mtext-style (/ ents)
  (@::help "去除多行文本中的格式，仅保留换行。使用时注意上下标文字。")
  (setq ents (pickset:to-list (ssget '((0 . "mtext")))))
  (mapcar
   '(lambda(ent)
     vla-put-TextString (e2o ent)
     (strcat 
      (string:from-list 
       (mapcar 
        'text:remove-fmt
        (string:to-list (text:get-mtext ent) "\\P"))
       "\\P")
      ))
   ents)
  )
(defun @text:set-style (/ ents hzstylename obj)
  (@::help "统一设置文本和多行文本中的字体格式。")
  (setq hzstylename (strcat "HZ" (rtos (@::get-config '@text:mtext-width)  2 3)))
  (if (null (tblsearch "style" hzstylename))
      (progn
	(setq obj (vla-add (vla-get-textstyles (vla-get-activedocument (vlax-get-acad-object)))
			   hzstylename))
	(vla-setfont obj "宋体" :vlax-false :vlax-false 1 0)
	(vla-put-width obj (@::get-config '@text:mtext-width))
	))
  (setq ents (pickset:to-list (ssget '((0 . "text,mtext")))))
  (foreach
   ent ents 
   (cond 
     ;; 单行文本
     ((= "TEXT" (entity:getdxf ent 0))
      (if (< (apply 'max (vl-string->list (entity:getdxf ent 1))) 127) 
          (entity:putdxf ent 7 (@::get-config '@text:en-style)) ;; 需建立小字体为simplex为样式
          (entity:putdxf ent 7 (@::get-config '@text:zh-style)) ;; 需建立小字体为 仿宋.ttc 字体样式
	  )
      )
     ;; 多行
     ((= "MTEXT" (entity:getdxf ent 0))
      (setq res (strcat 
                 "{"
                 (string:from-list 
                  (mapcar 
                   '(lambda (x)
		     (apply 'strcat
		      (mapcar '(lambda(str)
				(if (< (apply 'max (vl-string->list str)) 127) 
				    (setq str (strcat "\\F"(@::get-config '@text:en-font)"|c134;" str))
				    (setq str (strcat "\\f"(@::get-config '@text:zh-font)"|b0|i0|c134|p49;" str))))
		       (string:auto-split (text:remove-fmt x)))))
                   (string:to-list (text:get-mtext ent) "\\P"))
                  "\\P")
                 "}"))
      (vla-put-TextString (e2o ent) res)
      (entity:putdxf ent 7 hzstylename)
      )))
  (princ))
