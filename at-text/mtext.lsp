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
