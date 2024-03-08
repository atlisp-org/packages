(@:add-menu "AIGC" "生成内容" "(aigc:gen-content)")
(defun aigc:gen-content () 
  (@:help "选择一个文本，根据文本的条件和要求生成内容")
  (@:prompt "请选择一个文本:")
  (if 
    (and 
      (setq req (car (pickset:to-list (ssget ":S" '((0 . "*text"))))))
      (setq str-req (text:remove-fmt (text:get-mtext req))))
    (progn 
      (@:load-module 'aibot)
      (setq req-box (entity:getbox req 0))
      (setq fontsize (entity:getdxf req 40))

      (setq ent-mtext (entity:make-mtext 
                        "{\\C2;正在生成内容 .... }"
                        (polar 
                          (car req-box)
                          (* 1.5 pi)
                          fontsize)
                        fontsize
                        (* fontsize 40)
                        (* fontsize 40 3)))
      (entity:putdxf ent-mtext 62 3)
      (redraw ent-mtext)
      (if (setq response (@:aigc str-req)) 
        (progn 
          (setq response (text:from-markdown response))
          (vla-put-textstring (e2o ent-mtext) response))
        (progn 
     (@:prompt "生成内容被 中断。")
    (entdel ent-mtext))))))
