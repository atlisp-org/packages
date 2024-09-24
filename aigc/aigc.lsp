(@:add-menu "AIGC" "*��������" "(aigc:gen-content)")
(defun aigc:gen-content () 
  (@:help "ѡ��һ���ı��������ı���������Ҫ���������ݣ����԰棩")
  (@:prompt "��ѡ��һ���ı�:")
  (if 
    (and 
      (setq req (car (pickset:to-list (ssget ":S" '((0 . "*text"))))))
      (setq str-req (text:remove-fmt (text:get-mtext req))))
   
    (progn 
      (@:load-module 'aibot)
      (setq req-box (entity:getbox req 0))
      (setq fontsize (entity:getdxf req 40))

      (setq ent-mtext (entity:make-mtext 
                        "{\\C2;������������ .... }"
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
     (@:prompt "�������ݱ� �жϡ�")
    (entdel ent-mtext))))))
