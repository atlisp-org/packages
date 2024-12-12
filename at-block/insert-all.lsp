(defun @block:insert-all (/ pt0 pt1 ang dist blk)
  (@::prompt (strcat "����� -> �����п�\n"
		  " ����ǰ dwg �е����п���뵽��ָ�����ָ���������С�"))
  (if (and (setq pt0 (getpoint "����������:"))
	   (setq pt1 (getpoint pt0 "��������Բ����ľ���ͷ����:"))
	   (setq ang (angle pt0 pt1)
		 dist (distance pt0 pt1)))
      (progn 
	(setq blk (tblnext "block" t))
	(while blk
	  (block:insert (cdr (assoc 2 blk)) "" pt0 0 1)
	  (setq pt0 (polar pt0 ang dist))
	  (setq blk (tblnext "block"))))))

  
