(defun @lab:attxyz2csv (/ fp ss-blk)
  (@::prompt "@ʵ���� > ���������� \n �ѿ����Ժ�����ֵ������ D: ���Ե�ǰʱ���Ϊ���Ƶ�csv�ļ�����ɺ��csv�ļ���")
  (setq fp (open (setq filename
		       (strcat "D:/"
			       (car (string:to-list (@:timestamp) "."))
			       ".csv"))
		 "w"))
  (setq ss-blk (pickset:to-list (ssget "x" '((0 . "insert")))))
  (write-line (string:from-list
	       (append (mapcar 'car (block:get-attributes (car ss-blk))) '("X" "Y" "Z"))
	       ",")
	      fp)
  (mapcar
   '(lambda(x)
      (setq att% (block:get-attributes x))
      (write-line (strcat 
		   (string:from-list
		    (mapcar 'cdr (block:get-attributes (car ss-blk)))",")
		   ","
		   (string:from-list (mapcar '@:to-string (entity:getdxf x 10))","))
		  fp))
   ss-blk)
  (close fp)
  (@:cmd "notepad" filename)
  (princ))
