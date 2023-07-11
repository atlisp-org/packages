(defun @text:sort-serial-number (/ sp txts)
  (@:help '("重新编排文字前面的序号。"
	    "支持 单个数字 n. 或 n.n 形式开头的单行文本。"))
  (setq txts (pickset:to-list(ssget '((0 . "text")))))
  (setq txts (pickset:sort txts "Yx" 10))
  ;; 取第一个序号
  (while (and txts
	      (null (string:numberp (car (string:auto-split (entity:getdxf (car txts) 1))))))
    (setq txts (cdr txts)))
  (if (and txts
	   (string:numberp (car (string:auto-split (entity:getdxf (car txts) 1)))))
      (progn
	(setq sn (car (string:auto-split (entity:getdxf (car txts)1))))
	(setq txts (cdr txts))
	))
  (setq sp "")
  (foreach txt% txts
	   (if (string:numberp (car (string:auto-split (entity:getdxf txt% 1))))
	       (progn
		 (setq lst-sn (reverse (string:to-list sn ".")))
		 (if(= "" (car lst-sn))
		     (progn
		       (setq sp ".")
		       (setq lst-sn (cdr lst-sn))))
		 (setq lst-sn (reverse (cons (itoa (1+ (atoi (car lst-sn))))
					     (cdr lst-sn))))
		 (setq sn (string:from-lst lst-sn "."))
		 (entity:putdxf txt%
				1
				(strcat sn sp
					(string:from-list
					 (cdr (string:auto-split (entity:getdxf txt% 1)))
					 "")))))))

 
