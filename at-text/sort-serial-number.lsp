(defun @text:sort-serial-number (/ sp txts) 
  (@::prompt 
   '("重新编排文字前面的序号。" "支持 单个数字 n. 或 n.n 形式开头的文本。"))
  (setq txts (pickset:to-list (ssget '((0 . "text,mtext")))))
  (setq txts (pickset:sort txts "Yx" 10))
  ;; 跳过无序号的文本
  (while 
      (and txts
	   (cond 
             ((= "TEXT" (entity:getdxf (car txts) 0))
              (null (string:numberp (car (string:auto-split (entity:getdxf (car txts) 1))))))
             ((= "MTEXT" (entity:getdxf (car txts) 0))
              (setq mtxts (string:to-list (text:get-mtext (car txts)) "\\P"))
              (while 
		  (and 
		   mtxts
		   (null (string:numberp (car (string:auto-split (car mtxts))))))
		(setq mtxts (cdr mtxts)))
              (null mtxts))))
    (setq txts (cdr txts)))
  ;; 取第一个序号
  (if 
   (and 
    txts
    (cond 
      ((= "TEXT" (entity:getdxf (car txts) 0))
       (string:numberp (car (string:auto-split (entity:getdxf (car txts) 1)))))
      ((= "MTEXT" (entity:getdxf (car txts) 0))
       (setq mtxts (string:to-list (text:get-mtext (car txts)) "\\P"))
       (while 
           (and 
            mtxts
            (null (string:numberp (car (string:auto-split (car mtxts))))))
         (setq mtxts (cdr mtxts)))
       mtxts)))
   (progn 
     (cond 
       ((= "TEXT" (entity:getdxf (car txts) 0))
        (setq sn (car (string:auto-split (entity:getdxf (car txts) 1))))
        (setq txts (cdr txts)))
       ((= "MTEXT" (entity:getdxf (car txts) 0))
        (setq mtxts (string:to-list (text:get-mtext (car txts)) "\\P"))
        (setq mline 0)
        (while 
            (and 
             mtxts
             (null (string:numberp (car (string:auto-split (car mtxts))))))
          (setq mline (1+ mline))
          (setq mtxts (cdr mtxts)))
        (setq mline (1+ mline))
        (setq sn (car (string:auto-split (car mtxts))))))))
  (setq sp "")
  (foreach txt% txts 
	   (cond 
	     ((= "TEXT" (entity:getdxf txt% 0))
	      (if (string:numberp (car (string:auto-split (entity:getdxf txt% 1)))) 
		  (progn 
		    (setq lst-sn (reverse (string:to-list sn ".")))
		    (if (= "" (car lst-sn)) 
			(progn 
			  (setq sp ".")
			  (setq lst-sn (cdr lst-sn))))
		    (setq lst-sn (reverse 
				  (cons 
				   (itoa (1+ (atoi (car lst-sn))))
				   (cdr lst-sn))))
		    (setq sn (string:from-lst lst-sn "."))
		    (entity:putdxf 
		     txt%
		     1
		     (strcat 
		      sn
		      sp
		      (string:from-list 
                       (cdr (string:auto-split (entity:getdxf txt% 1)))
                       ""))))))
	     ((= "MTEXT" (entity:getdxf txt% 0))
	      (setq mtxts (string:to-list (text:get-mtext txt%) "\\P"))
	      (if (>= mline (length mtxts)) 
		  (setq mline 0))
	      (princ mline)
	      (repeat (length mtxts) 
		      (if 
		       (and 
			(p:stringp (nth mline mtxts))
			(string:numberp (car (string:auto-split (nth mline mtxts)))))
		       (progn 
			 (setq lst-sn (reverse (string:to-list sn ".")))
			 (if (= "" (car lst-sn)) 
			     (progn 
			       (setq sp ".")
			       (setq lst-sn (cdr lst-sn))))
			 (setq lst-sn (reverse 
				       (cons 
					(itoa (1+ (atoi (car lst-sn))))
					(cdr lst-sn))))
			 (setq sn (string:from-lst lst-sn "."))
			 (if (< mline (length mtxts)) 
			     (setq mtxts (list:replace-index 
					  mtxts
					  mline
					  (strcat 
					   sn
					   sp
					   (string:from-list 
					    (cdr (string:auto-split (nth mline mtxts)))
					    "")))))))
		      (setq mline (1+ mline)))
	      (setq mline 0)
	      (vla-put-textstring 
               (e2o txt%)
               (string:from-lst mtxts "\\P"))))))


