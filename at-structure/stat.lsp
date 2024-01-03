;; 统计梁截面
(@:add-menu "结构" "统计梁截面" "(at-structure:stat-beam-size)")
(defun at-structure:stat-beam-size(/ txts res)
  (@:help '("选择梁平法文字，统计梁截面"))
  (setq txts (pickset:to-list (ssget (list '(0 . "*text")
					   (cons 8 (@:get-config
						    '@structure:layer-beam))
					   ))))
  (setq res
	(vl-sort
	 (list:remove-duplicates
	  (apply 'append
		 (vl-remove nil
			    (mapcar '(lambda(x)
				      (re:match "/\\d+[xX]\\d+/g"
				       (text:remove-fmt (text:get-mtext x))))
				    txts))))
	 '<))
  (setq n 0)
  (table:make (getpoint (@:prompt "请给出绘制点:"))
	      "梁截面统计表"
	      '("序号""梁截面尺寸")
	      (mapcar '(lambda(x)
			(list
			 (setq n (1+ n))
			 x))
		      res)))
  
  
