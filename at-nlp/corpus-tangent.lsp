(setq at-nlp:*tangent*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'entity)))
       '(
	 ("天正标注" . "TCH_DIMENSION*")
	 ("门窗" . "TCH_OPENING")
	 ("墙体" . "TCH_WALL")
	 ("墙" . "TCH_WALL")
	 ("柱" . "TCH_COLUMN")
	 ("楼梯"  . "TCH_MULTISTAIR")
	 ("房间" . "TCH_SPACE")
	 ("标高" . "TCH_ELEVATION")
	 )))
(setq at-nlp:*tangent-attribute*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'attribute)))
       '(
	 ("墙高" . 39)
	 ("柱高" . 149)
	 ("左侧宽" . 47)
	 )))
