(setq at-nlp:*verb*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x)
			      'verb)))
       '(("绘制" . "entity:make-")
	 ("创建" . "entity:make-")
	 ("选择" . "ssget")
	 ("选中" . "ssget")
	 ("修改" . "entmod")
	 ("更改" . "entmod")
	 ("改" . "entmod")
	 )))
(setq at-nlp:*entity*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'entity)))
       '(("圆弧" . "arc")
	 ("弧" . "arc")
	 ("圆" . "circle")
	 ("单行文本" . "text")
	 ("多行文本" . "mtext")
	 ("文本" . "*text")
	 ("填充" . "hatch")
	 ("直线" . "line")
	 ("线段" . "line")
	 ("图块" . "insert")
	 ("块" . "insert")
	 ("参照" . "insert")
	 ("多段线" . "*polyline")
	 ("标注" . "dimension")
	 ("点" . "point")
	 ("矩形" . "rectange")
	 ("长方形" . "rectange")
	 )))
(setq at-nlp:*attribute*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'attribute)))
       '(
	 ("块名" . 2)
	 ("名称" . 1)
	 ("半径" . 40)
	 ("圆心" . 10)
	 ("中心" . 10)
	 ("图层名" . 8)
	 ("图层" . 8)
	 ("线型名" . 6)
	 ("线型" . 6)
	 ("线型比例" . 48)
	 ("坐标点" . 10)
	 ("坐标" . 10)
	 ("长" . "long")
	 ("宽" . "width")
	 ("色号" . 62)
	 ("颜色" . 62)
	 ("旋转角度" . 50)
	 )))
(setq at-nlp:*color*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'color)))
       '(("红" . 1)
	 ("黄" . 2)
	 ("绿" . 3)
	 ("青" . 4)
	 ("蓝" . 5)
	 ("洋红" . 6)
	 ("白" . 7)
	 ("灰" . 8)
	 ("浅灰" . 9)
	 )))
(setq at-nlp:*linetype*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'linetype)))
       '(("实线" . "Continuous")
	 ("虚线" . "HIDDEN")
	 ("点划线" . "DASHDOT")
	 ("点线" . "DOT")
	 ("双点划线" . "DIVIDE")
	 ("热力管" . "HOT_WATER_SUPPLY")
	 ("气体管" . "GAS_LINE")
	 )))
(setq at-nlp:*compare*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x)
			      'compare)))
       '(("大于" . ">")
	 ("小于" . "<")
	 ("等于" . "=")
	 ("大于等于" . ">=")
	 ("小于等于" . "<=")
	 ("不小于" . ">=")
	 ("不等于" . "/=")
	 ("不大于" . "<=")
	 
	 ("=" . "=")
	 ("为" . "=")
	 )))
(setq at-nlp:*bool*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x)
			      'bool)))
       '(("且" . "and")
	 ("或" . "or")
	 ("不" . "not")
	 )))
(setq at-nlp:*prep*
      (mapcar
       '(lambda(x)
	  (cons (car x) (cons (cdr x) 
			      'prep)))
       '(("的" . "prep")
	 ("了" . "prep")
	 ("所有" . "x")
	 )))
