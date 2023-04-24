(defun @lab:stat-number-inline ()
  (@:help '("统计每行上圆的数量，并标注在指定位置处"))
  ;; 选择小圆
  (setq ss-c (pickset:to-list (ssget '((0 . "circle")(-4 . "<")(40 . 20)))))
  ;; 按 Y 从上到下排序
  (setq ss-c (pickset:sort ss-c "Yx" 0.1))
  ;; 对选中的圆以Y轴进行分组,本例为当两个圆的圆心坐标Y值相差不大于1/10半径时为一组。
  (setq group-c (list:group-by ss-c 
                '(lambda (x y)
                   (equal
                     (cadr (entity:getdxf x 10))
                     (cadr (entity:getdxf y 10))
                     (* 0.1 (entity:getdxf x 40))))))
  ;; 标记每组圆的个数
  (if group-c
    (progn
  (setq pt (getpoint (@:prompt "请指定标注列所在的位置:")))
  (mapcar 
    '(lambda (x)
      (entity:make-text (itoa (length x))
        (list (car pt) (cadr (entity:getdxf (car x) 10)) 0)
        (* 2 (entity:getdxf (car x) 40))
        0 0.8 0 "RM"))
     group-c
     ))))

