(@:add-menu "@试验室" "加固板" '(at-lab:board))
(defun at-lab:board (/ lines l1 l2 )
  (push-var)
  (setvar "OSMODE" 0)
  ;; 选择两条水平直线
  (setq lines (ssget '((0 . "LINE"))))
  (setq lines (pickset:to-list (pickset:sort-with-dxf lines 10 1 0 nil))) ; 排序
  (setq l1 (car lines)
        l2 (cadr lines))
  ;; 底板
  (setq bottom-board (entity:make-rectangle 
                       (entity:getdxf l1 10)
                       (setq rb (polar (entity:getdxf l1 11) (* 0.5 pi) 9))))
  (entity:putdxf bottom-board 62 2)
  ;; 顶板
  (setq top-board (entity:make-rectangle 
                    (entity:getdxf l2 10)
                    (setq rt (polar (entity:getdxf l2 11) (* 1.5 pi) 9))))
  (entity:putdxf top-board 62 3)
  (setq thickness (distance rb rt))
  (if (> thickness 7) 
    ;; 填方
    (progn 
      (setq startpt (polar (entity:getdxf l1 10) (* 0.5 pi) 9)
            endpt rt)
      (while (< (+ 36 (car startpt)) (car endpt)) 
        (entity:make-rectangle 
          startpt
          (polar (polar startpt 0 9) (* 0.5 pi) thickness))
        (setq startpt (polar startpt 0 36)))
      (entity:make-rectangle 
        rt
        (polar (polar rt pi 9) (* 1.5 pi) thickness))
      ;; 堵板
      (entity:make-rectangle 
        (polar rt (* 0.5 pi) 9)
        (polar (polar rt 0 9) (* 1.5 pi) (+ thickness 9))))
    ;; 填板
    (progn 
      (entity:putdxf 
        (entity:make-rectangle (polar (entity:getdxf l1 10) (* 0.5 pi) 9) rt)
        62
        4)))
  (pop-var)
)