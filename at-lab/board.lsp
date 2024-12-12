(@:add-menu "@试验室" "加固板" '(at-lab:board))
(defun at-lab:board (/ lines pts-b pts-t flag thickness bottom-board top-board startpt endpt rt rb db) 
  (@::prompt '("当前版本只支持两条等长的水平直线"))
  (push-var nil)
  (setvar "OSMODE" 0)
  ;; 选择两条水平直线
  (setq lines (ssget '((0 . "LINE"))))
  (setq lines (pickset:to-list (pickset:sort-with-dxf lines 10 1 0 nil))) ; 排序
  (setq pts-b (vl-sort 
                (curve:get-points (car lines))
                '(lambda (x y) (< (car x) (car y)))))
  (setq pts-t (vl-sort 
                (curve:get-points (cadr lines))
                '(lambda (x y) (< (car x) (car y)))))
  (setq flag      (> (distance (car pts-b) (car pts-t)) 25)
        thickness (- (cadar pts-t) (cadar pts-b) 18))
  ;; 底板
  (setq bottom-board (entity:make-rectangle 
                       (car pts-b)
                       (setq rb (polar (last pts-b) (* 0.5 pi) 9))))
  (entity:putdxf bottom-board 62 2)
  (setvar "HPNAME" "ANSI34")
  (setvar "HPANG" 0)
  (command "-hatch" "s" bottom-board "" "")
  ;; 顶板
  (setq top-board (entity:make-rectangle 
                    (car pts-t)
                    (setq rt (polar (last pts-t) (* 1.5 pi) 9))))
  (entity:putdxf top-board 62 3)
  (command "-hatch" "s" top-board "" "")
  (setq startpt (polar (car pts-b) (* 0.5 pi) 9)
        endpt   rt)
  (if flag 
    ;; 填方
    (progn 

      (while (< (+ 36 (car startpt)) (car endpt)) 
        (entity:make-rectangle 
          startpt
          (polar (polar startpt 0 9) (* 0.5 pi) thickness))
        (setq startpt (polar startpt 0 36)))
      (entity:make-rectangle 
        rt
        (polar (polar rt pi 9) (* 1.5 pi) thickness))
      ;; 堵板
      (setq db (entity:make-rectangle 
                 (polar rt (* 0.5 pi) 9)
                 (polar (polar rt 0 9) (* 1.5 pi) (+ thickness 9))))
      (setvar "HPNAME" "ANSI34")
      (setvar "HPANG" 0)
      (command "-hatch" "s" db "" ""))
    ;; 填板
    (progn 
      (entity:putdxf 
        (setq tf (entity:make-rectangle 
                   startpt
                   rt))
        62
        4)
      (setvar "HPNAME" "ANSI34")
      (setvar "HPANG" (* 0.5 pi))
      (command "-hatch" "s" tf "" "")))
  (pop-var))