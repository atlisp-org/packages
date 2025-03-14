(defun at-dim:make-deviation (/ anzhuangfeng)
  (@::prompt "调整尺寸产生偏差。")
  (setq anzhuangfeng (getint "请输入要调整尺寸偏差值，减少输入-号:"))
  (mapcar
   '(lambda(x)
     (entity:putdxf
      x
      1
      (itoa (fix (+ anzhuangfeng (entity:getdxf x 42))))))
   (pickset:to-list (ssget '((0 . "DIMENSION"))))))
(defun at-dim:true-value ()
  (@::prompt "显示为尺寸的实际值。")
  (mapcar
   '(lambda(x)
     (entity:putdxf
      x
      1
      ""))
   (pickset:to-list (ssget '((0 . "DIMENSION"))))))
