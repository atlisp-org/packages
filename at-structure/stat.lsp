;; 统计梁截面
(@:add-menu "结构" "统计梁截面" "(at-structure:stat-beam-size)")
(defun at-structure:stat-beam-size(/ res n)
  (@:help '("选择梁平法文字，统计梁截面"))
  (setq res (at-structure:ai-stat-beam-size nil))
  (if res
      (progn
        (setq n 0)
        (table:make (getpoint (@:prompt "请给出绘制点:"))
                    "梁截面统计表"
                    '("序号""梁截面尺寸")
                    (mapcar '(lambda(x)
                              (list
                               (setq n (1+ n))
                               x))
                            res)))
    (@:prompt "未找到梁截面文字。"))
  (princ))
  
