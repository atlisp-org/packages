;;; 支持的写法
(push-var "cmdecho")
(push-var 'cmdecho)
(push-var '("clayer" "cmdecho"))
(push-var '(clayer cmdecho))
(push-var '("clayer" cmdecho))
(push-var (list "clayer" 'cmdecho))


(defun foobar1 ()
  (push-var nil)
  ;; 你的函数实现
  (pop-var)
  ;; 返回值
  )

(defun foobar2 ()
  "当调用其它有保存变量的函数时。"
  (push-var ’("clayer"))
  (foobar2) ;; foobar2 也使用了系统变量的保存也恢复。
  (pop-var)
  ;; 返回值
  )

