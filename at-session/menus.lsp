(@:add-menus
 '(("@lisp会话管理"
    (("第二个功能" "(at-session:hello)")
     ("第三个功能" "(at-session:hello)")))
   ("@lisp会话管理1"
    (("第二个功能" "(at-session:hello)")
     ("第三个功能" "(at-session:hello)")))
   ))
(defun testa (&key a b)
  "测试项目内跨文件函数"
  (alert a)
)
(defun testb (&optional a b )
  "函数功能说明" "函数返回值" exprs))
  