;;; @:add-menu 参数说明
;;; 1. 一级菜单 hello
;;; 2. 二级菜单 helloworld
;;; 3. 菜单执行的命令 (helloworld)
(@:add-menu "Hello" "helloworld" '(helloworld))
(defun helloworld ()
  (@:help "输出hello world 到命令行及对话框提示。")
  (princ "hello world.\n")
  (alert "hello world.")
  (princ)
)

