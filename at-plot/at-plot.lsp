;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'at-plot:first for package at-plot 's configitem first 
;;(@:define-config 'at-plot:first "I'm th default value for at-plot:first" "This Item 's Explain.")
;; (@:get-config 'at-plot:first) 
;; (@:set-config 'at-plot:first  "New Value")
;; Add menu in @lisp panel
(defun @plot:hello ()
  (@:help (strcat "The content can show in user interface .\n"))
  (princ))
;; 图框可用比例
(@:define-config '@plot:scale-of-frame "100 50 30 25 20 150 120 200 500 1 5.996" "图幅比例，以逗号或空格分隔")
(@:define-config '@plot:layers "*" "图框所在图层，多个图层以 ',' 进行分隔。支持 * 通配符。")
(@:define-config '@plot:export-path "D:\\output\\" "打印输出路径。")
