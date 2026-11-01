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
(@:define-config '@plot:default-frame "A2" "图幅识别失败时使用的默认图幅（A0/A1/A2/A3/A4）。")
(@:define-config '@plot:default-orient "L" "图幅识别失败时使用的默认方向（L=横，P=竖）。")
