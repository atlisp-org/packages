;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'road-cross:first for package road-cross 's configitem first 

(@:define-config 'road-cross:first "I'm th default value for road-cross:first" "This Item 's Explain.")
;; (@:get-config 'road-cross:first) 
;; (@:set-config 'road-cross:first  "New Value")
;; Add menu in @lisp panel
(@:add-menu "十字路口" "绘制道路1" "(road-cross:rd)" )
(@:add-menu "十字路口" "绘制道路2" "(road-cross:srd)" )
(@:add-menu "十字路口" "画线补路" "(road-cross:sld)" )
(@:add-menu "十字路口" "容差设定" "(road-cross:std)" )
(@:add-menu "十字路口" "列道路宽" "(road-cross:prd)" )
(@:add-menu "十字路口" "帮助" "(road-cross:help)" )

(defun road-cross:hello ()
  (@:help (strcat "The content can show in user interface .\n"
  	  		  ))
  (alert (strcat "十字路口生成器 's first function.\n"
		 "Created a config item road-cross:first .\n"
		 "THe config ietm th family is this item: " (@:get-config 'road-cross:first)
		 ))
  (princ)
  )
(@:define-hotkey  "rd" "(road-cross:rd)" )
(@:define-hotkey "srd" "(road-cross:srd)" )
(@:define-hotkey "sld" "(road-cross:sld)" )
(@:define-hotkey "std" "(road-cross:std)" )
(@:define-hotkey "prd" "(road-cross:prd)" )
