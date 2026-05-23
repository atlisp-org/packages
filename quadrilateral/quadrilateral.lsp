;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'quadrilateral:first for package quadrilateral 's configitem first 
;; (@:define-config 'quadrilateral:first "I'm th default value for quadrilateral:first" "This Item 's Explain.")
;; (@:get-config 'quadrilateral:first) 
;; (@:set-config 'quadrilateral:first  "New Value")
;; Add menu in @lisp panel
(@:define-config 'quadrilateral:scale 1  "绘图比例")
(@:define-config 'quadrilateral:evalcode "" "执行码")
(@:add-menu "四边形标注" "$四边形标注" "(quadrilateral:dim)" )

(defun quadrilateral:dim (/ eval-code)
  (@:help (strcat "标注由4条直线段组成的四边形的边长和对角长。\n"
		  "如需完整版请与作者联系。演示版最多只能标注6个四边形。\n"
		  "演示版执行码为 DEMO 。"
  	  	  ))
  (if (or (= "" (@:get-config 'quadrilateral:evalcode))
	  (= "DEMO" (@:get-config 'quadrilateral:evalcode)))
      (if (= (setq eval-code (getstring "请输入执行码, 演示请输入<DEMO>:"))
	     (@:get-eval-code  "quadrilateral"))
	  (@:set-config 'quadrilateral:evalcode eval-code)
	  (if (= "DEMO" eval-code)
	      (@:set-config 'quadrilateral:evalcode "DEMO")
	      (progn
		(princ "执行码错误! ")
		(@:set-config 'quadrilateral:evalcode ""))
	      ))
      )
  (if (/= "" (@:get-config 'quadrilateral:evalcode))
      (@:run-from-web "quadrilateral" quadrilateral:dim-backend))
  (princ)
  )
