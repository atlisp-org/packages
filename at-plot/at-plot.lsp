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
  (alert (strcat "@��ӡ���-���԰� 's first function.\n"
                 "Created a config item at-plot:first .\n"
                 "THe config ietm th family is this item: " (@:get-config 'at-plot:first)))
  (princ))
