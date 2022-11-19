;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file was created by @lisp DEV-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a first config item  'at-line:first for package at-line 's configitem first 
;(@:define-config 'at-line:first "I'm th default value for at-line:first" "This Item 's Explain.")
;; (@:get-config 'at-line:first) 
;; (@:set-config 'at-line:first  "New Value")
;; Add menu in @lisp panel
(defun at-curve:link-end (/ segments segment-pts)
  (@:help (strcat "连接线端点 .\n"
  	  	  ))
  (setq segments (pickset:to-list (ssget '((0 . "*LINE")))))
  (setq segment-pts (mapcar '(lambda (x)
			      (cond
				((= "LINE" (entity:getdxf x 0))
				 (entity:getdxf x '(10 11)))
				((= "LWPOLYLINE"  (entity:getdxf x 0))
				 (list (car (line:get-lwpoints x))
				       (last (line:get-lwpoints x))))))
			    segments))
  
  (entity:make-lwpline-bold (@curve:left-points segment-pts) nil 0 0 0)
  (entity:make-lwpline-bold (@curve:right-points segment-pts) nil 0 0 0)
  )
(defun @curve:left-points (segments)
  (vl-sort 
   (mapcar '(lambda (x) (if (< (car (car x))(car (last x))) (car x)(last x))) segments)
   '(lambda (x y)(> (cadr x)(cadr y))))
  )
(defun @curve:right-points (segments)
  (vl-sort 
   (mapcar '(lambda (x) (if (> (car (car x))(car (last x))) (car x)(last x))) segments)
   '(lambda (x y)(> (cadr x)(cadr y))))
   )


