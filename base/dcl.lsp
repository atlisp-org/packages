;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  DCL Éú³Éº¯Êý
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dcl:dialog (name label node)
  (strcat name ": dialog{label=\"" label "\";"
	  node
	  "ok_cancel;}")
  )
(defun dcl:row (node)
  (strcat ": row {"
	  node "}"))
       
(defun dcl:column (node)
  (strcat ": column {"
	  node "}"))
(defun dcl:edit-box (key width hight)
  (strcat ": edit_box { key=\"" key "\";"
	  (if width (strcat "width=" width ";") "")
	  (if hight (strcat "width=" hight ";") "")
	  "}"))

       
