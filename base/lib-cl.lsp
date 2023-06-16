;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; base : autolisp 基础库
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defun push (atom alist)
;;  (setq alist (append (list atom) alist)))

;;(defun pop (alist / first-atom )
;;  (setq first-atom (car alist))
;;  (setq alist (cdr alist))
;;  first-atom
;;  )

;;(set 'sort vl-sort)


;; String
(defun string (para)
  "将其它类型转化为字符串。"
  (cond
    ;((= 'INT (type para)) (itoa para))
    ;'((= 'REAL (type para)) (rtos para))
    ((= 'STR (type para)) para)
    ;((= 'LIST (type para)) (@:list-to-string para ";"))
    ((= 'SYM (type para)) (vl-symbol-name para))
    )
  )
(defun string-upcase (str)
  (strcase str))
(defun string-downcase (str)
  (strcase str T))
(defun string-capitalize (str)
  (strcat (string-upcase (substr str 1 1))
	  (substr str 2)))

(defun consp (alist)
  "Verifies that an item is cons." 
  (if (and (listp alist)
	   (not alisp))
      T
      nil))

(defun pair (x y)
  (cond ((and (null x) (null y)) '())
        ((and (not (atom x)) (not (atom y)))
         (cons (list (car x) (car y))
               (pair (cdr x) (cdr y))))))

(defun getf (plist property)
  (cadr (member property plist)))

(defun incf (x)
  (setq x (1+ x)))
;; Local variables:
;; coding: gb2312
;; End: 
