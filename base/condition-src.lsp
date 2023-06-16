;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; condition 状况处理函数
;;; 该文件需要 @lisp 编译处理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq @:*var-stack* nil) ;; 变量状态栈,在启动时初始化
(@:define-config
    'base:sysvar
    "autosnap;snapmode;blipmode;cmdecho;clayer;delobj;luprec;orthomode;osmode;plinewid;textstyle;filedia"
  (_"push vars list"))
  
(defun push-var (varlst)
  ;;"当前变量状态入栈,参数支持，单个字符串，符号。"
  ;;(setq varlst (vl-remove-if 'null (varlst)))
  (if (= 'list (type (car varlst)))
      (setq varlst (car varlst)))
  (if (or (null (car varlst))
	  (null varlst))
      (setq varlst (@:string-to-list (@:get-config 'base:sysvar) ";")))
  (if (null varlst)
      (setq varlst '("autosnap";捕捉标记
		     "snapmode"; 
		     "blipmode";光标痕迹
		     "cmdecho";普通命令的提示
		     "clayer";图层
		     "delobj"	;控制创建面域时是否保留原pline，0为保留，1为不保留
		     "luprec";长度精度
		     "orthomode";正交模式
		     "osmode";捕捉模式
		     "plinewid";多线段宽度
		     "textstyle";字体样式
		     "filedia"
		     )))
  (if (= 'str (type varlst)) (setq varlst (list varlst)))
  (if (= 'sym (type varlst)) (setq varlst (list varlst)))
  (setq varlst (vl-remove-if 'null varlst));;删除空元
  (if (= 'list (type varlst))
      (setq @:*var-stack*
	    (append 
	     (list (mapcar '(lambda (x) (cons x (getvar x))) varlst))
	     @:*var-stack*))))

(defun pop-var ()
  ;;"恢复保存的变量"
  (mapcar '(lambda (x) (setvar (car x) (cdr x))) (car @:*var-stack*))
  (setq @:*var-stack* (cdr @:*var-stack*)))

;;局部变量开始
;; (defun *error* (msg)
;;   (pop-var) ;还原系统变量
;;   (if (< 18 (atoi (substr (getvar "acadver") 1 2)))  ;判断CAD版本，高版本用command-s
;;       (command-s "undo" "e") ;CAD高版本用
;;       (command "undo" "e")) ;低版本用
;;   (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
;;       (princ (strcat "\n** Error: " msg " **")))
;;   (princ)
;;   )
