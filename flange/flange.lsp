;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'flange:first 用于 应用包 flange 的 第一个配置项 first 
;;(@:define-config 'flange:first "我是配置项 flange:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'flange:first) ;; 获取配置顶的值
;; (@:set-config 'flange:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单

(defun flange:make (pt-c DN Dw Dd Ds n t1 / ents)
  "pt-c 中心点 DN 公称直径(内径),Dw外径，Dd,螺丝间距,Ds 螺丝直径,n 螺丝个数，t1厚度"
  (push-var)
  (setvar "osmode" 0)
  (if (tblsearch "block" (strcat "Flange-DN" (itoa DN)))
      (block:insert (strcat "Flange-DN" (itoa DN)) "" pt-c 0 1)
    (progn
      (setq ents nil)
      (setq ents
	    (cons 
	     (entity:make-circle pt-c (* 0.5 Dw)) ents))
      (setq ents
	    (cons 
	     (entity:make-circle pt-c (* 0.5 DN)) ents))
      (setq ang 0)
      (repeat n
	      (setq ents
		    (cons 
		     (entity:make-circle (polar pt-c ang (* 0.5 Dd)) (* 0.5 Ds))
		     ents))
	      (setq ang (+ ang (/ (* 2 pi) n))))
      (mapcar '(lambda (x) (entity:putdxf x 39 t1)) ents)
      (entity:block ents (strcat "Flange-DN" (itoa DN)) pt-c)))
  (pop-var)
  )

(defun flange:draw ()
  (@:help (strcat "从参数表中读入数据，绘制法兰"))
  ;;实现代码
  ;; 加载参数
  (setq paras nil)
  (if (findfile (strcat (@:package-path "flange") "data.lst"))
      (progn
	(setq fp (open(strcat (@:package-path "flange") "data.lst")"r"))
	(while (setq str-l (read-line fp))
	  (if(/= "#" (substr str-l 1 1))
	      (setq paras (cons 
			   (cdr (string:to-list str-l "\t"))
			   paras))))
	;; 显示界面，选择参数。
	(setq paras (reverse paras))
	(setq para (assoc 
		    (ui:select "请选择型号"
			       (mapcar 'car paras))
		    paras)
		    )

	(setq pt-c (getpoint "点选要绘制的位置坐标:"))
	(setq para (mapcar 'read para))
	(flange:make pt-c (atoi (substr (vl-symbol-name (nth 0 para)) 3)) (nth 1 para) (nth 2 para)(nth 3 para) (nth 4 para)(nth 5 para))
	)))
 
