;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-arch:first 用于 应用包 at-arch 的 第一个配置项 first 
(@:define-config '@block:block-name "块名" "用于排号的块名称。")
(@:define-config '@block:attribute-name "属性名" "用于排号的块内属性的名称。")
;; (@:get-config 'at-arch:first) ;; 获取配置顶的值
;; (@:set-config 'at-arch:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "块" "自动编号" "(@block:set-number)")
(defun @block:set-number (/ num1 start ss-list)
  (setq ss-list (pickset:to-entlist (ssget (list (cons 0 "insert")
					    (cons 1 (@:get-config 'at-arch:block-name))))))
  (setq ss-list (vl-sort ss-list
			 '(lambda (en1 en2)
			   (if (> (caddr (assoc 10 (entget en1)))
				  (+ 1000 (caddr (assoc 10 (entget en2)))))
			       T
			       (if (and (< (caddr (assoc 10 (entget en1)))
					   (+ 1000 (caddr (assoc 10 (entget en2)))))
					(> (caddr (assoc 10 (entget en1)))
					   (- (caddr (assoc 10 (entget en2))) 1000))
					(< (cadr (assoc 10 (entget en1)))
					   (cadr (assoc 10 (entget en2)))))
		     T
		     nil)
			       ))))
  (setq start (getint "请输入块起始编号<1>:"))
  (if (null start) (setq start 1))
  (setq num1 0)
  ;;(print (length ss-list))
  (foreach en0 ss-list
	   ;;(@:debug "test" "set-tuhao")
	   (block:set-attributes
	    en0
	    (list (cons (@:get-config 'at-arch:attribute-name)
			(strcat (if (< (+ num1 start) 10) "0" "")
				(itoa (+ num1 start))))))
	   (setq num1 (1+ num1))
	   ))
