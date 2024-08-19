;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-structure:first 用于 应用包 at-structure 的 第一个配置项 first 
;;(@:define-config 'at-structure:first "我是配置项 at-structure:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-structure:first) ;; 获取配置顶的值
;; (@:set-config 'at-structure:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "结构工具" "动态查面积" "(at-structure:query-steelbar)" )
(@:add-menu "结构工具" "查钢筋面积" "(at-structure:menu-get-area)" )
(defun at-structure:menu-get-area (/ steelbar-str)
  (@:help "选中钢筋字符串的单行文本，如 %%1328@100,2%%13220+3%%13222 等，返回钢筋面积。")
  (setq steelbar-str (string:parse-by-lst (cdr (assoc 1 (entget (car (entsel))))) '(";" "；")))
  (foreach x steelbar-str
	   (format t "钢筋面积: ~d"
		   (at-structure:get-steel-area x))))

(defun at-structure:get-steel-area (steelbar-str / steelbar-lst steelbar-to-area gujin-zhishu)
  "计算钢筋文字得到钢筋面积"
  "Real number"
  (defun steel-to-area (steelbar-str / nxd)
    (setq nxd (string:parse-by-lst steelbar-str '("%%132" "%%130" "%%131"))) ;;钢筋符号
    (if (= 2 (length nxd )) ; nxd = (list 根数  直径)
	(cons * (list (if (= "" (car nxd)) 1.0 (atof (car nxd))) 0.25 pi (atof (cadr nxd)) (atof (cadr nxd))))))
  (setq steelbar-str (vl-string-left-trim "GN BTXY&:" steelbar-str)) 
  (if (vl-string-search "@" steelbar-str)
      (progn;; 箍筋/板筋/墙筋
	(setq steelbar-lst  (string:parse-by-lst steelbar-str '("@"))) ;; 间距符号
	(setq gujin-steel (string:parse-by-lst (car steelbar-lst) '("/")))
	(if (setq gujin-zhishu 
		  (cadr (string:parse-by-lst (cadr steelbar-lst) '("(" ")"))))
	    (setq gujin-zhishu (atoi gujin-zhishu))
	    (setq gujin-zhishu 1))
	    
	(eval (cons *
		    (cons
		     (cons /
			   (cons (cons +
				       (vl-remove nil (mapcar 'steel-to-area gujin-steel)))
				 (cons (length gujin-steel) (cons (atof (cadr steelbar-lst)) (cons 0.001  nil)))))
		     (cons gujin-zhishu nil)))))
      (progn ;; 纵筋
	;; 去架立筋括号
	(setq steelbar-str (string:subst-all "" ")" (string:subst-all "" "(" steelbar-str)))
	(eval (cons +  (vl-remove nil (mapcar 'steel-to-area (string:parse-by-lst steelbar-str '("+" "/"))))))))
  )

(defun at-structure:query-steelbar()
  "动态查钢筋面积。"
  (ui:dynquery '(lambda(x)
		 (cond
		   ((or (= name "TEXT")(= name "TCH_TEXT"))
		    (setq lst
			  (mapcar '(lambda (x / area )
				    (if (> (setq area (at-structure:get-steel-area x)) 0)
					(format nil "钢筋面积: ~d" area )
					"非钢筋文字"))
				  (string:parse-by-lst (entity:getdxf ent 1) '(";" "；"))))
		    (setq lst (vl-remove nil lst)))
		   (T (setq lst (list "非文字" name )))
		   )
		 lst)
  ))
