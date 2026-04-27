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
(@:add-menu "结构工具" "画点钢筋" "(at-structure:menu-draw-one-rebar)" )
(@:add-menu "结构工具" "画钢筋排" "(at-structure:menu-draw-edge-rebar)" )
(@:add-menu "结构工具" "画箍筋" "(at-structure:menu-draw-stirrup)")
(@:add-menu "结构工具" "绘柱截面" "(at-structure:menu-draw-columns)")
(defun at-structure:menu-draw-one-rebar()
  (@::help "画点钢筋")
  (at-structure:draw-one-rebar
   (getpoint "请点取位置点:")
   (getint "请输入钢筋直径:")
   ))
(defun at-structure:menu-draw-edge-rebar()
  (@::help "画钢筋排")
  (at-structure:draw-edge-rebar
   (setq pt-start (getpoint "起始点:"))
   (getpoint pt-start "终止点:")
   (getint "请输入个数:")
   (getint "请输入钢筋直径:")
   ))
(defun at-structure:menu-draw-stirrup ()
  (@::help "绘制箍筋，当给出的两点为水平或垂直时，绘制单肢箍")
  (setq pt-start (getpoint "起始点:"))
  (setq pt-end (getcorner pt-start "终止点:"))
  (at-structure:draw-stirrup
   pt-start
   (- (car pt-end)(car pt-start))
   (- (cadr pt-end)(cadr pt-start))
   ))
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
(defun at-structure:menu-draw-columns ()
  (@::help "从钢筋表中取信息绘制柱截面")
  (setq columns (text:get-matrix))
  (setq pt-base (getpoint "绘制基点:"))
  (foreach colu (cdr columns)
	   (setq bxh (nth 2 colu))
	   (setq b (read (car (string:to-list bxh "x"))))
	   (setq h (read (cadr (string:to-list bxh "x"))))
	   (if (eq "" (nth 3 colu))
	       (setq rebar-config
		     (mapcar  'read
			      (append 
			       (string:to-list (nth 4 colu) "%%132")
			       (string:to-list (nth 5 colu) "%%132")
			       (string:to-list (nth 6 colu) "%%132")
			       (cdr (string:parse-by-lst (nth 8 colu) '("%%132" "@")))
			       (string:parse-by-lst (nth 7 colu) '("(" "x" ")"))
			       )))
	     (progn
	       (setq rebars (mapcar 'read (string:to-list (nth 3 colu) "%%132")))
	       (setq rebar-config
		     (append
		      (list
		       4
		       (cadr rebars)
		       (/ (- (car rebars) 4) 4)
		       (cadr rebars)
		       (/ (- (car rebars) 4) 4)
		       (cadr rebars))
		      (mapcar 'read
			      (append
			       (cdr (string:parse-by-lst (nth 8 colu) '("%%132" "@")))
			       (string:parse-by-lst (nth 7 colu) '("(" "x" ")")))
			      )))))
	   (at-structure:draw-column-section
	    pt-base
	    b h
	    40
	    rebar-config)
	   (entity:make-text
	    (strcat  (car colu)" "(cadr colu))
	    (polar pt-base (* 1.5 pi) 250)
	    50 0 0.7 0 "lb")
	   (setq pt-base (polar pt-base 0 (* 2 b)))
	   ))
	    
	    
