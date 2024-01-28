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

(defun at-structure:query-steelbar(/ *error* dxf fx add_background add_box add_text display olderr oldos oldfill ss pd gr pt ent entold)
  "动态查钢筋面积。"
  (defun *error* (msg / i%)
    (if ss
	(mapcar 'entdel (pickset:to-entlist ss)))
    (print msg)
    (pop-var)
    (princ)
    )
  (defun dxf(ent i)
    (if (= (type ent) 'ename) 
	(setq ent (entget ent))
	)
    (cdr (assoc i ent))
    )
  (defun fx(ang)
    (cond
      ((>= (/ pi 2) ang 0) (list pi (+ pi (/ pi 2)) 1))
      ((>= pi ang (/ pi 2)) (list 0 (+ pi (/ pi 2)) 1))
      ((>= (+ pi (/ pi 2)) ang pi) (list 0 (/ pi 2) 0))
      ((>= (* 2 pi) ang (+ pi (/ pi 2))) (list pi (/ pi 2) 0))
      )
    )
  (defun add_background(p1 p2 p3 p4) ;; 背景
    (entmakex (list (cons 0 "SOLID") (cons 100 "AcDbEntity") (cons 62 8) (cons 100 "AcDbTrace")
                    (cons 10 p1) (cons 11 p4) (cons 12 p2) (cons 13 p3)
		    )))
  (defun add_box( pts / dxfcodes ) ;; 背景边框
    (setq dxfcodes (list (cons 0 "LWPOLYLINE")(cons 100 "AcDbEntity")(cons 100 "AcDbPolyline")(cons 62 2)
			 (cons 90 (length pts)) (cons 70  1) (cons 43 0) (cons 38 0.0) (cons 39 0.0)))
    (foreach pt% pts
	     (setq dxfcodes (append dxfcodes (list (cons 10 pt%) (cons 40 0.0) (cons 41 0.0) (cons 42 0.0) (cons 91 0)))))
    (entmakex (append dxfcodes (list '(210 0.0 0.0 1.0)))))
  ;;(entity:make-pline (list p1 p2 p3 p4) nil 0 1))
  (defun add_text(pt h ang txt style jus) ;; 加文本
    (entmakex (list (cons 0 "TEXT")
		    (cons 100 "AcDbEntity")
		    (cons 62 2)
		    (cons 100 "AcDbText")
		    (if (= jus 0) (cons 10 pt) (list 10 0.0 0.0 0.0))
		    (cons 40 h)
                    (cons 1 txt)
		    (cons 50 ang)
		    (cons 7 style)
		    (cons 72 (cond ((= jus 0) 0) ((= jus 1) 1) ((= jus 2) 1) ((= jus 3) 2)))
		    (if (= jus 0)
			(list 11 0.0 0.0 0.0) (cons 11 pt))
		    (cons 100 "AcDbText")
		    (cons 73 (cond ((= jus 0) 0) ((= jus 1) 2) ((= jus 2) 3) ((= jus 3) 2)))
		    )))
  (defun display (ent / obj laynm name st1 st2 st3 lst h ang n box-pts text-style )
    (setq text-style "vitalhz")
    (if (null (tblsearch "style" text-style))
	(setq text-style (getvar "textstyle")))
    (setq obj (vlax-ename->vla-object ent))
    (setq laynm (strcat "图层:" (dxf ent 8)))
    (setq name (dxf ent 0)) ;; 图元类型
    (cond
      ((or (= name "TEXT")(= name "TCH_TEXT"))
       (setq lst (mapcar '(lambda (x / area )
			   (if (> (setq area (at-structure:get-steel-area x)) 0)
			       (format nil "钢筋面积: ~d" area )
			       "非钢筋文字"))
			 (string:parse-by-lst (dxf ent 1) '(";" "；"))))
       (setq lst (vl-remove nil lst)))
      (T (setq lst (list "非文字" name )))
      )
    ;;(if ss (mapcar 'entdel (pickset:to-entlist ss)));; 删除原有
    (setq ss (ssadd)) ;; 显示的图元集
    (setq h (/ (getvar "viewsize") 50)) ;; 字高为屏幕的 1/40
    (setq ang (fx (angle (getvar "viewctr") pt))) ;; 到中心点的角度
    (setq n (* 1.4 (1+ (/ (apply 'max (mapcar 'strlen lst)) 2.0)))) ; 宽度
    ;;(setq box-pts (list  pt 
    ;;   (polar pt (car ang) (* n h))
    ;;   (setq st1 (polar pt (cadr ang) (+ h (* 1.8 h (1+(length lst))))))
    ;;    (polar st1 (car ang) (* n h))))
    (setq box-pts (list pt ;; 上左
			(setq st1 (polar pt (* 1.5 pi) (+ h (* 1.8 h (1+(length lst)))))) ;;下左
			(polar st1 0 (* n h)) ;; 下右
			(polar pt 0 (* n h))))
    
    (ssadd (apply 'add_background box-pts) ss)
    (ssadd (add_box box-pts) ss) 
    
    ;;(setq st2 (polar pt (car ang) (/ (* n h) 2)))
    (setq st2 (polar pt 0 (/ (* n h) 2)))
    ;;(if (= (caddr ang) 0)
    ;;	(setq pt (polar pt (/ pi 2) (* 0.4 h)))
    ;;	(setq pt (polar pt (/ pi 2) (+ (* 1.4 h) (* 1.8 h (length lst)))))
    ;;  )
    (setq n -1)
    (repeat (length lst)
	    (ssadd (add_text (setq st2 (polar st2 (* 1.5 pi) (* 1.8 h)))
			     h 0 (nth (setq n (1+ n)) lst)
			     text-style 
			     1)
		   ss)
	    ))

  (push-var nil)  
  (command "ucs" "w")
  (command "_.undo" "_m")
  (prompt "\n***移动光标至钢筋文字查看！***")
  (setvar "osmode" 0)
  (setvar "fillmode" 1)
  (setvar "cmdecho" 0)
  (setq ss (ssadd))
  (while (not pd)
    (while (not (progn
                  (setq gr (grread T 1))
		  (if (= (car gr) 5)
		      (setq pt (cadr gr)
			    ent (nentselp pt)
			    ent (if (and ent (= (type (last (last ent))) 'ename))
				    (last (last ent))
				    (car ent)
				    )
			    )
		      (setq pd T)
		      )
		  ))
      )
    (if (and (not pd) (not (equal ent entold)) (not (ssmemb ent ss)))
	(progn
          (if entold (redraw entold 4))
          (if ss (mapcar 'entdel (pickset:to-entlist ss)))
          (redraw ent 3)
	  (display ent)
	  (setq entold ent)
	  )
	)
    )
  (if entold (redraw entold 4))
  (if ss (mapcar 'entdel (pickset:to-entlist ss)))
  (pop-var)
  (princ)
  )
