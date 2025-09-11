(setq $psk-about "管道求解组件 PSK 0.61")

(setq $psk-regroot	"HKEY_CURRENT_USER\\Software\\InkPaint Computing\\PSK"
      $psk-install-path	(vl-registry-read $psk-regroot "Install Path")
)

(if (or (null $psk-install-path) (= $psk-install-path ""))
  (progn (princ "\n未设置应用程序安装路径。")
	 (vl-registry-write $psk-regroot "Install Path" "")
	 (vl-exit-with-value 1)
  )
)


;;; BOOKMARK - 变量初始化

;;;_$ (psk-get-filename "a")
;;;"C:\\Users\\hhs\\Desktop\\dd2\\a"
;;;_$ (psk-get-filename "\\a")
;;;"C:\\Users\\hhs\\Desktop\\dd2\\a"
;;;_$ (psk-get-filename "/a")
;;;"C:\\Users\\hhs\\Desktop\\dd2/a"
(defun psk-get-filename	(name / lead)
  (setq lead (substr name 1 1))
  (if (and (/= "\\" lead)
	   (/= "/" lead)
      )
    (setq name (strcat "\\" name))
  )
  (strcat $psk-install-path name)
)

(defun psk-getcustomfile (name)
  (psk-get-filename (strcat $psk-customdir name))
)
;;;(psk-combian-systems)
(defun psk-combian-systems (/ rt)
  (foreach pref $psk-services-duct
    (foreach sub $psk-subsystem
      (setq rt (cons (strcat (car pref) "-" (car sub)) rt))
    )
  )
  (foreach pref $psk-services
    (setq rt (cons (strcat (car pref) "-PIPE") rt)
          rt (cons (strcat (car pref) "-IDEN") rt)
    )
  )
  (reverse rt)
)
;;;(p-list->file (psk-combian-systems) "1.csv")


;;;(setq $psk-customdir "config/profiles/hanjia/")
;;;  $psk-customdir		      "config/profiles/default/"
  


(setq $psk-keypacks (p-lisp-load (psk-get-filename "partset.lsp")))
(setq $psk-keys (p-lisp-load (psk-get-filename "parts.lsp")))
(setq $psk-pipe-parts (p-lisp-load (psk-get-filename "pipe-parts.lsp")))


;;;(setq $psk-prop-defination
;;;       (eval
;;;	 (p-lisp-load (psk-get-filename "prop.lsp"))
;;;       )
;;;)


(defun psk-setting-load	(/ e settings)
  ;; 导入保存于每个dwg中的设置
  (if (setq settings (vlax-ldata-get "PSK" "SETTINGS"))
    (foreach e settings
      (set (read (car e)) (cdr e))
    )
  )

  ;; 程序因更新可能增加的变量 用默认值补充定义
  (foreach e $psk-settings-default
    (if	(null (vl-symbol-value (read(car e))))
      (set (read (car e)) (cdr e))
    )
  )

  ;; 对可能新增的变量保存
  (psk-setting-save)
)


(defun psk-setting-save	(/ e settings)
  (vlax-ldata-put
    "PSK"
    "SETTINGS"
    (mapcar '(lambda (e) (cons e (vl-symbol-value (read e))))
	    (mapcar 'car $psk-settings-default)
    )
  )
)
(defun psk-createvaluelast-load	(/ r)
  (if (setq r (vlax-ldata-get "PSK" "CREATEVALUELAST"))
    ;; 进行赋值 否则无法添加新变量
    (foreach r1 r
      (setq $psk-path-createvaluelast
             (p-set
               $psk-path-createvaluelast
               (cons
                 (car r1)
                 (p-set
                   (p-get $psk-path-createvaluelast
                          (car r1)
                   )
                   (cdr r1)
                 )
               )
             )
      )
    )
  )

  (if (setq r (vlax-ldata-get "PSK" "CREATETYPELAST"))
    (setq $psk-path-createtype r)
  )
)




(setq $psk-settings-desc
       '(("$PSK-CUSTOMDIR"
           1000
           "配置文件目录"
           "系统及图层等配置文件所在的目录"
           ("config/profiles/hanjia/" "config/profiles/default/" "")
         )
          ("$PSK-IDEN-TEXTHEIGHT"
            1040
            "标注文字高度"
            ""
            (2.5 3.0 4.0 5.0 "")
          )
          ("$PSK-IDEN-WIDFACTOR"
            1040
            "宽度因子"
            ""
            (0.5 0.6 0.7 0.75 0.8 0.9 1.0 "")
          )
          ("$PSK-IDEN-SCALE" 1040 "全局比例" "" (50. 100. 150. 200. ""))
          ("$PSK-IDEN-TEXTSTYLE" 1000 "标注文字样式")
          ("$PSK-IDEN-MINLENGTH" 1040 "标注管线长度限值")
          ("$PSK-AUTOREDRAW"
            1000
            "自动更新图面"
            "绘制管道时自动更新图面"
            (("Y" "是") ("N" "否"))
          )
          ("$PSK-TEMPL-PIPE" 1000 "水管标注模板" "水管标注模板" ("{SERV} DN{DN}" ""))
          ("$PSK-TEMPL-DUCT"
            1000
            "矩形风管标注模板"
            "矩形风管标注模板"
            ("{W}x{H}" "{SERV}D {W}x{H}" "{SERV}D {W}x{H} (H+{EL:3})" "")
          )
          ("$PSK-TEMPL-DUCTROUND" 1000 "圆形风管标注模板" "圆形风管标注模板" ("%%C{D}" ""))
          ("$PSK-TEMPL-REFPIPE"
            1000
            "冷媒管标注模板"
            "冷媒管标注模板"
            ("{SERV} {REFS}" "{REFS}" "")
          )
          ("$PSK-IDEN-OFFSET" 1070 "组合标注偏移" "组合标注第一个文字离开最近基线距离")
          ("$PSK-ERF-CREATE"
            1040
            "新建弯头半径"
            "创建管件时默认采用的弯头半径参数"
            (0.8 1.0 1.5 "")
          )
          ("$PSK-IDEN-OFFSET" 1070 "组合标注偏移" "组合标注第一个文字离开最近基线距离")
          ("$PSK-DUCT-FLEXTEND" 1070 "双线风管法兰突出距离" "双线风管法兰突出距离")
	  ("$PSK-ANGLE-TOLERANCE" 1040 "角度判断容差" "管道连接时以弧度角度判断容差")
        )
)
(setq $psk-settings-default
       '(("$PSK-IDEN-TEXTHEIGHT" . 3.0) ;_标注文字默认高度
	 ("$PSK-IDEN-WIDFACTOR" . 0.7) ;_标注文字宽度比例
	 ("$PSK-IDEN-SCALE" . 100.) ;_全局比例影响标注文字
	 ("$PSK-IDEN-TEXTSTYLE" . "HJ-GBXWXT")
	 ("$PSK-IDEN-MINLENGTH" . 1000.0) ;_小于该长度的直线不标注
	 ("$PSK-AUTOREDRAW" . "Y")
	 ("$PSK-DUCT-FLEXTEND" . 50.) ;_法兰边突出长度
	 ("$PSK-TEMPL-PIPE" . "{SERV} DN{DN}" )
	 ("$PSK-TEMPL-DUCT" . "{W}x{H}" )
	 ("$PSK-TEMPL-DUCTROUND" . "%%C{D}" )
	 ("$PSK-TEMPL-REFPIPE" . "{REFS}" )
	 ("$PSK-IDEN-OFFSET" . 1000. )
	 ("$PSK-ERF-CREATE" . 0.8 )
         ("$PSK-CUSTOMDIR" . "config/profiles/hanjia/")
	 ("$PSK-ANGLE-TOLERANCE" . 0.02)
	)
      $psk-sel-settings
       "$PSK-IDEN-TEXTHEIGHT"
      $psk-sel-path-create
       ".TYPE"
)


(defun psk-settings-change (/ change k)
  (setq	change
	 (propertybag-edit
	   (mapcar '(lambda (e) (cons e (vl-symbol-value (read e))))
		   (mapcar 'car $psk-settings-default)
	   )
	   $psk-settings-desc
	   '$psk-sel-settings
	   nil
	 )
  )
  (if (and change (/= 0 change))
    (progn
      (foreach e change
	(set (read (car e)) (cdr e))
      )
      (psk-setting-save)
    )
  )
)

;; 加载系统变量
;;;(load (psk-get-filename "var.lsp"))
(eval
  (p-lisp-load (psk-get-filename "var.lsp"))
)

(psk-setting-load)
(psk-createvaluelast-load)


(setq
  $psk-services	     (p-csvfile-read (psk-getcustomfile "services.csv"))
  $psk-services-duct (p-csvfile-read
		       (psk-getcustomfile "services-duct.csv")
		     )
  $psk-subsystem     (p-csvfile-read (psk-getcustomfile "subsystem.csv"))
  $psk-layer-config  (p-csvfile-read
		       (psk-getcustomfile "layer-configration.csv")
		     )

  $psk-prop-def	     (p-csvfile-read (psk-get-filename "prop.csv"))

  $psk-prop-def	     (mapcar
		       (function
			 (lambda (e)
			   (append (list (car e) (atoi (cadr e))) (cddr e))
			 )
		       )
		       $psk-prop-def
		     )
)


(setq $addnew-textstyle $PSK-iden-textstyle)


(p-textstyle-get
  $addnew-textstyle
  "gbxwxt.shx"
  "gbhzfs.shx"
  0.7
)
(setvar "LTSCALE" 1000.)


(load (psk-get-filename "command.lsp"))
(load (psk-get-filename "util.lsp"))