(@:add-menus 
  '("块参照操作"
    ("参照归层" (@block:xref-layer))
    ("重载选定" (@block:xfr))
    ("重载所有" (@block:czcz))
    ("卸载选定" (@block:xfx))
    ("卸载所有" (@block:xfxa))
    ("拆离选定" (@block:xfd))
    ("拆离所有" (@block:xfda))
    ("绑定选定" (@block:xfb))
    ("绑定所有" (@block:bdcz))
    ("命令提示" (@block:help-xref-hk))))
(defun @block:xref-layer (/ xrefs) 
  (@:help "将外部参照移至同一个图层，以便于锁定。图层名在设置中进行设置。")
  (setq xrefs (pickset:to-list (ssget "x" '((0 . "insert")))))
  ;; 去除非外参照块
  (setq xrefs (vl-remove-if-not 
                '(lambda (x) 
                   (and 
                     (assoc 1 (tblsearch "block" (entity:getdxf x 2)))
                     (findfile 
                       (cdr (assoc 1 (tblsearch "block" (entity:getdxf x 2)))))))
                xrefs))
  ;;到此 xrefs 就是所有的外部参照图元了。
  (layer:lock (@:get-config '@block:xref-layer) nil)
  (mapcar 
    '(lambda (xref%) 
       ;; 如果不存在参照名的图层则建立
       (if (null (tblsearch "layer" (@:get-config '@block:xref-layer))) 
         (layer:make (@:get-config '@block:xref-layer) nil nil nil))
       ;;如果参照名与参照的图层名不同则修改
       (if (/= (@:get-config '@block:xref-layer) (entity:getdxf xref% 8)) 
         (entity:putdxf xref% 8 (@:get-config '@block:xref-layer))))
    xrefs)
  (layer:lock (@:get-config '@block:xref-layer) t))
;;XFR        重载选定
;;CZCZ        重载所有
;;XFX        卸载选定
;;XFXA        卸载所有
;;XFD        拆离选定
;;XFDA        拆离所有
;;XFB        绑定选定
;;BDCZ        绑定所有
(defun @block:help-xref-hk () 
  (alert 
    (strcat "\n重载选定参照文件 命令：XFR" "\n重新加载所有外部参照 命令：CZCZ" "\n卸载选定参照文件 命令：XFX" 
            "\n卸载所有外部参照 命令：XFXA" "\n拆离选定参照文件 命令：XFD" "\n拆离所有外部参照 命令：XFDA" "\n绑定选定参照文件 命令：XFB" 
            "\n绑定所有外部参照 命令：BDCZ")))

(Defun @block:XFR ()  ;定义“重载选定参照文件”命令
  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入

  (princ "\n选定要重新载入的参照文件:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
        (command "-xref" "R" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
      ) ;end repeat
      (prompt "\n已经重载的文件为:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:CZCZ ()  ;定义“重新加载所有外部参照”命令
  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入
  (command "-xref" "R" "*")
  (princ))
(Defun @block:XFX ()  ;定义“卸载选定参照文件”命令

  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入

  (princ "\n选择要卸载的外部参照对象:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
        (command "-xref" "U" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
      ) ;end repeat
      (prompt "\n已卸载的外部参照文件是:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:XFXA ()  ;定义“卸载所有外部参照”命令
  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入
  (command "-xref" "U" "*")
  (princ))
(Defun @block:XFD ()  ;定义“拆离选定参照文件”命令

  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入

  (princ "\n选择要拆离的外部参照对象:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
        (command "-xref" "Detach" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
      ) ;end repeat
      (prompt "\n已拆离的外部参照文件为:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:XFDA ()  ;定义“拆离所有外部参照”命令
  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入
  (command "-xref" "Detach" "*")
  (princ))
(Defun @block:XFB ()  ;定义“绑定选定参照文件”命令

  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入

  (princ "\n选择要绑定的外部参照对象:")

  (if (setq SS (ssget)) 
    (progn 
      (setq Rnames "")
      (setq oldBT (getvar "BINDTYPE"))
      (setq BT (if (null BT) oldBT BT))
      (setq BT_tmp (getstring 
                     (strcat "输入绑定类型[绑定(N)/插入(Y)]<" (itoa BT) ">: ")))
      (if (null BT_tmp) (setq BT_tmp BT))
      (setq BT (atoi BT_tmp))
      (setvar "BINDTYPE" BT)
      (repeat (setq I (sslength SS)) 
        (setq E (ssname SS (setq I (1- I))))
        (setq ELIST (entget E))
        (setq Rname (cdr (assoc 2 ELIST))) ;_参照名
        (command "-xref" "Bind" Rname)
        (setq Rnames (strcat Rname ", " Rnames)) ;_strcat 连接成字符串
      ) ;end repeat
      (setvar "BINDTYPE" oldBT)
      (prompt "\n已绑定的外部参照文件为:")
      (princ Rnames)) ;end progn
  ) ;end if
  (princ))
(Defun @block:BDCZ ()  ;定义“绑定所有外部参照”命令
  (setvar "cmdecho" 0) ;命令执行过程不回显提示和输入
  (setq oldBT (getvar "BINDTYPE"))
  (setq BT (if (null BT) oldBT BT))
  (setq BT_tmp (getstring 
                 (strcat "输入绑定类型[绑定(0)/插入(1)]<" (itoa BT) ">: ")))
  (if (null BT_tmp) (setq BT_tmp BT))
  (setq BT (atoi BT_tmp))
  (setvar "BINDTYPE" BT)
  (command "-xref" "Bind" "*")
  (setvar "BINDTYPE" oldBT)
  (princ))

(@:define-hotkey "xfr" "(@block:xfr)")
(@:define-hotkey "czcz" "(@block:czcz)")
(@:define-hotkey "xfx" "(@block:xfx)")
(@:define-hotkey "xfxa" "(@block:xfxa)")
(@:define-hotkey "xfd" "(@block:xfd)")
(@:define-hotkey "xfda" "(@block:xfda)")
(@:define-hotkey "xfb" "(@block:xfb)")
(@:define-hotkey "bdcz" "(@block:bdcz)")