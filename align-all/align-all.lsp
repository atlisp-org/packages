;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'align-all:first 用于 应用包 align-all 的 第一个配置项 first 
(@:define-config 'align-all:hangju 2000.0 "排列图素时的默认行距。")
;; (@:get-config 'align-all:first) ;; 获取配置顶的值
;; (@:set-config 'align-all:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "创意绘图" "对齐图素" "(align-all:entity)")

;;调整行距hj 支持CAD文字、天正单行和多行文字、图块、属性字、天正标高、cad表格、cad尺寸
;;(setq BGhangju nil)
(defun align-all:entity (/ *error* a all b c e hangju1 hangju2 l m n p snap x x0 xyz 
                         xyz_new y y0 z) 
  (defun *error* (msg)  ;错误处理函数
    (pop-var)
    ;; (if snap (setvar "osmode" snap)) ;恢复捕捉
    (if (< 18 (atoi (substr (getvar "acadver") 1 2)))  ;判断CAD版本，高版本用command-s
      (command-s "undo" "e") ;CAD高版本用
      (command "undo" "e") ;低版本用
    )
    ;;(setvar "cmdecho" 1) ;打开命令行提示
    (princ msg))
  (push-var)

  (if 
    (setq a (ssget 
              (list 
                (cons 0 "*TEXT,DIMENSION,INSERT,ATTDEF,ACAD_TABLE,TCH_ELEVATION"))))
    (progn 
      (setq n (sslength a))
      (setq m 0)
      (while (< m n) 
        (setq all (append all (list (entget (ssname a m)))))
        (setq m (1+ m)))
      (setq l 0) ;按y坐标降序排列
      (setq m 1)
      (while (< l n) 
        (setq b (nth l all))
        (while (< m n) 
          (setq c (nth m all))
          (if (> (nth 2 (assoc '10 c)) (nth 2 (assoc '10 b))) 
            (progn 
              (setq all (subst 'aa (nth l all) all))
              (setq all (subst 'bb (nth m all) all))
              (setq all (subst c 'aa all))
              (setq all (subst b 'bb all))
              (setq b c)))
          (setq m (1+ m)))
        (setq l (1+ l))
        (setq m (1+ l)))
      (setq p (cdr (assoc '10 (car all))))
      ;;(setq hangju2 2000) ;默认行距为2000，自行修改
      (setvar "osmode" 16383)
      (setq hangju2 (getdist 
                      (strcat 
                        "\n输入行距 支持鼠标点选 <"
                        (rtos (@:get-config 'align-all:hangju) 2 2)
                        ">：")))
      (if (and (= 'real (type hangju2)) (> hangju2 0)) 
        (@:set-config 'align-all:hangju hangju2)
        (setq hangju2 (@:get-config 'align-all:hangju)))
      (setq x0 (car p))
      (setq y0 (cadr p))
      (setq m 0)
      (setvar "cmdecho" 0) ;关闭命令行提示
      (vl-cmdf "undo" "be") ;命令开始标记
      (setvar "osmode" 0) ;关闭捕捉
      (while (< m n) 
        (setq b (nth m all))
        (setq e (cdr (assoc -1 b))) ;图原名
        (setq z (nth 3 (assoc 10 b)))
        (setq x (nth 1 (assoc 10 b)))
        (setq y (nth 2 (assoc 10 b)))
        (setq xyz (list x y z)) ;老坐标
        (setq xyz_new (list x0 y0 z)) ;新坐标
        (vl-cmdf "move" e "" xyz xyz_new) ;移动
        (setq y0 (- y0 hangju2))
        (setq m (1+ m)))
      ;; (setvar "osmode" snap) ;打开捕捉
      (vl-cmdf "undo" "e") ;命令结束标记
      (setvar "cmdecho" 1) ;打开命令行提示
    ))
  (pop-var)
  (princ))
