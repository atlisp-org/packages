;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-curve:first 用于 应用包 at-curve 的 第一个配置项 first 
;;(@:define-config 'at-curve:first "我是配置项 at-curve:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'at-curve:first) ;; 获取配置顶的值
;; (@:set-config 'at-curve:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "曲线" "双线互连" "(at-curve:join)" )
(defun at-curve:join (/ l1 l2 pts1 pts2)
  (@:help "从最近点连接两线，合并成一条")
  (setq l1 (car (entsel (@:speak "请选择第一条线:"))))
  (setq l2 (car (entsel (@:speak "请选择第二条线:"))))
  (setq pts1 (curve:pline-3dpoints l1))
  (setq pts2 (curve:pline-3dpoints l2))
  (if (< (distance (car pts1) (car pts2))
	 (distance (last pts1) (car pts2)))
      (setq pts1 (reverse pts1)))
  (if (> (distance (last pts1) (car pts2))
	 (distance (last pts1) (last pts2)))
      (setq pts2 (reverse pts2)))
  (entdel l1)(entdel l2)
  (entity:make-lwpline-bold
   (append pts1 pts2)
   nil
   nil
   0 0))
  
