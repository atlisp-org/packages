;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'flange:first 用于 应用包 flange 的 第一个配置项 first 
;;(@:define-config 'flange:first "我是配置项 flange:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'flange:first) ;; 获取配置顶的值
;; (@:set-config 'flange:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 

(defun flange:draw ()
  (@:help (strcat "从参数表中读入数据，绘制法兰"))
  ;;实现代码
  (alert "flange")
  )
 
