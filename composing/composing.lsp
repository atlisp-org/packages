;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'composing:first 用于 应用包 composing 的 第一个配置项 first
;; (@:define-config 'composing:first "我是配置项 composing:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'composing:first) ;; 获取配置顶的值
;; (@:set-config 'composing:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "排版" "等间隙排版" "(composing:cluster-gas)")
(@:add-menu "排版" "总长等距排版" "(composing:cluster)")
