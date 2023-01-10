;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-xdata:first 用于 应用包 at-xdata 的 第一个配置项 first 
;; (@:get-config 'at-xdata:first) ;; 获取配置顶的值
;; (@:set-config 'at-xdata:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "扩展数据管理" "查询扩展数据" '(at-xdata:browser))

(defun at-xdata:browser () 
  (setq ss-x (ssget "x" '((-3 ("*")))))

  (@:log "INFO" (strcat "发现了" (itoa (sslength ss-x)) "个有扩展数据的图元"))
  (setq order-ent 0)
  (if (setq current (ssname ss-x order-ent)) 
    (progn 
      (pickset:zoom current)
      (sssetfirst nil (ssadd current))))
  (dcl:dialog "xdata")
  (dcl:mtext "xdata" 8 50)
  (dcl:button "next" "Next" "")
  (dcl:dialog-end-ok-cancel)
  (defun cb-next (/ current) 
    (setq order-ent (1+ order-ent))
    (if (>= order-ent (sslength ss-x))
      (setq order-ent 0))
    (if (setq current (ssname ss-x order-ent)) 
      (progn 
        (pickset:zoom current)
        (dcl:set-mtext 
          "xdata"
          (strcat 
            "第 " (itoa (1+ order-ent)) " 个，共 " (itoa (sslength ss-x)) " 个\n"
            "图元类型:"
            (entity:getdxf current 0)
            "\n"
            (if (wcmatch (entity:getdxf current 0) "INSERT") 
              (strcat "名称:" (block:get-effectivename current) "\n")
              "")
            "扩展数据\n"
            (string:from-list 
              (mapcar 
                '(lambda (x) 
                   (strcat 
                     "APPID:"
                     (car x)
                     "\n"
                     (vl-prin1-to-string (cdr x))))
                (cdr (assoc -3 (entget current '("*")))))
              "\n")))

        (sssetfirst nil (ssadd current)))
      (setq order 0)))
  (dcl:new "xdata")
  (dcl:set-mtext 
    "xdata"
    (strcat 
      "第 " (itoa (1+ order-ent)) " 个，共 " (itoa (sslength ss-x)) " 个\n"
      "图元类型:"
      (entity:getdxf current 0)
      "\n"
      (if (wcmatch (entity:getdxf current 0) "INSERT") 
        (strcat "名称:" (block:get-effectivename current) "\n")
        "")
      "扩展数据\n"
      (string:from-list 
        (mapcar 
          '(lambda (x) 
             (strcat 
               "APPID:"
               (car x)
               "\n"
               (vl-prin1-to-string (cdr x))))
          (cdr (assoc -3 (entget current '("*")))))
        "\n")))
  (set_tile "title" "扩展数据查看")
  (dcl:show)
  (princ))