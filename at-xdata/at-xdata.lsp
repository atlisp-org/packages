;; 配置选项
(@:define-config '@xdata:filter "*" "扩展数据过滤器，当框选有扩展数据的图元时，可以过滤掉不需要的。")
;; 向系统中添加菜单 
(@:add-menus 
  '("扩展数据管理"
    ("查询扩展数据" (at-xdata:browser))
    ("配置过滤器" (at-xdata:set-filter))))

(defun at-xdata:browser (/ gen-mtext order-ent current ss-x cb-next cb-prev 
                         cb-zoomext) 
  (@:help '("依次显示图元的扩展数据"))
  (setq ss-x (ssget (list (list -3 (list (@:get-config '@xdata:filter))))))
  (if (null ss-x) 
    (setq ss-x (ssget "x" (list (list -3 (list (@:get-config '@xdata:filter)))))))

  (if ss-x 
    (progn 
      (@:log "INFO" (strcat "发现了" (itoa (sslength ss-x)) "个有扩展数据的图元"))
      (setq order-ent 0)
      (if (setq current (ssname ss-x order-ent)) 
        (progn 
          (pickset:zoom current)
          (sssetfirst nil (ssadd current))
          (dcl:dialog "xdata")
          (dcl:mtext "xdata" 8 50)
          (dcl:begin-cluster "row" "操作")
          (progn 
            (dcl:button "prev" "Prev" "")
            (dcl:button "Zoomext" "zoomExt" "")
            (dcl:button "next" "Next" ""))
          (dcl:end-cluster)
          (dcl:dialog-end-ok-cancel)
          (defun gen-mtext () 
            (strcat 
              "第 "
              (itoa (1+ order-ent))
              " 个，共 "
              (itoa (sslength ss-x))
              " 个\n"
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
          (defun cb-zoomext () 
            (vla-ZoomExtents *ACAD*))
          (defun cb-prev (/ current) 
            (setq order-ent (1- order-ent))
            (if (< order-ent 0) 
              (setq order-ent (1- (sslengh ss-x))))
            (if (setq current (ssname ss-x order-ent)) 
              (progn 
                (pickset:zoom current)
                (dcl:set-mtext "xdata" (gen-mtext))
                (sssetfirst nil (ssadd current)))
              (setq order 0)))
          (defun cb-next (/ current) 
            (setq order-ent (1+ order-ent))
            (if (>= order-ent (sslength ss-x)) 
              (setq order-ent 0))
            (if (setq current (ssname ss-x order-ent)) 
              (progn 
                (pickset:zoom current)
                (dcl:set-mtext "xdata" (gen-mtext))
                (sssetfirst nil (ssadd current)))
              (setq order 0)))
          (dcl:new "xdata")
          (dcl:set-mtext "xdata" (gen-mtext))
          (set_tile "title" "扩展数据查看")
          (dcl:show))))
    (@:log "INFO" (@:speak "没有发匹配的图元记录。如确定存在，请配置过过滤器")))
  (princ))


(defun at-xdata:set-filter (/ ss-x) 
  "函数功能说明"
  "函数返回值"
  (@:help '("设置扩展数据过滤选项，以方便后续操作"))
  (setq ss-x (ssget "x" '((-3 ("*")))))
  (setq xdata-appids (mapcar 
                       '(lambda (x) 
                          (mapcar 
                            'car
                            (cdr (assoc -3 (entget x '("*"))))))
                       (pickset:to-list ss-x)))
  (setq xdata-appids (list:remove-duplicates 
                       (list:flatten xdata-appids)))
  (if xdata-appids 
    (progn 
      (setq res (ui:select-multi "请选择要匹配的扩展数据组appid" xdata-appids))

      (if res 
        (@:set-config '@xdata:filter (string:from-list res ","))
        (@:set-config '@xdata:filter "*"))
      (@:log "INFO" "已完成 @xdata:filter 的设置"))
      (@:log "INFO" (@:speak"图中没有发现含有扩展数据的图元，配置选项值没有修改。"))
  
  ))
