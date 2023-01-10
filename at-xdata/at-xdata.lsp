;; 向系统中添加菜单 
(@:add-menu "扩展数据管理" "查询扩展数据" '(at-xdata:browser))
;; (@:add-menu "扩展数据管理" "检查扩展数据" '(at-xdata:check))
(defun at-xdata:browser () 
  (@:help '("依次显示图元的扩展数据"))
  (setq ss-x (ssget '((-3 ("*")))))
  (if (null ss-x) 
    (setq ss-x (ssget "x" '((-3 ("*"))))))
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
              (setq order-ent (1-(sslengh ss-x))))
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
          (dcl:show)
          (princ))))))