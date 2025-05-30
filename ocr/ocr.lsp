;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'ocr:first 用于 应用包 ocr 的 第一个配置项 first 
(@:define-config 'ocr:first "我是配置项 ocr:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'ocr:first) ;; 获取配置顶的值
;; (@:set-config 'ocr:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "ocr" "识别文字" "(ocr:viewport)" )
(defun ocr:viewport ()
  (@::prompt '("识别当前视口中的文字"))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (setq sset (vla-Add (vla-get-SelectionSets *doc*) (strcat "TEST_OCR1"  (@::timestamp))))
  (vla-SelectOnScreen sset)
  (vla-export *DOC* (strcat @::*prefix* "tmp/ocr") "BMP" sset)
  (setq req (@::postjson "http://localhost:5000/api/ocr" (strcat "{\"filepath\":\""(@::path-unix-format(strcat @::*prefix* "tmp\\ocr.BMP")) "\"}")))
  (vla-delete sset)
  (princ req)
  req
  )
