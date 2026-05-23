(@:add-menu "结构计算工具" "小虎工具箱" "(stru:jiegou-tools \"XiaoHu\")")
(@:add-menu "结构计算工具" "结构计算STR" "(stru:jiegou-tools \"STR\")")
(@:add-menu "结构计算工具" "老董结构" "(stru:jiegou-tools \"LaoDong\")")
(@:add-menu "结构计算工具" "结构计算" "(stru:jiegou-tools \"JieGou\")")
(@:add-menu "结构计算工具" "钢结构细部" "(stru:jiegou-tools \"steel\")")

(defun stru:jiegou-tools ( file-capital / app-name)
  (setq app-name (strcat "structure-tools/" file-capital "-tools.exe"))
  (if (null (findfile (strcat "packages/" app-name)))
      (progn
	(@:log "INFO" "没有发现 文件，正在下载...\n")
	(@:down-pkg-file (@:uri) app-name edition))
      (command "start" (findfile (strcat "packages/" app-name)))))

  
;; Local variables:
;; coding: gb2312
;; End:
