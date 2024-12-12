(defun @block:tchpr2insert ()
  ;; 使用高版本的CAD软件用修复方法破解。
  (@::prompt "需要以 修复 形式打开被加密文件。")
  (if (tblsearch "block" "TCH_PR")
      (progn
	(block:insert "TCH_PR" "" (getpoint "请点击要解开图形的位置:") 0 1)
	(@::prompt "已解开保护，可以尝试分解图形。")
	)
      (@::prompt "没有发现被保护的图形，需要以 修复 形式打开被加密文件。"))
  (princ))
