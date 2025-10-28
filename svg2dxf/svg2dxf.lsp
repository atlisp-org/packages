
(@:add-menus
 '("svg2dxf"
   ("svg转dxf" "(svg2dxf:svg2dxf)" )
   ("图片转svg" "(svg2dxf:img2svg)" )
   ))
(defun svg2dxf:hello ()
  (@:help (strcat "这里的内容用于在运行这个功能开始时，对用户进行功能提示。\n"
		  "如怎么使用，注意事项等。\n当用户设置了学习模式时，会在命令行或弹窗进行提示。\n"
		  ))
  ;; 以下部分为你为实现某一功能所编写的代码。
  (alert (strcat "svg2dxf 的第一个功能.\n"
		 "创建了一个配置项 svg2dxf:first .\n"
		 "这个配置项的值为: " (@:get-config 'svg2dxf:first)
		 ))
  (princ)
  )
(defun svg2dxf:svg2dxf()
  (@::help "开发中...,该功能需要调用python和相关工具")
  )
(defun svg2dxf:svg2dxf()
  (@::help "开发中...,该功能需要安装 rust 和vtracer.")
  )

