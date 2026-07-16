(@:add-menus
 '("@暖通"
   ("插入说明" (@hvac:draw-readme))
   ("平面图样例" (@hvac:draw-plan-example))
   ("布置风机风口"(@hvac:insert-block "方壁式轴流风机"))
   ;; ("插入风机"(@hvac:insert-block "风机盘管"))
   ("插入新风机组"(@hvac:insert-block "新风机组"))
   ("风机型号说明"(@hvac:fengji-info))
   ("风机统计"(@hvac:stat-fengji))
   ("标地暖管"(@hvac:dim-pipe))
   ("批量标地暖管"(@hvac:batch-dim-pipe))
   ("分集水器平衡分析"(@hvac:equip-balance))
   ("绘制设备表"(@hvac:make-equip-bom))
   ))
(setq @hvac:*fengji*
      (list
       ;;型号 全称 冷量(W)  热量(W)  N(W)   风量(m3/h)
       '("FP51" "卧式暗装风机盘管FP-51" 2890  4820 59 510)
       '("FP85" "卧式暗装风机盘管FP-85"4520 7670 84 850)
       '("FP102" "卧式暗装风机盘管FP-102" 5420 8850 118 1020)
       '("FP136" "卧式暗装风机盘管FP-136" 7200 10800 156 1360)))
      
(defun @hvac:fengji-info()
  (alert "卧式暗装风机盘管FP-51,冷量=2890W,热量=4820W,N=59W,风量=510m3/h
卧式暗装风机盘管FP-85,冷量=4520W,热量=7670W,N=84W,风量=850m3/h
卧式暗装风机盘管FP-102,冷量=5420W,热量=8850W,N=118W,风量=1020m3/h
卧式暗装风机盘管FP-136,冷量=7200W,热量=10800W,N=156W,风量=1360m3/h"))
(defun @hvac:stat-fengji (/ fengjis)
  (@::help "统一所选风机，形成材料表")
  (setq fengjis (block:ssget nil "方壁式轴流风机" nil))
  (setq statres
	(mapcar '(lambda(x)
		   (block:get-dynprop x "型号"))
		(pickset:to-list fengjis)))
  ;; (princ statres)
  (setq statres (stat:stat  statres))
  (if (assoc "未知型号" statres)
      (progn
	(alert "发现未知型号的风机，已定位，请修改！")
	(setq fengjis
	      (vl-remove-if-not
	       '(lambda(x)
		  (= "未知型号" (block:get-dynprop x "型号")))
	       (pickset:to-list
		(block:ssget "x" "方壁式轴流风机" nil))))
	(sssetfirst nil (pickset:from-list fengjis))
	)
    (progn
      (setq i 0)
      (ui:dyndraw
       (table:make
	'(0 0 0)
	"空调系统主要设备参数表"
	(list  "序号" "" "规格型号及技术参数""单位""数量")
	(mapcar '(lambda(x / data )
		   (setq data (assoc (car x) @hvac:*fengji*))
		   (list
		    (setq i (1+ i))
		    (cadr  data)
		    (strcat
		     "冷量="  (itoa (nth 2  data))"W "
		     "热量="  (itoa (nth 3  data))"W "
		     "N="  (itoa (nth 4  data))"W "
		     "风量="  (itoa (nth 3  data))"m³/h")
		    "台"
		    (cdr x)
		    ))
		statres)
	)
       '(0 0 0)
       ))))
(defun @hvac:insert-block (dwgname / downfile)
  (@::help "插入图块模板")
  (setq downfile (strcat "at-hvac/" dwgname ".dwg"))
  (if (null (findfile (strcat "packages/" downfile)))
      (progn
	(@:load-module 'pkgman)
	(@:down-pkg-file (@:uri) downfile "stable")(@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))(sleep 5))
      )
  (if (findfile (strcat "packages/" downfile))
      (progn
	(ui:dyndraw
	 (block:insert dwgname (@::package-path "at-hvac") '(0 0 0)0 1)
	 '(0 0 0)))
    ))

(defun @hvac:draw-readme ()
  (@:help '("插入暖通说明。" ))
  (@:load-module 'pkgman)
  (if @::require-down
      (@::require-down "at-hvac/readme-hvac.dwg"))
    
  (if (findfile (strcat @::*prefix* "packages/at-hvac/readme-hvac.dwg"))
      (progn
	(setq readme-hvac
	      (block:insert
	       "readme-hvac"
	       (strcat @::*prefix* "packages/at-hvac/")
	       '(0 0 0)
	       0 1))
	(if (ui:dyndraw readme-hvac '(0 0 0))
	    (if (string-equal "insert" (entity:getdxf readme-hvac 0))
		(progn
		  (vla-explode (e2o readme-hvac))
		  (vla-delete (e2o readme-hvac))))))
      ))
(defun @hvac:draw-plan-example ()
  (@:help '("插入暖通平面图样例。"
	    ))
  (@:load-module 'pkgman)
  (if @::require-down
      (@::require-down "at-hvac/example-hvac.dwg"))

  (if (findfile (strcat @::*prefix* "packages/at-hvac/example-hvac.dwg"))
      (progn
	(setq example-plan-hvac
	      (block:insert
	       "example-plan-hvac"
	       (strcat @::*prefix* "packages/at-hvac/")
	       '(0 0 0)
	       0 1))
	(ui:dyndraw readme-hvac '(0 0 0))
	)))
