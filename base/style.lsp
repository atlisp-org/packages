
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 添加 非打印图层
(if (= (member "org" (layer:list)) nil)
    (progn
      (command "-layer" "n" "org" "")
      (command "-layer" "c" "t" "210,180,60" "org" "")
      )
  )
(if (= (member "noprint" (layer:list)) nil)
    (progn
      (command "-layer" "n" "noprint" "")
      (command "-layer" "c" "t"  "210,120,60" "noprint" "")
      )
  )
;;;定制中文字体样式
(if (and (findfile "tssdeng.shx") (findfile "hztxt.shx"))
    (progn
      (if (= (tblsearch "style" "vitalhz") nil)
	  (command ".style" "vitalhz" "tssdeng,hztxt" "0" "0.71" "0" "N" "N" "N")
	  )
      (if (= (tblsearch "style" "vitalhz250") nil)
	  (command ".style" "vitalhz250" "tssdeng,hztxt" "250" "0.71" "0" "N" "N" "N")
	  )
      (if (= (tblsearch "style" "vitalhz350") nil)
        (command ".style" "vitalhz350" "tssdeng,hztxt" "350" "0.71" "0" "N" "N" "N")
	)
      (if (= (tblsearch "style" "vitalhz500") nil)
	  (command ".style" "vitalhz500" "tssdeng,hztxt" "500" "0.71" "0" "N" "N" "N")
      )
      (if (= (tblsearch "style" "vitalhz700") nil)
	  (command ".style" "vitalhz700" "tssdeng,hztxt" "700" "0.71" "0" "N" "N" "N")
	  ) ))

;; Local variables:
;; coding: gb2312
;; End: 
