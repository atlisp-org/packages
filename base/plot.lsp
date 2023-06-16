;; 打印样式表
(@:define-config 'base:ctb "monochrome.ctb" (_"Print style file."))
;;打印驱动配置
(@:define-config 'base:plotter "VitalPDF.pc3" "Please change it to your ploter,and setup page sizes。")
(cond
 (is-zwcad
  (@:define-config 'base:plotter-pdf "VitalPDFzw.pc5" "PDF Printer"))
 (is-gcad
  (@:define-config 'base:plotter-pdf "VitalPDFg.pc3" "PDF Printer"))
 (t
  (@:define-config 'base:plotter-pdf "VitalPDF.pc3" "PDF Printer")))
;; (@:define-config 'base:projects-output "D:\\Output" "本地输出目录")
(defun @:update-ctb-dialog ()
  (@:set-config 'base:ctb
		(nth (atoi (get_tile "ctbName")) list-ctb))
  (done_dialog))
(setq list-ctb '())
;;(@:add-menu "输出打印" "选打印样式表" "(@:set-ctb)")
(defun @:set-ctb (/ path-ctb fn dcl_fp update-ctb-dialog) 
  "选择打印样式表。"
  (defun update-ctb-dialog ()
    (@:set-config 'base:ctb
		  (nth (atoi (get_tile "ctbName")) list-ctb))
    (done_dialog))
  (cond
   (is-zwcad
    (setq path-ctb (strcat (getvar "roamablerootprefix") "Printstyle\\")))
   (is-gcad
    (setq path-ctb (strcat (system:dir (getvar "roamablerootprefix")) "PrintStyles\\")))
   ((and (< (@:acadver) 18) (not is-zwcad)) ;;旧版acad
    (setq path-ctb (strcat (getvar "roamablerootprefix") "Plot Styles\\")))
   (t
    (setq path-ctb (strcat (getvar "roamablerootprefix") "Plotters\\Plot Styles\\"))))

  (setq dcl_fp (open (strcat (@:package-path "base") "ctb.dcl") "w"))
  (write-line (strcat "ctb : dialog { "
		      "label =\"Please select CTB file\";") dcl_fp)
  (write-line (strcat ":row {"
		      ": popup_list {"
		      "key = \"ctbName\";"
		      "} }"
		      "ok_cancel;"
		      "}" )
	      dcl_fp)
  (close dcl_fp)

  (setq list-ctb (directory path-ctb "*.ctb" 1))
  (setq dcl_id (load_dialog (strcat (@:package-path "base") "ctb.dcl")))
  (if (not (new_dialog "ctb" dcl_id))
      (exit))
  (start_list "ctbName")
  (mapcar 'add_list list-ctb)
  (end_list)
  ;; 设置默认值
  (set_tile "ctbName" (@:to-string
		       (- (length list-ctb)
			  (length (member (@:get-config 'base:ctb)
					  list-ctb)))))
  
  (action_tile "accept" "(update-ctb-dialog)")
  (start_dialog)
  (unload_dialog dcl_id)
  )


(defun plot:to-pdf ( tufu zongheng pt1 pt2 file-name
		     / minpoint maxpoint oldosmode )
  "打印到PDF文件。参数： 图幅 纵横 第一点 第二点 输出文件名。"
  (cond
   (is-zwcad
    (if (null (and
	       (findfile (strcat (getvar "roamablerootprefix") "Plotters\\VitalPDFzw.pc5"))
	       (findfile (strcat (getvar "roamablerootprefix") "Plotters\\PMP Files\\VitalPDFzw.pmp"))))
	(if (and (findfile (strcat (@:package-path "base")"VitalPDFzw.pc5"))
		 (findfile (strcat (@:package-path "base")"VitalPDFzw.pmp")))
	    (progn
	      (vl-file-copy (strcat (@:package-path "base") "VitalPDFzw.pmp")
			    (strcat (getvar "roamablerootprefix") "Plotters\\PMP Files\\VitalPDFzw.pmp"))
	      (vl-file-copy (strcat (@:package-path "base") "VitalPDFzw.pc5")
			    (strcat (getvar "roamablerootprefix") "Plotters\\VitalPDFzw.pc5"))
	      )
	  (progn
	    (@:down-pkg-file (@:uri) "base/VitalPDFzw.pc5" @:*edition*)
	    (@:down-pkg-file (@:uri) "base/VitalPDFzw.pmp" @:*edition*)
	    (alert "下载PDF打印配置文件，请稍候...")
	    (vl-file-copy (strcat (@:package-path "base") "VitalPDFzw.pmp")
			  (strcat (getvar "roamablerootprefix") "Plotters\\PMP Files\\VitalPDFzw.pmp"))
	    (vl-file-copy (strcat (@:package-path "base") "VitalPDFzw.pc5")
			  (strcat (getvar "roamablerootprefix") "Plotters\\VitalPDFzw.pc5"))
	    
	    (sleep 3)))))
   (is-gcad
    (if (null (and
	       (findfile (strcat(system:dir  (getvar "roamablerootprefix")) "Plot\\VitalPDFg.pc3"))
	       (findfile (strcat (system:dir (getvar "roamablerootprefix")) "Plot\\PMP\\VitalPDFg.pmp"))))
	(if (and (findfile (strcat (@:package-path "base")"VitalPDFg.pc3"))
		 (findfile (strcat (@:package-path "base")"VitalPDFg.pmp")))
	    (progn
	      (vl-file-copy (strcat (@:package-path "base") "VitalPDFg.pmp")
			    (strcat (system:dir (getvar "roamablerootprefix")) "Plot\\PMP\\VitalPDFg.pmp"))
	      (vl-file-copy (strcat (@:package-path "base") "VitalPDFg.pc3")
			    (strcat (system:dir (getvar "roamablerootprefix")) "Plot\\VitalPDFg.pc3"))
	      )
	  (progn
	    (@:down-pkg-file (@:uri) "base/VitalPDFg.pc3" @:*edition*)
	    (@:down-pkg-file (@:uri) "base/VitalPDFg.pmp" @:*edition*)
	    (alert "下载PDF打印配置文件，请稍候...")
	    (vl-file-copy (strcat (@:package-path "base") "VitalPDFg.pmp")
			  (strcat(system:dir  (getvar "roamablerootprefix")) "Plot\\PMP\\VitalPDFg.pmp"))
	    (vl-file-copy (strcat (@:package-path "base") "VitalPDFg.pc3")
			  (system:dir (strcat (getvar "roamablerootprefix")) "Plot\\VitalPDFg.pc3"))
	    
	    (sleep 3)))))
   ((null (and
	       (findfile (strcat (getvar "roamablerootprefix") "Plotters\\VitalPDF.pc3"))
	       (findfile (strcat (getvar "roamablerootprefix") "Plotters\\PMP Files\\VitalPDF.pmp"))))
	(if (and (findfile (strcat (@:package-path "base")"VitalPDF.pc3"))
		 (findfile (strcat (@:package-path "base")"VitalPDF.pmp")))
	    (progn
	      (vl-file-copy (strcat (@:package-path "base") "VitalPDF.pmp")
			    (strcat (getvar "roamablerootprefix") "Plotters\\PMP Files\\VitalPDF.pmp"))
	      (vl-file-copy (strcat (@:package-path "base") "VitalPDF.pc3")
			    (strcat (getvar "roamablerootprefix") "Plotters\\VitalPDF.pc3"))
	      )
	  (progn
	    (@:down-pkg-file (@:uri) "base/VitalPDF.pc3" @:*edition*)
	    (@:down-pkg-file (@:uri) "base/VitalPDF.pmp" @:*edition*)
	    (alert "下载PDF打印配置文件，请稍候...")
	    (vl-file-copy (strcat (@:package-path "base") "VitalPDF.pmp")
			  (strcat (getvar "roamablerootprefix") "Plotters\\PMP Files\\VitalPDF.pmp"))
	    (vl-file-copy (strcat (@:package-path "base") "VitalPDF.pc3")
			  (strcat (getvar "roamablerootprefix") "Plotters\\VitalPDF.pc3"))
	    
	    (sleep 3)))))
  
  (cond
   (is-zwcad (setq path-ctb (strcat (getvar "roamablerootprefix") "Printstyle\\")))
   (is-gcad (setq path-ctb (strcat (getvar "roamablerootprefix") "PrintStyles\\")))
   (t (setq path-ctb (strcat (getvar "roamablerootprefix") "Plotters\\Plot Styles\\"))))
  
  (if (null (findfile (strcat path-ctb (@:get-config 'base:ctb))))
      (progn
	(princ (strcat "没有发现打印样式表文件: " (@:get-config 'base:ctb) "\n 将使用 monochrome.ctb."))
	(@:set-config 'base:ctb "monochrome.ctb")
	))
  
  
  (push-var)
  (setvar "filedia" 0)
  ;;(command "ucs" "w")
  (setvar "osmode" 0)

  (setq minpoint (list (min (nth 0 pt1) (nth 0 pt2))
		       (- (min (nth 1 pt1) (nth 1 pt2)) 0.01)
		       0))
  (setq maxpoint (list (+ (max (nth 0 pt1) (nth 0 pt2)) 0.01)
		       (max (nth 1 pt1) (nth 1 pt2))
		       0))
  ;;(setq minpoint p1)(setq maxpoint p2)
  ;;布局空间采用不同的方法
  (if (= (getvar "ctab") "Model")
      (command "-plot" "y" "" (@:get-config 'base:plotter-pdf) tufu "M"
	       zongheng "n" "w" minpoint maxpoint "f" "c" "y" (@:get-config 'base:ctb) "y" "a"
	       file-name "n" "y" )
      (command "-plot" "y" "" (@:get-config 'base:plotter-pdf) tufu "M"
	       zongheng "n" "w" minpoint maxpoint "f" "c" "y" (@:get-config 'base:ctb) "y" "n" "n" "n"
	       file-name "n" "y" )

      )
  (pop-var))

(defun plot:to-dev ( tufu zongheng p1 p2 
		    / minpoint maxpoint oldosmode )
  "打印到设备。参数： 图幅 纵横 第一点 第二点 输出文件名。"
  (push-var)
  (setvar "filedia" 0)
  ;;(command "ucs" "w")
  (setvar "osmode" 0)
  (setq minpoint (list (min (nth 0 p1) (nth 0 p2))
		       (1- (min (nth 1 p1) (nth 1 p2)))
		       0))
  
  (setq maxpoint (list (1+ (max (nth 0 p1) (nth 0 p2)))
		       (max (nth 1 p1) (nth 1 p2))
		       0))
  ;;(setq minpoint p1)(setq maxpoint p2)
  (command "-plot" "y" "" (@:get-config 'base:plotter) tufu "M"
	   zongheng "n" "w" minpoint maxpoint "f" "c" "y" (@:get-config 'base:ctb) "y" "a"
	   "n" "n" "y" )
  (pop-var)
  )
(defun plot:Plot->File (ConfigName MediaName Window FileName  /
				   AcadObj ActiveDocObj  ActiveLayoutObj PlotObj ret)

  ;; ConfigName -- 配置文件名
  ;; MediaName -- 图纸尺寸
  ;; Window -- 表（窗口左下点 右上点)
  ;; FileName -- 输出文件名
  (setq        AcadObj                (vlax-get-acad-object)
               ActiveDocObj        (vla-get-ActiveDocument AcadObj)
               ActiveLayoutObj        (vla-get-ActiveLayout ActiveDocObj))
  (vla-RefreshPlotDeviceInfo ActiveLayoutObj)
  (vla-Put-ConfigName ActiveLayoutObj ConfigName)
  (setq PlotObj (vla-get-Plot ActiveDocObj))
  (vla-Put-CanonicalMediaName ActiveLayoutObj MediaName)
  (vla-Put-StandardScale ActiveLayoutObj acScaleToFit)
  (vla-Put-PlotRotation ActiveLayoutObj ac0degrees)
  (vla-put-PlotOrigin ActiveLayoutObj
		      (vlax-make-variant
		       (vlax-make-safearray vlax-vbDouble '(0 . 1))))
  (vla-Put-CenterPlot ActiveLayoutObj :vlax-false)
  (vla-SetWindowToPlot
   ActiveLayoutObj
   (vlax-make-variant
    (vlax-safearray-fill
     (vlax-make-safearray vlax-vbDouble '(0 . 1))
     (car Window)))
   (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble '(0 . 1)) (cadr Window))))
  (setq ret nil)
  (vla-Put-PlotType ActiveLayoutObj acWindow)
  (and (xd::apply vla-PlotToFile (list PlotObj FileName)) (setq ret filename))
  (foreach o (list PlotObj ActiveLayoutObj ActiveDocObj AcadObj)
	   (vlax-release-object o)  )
  ret)
