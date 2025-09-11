(defun psk-draw-text (p text a / h)
  (setq a (p-angle-normal a))
  (if (and (> a 1.65806)
	   (< a 4.79966)
      )
    ;; 文字旋转超过95度但小于275度时要求将文字旋转180度以防止文字倒置
    (setq a (+ pi a))
  )
  (setq h (* $psk-iden-textheight $psk-iden-scale))
  (p-make-text text p "M" h $psk-iden-widfactor a)
)


(defun psk-comp-getidentempl (comp / str tmpl)
  (setq str (p-get comp ".TYPE"))
  (cond
    ((= "DUCT-ROUND" str)
     (setq tmpl $psk-templ-ductround)
    )
    ((= "DUCT-RECT" str)
     (setq tmpl $psk-templ-duct)
    )
    ((= "PIPE" str)
     (setq tmpl $psk-templ-pipe)
    )
    ((= "REFPIPE" str)
     (setq tmpl "{REFS}")
    )
    ((= "REFPIPE" str)
     (setq tmpl "{REFS}")
    )

  )
  tmpl
)

(defun psk-translate-serv (comp)
  (if (wcmatch (psk-comp-gettype comp) "DUCT*")
    (p-set
      comp
      (cons "SERV"
	    (cadr (p-get $psk-services-duct (p-get comp "SERV")))
      )
    )
    (p-set
      comp
      (cons "SERV"
	    (cadr (p-get $psk-services (p-get comp "SERV")))
      )
    )
  )
)
;; (psk-comp-iden (psk-comp-load (car (entsel))))
(defun psk-comp-iden (en / p ps str tmpl)
  (setq comp (psk-comp-load en))
  ;; 采用entmake方式创建图层后首次执行显示颜色不正常或不显示的问题解决
  (psk-set-customlayerbyid
    (strcat (p-get comp "SERV") "-IDEN")
  )

  (setq tmpl (psk-comp-getidentempl comp))

  (setq comp (psk-translate-serv comp))


  (if (setq str (p-template-eval tmpl comp))
    (progn
      (setq ps (mapcar 'cadr (psk-path-getports comp))
	    p  (p-mid (car ps) (cadr ps))
      )

      (if (> (distance (car ps) (cadr ps)) $psk-iden-minlength)
	(progn
	  (setq en (psk-draw-text p str (angle (car ps) (cadr ps))))
;;;	  (if (null (setq uid
;;;			   (cdar
;;;			     (p-xdata-get (p-get comp -1) "PSK-DRAWID")
;;;			   )
;;;		    )
;;;	      )
;;;	    (progn
;;;	      (setq uid (p-uid))
;;;	      (p-xdata-set
;;;		(p-get comp -1)
;;;		"PSK-DRAWID"
;;;		(list (cons 1071 uid))
;;;	      )
;;;	    )
;;;	  )
	  (p-xdata-set en "PSK-IDEN" (list (cons 1071 0)))
	)
      )
    )
  )
)
(defun psk-cmd-iden (/ comp comps done r skip)
  (princ "\n选择需要标注的管线:")

  (setq	skip 0
	done 0
  )

  (foreach e (p-ss->enames
	       (ssget '((0 . "LINE,TEXT,INSERT") (-3 ("PSK-PATH,PSK-IDEN,PSK-EQUIP"))))
	     )
    (if	(= "TEXT" (p-dxf e 0))
      (entdel e)
      (setq comps (cons e comps))
    )
  )

  (foreach comp	comps
    (if (= "INSERT" (p-dxf comp 0))
      (setq r (psk-comp-idenequip comp))
      (setq r (psk-comp-iden comp))
    )
    
    (if	r
      (setq done (1+ done))
      (setq skip (1+ skip))
    )
  )

  (princ (strcat "标注了 " (itoa done) " 个对象"))
  (if (> skip 0)
    (princ
      (strcat ", "
	      (itoa skip)
	      " 个因长度过小忽略标注 ($psk-iden-minlength = "
	      (rtos $psk-iden-minlength 2 0)
	      ")"
      )
    )
  )
)

(defun psk-comp-idenequip (en / str tmpl)
;;;  (princ "\n选择需要标注的设备:")
;;;
;;;  (foreach e (p-ss->enames
;;;	       (ssget '((0 . "INSERT") (-3 ("PSK-EQUIP"))))
;;;	     )
  (psk-set-customlayerbyid "EQUIP-IDEN")

  (setq tmpl "{NAME}")

  (if (setq str (p-template-eval tmpl (psk-comp-load en)))
    (setq en (psk-draw-text (p-dxf en 10) str (p-dxf en 50)))
  )

  (p-xdata-set en "PSK-IDEN" (list (cons 1071 0)))
)
;;

(defun psk-pipegroup-iden (/ a en h p1 p2 p3 pd paths str tlen)
  (while (and
	   (setq p1 (getpoint "\n指定起点: "))
	   (setq p2 (getpoint p1 "\n指定终点: "))
	   (setq paths
		  (p-ss->enames
		    (ssget "F" (list p1 p2) '((0 . "LINE") (-3 ("PSK-PATH"))))
		  )
	   )
	 )
    (progn
      (setq p1 (trans p1 1 0))
      ;; 2022-1-19
      (setq p2 (trans p2 1 0))
      (setq pd p1)
      (setq paths
		  (mapcar
		    (function
		      (lambda (e)
			(list
			  (distance p1
				    (inters p1 p2 (p-dxf e 10) (p-dxf e 11))
			  )
			  e
			)
		      )
		    )
		    paths
		  )
	    paths (vl-sort paths
			   (function (lambda (e1 e2)
				       (< (car e1) (car e2))
				     )
			   )
		  )
	    paths (mapcar (function cadr) paths)
      )

      ;; 引线与标注直线的第一个交点
      (setq p1 (p-line-closestpoint (car paths) p2 nil))
      (setq a (p-angle-normal (angle p1 p2)))
      ;; 引线与标注直线的最后一个交点
      (setq p2 (p-line-closestpoint (last paths) p2 nil))
      ;; 计算引线终点
      (setq p3 (polar p2 a $psk-iden-offset))
      ;; 每行标注总高（含文字底线）
      (setq h (* (+ 1. $psk-iden-textheight) $psk-iden-scale))

      (setq tlen  nil
	    elast (entlast)
      )
      ;; 生成所有文字内容
      (foreach path paths
	(setq path (psk-comp-load path))
	(psk-set-customlayerbyid
	  (strcat (p-get path "SERV") "-IDEN")
	)

	(setq tmpl (psk-comp-getidentempl path))
	(setq path (psk-translate-serv path))

	(setq str  (p-template-eval tmpl path)
	      en   (psk-draw-text p3 str (- a $pi/2))
	      tlen (cons (caadr (textbox (entget en))) tlen)
	      p3   (polar p3 a h)
	)
      )

      ;; 文字底线长度
      (setq tlen (apply (function max) tlen)
	    tlen (+ 200 (p-round tlen 100))
      )
      (if (and (> a 0.0872665)
	       (< a 3.22886)
	  )
	(setq p3 (polar p2 a (- $psk-iden-offset (* h 1.5))))
	(setq p3 (polar p2 a (- $psk-iden-offset (* h 0.5))))
      )
      ;; 所有文字底线
      (foreach path paths
	(setq p3 (polar p3 a h))
	(p-make-line
	  (polar p3 (- a $pi/2) (* tlen -0.5))
	  (polar p3 (- a $pi/2) (* tlen 0.5))
	)
      )
      (setq p1 (polar p3 (- a $pi/2) (* tlen -0.5)))
      (p-make-line p1 (p-line-closestpoint (car paths) p1 t))

      ;; 提示移动标注
;;;      (setq ss (p-enames-after elast nil))
;;;      (command "_.MOVE" ss "" pd (getdist pd))
    )
  )
)
;;
;;;(defun psk-pipegroup-iden (/ a en h p1 p2 p3 paths str tlen)
;;;  (while (and
;;;	   (setq p1 (getpoint "\n指定起点: "))
;;;	   (setq p2 (getpoint p1 "\n指定终点: "))
;;;	   (setq p3 (getpoint p2 "\n指定文字位置: "))
;;;	   (setq paths
;;;		  (p-ss->enames
;;;		    (ssget "F" (list p1 p2) '((0 . "LINE") (-3 ("PSK-PATH"))))
;;;		  )
;;;	   )
;;;	 )
;;;    (progn
;;;      ;; 确定标注顺序
;;;      (setq paths
;;;		  (mapcar
;;;		    (function
;;;		      (lambda (e)
;;;			(list
;;;			  (distance p1
;;;				    (inters p1 p2 (p-dxf e 10) (p-dxf e 11))
;;;			  )
;;;			  e
;;;			)
;;;		      )
;;;		    )
;;;		    paths
;;;		  )
;;;	    paths (vl-sort paths
;;;			   (function (lambda (e1 e2)
;;;				       (< (car e1) (car e2))
;;;				     )
;;;			   )
;;;		  )
;;;	    paths (mapcar (function cadr) paths)
;;;      )
;;;
;;;      ;; 引线与标注直线的第一个交点
;;;      (setq p1 (p-line-closestpoint (car paths) p1 nil))
;;;      ;; 引线与标注直线的最后一个交点
;;;      (setq p12 (p-line-closestpoint (last paths) p1 nil))
;;;      ;; 计算引线终点
;;;      (setq a (angle p1 p12))
;;;      (setq p3 (polar p12 a $psk-iden-offset)
;;;;;;	    p3 (polar p3 (- a $pi/2) (* tlen -0.5))
;;;      )
;;;      ;; 每行标注总高（含文字底线）
;;;      (setq h (* (+ 1. $psk-iden-textheight) $psk-iden-scale))
;;;
;;;      (setq tlen nil)
;;;      ;; 所有文字内容
;;;      (foreach path paths
;;;	(setq path (psk-comp-load path))
;;;	(psk-set-customlayerbyid
;;;	  (strcat (p-get path "SERV") "-IDEN")
;;;	)
;;;
;;;	(setq tmpl (psk-comp-getidentempl path))
;;;
;;;	(setq str  (p-template-eval tmpl path)
;;;	      en   (psk-draw-text p3 str (- a $pi/2))
;;;	      tlen (cons (caadr (textbox (entget en))) tlen)
;;;	      p3   (polar p3 a h)
;;;	)
;;;      )
;;;
;;;      ;; 文字底线长度
;;;      (setq tlen (apply (function max) tlen)
;;;	    tlen (+ 200 (p-round tlen 100))
;;;      )
;;;      (if (and (>= a (+ 0.15806 pi))
;;;	       (< a $2pi)
;;;	  )
;;;	(setq p3 (polar p2 a (- $psk-iden-offset (* h 0.5))))
;;;	(setq p3 (polar p2 a (- $psk-iden-offset (* h 1.5))))
;;;      )
;;;      ;; 所有文字底线
;;;      (foreach path paths
;;;	(setq p3 (polar p3 a h))
;;;	(p-make-line
;;;	  (polar p3 (- a $pi/2) (* tlen -0.5))
;;;	  (polar p3 (- a $pi/2) (* tlen 0.5))
;;;	)
;;;      )
;;;      (setq p1 (polar p3 (- a $pi/2) (* tlen -0.5)))
;;;      (p-make-line p1 (p-line-closestpoint (car paths) p1 t))
;;;    )
;;;  )
;;;)