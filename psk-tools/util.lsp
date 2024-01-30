;; PSK扩展工具

;; 清除对象上的所有扩展数据
(defun xdata-clear (/ ss)
  (princ "\n选择要清除扩展数据的对象:")
  (if (setq ss (ssget))
    (progn
      (foreach en (p-ss->enames ss)
	(p-xdata-remove en "*")
      )
    )
  )
)
;; 匹配扩展数据
(defun xdata-match (/ en ss xdata)
  (if (and (setq en (car (entsel "\n选择源对象:")))
	   (princ "\n选择要匹配扩展数据的对象:")
	   (setq ss (ssget))
      )
    (progn
      (setq xdata (p-xdata-get-inner en "*"))
      (foreach en (p-ss->enames ss)
	(p-xdata-set-inner en xdata)
      )
    )
  )
)
;; 删除APPID为"PSK-PATH"中的指定属性"FRIC"
(defun prop-clear (/ ss)
  (princ "\n选择要删除属性的对象:")
  (if (setq ss (ssget))
    (progn
      (foreach en (p-ss->enames ss)
	(p-xprop-remove en "PSK-PATH" "FRIC")
      )
    )
  )
)
;; 将普通直线转换为"PSK-PATH"对象
(defun convert-path (/ ss)
  (princ "\n选择要转换为路径的对象:")
  (if (setq ss (ssget '((0 . "LINE"))))
    (progn
      (foreach en (p-ss->enames ss)
	(p-xprop-set
	  en
	  "PSK-PATH"
	  '(
	    (".TYPE" . "PIPE")
	   )
	)
      )
    )
  )
)

(defun exportprop ()
  (setq file (open "C:/1.csv" "w"))
  (foreach e $psk-prop-defination
    (write-line
      (strcat (car e) "," (itoa (cadr e)) "," (caddr e) "," (cadddr e))
      file
    )
  )
  (close file)
)
;;

;; 批量设置对象的属性
;;;(setprop '(lambda (en) (p-xprop-set en "PSK-PART" '("FLR" . 500))))
;;;(setprop '(lambda (en) (p-xprop-set en "PSK-EQUIP" '("CLD" . 4.3))))
(defun setprop (fun)
  (if (setq ss (ssget))
    (progn
      (foreach en (p-ss->enames ss)
	(apply fun (list en))
      )
    )
  )
)
;; 批量转换介质（用于版本 0.60前的旧图转换）
(defun convertserv ()
  (if (setq ss (ssget))
    (progn
      (foreach en (p-ss->enames ss)
	(if (setq serv (p-get '(("S" . "SA")
				("H" . "RA")
				("X" . "OA")
				("P" . "EA")
				("RS" . "RS")
				("RP" . "RP")
				("PY" . "SE")
				("JY" . "PS")
				("XB" . "MA")
				("P(Y)" . "EA(SE)")
				("S(B)" . "SA(MA)")
			       )
			      (psk-comp-get en "SERV")
		       )
	    )
	  (psk-comp-set en (cons "SERV" serv))
	)
      )
    )
  )
)

;; 汇总冷负荷
(defun psk-cld-total (/ tot)
  (setq tot 0.)
  (if (setq ss (ssget))
    (progn
      (foreach en (p-ss->enames ss)
	(setq tot (+ tot (p-xprop-get en "PSK-EQUIP" "CLD")))
      )
    )
  )
  (princ tot)
)




;; 管线辅助工具 2021-4-1
(defun p-groupby (ents sortby / e lst p r)
  (setq	ents (mapcar (function (lambda (e) (list (p-dxf e sortby) e)))
		     ents
	     )
	ents (vl-sort
	       ents
	       (function
		 (lambda (e1 e2) (< (car e1) (car e2)))
	       )
	     )
  )
  ;;  分组
  (while ents
    (setq e (car ents)
	  p (car e)
    )
    (while (equal (car e) p)
      (setq lst	 (cons (cadr e) lst) ;_ 将相同项合并
	    ents (cdr ents)
	    e	 (car ents)
      )
    )
    (setq r   (cons (cons p lst) r)
	  lst nil
    )
  )
  r
)
;; 两条直线连接打断
;;;(mt-line-interbreak (car (entsel)) (car (entsel)))
(defun mt-line-interbreak (line1 line2 / p)
  (if (and (not (equal line1 line2))
	   (setq p (p-line-getinters line1 line2))
      )
    (progn
      (psk-line-breakat line1 p)
      (psk-line-breakat line2 p)
    )
  )
)
;; 选定一组直线，按图层分组，如果每个图层中的直线是两条，对该两直线进行连接打断操作
(defun mt-lines-interbreak (/ ents)
  (princ "\n选择要连接打断的直线:")
  (if (setq ents (ssget))
    (progn
      (setq ents (p-ss->enames ents)
	    ents (p-groupby ents 8)
      )

      (foreach lines ents
	(setq lines (cdr lines))
	(if (= (length lines) 2)
	  (progn
	    (mt-line-interbreak (car lines) (cadr lines))
	  )
	)
      )
    )
  )
)

;;;(defun c:p2 ()
;;;  (setq p1 (getpoint "\n1:"))
;;;  (setq p2 (getpoint "\n2:"))
;;;  (p-clipboard-set
;;;    (vl-prin1-to-string
;;;      (list (vl-filename-base (getvar 'dwgname))
;;;	    (list "REF" p1 '(1 0 0) 0)
;;;	    (list "CD" p2 '(1 0 0) 0)
;;;      )
;;;    )
;;;  )
;;;)


(defun c:bb (/)
  (p-commandrun '(mt-lines-interbreak))
)
(defun c:xclear	(/)
  (p-commandrun '(xdata-clear))
)
(defun c:xma (/)
  (p-commandrun '(xdata-match))
)
(defun c:convertpath (/)
  (p-commandrun '(convert-path))
)

(defun c:cldtot (/)
  (p-commandrun '(psk-cld-total))
)

;; 测试进度提示
;;;(defun run (/ r)
;;;  (pdb-list-clear "LIST")
;;;  (setq r 0)
;;;  (repeat 300000
;;;    (setq r (1+ r))
;;;
;;;    (if	(= 0 (rem r 10000))
;;;      (progn
;;;	(pdb-list-append "LIST" (itoa r))
;;;	(pdb-list-select "LIST" (1- (/ r 10000)))
;;;      )
;;;    )
;;;  )
;;;)
;;;
;;;(defun progress	(/ id r)
;;;  (if (and (>= (setq id (load_dialog (psk-get-filename "\\dialogs.dcl"))) 0)
;;;	   (new_dialog "progressDlg" id)
;;;      )
;;;    (progn
;;;      (action_tile "OK" "(run)")
;;;
;;;      (start_dialog)
;;;      (done_dialog 1)
;;;      (unload_dialog id)
;;;    )
;;;  )
;;;)

;;;(defun psk-create-ductvert (name p1 w h /)
;;;  (command
;;;    "._insert"
;;;    (psk-get-filename (strcat "\\dwg\\" name ".dwg"))
;;;    "NON"
;;;    p1
;;;    w
;;;    h
;;;    0.
;;;  )
;;;)
;;;(defun sav (name / h p1 w)
;;;  (if (and (setq p1 (getpoint "\n指定插入点:"))
;;;           (setq w (p-edit-value "输入宽度" 1600.))
;;;           (setq h (p-edit-value "输入高度" 400.))
;;;      )
;;;    (psk-create-ductvert name p1 w h)
;;;  )
;;;)
;;;(defun c:sav (/ h p1 w)
;;;  (sav "SAD-V")
;;;)
;;;(defun c:rav (/ h p1 w)
;;;  (sav "RAD-V")
;;;)


;; (psk-csvfile-get "sizes/flange pn.csv" "FL-PL-2.5-15")
;; (("Name" . "FL-PL-2.5-15") ("OD" . 80.0) ("BLTD" . 55.0) ("BLTH" . 11.0) ("BLTN" . "M10") ("BLTA" . 4.0) ("L" . 12.0))
(defun psk-csvfile-get (filename key / csv)
  (if (setq csv (p-csvfile-readcache (psk-get-filename filename) '$psk-csvread-cache))
;;;    (progn
;;;      (setq prop (p-csvread-get1 csv key))
;;;      (p-get1 prop names)
;;;    )
    (p-csvread-get1 csv key)
  )
)

;;(vlax-ldata-put "PSK" "CREATEVALUELAST" nil)


;; 关闭风管相关图层
(defun psk-ductlayeroff	()
  (command "._-LAYER" "OFF" "M-*风*,M-*烟*" "")
)
;; 打开风管相关图层
(defun psk-ductlayeron	()
  (command "._-LAYER" "ON" "M-*风*,M-*烟*" "")
)