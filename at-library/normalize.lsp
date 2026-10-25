;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 图库文件批量归一化（开发/维护用）
;;
;; 解决三个实测问题（见 ~/www/dw/library/index/library-report.md）：
;;   1. INSBASE 与几何中心偏离 → 插入时符号跑到屏幕外；
;;   2. $INSUNITS=0 → 客户端无法换算真实尺寸；
;;   3. 无用图层/空块堆积 → 文件偏大。
;;
;; 用 ObjectDBX 直接改文件，不打开图纸窗口；**输出到另一个目录**，原图不动，
;; 人工核对后再整体替换。属于维护者批量工具，不进日常命令。
;;
;; 注意：这里不复用 lib 的 dbx:interface —— 它依赖 @:acadver，而 @:acadver 在
;; 本仓库里并没有定义（见 reports/code-review-libsrc-20261021.md）。直接读
;; (getvar "ACADVER") 更稳。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun at-library:--acadver (/ v)
  (setq v (getvar "ACADVER"))
  (atof v))

(defun at-library:--dbx-open (file / ver dbx)
  "用 ObjectDBX 打开 dwg（不显示窗口）。失败返回 nil。"
  (setq ver (fix (at-library:--acadver)))
  (setq dbx (vl-catch-all-apply
	     'vla-getinterfaceobject
	     (list *ACAD* (strcat "ObjectDBX.AxDbDocument." (itoa ver)))))
  (if (or (null dbx) (vl-catch-all-error-p dbx))
      nil
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx (findfile file))))
	nil
      dbx)))

(defun at-library:--dbx-close (dbx)
  (if (and dbx (not (vlax-object-released-p dbx)))
      (vl-catch-all-apply 'vlax-release-object (list dbx)))
  (setq *DBX* nil))

(defun at-library:--dbx-bbox (ms / mn mx p1 p2 x0 y0 x1 y1 any)
  "遍历模型空间求包围盒，返回 (x0 y0 x1 y1)；没有几何返回 nil。"
  (vlax-for obj ms
    (if (not (vl-catch-all-error-p
	      (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
	(progn
	  (setq p1 (vl-catch-all-apply 'vlax-safearray->list (list mn))
		p2 (vl-catch-all-apply 'vlax-safearray->list (list mx)))
	  (if (and (not (vl-catch-all-error-p p1))
		   (not (vl-catch-all-error-p p2))
		   (numberp (car p1)) (numberp (car p2)))
	      (progn
		(setq x0 (if any (min x0 (car p1)) (car p1))
		      y0 (if any (min y0 (cadr p1)) (cadr p1))
		      x1 (if any (max x1 (car p2)) (car p2))
		      y1 (if any (max y1 (cadr p2)) (cadr p2))
		      any t))))))
  (if any (list x0 y0 x1 y1) nil))

(defun at-library:normalize-file (dwg outdir units / newfile dbx ms box cx cy w h)
  "把一个 dwg 归一化后写到 outdir：基点回到几何中心、补齐单位、清理无用数据。

参数：
  dwg    源文件
  outdir 输出目录（不存在会创建）
  units  $INSUNITS 值，nil 表示按 4(mm) 处理
返回：新文件路径；失败返回 nil
示例：(at-library:normalize-file \"C:/lib/a.dwg\" \"C:/lib/normalized\" 4)"
  (cond
   ((not (findfile dwg))
    (@::@log "WARN" (strcat "文件不存在: " dwg)) nil)
   (t
    (if (not (findfile outdir))
	(@::mkdir (if (listp outdir) outdir (@::path (strcat @::path-os outdir "/")))))
    (setq newfile (strcat (@::path-os (vl-string-right-trim "\\/" outdir)) "/"
			  (vl-filename-base dwg) ".dwg"))
    (if (findfile newfile) (vl-file-delete newfile))
    (if (null (vl-file-copy dwg newfile))
	(progn (@::@log "ERROR" (strcat "复制失败: " dwg)) nil)
      (progn
      (setq dbx (at-library:--dbx-open newfile))
      (cond
       ((null dbx)
	(@::@log "WARN" (strcat "ObjectDBX 打不开: " newfile))
	(vl-file-delete newfile)
	nil)
       (t
	(setq ms (vla-get-modelspace dbx))
	(setq box (at-library:--dbx-bbox ms))
	(if box
	    (progn
	      (setq cx (/ (+ (car box) (caddr box)) 2.0)
		    cy (/ (+ (cadr box) (cadddr box)) 2.0)
		    w (- (caddr box) (car box))
		    h (- (cadddr box) (cadr box)))
	      ;; 偏差超过符号尺寸的 5% 才改基点，避免动本来正确的文件
	      (if (> (sqrt (+ (* cx cx) (* cy cy)))
		     (* 0.05 (max w h 1e-6)))
		  (vl-catch-all-apply
		   'vla-SetVariable
		   (list dbx "INSBASE" (vlax-3d-point cx cy 0.0))))))
	(vl-catch-all-apply 'vla-SetVariable
			    (list dbx "INSUNITS" (if units units 4)))
	(vl-catch-all-apply 'vla-PurgeAll (list dbx))
	(vl-catch-all-apply 'vla-Save (list dbx))
	(at-library:--dbx-close dbx)
	newfile)))))))

(defun at-library:normalize-dir (src outdir units / files n ok)
  "批量归一化一个目录下的所有 dwg（不递归）。

参数：src 源目录；outdir 输出目录；units 目标 INSUNITS（nil=4）
返回：成功处理的文件数
示例：(at-library:normalize-dir \"C:/lib/乔木\" \"C:/lib/normalized\" 4)"
  (setq files (vl-directory-files src "*.dwg" 1))
  (setq n 0 ok 0)
  (foreach f files
    (setq n (1+ n))
    (if (at-library:normalize-file (strcat (@::path-os src) "/" f) outdir units)
	(setq ok (1+ ok)))
    (if (= 0 (rem n 20))
	(princ (strcat "\r归一化 " (itoa n) "/" (itoa (length files))))))
  (princ (strcat "\n完成：" (itoa ok) "/" (itoa (length files)) " 个文件 → "
		 (@::path-os outdir)))
  ok)
