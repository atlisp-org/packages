;; 图框标准表（短边高度 / 图幅名 / 加长宽高比 / 加长标记）**唯一来源是 lib 的 frame: 命名空间**
;; （lib/src/frame/，见 frame:heights/names/ratios/elongations）。保留 @plot: 变量名只是为了兼容
;; 既有消费者（train.lsp 用到 @plot:height-of-frame 与 @plot:frame-type）。
;; 不要再在本包内写一份字面量表——lib / 本包 / mcp-server 曾各写一份，改一处必漏两处。
(require 'frame:*)
(setq @plot:ratio-of-w/h (frame:ratios))
(setq @plot:frame-type2 (frame:elongations))
(setq @plot:height-of-frame (frame:heights))
(setq @plot:frame-type (frame:names))
;; 基准快照：@plot:load-config 每次从基准重建，避免反复 append 让列表无限增长
(setq @plot:*base-ratio* @plot:ratio-of-w/h)

(setq @plot:*frames* '()) ; 图框识别结果数据
(@:add-menu "通用打印" "配置" "(@plot:setup)")
(defun @plot:setup (/ res)
  "通用打印基本信息"
  (setq res 
	(ui:input "配置信息"
		  (mapcar '(lambda (x) (list (strcase (vl-symbol-name (car x)) T)(cadr x)(cddr x)))
			  (vl-remove-if '(lambda (x) (not (wcmatch (vl-symbol-name (car x)) "`@PLOT:*")));;大写
					(if @:*config.db*
					    @:*config.db* (@:load-config))))))
  (foreach res% res
   	   (@:set-config (read (car res%)) (cdr res%)))
  ;; 配置变了 → 表缓存失效，下次调用重新加载
  (setq @plot:*tables-loaded* nil)
  )

(@:add-menu "通用打印" "AI识别图框" "(@plot:frame-recognition)")
(defun @plot:pts->box (pts / xs ys)
  "四点表 → 规范化外框 (xmin ymin xmax ymax)"
  (setq xs (mapcar 'car pts) ys (mapcar 'cadr pts))
  (list (apply 'min xs) (apply 'min ys) (apply 'max xs) (apply 'max ys)))
(defun @plot:frames-dedupe (/ boxes kept before)
  "去掉被其它图框包含的内框（外框优先），返回去除条数。
一个图框通常有内外两道矩形，两者都满足「4 点闭合」→ 旧实现会当成两张图框。
判定下沉到 lib 的 frame:bbox-dedupe（与 mcp-server 侧同一套语义）。"
  (setq before (length @plot:*frames*))
  (setq boxes (mapcar '(lambda (x) (@plot:pts->box (car x))) @plot:*frames*))
  (setq kept (frame:bbox-dedupe boxes))
  (setq @plot:*frames*
	(vl-remove-if-not
	 '(lambda (x)
	    (vl-some '(lambda (k) (equal (@plot:pts->box (car x)) k 1e-6)) kept))
	 @plot:*frames*))
  (- before (length @plot:*frames*)))
(defun @plot:frame-recognition ()
  (@plot:delete-mark)
  (@plot:frame-recognition-by-polyline)
  ;; 块图框：图框画成属性块/普通块的情况（旧版此模式只选中不记录）
  (@plot:frame-recognition-by-block)
  ;; 直线模式默认不开（线数量大时噪声多、也最慢），需要时用菜单「识别直线框」
  ;; (@plot:frame-recognition-by-line)
  (setq deduped (@plot:frames-dedupe))
  (@plot:mark-frames)
  (@:prompt (strcat "AI 共识别了 "
		 (itoa (length @plot:*frames*))
		 " 个图框"
		 (if (> deduped 0)
		     (strcat "（已去除 " (itoa deduped) " 个被包含的内框）")
		   "")
		 "。\n"))
  (princ)
  )
(defun @plot:get-frame-type (w h)
  "识别图框大小"
  )
(@:add-menu "通用打印" "识别PL图框" "(@plot:frame-recognition-by-polyline)")
;; 
(defun @:get-rec-points (en0 / ddlist dd1 tmplist )
  "生成多段线的点序"
  (setq ddlist nil) 
  (setq tmplist (entget en0))
  (repeat 
   (cdr (assoc 90 (entget en0))) ;;计算节点数
   (setq dd1 (cdr (assoc 10 tmplist))) ;;取顶点数据
   (setq tmplist (member (assoc 10 tmplist) tmplist))
   (setq tmplist (cdr tmplist))
   (setq ddlist (append ddlist (list dd1) )) ;;下一个顶点
   )
  )
(defun @plot:init ()
  "图框识别初始化"
  (if (tblsearch "layer" "temp-frames")
      T
    (entity:make-layer "temp-frames" 3 "DASHED" nil))
  (layer:plotable '("temp-frames") nil)
  )
(defun @plot:get-height (frame-pts)
  "获取图框高度"
  (- (cadr (cadr frame-pts))(cadr (car frame-pts))))
(defun @plot:get-width (frame-pts)
  "获取图框宽度"
  (- (car (caddr frame-pts))(car (car frame-pts))))
(defun @plot:get-ratio (w h)
  "获取图框宽高比"
  (setq w (float w))
  (setq h (float h))
  (if (/= 0 (min w h))
      (/ (max w h)(min w h))))
;; 表加载缓存（性能关键）
;; 直线识别对**每条 LINE** 调用一次 @plot:frame-length?，旧实现每次都在里面读配置 +
;; @plot:load-config（= 两次文件读取）。本图 8419 条 LINE → 1.6 万次文件 I/O，
;; 实测这就是「识别直线框」慢的主因。配置与 frames-*.db 在会话内不会变，故首次读取后缓存；
;; 改过配置/DB 后调用 (@plot:reload-config) 刷新。
(setq @plot:*tables-loaded* nil)
(setq @plot:*load-config-count* 0)	;诊断：实际读文件的次数（正常应为 1）

(defun @plot:reload-config ()
  "强制重新读取配置与 frames-scale.db / frames-ratio.db（改过配置或 DB 后调用）"
  (setq @plot:*tables-loaded* nil)
  (@plot:ensure-tables)
  (princ))

(defun @plot:load-config ()
  "加载本地配置（缓存版，等价于 (@plot:ensure-tables)）"
  (@plot:ensure-tables))

(defun @plot:ensure-tables ()
  "确保图幅表就绪：配置里的比例 + frames-scale.db + frames-ratio.db（只读一次文件）"
  (if (not @plot:*tables-loaded*)
      (progn
	(setq @plot:*load-config-count* (1+ @plot:*load-config-count*))
	;; 1) 配置项里的比例表
	(setq @plot:scale-of-frame
	      (mapcar 'atof
		      (string:parse-by-lst (@:get-config '@plot:scale-of-frame) '("," " "))))
	;; 2) frames-scale.db 追加比例
	(if (findfile (strcat @:*prefix-config* "frames-scale.db"))
	    (setq @plot:scale-of-frame
		  (append @plot:scale-of-frame
			  (vl-remove
			   nil
			   (mapcar 'read
				   (string:to-lst (@:get-file-contents (strcat @:*prefix-config* "frames-scale.db")) "\n"))))))
	;; 3) 宽高比表从基准重建，再合并 frames-ratio.db（避免反复 append 膨胀）
	(setq @plot:ratio-of-w/h @plot:*base-ratio*)
	(if (findfile (strcat @:*prefix-config* "frames-ratio.db"))
	    (setq @plot:ratio-of-w/h
		  (append @plot:ratio-of-w/h
			  (vl-remove nil
				     (mapcar 'read
					     (string:to-lst (@:get-file-contents (strcat @:*prefix-config* "frames-ratio.db")) "\n"))))))
	(setq @plot:*tables-loaded* T)))
  @plot:*tables-loaded*)
(defun @plot:frame-length? (len / i j k scale ratio flag-hit)
  "测试长度是否符合图框要求"
  ;; 逐线调用：表必须走缓存，不能在这里读配置/文件（见 @plot:ensure-tables 注释）
  (@plot:ensure-tables)
  (setq flag-hit nil)
  (setq i 0)
  ;;最小最大判断
  (if (< (* (apply 'min @plot:height-of-frame)
	    (apply 'min @plot:scale-of-frame))
	 len 
	 (* (apply 'max @plot:height-of-frame)
	    (apply 'max @plot:scale-of-frame)
	    (apply 'max @plot:ratio-of-w/h)))
      (while (and (null flag-hit)
		  (< i (length @plot:height-of-frame)))
	(setq j 0)
	(while (and (null flag-hit)
		    (< j (length @plot:scale-of-frame)))
	  (if (equal len
		     (* (nth i @plot:height-of-frame)
			(nth j @plot:scale-of-frame))
		     (* 0.001 len))
	      (setq flag-hit T)    
	    (progn 
	      (setq k 0) 
	      (while (and (null flag-hit)
			  (< k (length @plot:ratio-of-w/h)))
		(if (equal len
			   (* (nth i @plot:height-of-frame)
			      (nth j @plot:scale-of-frame)
			      (nth k @plot:ratio-of-w/h))
			   (* 0.0019 len))
		    (setq flag-hit T))
		
	    (setq k (1+ k))
	    )))
	  (setq j (1+ j)))
	(setq i (1+ i))))
  flag-hit)
;;(princ "test\n")
(defun @plot:framep (frame-pts / test-ratio height flag-hit i j)
  "测试是否多段线图框，依据矩形高度和宽高比，"
  (if (>= (length frame-pts) 4)
      (progn
	(@plot:ensure-tables)
	(setq frame-pts
	      (vl-sort
	       frame-pts
	       '(lambda (pt1 pt2)
		 (or (< (car pt1)(car pt2))
		  (and (equal (car pt1)(car pt2) 0.001)
		   (< (cadr pt1)(cadr pt2)))))))
	(setq flag-hit nil)
	(setq test-ratio nil)
	;;两低点 1 3 的 y 值是否一致
	(if (equal (car (car frame-pts))
	     (car (cadr frame-pts))
	     0.001)
	    (progn
	      (setq height (min (@plot:get-height frame-pts)
				(@plot:get-width frame-pts)))
	      ;; 测试长宽比
	      (setq i 0)
	      (while (and (null test-ratio)
			  (< i (length @plot:ratio-of-w/h)))
		(if (equal (@plot:get-ratio (@plot:get-height frame-pts)
					    (@plot:get-width frame-pts))
			   (nth i @plot:ratio-of-w/h)
			   0.0035)
		    (setq test-ratio T))
		(setq i (1+ i)))))
	;; 测试高度
	(if test-ratio
      (progn
	(setq flag-hit nil)
	(foreach
	 h
	 @plot:height-of-frame
	 (foreach
	  scale @plot:scale-of-frame
	  (if (equal height
		     (* h scale)
		     (* 0.001 height))
	      (setq flag-hit T))
	  ))))
	flag-hit)))
(defun @plot:frame-recognition-by-polyline (/ frames total)
  "识别多段线矩形图框"
  ;; 识别多段线
  ;; (@:help (strcat "识别多段线矩形图框"))
  (setq total (length @plot:*frames*))
  (if (setq frames (ssget "x" (list '(0 . "*POLYLINE")
				    '(90 . 4)
				    '(70 . 1)
				    '(60 . 0)
				    (cons 8 (@:get-config '@plot:layers))
				    '(-4 . "<NOT")
				    '(8 . "temp-frames")
				    '(-4 . "NOT>"))
			  ))
      (progn
	(setq frames (pickset:to-list frames))
	;; 去除非矩形
	(setq frames (mapcar
		      '(lambda (x) (cons (@:get-rec-points x)(entity:getdxf x 410)))
		      frames))
	(setq frames (vl-remove-if-not
		      '(lambda (y / x)
			 (setq x (car y))
			 (and 
			  (equal (distance (car x)(caddr x))
				 (distance (cadr x)(cadddr x))
				 (* 0.001 (distance (car x)(caddr x))))
			  (/= (distance (car x)(caddr x))
			      (@plot:get-width x))
			  (/= (distance (car x)(caddr x))
			      (@plot:get-height x))
			  ))
		      frames))
	;; (print frames)
	(setq frames
	      (vl-remove-if-not
	       '(lambda (y / x)
		  (setq x (car y))
		  (@plot:framep x))
	       (mapcar '(lambda (y / x)
			  ;; 比例条件
			  (setq x (car y))
			  (cons 
			   (vl-sort 
			    x
			    '(lambda (pt1 pt2)
			       (if (and (equal (car pt1)(car pt2) 0.001)
					(< (cadr pt1)(cadr pt2)))
				   T
				 (if (< (car pt1)(car pt2))
				    T
				   nil))
			       ))
			   (cdr y)
			   ))
		       frames)))
	(setq @plot:*frames* frames)
	(@:prompt (strcat "识别了 "
			  (itoa (-(length @plot:*frames*)
				  total))
			  " 个多段线矩形图框。\n"))))
  (princ)
  )

(@:add-menu "通用打印" "识别直线框" "(@plot:frame-recognition-by-line)")
(defun @plot:frame-lines? (bm tp lt rt / fuzz)
  "测试4条线段是否为矩形框"
  (setq fuzz 0.01)
  (and
   (equal (car (line:mid bm)) (car (line:mid tp)) fuzz)
   (equal (cadr (line:mid lt)) (cadr (line:mid rt)) fuzz)
   (> fuzz (distance
	    (polar (line:mid bm)
		   (* 0.5 pi)
		   (* 0.5 (distance (line:mid bm)(line:mid tp))))
	    (polar (line:mid lt)
		   0
		   (* 0.5 (distance (line:mid lt)(line:mid rt))))))
   (equal (line:length bm) (line:length tp) fuzz)
   (equal (line:length lt) (line:length rt) fuzz)
   (equal (distance (line:mid lt) (line:mid rt))
    	  (line:length bm)
	  fuzz)
   (equal (distance (line:mid bm) (line:mid tp))
    	  (line:length lt)
	  fuzz)
   ;; ;; 宽高比
   (@plot:framep (vl-sort 
    		  (append (entity:getdxf bm '(10 11))
			  (entity:getdxf tp '(10 11)))
		  '(lambda (pt1 pt2)
		     (or (< (car pt1)(car pt2))
			 (and (equal  (car pt1)(car pt2) 0.001)
			      (< (cadr pt1)(cadr pt2)))))))
   ))
(defun @plot:frame-recognition-by-line
    (/ frames segments frame-length?
       seg-v seg-h ent-first frame got-tk
       total frame-pts seg-h-y y0 dy-max
       )
  ;; 线性图框识别：seg-h 已按 y 升序。上下边的 y 距离 dy 要么是短边（横版），
  ;; 要么是长边（竖版/加长，最长 3.53×短边），故保守上界为
  ;;   max(高) × max(比例) × max(加长比)
  ;; 超过上界的配对不可能成立 → 内层直接跳出（把 N² 压成「每个下边只看一个窗口」）。
  ;; 只用「短边上界」会漏掉竖版/加长图框（@plot:framep 取的是 min(宽,高)）。
  (@plot:ensure-tables)
  (setq dy-max (* (apply 'max @plot:height-of-frame)
		  (apply 'max @plot:scale-of-frame)
		  (apply 'max @plot:ratio-of-w/h)))
  "识别由4个线段组成的矩形图框"
  ;; 识别多段线
  (@:log "INFO" "选择并排除杂线...")
  (std:timer-start)
  (setq total (length @plot:*frames*))
  (setq segments (pickset:to-list
		  (ssget "x" (list '(0 . "LINE")
				   (cons 8 (@:get-config '@plot:layers))
				   '(-4 . "<NOT")
				   '(8 . "temp-frames")
				   '(-4 . "NOT>")))))
  (@:log "INFO" (strcat "发现 " (itoa(length segments)) "条线。"))
 
  (setq segments (vl-remove-if-not
		  '(lambda (x)(@plot:frame-length? (line:length x)))
		  segments))
  (@:log "INFO" (strcat (itoa (length segments)) "条线符合图框尺寸。"))
  (@:log "INFO" "正在分类边框线...\n")
  (setq seg-h (vl-remove-if-not
	       '(lambda (x / ang)
		  (setq ang (apply 'angle (entity:getdxf x '(10 11))))
		  (while (>= ang pi)
		    (setq ang (- ang pi)))
		  (or 
		   (equal ang  0 0.001)
		   (equal ang  pi 0.001)))
	       segments))
  
  (@:log "INFO" (strcat "seg-h: "(itoa (length seg-h)) ))
  (@:log "INFO" "正在进行边框线排序...")
  (setq seg-h
	(vl-sort seg-h
		 '(lambda (ent1 ent2) (< (cadr (entity:getdxf ent1 10))
				       (cadr (entity:getdxf ent2 10))))))
  ;; (setq seg-v
  ;; 	(vl-sort seg-v
  ;; 		 '(lambda (ent1 ent2) (< (car (entity:getdxf ent1 10))
  ;; 				       (car (entity:getdxf ent2 10))))))
  (@:log "INFO" "从直线图元中识别矩形框。\n")
  ;; 预取各水平线的 y，避免配对循环里对同一图元反复 entget
  (setq seg-h-y (mapcar '(lambda (x) (cadr (entity:getdxf x 10))) seg-h))
  (setq bm% 0)(setq got-tk nil)
  (while  (< bm% (- (length seg-h) 1))
    (setq tp% (1+ bm%))
    (setq y0 (nth bm% seg-h-y))
    (while  (and (< tp% (length seg-h))
		 (<= (- (nth tp% seg-h-y) y0) dy-max))
      ;; 从 seg-v 中取相应图元并从表中移除
      ;; 检测 bm% tp% 两个线是否符合图框要求
      (if (@plot:framep
	   (append
	    (entity:getdxf (nth bm% seg-h) '(10 11))
	    (entity:getdxf (nth tp% seg-h) '(10 11))))
	  (progn
	    (setq got-tk T)
	    (setq frame (list (nth bm% seg-h)
			      (nth tp% seg-h)
			      ))
	    (setq frame-pts 
		  (vl-sort 
		   (append (entity:getdxf (car frame) '(10 11))
				 (entity:getdxf (cadr frame) '(10 11)))
		   '(lambda (pt1 pt2)
		      (or (< (car pt1)(car pt2))
			  (and (equal (car pt1)(car pt2) 0.001)
			       (< (cadr pt1)(cadr pt2)))))))
	    ;; 验证有水平线。
	    (setq seg-v1 
		  (ssget "F" (entity:getdxf (car frame) '(10 11))
			 ;;frame-pts
			 (list '(0 . "LINE")
			       (cons 8 (@:get-config '@plot:layers))
			       '(-4 . "<NOT")
			       '(8 . "temp-frames")
			       '(-4 . "NOT>"))))
	    (if (= 'pickset (type seg-v1))
		(progn
		  (setq  seg-v1
			 (vl-remove-if-not
			  '(lambda (x / ang)
			     (setq ang (apply 'angle (entity:getdxf x '(10 11))))
			     (while (>= ang pi)
			       (setq ang (- ang pi)))
			     (equal ang (* 0.5 pi) 0.001))
			  (pickset:to-list seg-v1)))
		  (if (and seg-v1 (>= (length seg-v1) 2)
			   )
		      (setq @plot:*frames*
			    (append @plot:*frames* (list (cons frame-pts  (entity:getdxf (car seg-v1) 410)) )))
		  
		    ))))
	)
      (setq tp% (1+ tp%)))
    (setq bm% (1+ bm%))
    )
  (princ "\n")
  (@:prompt (strcat "识别了 "
		    (itoa (-(length @plot:*frames*)
			    total))
		    " 个直线图框"
		    ;; 诊断：表只应加载 1 次（旧实现是「每条线一次」）。
		    ;; 若这个数字随重跑次数一直涨，说明缓存被意外失效了。
		    "（图幅表加载 " (itoa @plot:*load-config-count*) " 次）"))
  (std:timer-end)
  (princ)
  )

(@:add-menu "通用打印" "识别块图框" "(@plot:frame-recognition-by-block)")

(defun @plot:frame-recognition-by-block (/ ss i ref fi bname seen hits rs j n box pts layout)
  "识别**块图框**并把结果并入 @plot:*frames*。
核心判定在 lib 的 `frame:ref-p`（块参照世界包围盒 → 标准图幅匹配），本函数只负责：
1. **按块名去重**后判定：块参照可能有几千个，但唯一块名通常只有几十个——
   逐个参照重算是把 CAD 拖死的主因（实测 800 个参照即触发 stalled）；
2. 过滤误判：短边 >=2000 / 带属性 / 块内有图签文字（`frame:title-block-p`）；
3. 记录 `(4角点 . 布局)` 到 @plot:*frames* 并报数。
说明：旧实现只 `sssetfirst` 选中，不记录、不标记、不报数，且依赖 at-pm 的 @pm:frame-p。
注意：本函数依赖 lib 的 `frame:ref-p`（与 lib 版本耦合），缺失时提示先更新 lib。"
  (@plot:ensure-tables)
  ;; 版本耦合守卫：atoms-family 的 format=1 返回**字符串**，必须按字符串比对
  (if (not (member "FRAME:REF-P" (atoms-family 1)))
      (@:alert "本功能需要 lib 的 frame:ref-p。请先更新函数库：@i lib")
    (progn
      (setq n 0
	    seen nil
	    hits nil)
      ;; 第一遍：每个**块名**只判定一次
      (if (setq ss (ssget "_X" '((0 . "INSERT"))))
	  (progn
	    (setq i 0)
	    (repeat (sslength ss)
	      (setq ref (ssname ss i)
		    bname (cdr (assoc 2 (entget ref))))
	      ;; 跳过匿名块（*U 动态块/*D 标注块等）与已判过的块名
	      (if (and (not (member bname seen))
		       (not (wcmatch bname "`**")))
		  (progn
		    (setq seen (cons bname seen))
		    (if (frame:ref-p ref @plot:scale-of-frame nil nil)
			(setq hits (cons bname hits)))))
	      (setq i (1+ i)))
	    )
	)
      ;; 第二遍：只处理命中块名的所有参照（此时数量很小）
      (foreach bname (reverse hits)
	(if (setq rs (ssget "_X" (list (cons 0 "INSERT") (cons 2 bname))))
	    (progn
	      (setq j 0)
	      (repeat (sslength rs)
		(setq ref (ssname rs j))
		(setq fi (frame:ref-p ref @plot:scale-of-frame nil nil))
		(if (and fi
			 (or (>= (min (cdr (assoc "width" fi)) (cdr (assoc "height" fi))) 2000)
			     (block:get-attributes ref)
			     (frame:title-block-p ref nil)))
		    (progn
		      (setq box (cdr (assoc "box" fi))
			    pts (apply 'point:rec-2pt->4pt box)
			    layout (cdr (assoc 410 (entget ref))))
		      (setq @plot:*frames* (append @plot:*frames* (list (cons pts layout))))
		      (setq n (1+ n))
		      )
		    )
		(setq j (1+ j))
		)
	      )
	    )
	)
      (@:prompt (strcat "识别了 "
			(itoa n)
			" 个块图框（检查了 " (itoa (length seen)) " 种块名）。"))
      )
    )
  (princ)
  )
