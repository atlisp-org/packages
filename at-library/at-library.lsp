;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @图库 —— 在线图库（atlisp.cn/dw/library）的命令入口
;;
;; 数据层在 lib 的 library: 命名空间（lib/src/library/）：
;;   library:search / library:info / library:insert / library:update
;; 本包只提供「交互 + 出图」的原语：搜索插入、图例、明细表、批量归一化。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'library:*)

(defun at-library:--mm (x / s)
  "把 mm 值按当前标注比例换算成图纸单位。
   （仓库里的 @:scale 实际没有定义，见 reports/code-review-libsrc-20261021.md，
   所以这里自己实现，避免运行期「函数未定义」。）"
  (setq s (getvar "DIMSCALE"))
  (* x (if (and s (> s 0)) s 1.0)))

(@:add-menu "@图库" "搜索并插入" "(at-library:insert)")
(@:add-menu "@图库" "按目录浏览" "(at-library:browse)")
(@:add-menu "@图库" "插入标准图框" "(at-library:frame (getstring \"\\n图幅(A0-A4): \") (getstring \"\\n方向(横/竖): \") (getpoint \"\\n左下角: \"))")
(@:add-menu "@图库" "生成图例" "(at-library:legend (getpoint \"\\n图例左上角: \"))")
(@:add-menu "@图库" "标准件明细表" "(at-library:bom (getpoint \"\\n明细表位置: \"))")
(@:add-menu "@图库" "更新图库索引" "(at-library:update)")
(@:add-menu "@图库" "图库体检/统计" "(at-library:stats)")

(defun at-library:update nil
  "强制更新图库索引并报告符号数量。"
  (if (library:update)
      (progn
	(princ (strcat "\n图库索引已更新，共 "
		       (itoa (library:load))
		       " 个符号。"))
	(princ))
    (princ "\n图库索引更新失败，请检查网络。")))

(defun at-library:stats (/ st)
  "打印本地图库索引概况（符号数 / 索引时间 / 本地路径）。"
  (library:load)
  (setq st (vl-file-systime (library:index-file)))
  (princ (strcat "\n图库符号总数：" (itoa (length *library:rows*))))
  (if st
      (princ (strcat "\n索引时间："
		     (itoa (car st)) "-" (itoa (cadr st)) "-" (itoa (caddr st))
		     " " (itoa (nth 3 st)) ":" (itoa (nth 4 st)))))
  (princ (strcat "\n本地索引：" (library:index-file)))
  (princ))

(defun at-library:--print (res / i)
  "把搜索结果按序号打印出来。"
  (setq i 0)
  (foreach item res
    (setq i (1+ i))
    (princ (strcat "\n  " (itoa i) ". ["
		   (cdr (assoc "disc" item)) "] "
		   (cdr (assoc "name" item))
		   (if (cdr (assoc "dup" item)) " (重复)" "")))
    (if (= 0 (rem i 3)) (princ "\n")))
  (princ (strcat "\n共 " (itoa (length res)) " 条"))
  res)

(defun at-library:--pick (res / n)
  "让用户从结果里选一个，返回 alist。"
  (setq n (getint "\n选择序号 <取消>: "))
  (if (and n (> n 0) (<= n (length res)))
      (nth (1- n) res)))

(defun at-library:insert (kw / res item pt)
  "搜索图库并插入：输入关键字（支持拼音首字母）→ 选序号 → 点插入点。"
  (setq kw (getstring T "\n图库关键字（名称/拼音首字母）: "))
  (if (= kw "")
      (princ "\n未输入关键字。")
    (progn
      (setq res (at-library:--print (library:search kw 30 nil)))
      (cond
       ((null res) (princ "\n没有找到匹配的符号。"))
       ((null (setq item (at-library:--pick res))) (princ "\n已取消。"))
       ((null (setq pt (getpoint (strcat "\n插入点（" (cdr (assoc "name" item)) "）: "))))
	(princ "\n已取消。"))
       ((library:insert (cdr (assoc "id" item)) pt 1 0 nil) (princ "\n已插入。"))
       (t (princ "\n插入失败。"))))))

(defun at-library:browse (category / res item pt)
  "按专业/目录浏览图库（空关键字 + 目录过滤），选中后插入。

示例：(at-library:browse \"电气/元件库\")"
  (setq category (if (and category (/= category ""))
		     category
		   (getstring T "\n专业或目录（如 电气 或 电气/元件库，空=全部）: ")))
  (setq res (at-library:--print (library:search "" 40 category)))
  (if (setq item (at-library:--pick res))
      (if (setq pt (getpoint "\n插入点: "))
	  (library:insert (cdr (assoc "id" item)) pt 1 0 nil))))

(defun at-library:insert-by-name (kw pt / res)
  "按关键字取第一条命中的符号并插入到 pt（给其他程序调用）。

返回：块参照图元名
示例：(at-library:insert-by-name \"银杏\" (getpoint))"
  (setq res (library:search kw 1 nil))
  (if res
      (library:insert (cdr (assoc "id" (car res))) pt 1 0 nil)
    (progn (@::@log "WARN" (strcat "图库无匹配: " kw)) nil)))

(defun at-library:--std-size (size / k)
  "标准图幅尺寸（横放：宽 高）。未知图幅返回 nil。"
  (setq k (strcase size))
  (cond ((= k "A0") '(1189.0 841.0))
	((= k "A1") '(841.0 594.0))
	((= k "A2") '(594.0 420.0))
	((= k "A3") '(420.0 297.0))
	((= k "A4") '(297.0 210.0))
	(t nil)))

(defun at-library:frame (size orient pt / dir res best target w h sw sh d bd)
  "插入标准图框（图库「图框库」：A0–A4 横竖 + 标题栏 + 会签栏）。

参数：
  size   图幅，\"A0\"~\"A4\"（也可直接给完整名，如 \"A2-594x420\"）
  orient 方向，\"竖\" = 竖栏图框，其它（含 nil）= 横栏图框
  pt     图框左下角点
返回：块参照图元名；图库没有该图幅时打印可选项并返回 nil
示例：(at-library:frame \"A2\" \"横\" '(0 0))
      (at-library:frame \"A3\" \"竖\" (getpoint))"
  (require 'library:*)
  (setq dir (if (and orient (= orient "竖")) "竖栏图框" "横栏图框"))
  (setq res (library:search (strcat "图框库/" dir "/" size) 12 (strcat "图框库/" dir)))
  ;; 同一图幅可能有多个变体（图库里 A2 横栏就有 420x594 和 594x420 两个），
  ;; 按标准图幅尺寸挑最接近的：横放 594x420，竖放 420x594。
  (setq target (at-library:--std-size size))
  (if (and target (= dir "竖栏图框"))
      (setq target (list (cadr target) (car target))))
  (if (and target res)
      (progn
	(setq bd nil best nil)
	(foreach x res
	  (setq sw (car (cdr (assoc "size" x)))
		sh (cadr (cdr (assoc "size" x))))
	  (setq d (+ (abs (- sw (car target))) (abs (- sh (cadr target)))))
	  (if (or (null bd) (< d bd))
	      (setq bd d best x)))
	(setq res (if best (list best) res))))
  (cond
   ((null res)
    ;; 图库的图幅并不齐全（横栏只有 A1/A2，A3/A4 只在竖栏里），把可用的列出来
    (princ (strcat "\n图库没有「" size " "
		   (if (= dir "横栏图框") "横" "竖") "」，可用的有："))
    (foreach x (library:search "" 24 "图框库")
      (princ (strcat " " (cdr (assoc "name" x)))))
    (@::@log "WARN" (strcat "图库没有这个图框: " size dir))
    nil)
   (t
    (library:insert (cdr (assoc "id" (car res))) pt 1 0 "bl"))))
