;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 标准件明细表：把图纸里的图库标准件汇总成表（序号/名称/规格/标准号/数量）。
;;
;; 标准号从「块名 = 清洗后的图库 id」里解析——图库的机械目录本来就是按标准号
;; 分的（机械/螺钉/六角头螺栓-GB5782-86/M6x40），不需要额外维护对照表。
;;
;; 不用 table:make：它依赖仓库里并不存在的 @:scale（见
;; reports/code-review-libsrc-20261021.md），这里自己画线，四种 CAD 都能跑。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'library:*)

;; 明细表放置的图层名。
(setq *at-library:bom-layer* "明细表")

(defun at-library:--segs (s)
  "把块名/id 按 - 和 / 切成段（块名里的 / 已经被 library:--safe-name 换成了 -）。"
  (string:to-list (string:subst-all "-" "/" s) "-"))

(defun at-library:--spec-in (s / lst)
  "最后一段 = 规格（M6x40 / 2 / 16）。"
  (setq lst (at-library:--segs s))
  (if lst (last lst) s))

(defun at-library:--name-in (s / lst i n out)
  "标准号前一段 = 名称（机械-螺栓-六角头螺栓-GB5780-86-M6x40 → 六角头螺栓）。"
  (setq lst (at-library:--segs s) n (length lst) i 0 out nil)
  (while (and (< i n) (null out))
    (if (member (strcase (substr (nth i lst) 1 2))
		'("GB" "JG" "CJ" "DB" "TC" "CE"))
	(setq out (nth (if (> i 0) (1- i) i) lst)))
    (setq i (1+ i)))
  (cond (out out)
	((and lst (> n 1)) (nth (1- n) lst))
	(t s)))

(defun at-library:--std-in (s / n i pos out c2 d)
  "从字符串里抽标准号：GB5782-86 / GB/T 50106-2010 / JGJ/T 331-2014 …
找不到返回空串。"
  (setq n (strlen s) i 1 pos nil)
  (while (and (<= i (1- n)) (null pos))
    (if (member (strcase (substr s i 2)) '("GB" "JG" "CJ" "CE" "DB" "TC"))
	(setq pos i)
      (setq i (1+ i))))
  (if (null pos)
      ""
    (progn
      (setq out "" i pos)
      (while (and (<= i n)
		  (setq c2 (ascii (substr s i 1)))
		  (or (and (>= c2 65) (<= c2 90))
		      (and (>= c2 97) (<= c2 122))
		      (= (substr s i 1) "/")))
	(setq out (strcat out (substr s i 1)) i (1+ i)))
      (while (and (<= i n) (= (substr s i 1) " ")) (setq i (1+ i)))
      (while (and (<= i n)
		  (setq c2 (ascii (substr s i 1)))
		  (or (and (>= c2 48) (<= c2 57)) (= (substr s i 1) ".") (= (substr s i 1) "_")))
	(setq out (strcat out (substr s i 1)) i (1+ i)))
      (if (and (< i n) (member (substr s i 1) '("-" "_"))
	       (setq d (ascii (substr s (1+ i) 1)))
	       (and (>= d 48) (<= d 57)))
	  (progn
	    (setq out (strcat out "-") i (1+ i))
	    (while (and (<= i n)
			(setq d (ascii (substr s i 1)))
			(and (>= d 48) (<= d 57)))
	      (setq out (strcat out (substr s i 1)) i (1+ i)))))
      out)))

(defun at-library:--bom-rows (/ ss tbl name cell rows i)
  "汇总当前布局的块：((序号 名称 规格 标准号 数量) ...)。"
  (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 410 (getvar "CTAB")))))
  (setq tbl '())
  (if ss
      (foreach e (pickset:to-list ss)
	(setq name (entity:getdxf e 2))
	(setq cell (assoc name tbl))
	(if cell
	    (setq tbl (subst (cons name (1+ (cdr cell))) cell tbl))
	  (setq tbl (cons (cons name 1) tbl)))))
  (setq i 0 rows '())
  (foreach cell (vl-sort tbl '(lambda (a b) (< (car a) (car b))))
    (setq name (car cell))
    (setq i (1+ i))
    (setq rows (cons (list (itoa i)
			   (at-library:--name-in name)
			   (at-library:--spec-in name)
			   (at-library:--std-in name)
			   (itoa (cdr cell)))
		     rows)))
  ;; 只留「有标准号」的（真正的标准件）；一个都没有时退回全量块明细，
  ;; 这样在纯建筑图里也能当块统计表用。
  (if (vl-some '(lambda (r) (> (strlen (nth 3 r)) 0)) rows)
      (setq rows (vl-remove-if '(lambda (r) (= (strlen (nth 3 r)) 0)) rows)))
  ;; 重新编号
  (setq i 0)
  (mapcar '(lambda (r) (setq i (1+ i)) (cons (itoa i) (cdr r)))
	  (reverse rows)))

(defun at-library:--cell-text (pt str hgt)
  (entmakex (list '(0 . "TEXT") (cons 10 pt) (cons 1 str) (cons 40 hgt)
		  (cons 7 (getvar "TEXTSTYLE")) (cons 410 (getvar "CTAB")))))

(defun at-library:--hline (x1 x2 y)
  (entity:make-line (list x1 y) (list x2 y)))

(defun at-library:--grid (pt all-rows colw rowh hgt / x0 y0 total x acc i y)
  "画一个带框线的表格（all-rows 首行是表头），返回底边 y。"
  (setq x0 (car pt) y0 (cadr pt) y y0
	total (apply '+ colw))
  (foreach row all-rows
    (setq x x0 acc 0 i 0)
    (foreach cell row
      (at-library:--cell-text
       (list (+ x acc (at-library:--mm 2.0)) (- y (at-library:--mm 2.8)))
       cell hgt)
      (setq acc (+ acc (nth i colw)) i (1+ i)))
    (setq y (- y rowh))
    (at-library:--hline x0 (+ x0 total) y))
  ;; 竖线：左边界 + 每一列的分隔线
  (entity:make-line (list x0 y0) (list x0 y))
  (setq acc 0)
  (foreach w colw
    (setq acc (+ acc w))
    (entity:make-line (list (+ x0 acc) y0) (list (+ x0 acc) y)))
  y)

(defun at-library:bom (pt / rows headers colw hgt old n)
  "在当前布局生成标准件明细表（序号/名称/规格/标准号/数量）。

参数：pt 表格左上角点；返回数据行数
示例：(at-library:bom (getpoint \"\\n明细表位置: \"))"
  (require 'entity:*)
  (if (null pt) (setq pt (getpoint "\n明细表位置: ")))
  (setq rows (at-library:--bom-rows))
  (cond
   ((null pt) (princ "\n已取消。") 0)
   ((null rows) (princ "\n当前布局没有块参照。") 0)
   (t
    (if (not (tblsearch "LAYER" *at-library:bom-layer*))
	(entity:make-layer *at-library:bom-layer* 7 "Continuous" nil))
    (setq headers '("序号" "名称" "规格" "标准号" "数量")
	  colw (mapcar '(lambda (v) (at-library:--mm v)) '(14.0 55.0 30.0 40.0 16.0))
	  hgt (at-library:--mm 3.5))
    (setq old (getvar "CLAYER"))
    (setvar "CLAYER" *at-library:bom-layer*)
    (at-library:--grid pt (cons headers rows) colw (at-library:--mm 8.0) hgt)
    (setvar "CLAYER" old)
    (setq n (length rows))
    (princ (strcat "\n明细表已生成，共 " (itoa n) " 行。"))
    n)))
