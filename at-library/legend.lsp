;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动图例：扫描当前布局里用到的块，按「符号 + 名称」排成一列图例。
;;
;; 这是画图人最烦的重复工：图纸改了，图例要跟着改。块名如果来自图库
;; （library:insert 生成的块名 = 清洗后的 id），还能反查出专业和标准号。
;;
;; 不用 table:make（它依赖仓库里并不存在的 @:scale），这里自己画表格线 + 文字，
;; 保证在 AutoCAD/中望/浩辰/BricsCAD 上都能跑。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'library:*)

;; 图例放置的图层名。
(setq *at-library:legend-layer* "图例")
;; 图例单元格边长（mm，乘当前标注比例）。
(setq *at-library:legend-cell* 14.0)

(defun at-library:--text (pt str hgt)
  "在 pt 写单行文字（左下对齐）。"
  (entmakex (list '(0 . "TEXT")
		  (cons 10 pt)
		  (cons 1 str)
		  (cons 40 hgt)
		  (cons 7 (getvar "TEXTSTYLE"))
		  (cons 410 (getvar "CTAB")))))

(defun at-library:--used-blocks (/ ss names e n)
  "当前布局里用到的块名（去重、按名排序）。"
  (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 410 (getvar "CTAB")))))
  (if ss
      (foreach e (pickset:to-list ss)
	(setq n (entity:getdxf e 2))
	(if (and n (not (member n names)))
	    (setq names (cons n names)))))
  (vl-sort names '(lambda (a b) (< (strcase a) (strcase b)))))

(defun at-library:legend (pt / names cell hgt gap row-w x y i box w h sc cx cy ent
			     old-layer lines n)
  "在当前布局生成图例：左侧符号，右侧块名（图库块附带专业）。

参数：pt 图例左上角点；返回生成的行数
示例：(at-library:legend (getpoint \"\\n图例左上角: \"))"
  (require 'frame:*)
  (require 'entity:*)
  (require 'block:*)
  (if (null pt) (setq pt (getpoint "\n图例左上角: ")))
  (setq names (at-library:--used-blocks))
  (cond
   ((null pt) (princ "\n已取消。") 0)
   ((null names) (princ "\n当前布局没有块参照。") 0)
   (t
    (if (not (tblsearch "LAYER" *at-library:legend-layer*))
	(entity:make-layer *at-library:legend-layer* 7 "Continuous" nil))
    (setq cell (* (at-library:--mm *at-library:legend-cell*) 1.0)
	  hgt (at-library:--mm 3.5)
	  gap (at-library:--mm 2.0))
    (setq x (car pt) y (cadr pt) i 0
	  old-layer (getvar "CLAYER"))
    (setvar "CLAYER" *at-library:legend-layer*)
    (foreach name names
      (setq cx (+ x (/ cell 2.0))
	    cy (- y (/ cell 2.0)))
      (setq box (frame:block-box name))
      (cond
       ((and box
	     (> (- (car (cadr box)) (car (car box))) 1e-6)
	     (> (- (cadr (cadr box)) (cadr (car box))) 1e-6))
	(setq w (- (car (cadr box)) (car (car box)))
	      h (- (cadr (cadr box)) (cadr (car box))))
	(setq sc (min (/ cell w) (/ cell h)))
	(setq ent (block:insert name "" (list cx cy 0.0) 0 sc))
	(if ent
	    (vla-move (vlax-ename->vla-object ent) (vlax-3d-point '(0.0 0.0 0.0))
		    (vlax-3d-point
		     (list (- cx (* sc (/ (+ (car (car box)) (car (cadr box))) 2.0)))
			   (- cy (* sc (/ (+ (cadr (car box)) (cadr (cadr box))) 2.0)))
			   0.0)))))
       (t
	(setq ent (block:insert name "" (list cx cy 0.0) 0 1.0))))
      (setq row-w (+ cell gap gap (* hgt (strlen name)) hgt))
      (at-library:--text (list (+ x cell gap) (- cy (/ hgt 2.0))) name hgt)
      (entity:make-line (list x y) (list (+ x row-w) y))
      (setq lines (cons row-w lines)
	    i (1+ i)
	    y (- y cell)))
    (setq n (if lines (apply 'max lines) (+ cell gap)))
    (entity:make-line (list x y) (list (+ x n) y))
    (entity:make-line (list x (cadr pt)) (list x y))
    (entity:make-line (list (+ x n) (cadr pt)) (list (+ x n) y))
    (setvar "CLAYER" old-layer)
    (princ (strcat "\n图例已生成，共 " (itoa i) " 项。"))
    i)))
