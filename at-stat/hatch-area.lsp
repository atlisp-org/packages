;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-stat 填充面积统计（hatch-area）
;; 按填充图案名称汇总面积并绘制表格。
;; 需求来源：明经论坛 thread-107330「按填充类型统计面积并产生表格」。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(entity:*))

(defun at-stat:hatch-area-data (ents factor / res ent pat a rec)
  "按填充图案汇总面积。
  ents: 填充 ename 表/选择集；factor: 面积换算系数（nil = 1e-6，即 mm2→m2）。
  返回二维矩阵 ((图案 面积 数量) ...)，按面积降序。"
  "二维矩阵"
  "(at-stat:hatch-area-data (pickset:to-list (ssget '((0 . \"HATCH\")))) 1.0)"
  (if (null factor) (setq factor 1e-6))
  (foreach ent (if (= 'pickset (type ents)) (pickset:to-list ents) ents)
    (if (and ent (= "HATCH" (entity:getdxf ent 0)))
        (progn
          (setq pat (entity:getdxf ent 2))
          (if (or (null pat) (= pat "")) (setq pat "(未命名)"))
          (if (setq a (at-stat:ent-area ent))
              (if (setq rec (assoc pat res))
                  (setq res (subst (list pat (+ (cadr rec) a)
                                         (1+ (caddr rec)))
                                   rec res))
                  (setq res (cons (list pat a 1) res)))))))
  (mapcar '(lambda (r) (list (car r) (* (cadr r) factor) (caddr r)))
          (sort res '(lambda (a b) (> (cadr a) (cadr b))))))

(defun at-stat:ai-hatch-area (ents factor)
  "AI 调用入口：按填充图案汇总面积，返回二维矩阵，不绘制表格。
  ents: 填充 ename 表/选择集；factor: 缺省 1e-6（mm2→m2）。
  示例: (at-stat:ai-hatch-area (pickset:to-list (ssget '((0 . \"HATCH\")))) 1e-6)"
  "((图案 面积 数量) ...)，无填充返回 nil"
  "(at-stat:ai-hatch-area (pickset:to-list (ssget '((0 . \"HATCH\")))) 1.0)"
  (at-stat:hatch-area-data ents factor))

(defun at-stat:ai-hatch-area-table (ents pt factor / data)
  "AI 调用入口：按填充图案统计面积并绘制表格。
  返回表格 ename，失败返回 nil。
  示例: (at-stat:ai-hatch-area-table (pickset:to-list (ssget '((0 . \"HATCH\")))) '(0 0) 1e-6)"
  "表格 ename 或 nil"
  "(at-stat:ai-hatch-area-table (pickset:to-list (ssget '((0 . \"HATCH\")))) '(0 0) 1e-6)"
  (setq data (at-stat:hatch-area-data ents factor))
  (if data
      (at-stat:make-table "填充面积统计表"
                          '("填充图案" "面积" "数量")
                          data pt)
      (progn
        (princ "at-stat:ai-hatch-area-table 没有可统计的填充")
        nil)))

(defun at-stat:hatch-area (/ ents factor pt)
  "填充面积统计：选择填充，按图案汇总面积并绘制表格。"
  "表格 ename 或 nil"
  "(at-stat:hatch-area)"
  (setq ents (at-stat:pick-ents "选择填充: " '((0 . "HATCH"))))
  (if ents
      (progn
        (setq factor (getreal "\n面积系数 <1e-6 mm2→m2>:"))
        (if (null factor) (setq factor 1e-6))
        (setq pt (getpoint "\n表格插入点: "))
        (if pt
            (at-stat:ai-hatch-area-table ents pt factor))))
  (princ))
