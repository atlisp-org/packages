;; ============================================================
;; at-landscape/area.lsp
;; 内轮廓面积及随机色填充
;; ============================================================
(require '(entity:* curve:* pickset:*))

;; 注册菜单
(@:add-menus
 '("景观面积"
   ("内轮廓面积及填充" "(at-landscape:area)")))

;; ------------------------------------------------------------
;; 1. 内轮廓面积及随机色填充
;; ------------------------------------------------------------
(defun at-landscape:light-colors ()
  "浅色填充颜色表（ACI 中偏浅的颜色，用于随机色填充）。"
  "颜色号列表"
  "(at-landscape:light-colors)"
  '(9 10 20 30 40 50 51 60 70 80 90 110 120
    130 140 150 160 170 180 190 200 210 220 230 240 250))

(defun at-landscape:ent-center (ent / b p)
  "取图元标注中心：能用形心的用形心，否则取包围盒中心。"
  "二维点，失败返回 nil"
  "(at-landscape:ent-center (car (entsel)))"
  (setq p (vl-catch-all-apply 'at-landscape:center-of (list ent)))
  (cond
   ((and p (not (vl-catch-all-error-p p)) (listp p)) p)
   ((setq b (entity:getbox ent 0.0))
    (list (/ (+ (car (car b)) (car (cadr b))) 2.0)
          (/ (+ (cadr (car b)) (cadr (cadr b))) 2.0)))
   (t nil)))

(defun at-landscape:area-text (a factor)
  "把面积值格式化为标注文字（默认按 mm→m2 换算）。"
  "字符串"
  "(at-landscape:area-text 1234567.0 1e-6)"
  (strcat (rtos (* a (if (numberp factor) factor 1e-6)) 2 2) " m2"))

(defun at-landscape:do-area (ents factor colors / res e a p tx)
  "核心：标注封闭图元的内轮廓面积，并按需做随机浅色填充。"
  "((源图元 面积文字) ...) 列表，无有效图元时返回 nil"
  "(at-landscape:do-area (list (car (entsel))) 1e-6 t)"
  (setq res nil)
  (foreach e ents
    (if (and e (setq a (at-landscape:area-of e)))
        (progn
          (if colors
              (at-landscape:hatch
               e
               (at-landscape:pick-color
                (at-landscape:light-colors)
                (fix (* 1000.0 (at-landscape:rand))))
               "SOLID" 1.0))
          (setq p (at-landscape:ent-center e))
          (if p
              (progn
                (setq tx (at-landscape:text
                          (at-landscape:area-text a factor)
                          p
                          (at-landscape:text-size)
                          "MM"))
                (if tx (setq res (cons (list e tx) res))))))))
  (reverse res))

(defun at-landscape:area (/ ss ents factor colors res)
  "内轮廓面积及填充：选择封闭区域，在形心标注面积（m2），可随机浅色填充。"
  "面积文字 ename 列表"
  "(at-landscape:area)"
  (if (setq ss (ssget '((0 . "LWPOLYLINE,CIRCLE,REGION,SPLINE"))))
      (progn
        (setq ents (pickset:to-entlist ss))
        (sssetfirst nil nil)
        (setq factor (getreal "\n面积换算系数 <1e-6>:"))
        (if (null factor) (setq factor 1e-6))
        (setq colors
              (if (= "No" (getkword "\n随机色填充 [Yes/No] <Yes>:")) nil t))
        (startundo)
        (setq res (at-landscape:do-area ents factor colors))
        (endundo)
        (if res
            (princ (strcat "\n已标注 " (itoa (length res)) " 个封闭区域。"))
            (princ "\n未找到可标注的封闭区域。")))
      (princ "\n未选择图元。"))
  (princ))

(defun at-landscape:ai-area (bounds factor colors / ents res)
  "AI 调用入口：标注封闭区域面积，可随机浅色填充。
  bounds: ename 列表或选择集；factor: 面积换算系数（缺省 1e-6）；
  colors: 是否随机色填充（缺省 T）。"
  "((源图元 面积文字) ...) 列表，失败返回 nil"
  "(at-landscape:ai-area (list (car (entsel))) 1e-6 t)"
  (cond
   ((= 'pickset (type bounds)) (setq ents (pickset:to-entlist bounds)))
   ((= 'ename (type bounds)) (setq ents (list bounds)))
   ((listp bounds) (setq ents bounds)))
  (if ents
      (setq res (at-landscape:do-area ents factor
                                      (if (null colors) t colors)))
      (princ "at-landscape:ai-area bounds 必须是 ename 列表或选择集"))
  res)
