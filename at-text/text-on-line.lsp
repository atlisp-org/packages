(defun at-text:att-on-line (blk)
  "属性值缩到表格内，一次一选代码"
  (setq blk (car(entsel)))
  (setq flag t)
  (while flag
    (setq att (entnext blk))
    (setq box (entity:getbox att 1))
    (setq 4pt (apply 'point:rec-2pt->4pt box))
    (if (ssget "f" 4pt)
      (entity:putdxf att 41 (* 0.8 (entity:getdxf att 41)))
      (setq flag nil)
    )))