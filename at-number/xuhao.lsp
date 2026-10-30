;;; 全能编号：选中点/文字/块插入点，按顺序批量编号。
;;; 支持前缀/后缀/起始号/步长，可横向、纵向或蛇形排序。
;;; 注：部分 CAD（如 ZWCAD）无 startundo/endundo，统一用 UNDO Begin/End 命令实现。
(@:define-config '@number:prefix "" "编号前缀")
(@:define-config '@number:suffix "" "编号后缀")
(@:define-config '@number:offset 300
                 "编号文字距对象的偏移量(绘图单位，实际值乘以绘图比例)")
(@:add-menu "编号工具" "全能编号" "(at-number:xuhao)")

(defun at-number:number-string (n)
  "把编号数值格式化为字符串：整数取整，实数保留 2 位。
   n: 数值。
   return: 字符串。
   示例: (at-number:number-string 5) => \"5\""
  (if (= n (fix n))
      (itoa (fix n))
      (rtos n 2 2)))

(defun at-number:insertion-point (ent / box p)
  "取对象的编号参考点：点/文字/块取插入点(dxf 10)，其它取包围盒中心。
   ent: 对象 ename。
   return: 点。
   示例: (at-number:insertion-point (car (entsel)))"
  (setq p (vl-catch-all-apply 'entity:getdxf (list ent 10)))
  (if (and p (listp p) (not (vl-catch-all-error-p p)) (numberp (car p)))
      p
      (progn
        (setq box (vl-catch-all-apply 'entity:getbox (list ent 0)))
        (if (and box (listp box) (cadr box)
                 (not (vl-catch-all-error-p box)))
            (point:centroid box)
            '(0.0 0.0 0.0)))))

(defun at-number:sort-by-x (ents)
  "按 x 从左到右排序。
   ents: 对象 ename 列表。
   return: 排序后的 ename 列表。
   示例: (at-number:sort-by-x (pickset:to-list (ssget)))"
  (vl-sort ents
           '(lambda (a b)
              (< (car (at-number:insertion-point a))
                 (car (at-number:insertion-point b))))))

(defun at-number:sort-by-y (ents)
  "按 y 从上到下排序。
   ents: 对象 ename 列表。
   return: 排序后的 ename 列表。
   示例: (at-number:sort-by-y (pickset:to-list (ssget)))"
  (vl-sort ents
           '(lambda (a b)
              (> (cadr (at-number:insertion-point a))
                 (cadr (at-number:insertion-point b))))))

(defun at-number:sort-by-snake (ents / items rows cur cury rowtol res dir r)
  "按蛇形(按行折返)排序：首行从左到右、次行从右到左，依此类推。
   ents: 对象 ename 列表。
   return: 排序后的 ename 列表。
   示例: (at-number:sort-by-snake (pickset:to-list (ssget)))"
  (setq rowtol (@::scale (max 1.0 (@::get-config '@number:fontsize))))
  (setq items
        (mapcar '(lambda (e / p)
                   (setq p (at-number:insertion-point e))
                   (list e (car p) (cadr p)))
                ents))
  (setq items (vl-sort items '(lambda (a b) (> (caddr a) (caddr b)))))
  (setq rows nil
        cur nil
        cury nil)
  (foreach it items
    (if (and cur (<= (abs (- cury (caddr it))) rowtol))
        (setq cur (cons it cur))
        (progn
          (if cur (setq rows (cons cur rows)))
          (setq cur (list it)
                cury (caddr it)))))
  (if cur (setq rows (cons cur rows)))
  (setq rows (reverse rows)
        res nil
        dir 1)
  (foreach r rows
    (setq r (vl-sort r '(lambda (a b) (< (cadr a) (cadr b)))))
    (if (= dir -1) (setq r (reverse r)))
    (setq res (append res (mapcar 'car r))
          dir (- dir)))
  res)

(defun at-number:default-offset ()
  "返回编号文字的默认偏移点(向右偏移，距离取配置 @number:offset)。
   return: 点。
   示例: (at-number:default-offset)"
  (list (@::scale (@::get-config '@number:offset)) 0.0 0.0))

(defun at-number:xuhao (/ ents prefix suffix start step order offset pos res i en num)
  "交互命令：选中点/文字/块，按顺序批量编号。
   return: 无。
   示例: 在命令行执行 (at-number:xuhao)"
  (@::prompt '("全能编号：选中点/文字/块，按顺序批量编号。"
               "支持前缀、后缀、起始号、步长，可横向/纵向/蛇形排序。"))
  (if (null (tblsearch "layer" (@::get-config '@number:layer)))
      (layer:make (@::get-config '@number:layer) 1 nil nil))
  (if (null (setq ents (cadr (ssgetfirst))))
      (setq ents (ssget '((0 . "POINT,TEXT,MTEXT,INSERT")))))
  (if (null ents)
      (princ "\n[编号] 未选择对象。")
      (progn
        (setq ents (pickset:to-list ents))
        (initget "X Y Snake")
        (setq order (getkword
                     "排序方式 [X(从左到右)/Y(从上到下)/Snake(蛇形)] <X>: "))
        (if (null order) (setq order "X"))
        (setq prefix (getstring t
                                (strcat "编号前缀 <"
                                        (@::get-config '@number:prefix) ">: ")))
        (if (= "" prefix) (setq prefix (@::get-config '@number:prefix)))
        (setq suffix (getstring t
                                (strcat "编号后缀 <"
                                        (@::get-config '@number:suffix) ">: ")))
        (if (= "" suffix) (setq suffix (@::get-config '@number:suffix)))
        (setq start (getint "起始编号 <1>: "))
        (if (null start) (setq start 1))
        (setq step (getint "步长 <1>: "))
        (if (null step) (setq step 1))
        (setq offset (getdist
                      (strcat "文字偏移距离 <"
                              (rtos (@::scale (@::get-config '@number:offset))
                                    2 1) ">: ")))
        (if (null offset)
            (setq offset (@::scale (@::get-config '@number:offset))))
        (setq pos (list offset 0.0 0.0))
        (setq ents (cond ((= "Y" (strcase order))
                          (at-number:sort-by-y ents))
                         ((= "SNAKE" (strcase order))
                          (at-number:sort-by-snake ents))
                         (t (at-number:sort-by-x ents))))
        (setq i 0
              res nil)
        (command "_.UNDO" "Begin")
        (foreach e ents
          (setq num (+ start (* i step)))
          (setq en (entity:make-text
                    (strcat prefix
                            (at-number:number-string num)
                            suffix)
                    (mapcar '+ (at-number:insertion-point e) pos)
                    (@::scale (@::get-config '@number:fontsize))
                    0 0.72 0 "LM"))
          (entity:putdxf en 8 (@::get-config '@number:layer))
          (setq res (cons en res)
                i (1+ i)))
        (command "_.UNDO" "End")
        (princ (strcat "\n[编号] 已生成 " (itoa (length res)) " 个编号。"))))
  (princ))

(defun at-number:ai-number (ents prefix start step order pos / lst i res en num)
  "AI 调用入口：给一组对象(点/文字/块插入点)按顺序批量编号，无交互、不弹窗。
   ents: 对象 ename 列表或选择集。
   prefix: 编号前缀(字符串，如 \"M\")；后缀取配置 @number:suffix(如 \"#\")。
   start: 起始编号(缺省 1)；step: 步长(缺省 1)。
   order: 排序方式 \"x\"(从左到右)/\"y\"(从上到下)/\"snake\"(蛇形)(缺省 \"x\")。
   pos: 文字偏移点(相对对象插入点的 dx dy dz，缺省向右偏移)。
   return: 生成的编号文字 ename 列表；未选择对象返回 nil。
   示例: (at-number:ai-number (ssget) \"M\" 1 1 \"snake\" nil)
         (at-number:ai-number ents \"\" 1 1 \"x\" '(600.0 0.0 0.0))"
  (setq lst (cond ((null ents) nil)
                  ((= 'pickset (type ents)) (pickset:to-list ents))
                  ((listp ents) ents)
                  (t (list ents))))
  (if (null lst)
      (progn (princ "\n[编号] 未提供对象。") nil)
      (progn
        (setq start (if (numberp start) start 1)
              step (if (numberp step) step 1)
              order (if (and order (= 'str (type order)))
                        (strcase order) "X")
              prefix (if (= 'str (type prefix)) prefix "")
              pos (if (and pos (listp pos)) pos (at-number:default-offset)))
        (if (null (tblsearch "layer" (@::get-config '@number:layer)))
            (layer:make (@::get-config '@number:layer) 1 nil nil))
        (setq lst (cond ((= "Y" order) (at-number:sort-by-y lst))
                        ((= "SNAKE" order) (at-number:sort-by-snake lst))
                        (t (at-number:sort-by-x lst))))
        (setq i 0
              res nil)
        (command "_.UNDO" "Begin")
        (foreach e lst
          (setq num (+ start (* i step)))
          (setq en (entity:make-text
                    (strcat prefix
                            (at-number:number-string num)
                            (@::get-config '@number:suffix))
                    (mapcar '+ (at-number:insertion-point e) pos)
                    (@::scale (@::get-config '@number:fontsize))
                    0 0.72 0 "LM"))
          (entity:putdxf en 8 (@::get-config '@number:layer))
          (setq res (cons en res)
                i (1+ i)))
        (command "_.UNDO" "End")
        (reverse res))))
