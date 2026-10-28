(defun @block:curve-fence-points (curve / o len n i pts)
  "沿曲线等距采样点，供 ssget \"F\" 围栏选择使用。
  curve: 曲线图元名(ename)或 VLA 对象，支持 LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE/SPLINE 等。
  return: 点列表(至少 2 个，按曲线方向排列)；取不到曲线长度时退回 curve:get-points。
  说明: 直接用 curve:get-points 只拿到控制点/端点，带弧段(bulge)、圆弧、样条的路线会
        退化成弦，沿弧布置的块会被漏选，所以这里按距离采样。
  示例: (ssget \"F\" (@block:curve-fence-points (car (entsel))) '((0 . \"INSERT\")))"
  (setq o (if (= 'ename (type curve)) (e2o curve) curve))
  (setq len (vl-catch-all-apply 'vlax-curve-getDistAtParam
                                (list o (vlax-curve-getEndParam o))))
  (if (and (numberp len) (> len 0))
      (progn
        ;; 每 50 图形单位一个采样点，限制在 16~2000 个点
        (setq n (fix (/ len 50.0)))
        (if (< n 16) (setq n 16))
        (if (> n 2000) (setq n 2000))
        (setq i 0)
        (while (<= i n)
          (setq pts (cons (vlax-curve-getPointAtDist o (* len (/ (float i) n))) pts))
          (setq i (1+ i)))
        (reverse pts))
      (curve:get-points curve)))
(defun @block:sort-by-curve (blks curve / o i lst)
  "按块沿曲线的投影距离排序。
  blks: 块参照 ename 列表；curve: 曲线 ename 或 VLA 对象。
  return: 排序后的 ename 列表(距离相同则保持原顺序)。
  说明: 取块包围盒中心(而非插入点)投影，因为有些块定义的几何远离基点，插入点不在块上；
        距离只算一次并带原序号做次序键，避免在比较函数里重复计算。"
  (setq o (if (= 'ename (type curve)) (e2o curve) curve)
        i 0)
  (setq lst
        (mapcar
         '(lambda (x / d bx pt item)
            (setq bx (vl-catch-all-apply 'entity:getbox (list x 0)))
            (setq pt (if (and bx (listp bx) (cadr bx))
                         (list (/ (+ (car (car bx)) (car (cadr bx))) 2.0)
                               (/ (+ (cadr (car bx)) (cadr (cadr bx))) 2.0)
                               0.0)
                         (entity:getdxf x 10)))
            (setq d (vl-catch-all-apply
                     'vlax-curve-getDistAtPoint
                     (list o (vl-catch-all-apply
                              'vlax-curve-getClosestPointTo
                              (list o pt)))))
            ;; 距离相同用原序号保持稳定顺序
            (setq item (list x (if (numberp d) d 1e99) i))
            (setq i (1+ i))
            item)
         blks))
  (mapcar 'car
          (vl-sort lst
                   '(lambda (a b)
                      (if (equal (cadr a) (cadr b) 1e-6)
                          (< (caddr a) (caddr b))
                          (< (cadr a) (cadr b)))))))
(defun @block:find-on-curve (curve blknames / names pts ss1 blks)
  "沿曲线查找指定块名的块参照，按路径顺序返回。
  curve: 曲线 ename 或 VLA 对象；
  blknames: 块名或块名列表(字符串可用逗号/分号/空格分隔多个，如 \"车位,车位-无障碍\")。
  return: 块参照 ename 列表(按沿曲线方向排序)；未找到返回 nil。
  示例: (@block:find-on-curve (car (entsel)) \"车位,车位-无障碍\")"
  (setq names (@block:name-list blknames))
  (setq pts (@block:curve-fence-points curve))
  (setq ss1 (if (and pts names)
                (ssget "F" pts (@block:block-filter names))))
  (setq blks (vl-remove-if-not
              '(lambda (x) (member (block:get-effectivename x) names))
              (pickset:to-list ss1)))
  (if blks (@block:sort-by-curve blks curve)))
(defun @block:number-blocks (blks attname start / n ok miss)
  "按给定顺序为块写入递增编号。
  blks: 块参照 ename 列表(已按顺序排好)；attname: 属性名；start: 起始编号(缺省 1)。
  说明: 没有该属性的块会被跳过并打印提示(princ)，编号保持连续、不留空号。
  return: 实际写入编号的块 ename 列表。
  示例: (@block:number-blocks blks \"NUMBER\" 1)"
  (setq miss (vl-remove-if '(lambda (e) (@block:has-attribute-p e attname)) blks))
  (setq @block:last-hint nil)
  (if miss (@block:missing-attribute-hint miss attname))
  (setq n (if start start 1)
        ok nil)
  (foreach e blks
           (if (@block:has-attribute-p e attname)
               (progn
                 (block:set-attributes e (list (cons attname (@block:number-string e n attname))))
                 (setq n (1+ n))
                 (setq ok (cons e ok)))))
  (reverse ok))
(defun @block:ai-number-by-route (curve blkname attname start / blks)
  "AI 调用入口：沿曲线按路径顺序对指定属性块递增编号，无交互、不弹窗。
  curve: 曲线 ename 或 VLA 对象；
  blkname: 块名或块名列表(字符串可用逗号/分号/空格分隔，如 \"车位,车位-无障碍\")；
  attname: 属性名；start: 起始编号(缺省 1)。
  多块名时统一连续编号(同一条路线、同一个序号序列)。
  return: 按路径顺序的块 ename 列表；未找到块或参数不合法时返回 nil。
  示例: (@block:ai-number-by-route (car (entsel)) \"车位\" \"NUMBER\" 1)
        (@block:ai-number-by-route route-ename \"车位,车位-无障碍\" \"NUMBER\" 1)"
  (if (and curve blkname attname
           (setq blks (@block:find-on-curve curve blkname)))
      (@block:number-blocks blks attname start)))
(defun @block:numbering-by-route (/ *error* lwpl blks start names summary n c)
  "按曲线路线顺序对块进行递增编号。
  选择一条曲线(直线/多段线/圆弧/圆/样条等)，沿曲线找到配置的块并按路径顺序写属性编号。
  @block:block-name 可配置多个块名(逗号/分号/空格分隔)，多个块名共用一条连续编号序列。
  曲线带弧段时按距离采样围栏，不会漏掉弧上/弧旁的块。
  return: nil(编号完成后高亮所选块)。"
  (@::prompt "按曲线路顺序对块进行递增编号")
  (defun *error* (msg)
    ;; 重启动处理
    (princ (strcat msg))
    (princ))
  (@:prompt "请选择一条曲线:")
  (setq lwpl (ssname (ssget ":S" '((0 . "*line"))) 0))
  (cond
    ((null lwpl)
     (princ (@:speak "未选择曲线。")))
    ((or (null (setq names (@block:block-names)))
         (= "" (@:get-config '@block:attribute-name)))
     (@block:setup))
    ((null (setq blks (@block:find-on-curve lwpl names)))
     (princ (@:speak "沿该路线未找到匹配的块，请检查块名/属性块设置。")))
    (t
     (setq start (getint "请输入块起始编号<1>:"))
     (if (null start) (setq start 1))
     (setq blks (@block:number-blocks blks (@:get-config '@block:attribute-name) start))
     (sssetfirst nil (pickset:from-list blks))
     ;; 多块名时给出各块名数量
     (setq summary nil)
     (foreach nm names
              (setq c (length (vl-remove-if-not
                               '(lambda (x) (= (block:get-effectivename x) nm))
                               blks)))
              (if (> c 0) (setq summary (cons (strcat nm " " (itoa c)) summary))))
     (princ (@:speak (strcat "已编号 " (itoa (length blks)) " 个块"
                             (if (cdr summary)
                                 (strcat "（" (string:from-list (reverse summary) "、") "）")
                                 "")
                             "。")))))
  (princ))
