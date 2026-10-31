;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-stat 统计工具（stat-tools）
;; 复现 iMini 统计表格类命令：块统计表/文字统计表/面积标注/点选标面积/
;; 面积分层统计表/房间墙面面积统计/长度分层统计表/标注线段长度/
;; 同形状统计/标准层面积分摊表/文字输出Excel/文字计算/苗木统计表
;; 主文件：菜单注册 + 公共辅助（统一统计框架）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:add-menus
 '(("统计表格"
    ("块统计表" "(at-stat:block-list)")
    ("文字统计表" "(at-stat:text-list)")
    ("面积标注" "(at-stat:area-text)")
    ("点选标面积" "(at-stat:point-area)")
    ("面积分层统计表" "(at-stat:by-layer-area)")
    ("房间墙面面积统计" "(at-stat:wall-area)")
    ("长度分层统计表" "(at-stat:total-length)")
    ("标注线段长度" "(at-stat:text-length)")
    ("同形状统计" "(at-stat:shape-list)")
    ("标准层面积分摊表" "(at-stat:area-analyze)")
    ("文字输出Excel" "(at-stat:text2excel)")
    ("文字计算" "(at-stat:cale-txt)")
    ("苗木统计表" "(at-stat:mmtj)")
    )))

;; ================= 公共辅助 =================

(defun at-stat:to-str (x)
  "任意值转字符串（表格/CSV 单元用），nil 变空串。"
  "字符串"
  "示例: (at-stat:to-str 3.14159) => \"3.14159\""
  (cond ((null x) "")
        ((= 'str (type x)) x)
        ((numberp x) (rtos x 2 4))
        (t (vl-prin1-to-string x))))

(defun at-stat:fmt (num prec)
  "数字格式化字符串（2 型小数，精度 prec，缺省 2 位）。"
  "字符串"
  "示例: (at-stat:fmt 3.14159 3) => \"3.142\""
  (if (numberp num)
      (rtos num 2
            (if (and (numberp prec) (>= prec 0) (<= prec 8)) prec 2))
      ""))

(defun at-stat:take (lst n / res)
  "取列表前 n 项（n 为 nil 或 >= 长度时返回原列表）。"
  "列表"
  "示例: (at-stat:take '(1 2 3 4) 2) => (1 2)"
  (if (and (numberp n) (>= n 0) (< n (length lst)))
      (progn
        (setq res nil)
        (while (and lst (> n 0))
          (setq res (cons (car lst) res)
                lst (cdr lst)
                n (1- n)))
        (reverse res))
      lst))

(defun at-stat:chunk (lst n / res cur)
  "按每行 n 个把列表分块（最后一块可不足 n 个）。"
  "二维列表"
  "示例: (at-stat:chunk '(1 2 3 4 5) 2) => ((1 2) (3 4) (5))"
  (setq res nil cur nil)
  (foreach x lst
    (setq cur (cons x cur))
    (if (= n (length cur))
        (progn
          (setq res (cons (reverse cur) res))
          (setq cur nil))))
  (if cur (setq res (cons (reverse cur) res)))
  (reverse res))

;; ---------- 几何辅助 ----------

(defun at-stat:pline-verts (ent / res pt b)
  "取多段线（LWPOLYLINE/POLYLINE）顶点：((x y) . 凸度) 列表。"
  "顶点列表"
  "示例: (at-stat:pline-verts (car (entsel)))"
  (cond
   ((= "LWPOLYLINE" (entity:getdxf ent 0))
    (progn
      (setq res nil pt nil b 0.0)
      (foreach x (entget ent)
        (cond
         ((= 10 (car x))
          (if pt
              (progn
                (setq res (cons (cons (list (car pt) (cadr pt)) b) res))
                (setq b 0.0)))
              (setq pt (cdr x))))
         ((= 42 (car x))
          (setq b (cadr x))))
      (if pt
          (setq res (cons (cons (list (car pt) (cadr pt)) b) res)))
      (reverse res)))
   ((= "POLYLINE" (entity:getdxf ent 0))
    (progn
      (setq res nil)
      (setq ent (entnext ent))
      (while (and ent (= "VERTEX" (cdr (assoc 0 (entget ent)))))
        (setq res (cons (cons (list (car (cdr (assoc 10 (entget ent))))
                                   (cadr (cdr (assoc 10 (entget ent)))))
                             (if (cdr (assoc 42 (entget ent))) (cdr (assoc 42 (entget ent))) 0.0))
                        res))
        (setq ent (entnext ent)))
      (reverse res)))
   (t nil)))

(defun at-stat:closed-p (ent / typ)
  "判断实体是否为封闭区域（闭合多段线/填充/实体/面域/闭合样条/整椭圆）。"
  "T 或 nil"
  "示例: (at-stat:closed-p (car (entsel)))"
  (if (and ent (= 'ename (type ent)))
      (progn
        (setq typ (entity:getdxf ent 0))
        (cond
         ((member typ '("LWPOLYLINE" "POLYLINE" "SPLINE"))
          (= 1 (logand (or (entity:getdxf ent 70) 0) 1)))
         ((member typ '("HATCH" "SOLID" "3DFACE" "REGION")) t)
         ((= typ "ELLIPSE")
          (< (abs (- (or (entity:getdxf ent 42) 0.0)
                     (* 2.0 pi))) 0.001))
         (t nil)))
      nil))

(defun at-stat:lwpl-area (ent / vs n i p q pt1 pt2 b1 c r th seg area)
  "闭合多段线面积（含圆弧段，凸度公式，返回绝对值）。"
  "面积；未闭合返回 nil"
  "示例: (at-stat:lwpl-area (car (entsel)))"
  (if (not (at-stat:closed-p ent))
      nil
      (progn
        (setq vs (at-stat:pline-verts ent))
        (if (< (length vs) 3)
            nil
            (progn
              (setq n (length vs) area 0.0 i 0)
              (while (< i n)
                (setq p (nth i vs)
                      q (nth (mod (1+ i) n) vs)
                      pt1 (car p) pt2 (car q)
                      b1 (cdr p)
                      c (distance pt1 pt2)
                      th (* 4.0 (atan b1)))
                ;; 鞋带公式
                (setq seg (* 0.5 (- (* (car pt1) (cadr pt2))
                                    (* (car pt2) (cadr pt1)))))
                ;; 圆弧段修正
                (if (and (/= b1 0.0)
                         (/= 0.0 (sin (/ th 2.0))))
                    (progn
                      (setq r (/ c (* 2.0 (sin (/ th 2.0)))))
                      (setq seg (+ seg (* 0.5 (* r r) (- th (sin th)))))))
                (setq area (+ area seg))
                (setq i (1+ i)))
              (abs area))))))

(defun at-stat:quad-area (p1 p2 p3 p4)
  "四边形面积（对角线叉积，返回绝对值）。"
  "面积"
  "示例: (at-stat:quad-area '(0 0) (4 0) (4 3) (0 3)) => 12.0"
  (abs (* 0.5
          (- (* (- (car p2) (car p1)) (- (cadr p4) (cadr p1)))
             (* (- (car p4) (car p1)) (- (cadr p2) (cadr p1)))))))

(defun at-stat:curve-area (ent / obj len n i pts p1 p2 area)
  "闭合曲线面积采样近似（鞋带公式，返回绝对值）。"
  "面积"
  "示例: (at-stat:curve-area (car (entsel)))"
  (setq obj (e2o ent)
        len (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj))
        n 256 pts nil area 0.0 i 0)
  (while (<= i n)
    (setq pts (cons (vlax-curve-getpointatdist obj (* len (/ i n))) pts))
    (setq i (1+ i)))
  (reverse pts)
  (setq i 0)
  (while (< i (length pts))
    (setq p1 (nth i pts)
          p2 (nth (mod (1+ i) (length pts)) pts))
    (setq area (+ area (* 0.5 (- (* (car p1) (cadr p2))
                                 (* (car p2) (cadr p1))))))
    (setq i (1+ i)))
  (abs area))

(defun at-stat:ent-area (ent / r)
  "封闭实体面积（闭合多段线/填充/实体/面域/闭合样条/整椭圆，绝对值）。"
  "面积；非封闭或出错返回 nil"
  "示例: (at-stat:ent-area (car (entsel)))"
  (cond
   ((null ent) nil)
   ((= 'str (type ent)) (if (setq ent (nameent ent))
                            (at-stat:ent-area ent)
                            nil))
   ((or (= "LWPOLYLINE" (entity:getdxf ent 0))
        (= "POLYLINE" (entity:getdxf ent 0)))
    (at-stat:lwpl-area ent))
   ((or (= "SOLID" (entity:getdxf ent 0))
        (= "3DFACE" (entity:getdxf ent 0)))
    (at-stat:quad-area (entity:getdxf ent 10) (entity:getdxf ent 11)
                          (entity:getdxf ent 12) (entity:getdxf ent 13)))
   ((= "ELLIPSE" (entity:getdxf ent 0))
    (if (at-stat:closed-p ent)
        (* pi (distance (entity:getdxf ent 10) (entity:getdxf ent 11))
           (distance (entity:getdxf ent 10) (entity:getdxf ent 12)))
        nil))
   (t
    (if (not (at-stat:closed-p ent))
        nil
        (progn
          (setq r (vl-catch-all-apply 'vla-get-area (list (e2o ent))))
          (if (and (numberp r) (not (vl-catch-all-error-p r)))
              (abs r)
              (vl-catch-all-apply 'at-stat:curve-area (list ent))))))))

(defun at-stat:ent-length (ent / r)
  "线型实体长度（LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE/SPLINE/ELLIPSE）。"
  "长度；不支持的类型返回 nil"
  "示例: (at-stat:ent-length (car (entsel)))"
  (cond
   ((null ent) nil)
   ((= "LINE" (entity:getdxf ent 0))
    (distance (entity:getdxf ent 10) (entity:getdxf ent 11)))
   ((= "CIRCLE" (entity:getdxf ent 0))
    (* 2.0 pi (if (entity:getdxf ent 40) (entity:getdxf ent 40) 0.0)))
   ((member (entity:getdxf ent 0)
            '("LWPOLYLINE" "POLYLINE" "ARC" "SPLINE" "ELLIPSE"))
    (if (setq r (vl-catch-all-apply 'curve:length (list ent)))
        (if (numberp r) r nil)
        nil))
   (t nil)))

(defun at-stat:centroid (ent / b)
  "实体中心点：多段线用顶点形心，其他用包围盒中心。"
  "点坐标"
  "示例: (at-stat:centroid (car (entsel)))"
  (cond
   ((or (= "LWPOLYLINE" (entity:getdxf ent 0))
        (= "POLYLINE" (entity:getdxf ent 0)))
    (point:centroid (mapcar 'car (at-stat:pline-verts ent))))
   (t
    (progn
      (setq b (entity:getbox ent 0))
      (if b
          (list (/ (+ (car (car b)) (car (cadr b))) 2.0)
                (/ (+ (cadr (car b)) (cadr (cadr b))) 2.0) 0.0)
          '(0 0 0))))))

(defun at-stat:pt-in-poly (pt pts / i p1 p2 in)
  "2D 点是否在 2D 闭合点列内部（射线法）。"
  "T 或 nil"
  "示例: (at-stat:pt-in-poly '(5 5) '((0 0) (10 0) (10 10) (0 10)))"
  (if (< (length pts) 3)
      nil
      (progn
        (setq i 0 in nil)
        (while (< i (length pts))
          (setq p1 (nth i pts)
                p2 (nth (mod (1+ i) (length pts)) pts))
          (if (and (/= (car p1) (car p2))
                   (> (max (car p1) (car p2)) (car pt))
                   (< (min (car p1) (car p2)) (car pt))
                   (< (cadr pt)
                      (+ (cadr p1)
                         (* (/ (- (cadr p2) (cadr p1))
                                (- (car p2) (car p1)))
                            (- (car pt) (car p1))))))
              (setq in (not in)))
          (setq i (1+ i)))
        in)))

(defun at-stat:pt-in-ent-p (pt ent / pt2 ray far n)
  "点是否在封闭实体内部（2D 判定，取 X/Y 坐标）。"
  "T 或 nil"
  "示例: (at-stat:pt-in-ent-p (getpoint) (car (entsel)))"
  (if (or (null ent) (not (at-stat:closed-p ent)))
      nil
      (progn
        (setq pt2 (list (car pt) (cadr pt)))
        (cond
         ((member (entity:getdxf ent 0) '("LWPOLYLINE" "POLYLINE"))
          (at-stat:pt-in-poly pt2
                                (mapcar 'car (at-stat:pline-verts ent))))
         ((= "SOLID" (entity:getdxf ent 0))
          (at-stat:pt-in-poly pt2 (list (entity:getdxf ent 10)
                                          (entity:getdxf ent 11)
                                          (entity:getdxf ent 12)
                                          (entity:getdxf ent 13))))
         (t
          (progn
            (setq far (list (+ (car pt) 1e9) (cadr pt) 0.0))
            (setq ray (entmakex (list (cons 0 "LINE")
                                      (cons 10 pt2)
                                      (cons 11 far)
                                      (cons 8 "0"))))
            (setq n (if ray
                        (length (vlax-invoke (e2o ent)
                                              'intersectwith
                                              (e2o ray) 0))
                        0))
            (if ray (entdel ray))
            (/= 0 (rem n 2))))))))

(defun at-stat:all-closed-ents ( / ss)
  "取模型空间所有封闭区域实体。"
  "图元列表"
  "示例: (at-stat:all-closed-ents)"
  (setq ss (ssget "x"
                  '((0 . "LWPOLYLINE,POLYLINE,HATCH,SOLID,3DFACE,REGION,SPLINE,ELLIPSE"))))
  (vl-remove-if-not 'at-stat:closed-p (pickset:to-list ss)))

(defun at-stat:find-boundary (pt ents / best best-area e a)
  "找包含点 pt 的闭合边界（取最内层，即面积最小者）。
  pt: 点；ents: 候选封闭实体列表，nil 时全图查找。"
  "边界 ename；未找到返回 nil"
  "示例: (at-stat:find-boundary (getpoint) nil)"
  (if (null ents)
      (setq ents (at-stat:all-closed-ents)))
  (setq best nil best-area nil)
  (foreach e ents
    (if (and (at-stat:pt-in-ent-p pt e)
             (setq a (at-stat:ent-area e))
             (or (null best-area) (< a best-area)))
        (progn
          (setq best e)
          (setq best-area a))))
  best)

;; ---------- 表格/文件辅助 ----------

(defun at-stat:merge-box (b1 b2 / p1 p2)
  "合并两个包围盒 (p1 p2)。"
  "包围盒；入参为 nil 时返回另一个"
  "示例: (at-stat:merge-box (entity:getbox e1 0) (entity:getbox e2 0))"
  (if (null b1) b2
      (if (null b2) b1
          (progn
            (setq p1 (mapcar 'min (car b1) (car b2))
                  p2 (mapcar 'max (cadr b1) (cadr b2)))
            (list p1 p2)))))

(defun at-stat:box-size-str (box)
  "包围盒尺寸字符串 WxH（绘图单位）。"
  "字符串"
  "示例: (at-stat:box-size-str '((0 0) (4 3))) => \"4.00x3.00\""
  (if (null box)
      ""
      (strcat (at-stat:fmt (- (car (cadr box)) (car (car box))) 2)
              "x"
              (at-stat:fmt (- (cadr (cadr box)) (cadr (car box))) 2))))

(defun at-stat:ensure-layer (name)
  "图层不存在则创建，并设为当前图层。"
  "T"
  "示例: (at-stat:ensure-layer \"STAT\")"
  (if (null name)
      nil
      (progn
        (if (null (tblobjname "layer" name))
            (progn
              (layer:make name 7 "Continuous" 0)
              (princ (strcat "\nat-stat: 已创建图层 " name))))
        (setvar "clayer" name)
        t)))

(defun at-stat:ent-layer (ent layer / ed)
  "设置图元图层。"
  "T"
  "示例: (at-stat:ent-layer (car (entsel)) \"0\")"
  (if (and ent layer (/= (entity:getdxf ent 8) layer))
      (progn
        (setq ed (entget ent))
        (if (assoc 8 ed)
            (setq ed (subst (cons 8 layer) (assoc 8 ed) ed))
            (setq ed (cons (cons 8 layer) ed)))
        (entmod ed)))
  t)

(defun at-stat:make-table (title header data pt / ent)
  "在图中绘制统计表（基于 table:make）。
  title: 标题；header: 表头列表；data: 二维矩阵；pt: 插入点。"
  "表格 ename；data 为空返回 nil"
  "示例: (at-stat:make-table \"统计\" '(\"名称\" \"数量\") '((\"A\" 3)) '(0 0 0))"
  (if (null data)
      (progn (princ "\nat-stat: 无数据，未绘制表格。") nil)
      (progn
        (if (null table:make) (require 'table:*))
        (setq ent (vl-catch-all-apply 'table:make (list pt title header data)))
        (if (vl-catch-all-error-p ent)
            (progn
              (princ (strcat "\nat-stat: 绘制表格失败: "
                             (vl-catch-all-error-message ent)))
              nil)
            ent))))

(defun at-stat:to-csv (data file / fp *error*)
  "二维表写入 CSV 文件（第一行作表头）。
  data: 二维矩阵（数字/字符串）；file: 路径。"
  "成功返回文件路径，失败返回 nil"
  "示例: (at-stat:to-csv '((\"A\" 1)) \"d:/stat.csv\")"
  (defun *error* (msg)
    (if (and fp (eq 'int (type fp))) (close fp))
    (princ (strcat "\nat-stat: 写 CSV 失败: " msg))
    nil)
  (if (null data)
      (progn (princ "\nat-stat: 无数据，未写 CSV。") nil)
      (progn
        (setq fp (open file "w"))
        (if (null fp)
            (progn (princ (strcat "\nat-stat: 无法打开文件 " file)) nil)
            (progn
              (foreach row data
                (write-line (string:from-list (mapcar 'at-stat:to-str row) ",") fp))
              (close fp)
              (princ (strcat "\nat-stat: 已写入 " file))
              file)))))

(defun at-stat:ext (file / i pos)
  "取文件扩展名（大写，不含点）。"
  "字符串；无扩展名返回 nil"
  "示例: (at-stat:ext \"a/b.CSV\") => \"CSV\""
  (if (and (stringp file) (> (strlen file) 1))
      (progn
        (setq i 0 pos -1)
        (while (and (< i (strlen file))
                    (setq pos (vl-string-search "." file i)))
          (setq i (1+ pos)))
        (if (>= pos 0) (strcase (substr file (+ pos 1))) nil))
      nil))

(defun at-stat:excel-col (i)
  "Excel 列号转列字母（1=A，26=Z，27=AA）。"
  "字符串"
  "示例: (at-stat:excel-col 27) => \"AA\""
  (if (<= i 26)
      (chr (+ 64 i))
      (strcat (at-stat:excel-col (/ (1- i) 26))
              (chr (+ 65 (rem (1- i) 26)))))
  )

(defun at-stat:excel-set-row (xl row rownum / addr)
  "把一行（列表）写入 Excel 指定行。"
  "T"
  "示例: (at-stat:excel-set-row xl '(\"A\" 1) 1)"
  (setq addr (strcat (at-stat:excel-col 1) (itoa rownum)))
  (if (> (length row) 1)
      (setq addr (strcat addr ":" (at-stat:excel-col (length row)) (itoa rownum))))
  (excel:set-rangevalue xl addr row)
  t)

(defun at-stat:write-xlsx (data file / xl i row *error*)
  "用 Excel COM 写二维表到 xlsx（需安装 Excel，Excel 后台运行）。
  data: 二维矩阵；file: 路径。"
  "成功返回文件路径，失败返回 nil"
  "示例: (at-stat:write-xlsx '((\"A\" 1)) \"d:/stat.xlsx\")"
  (defun *error* (msg)
    (if xl (vl-catch-all-apply 'vlax-release-object (list xl)))
    (princ (strcat "\nat-stat: 写 Excel 失败: " msg))
    nil)
  (if (null excel:new) (require 'excel:*))
  (setq xl (vl-catch-all-apply 'excel:new (list nil)))
  (if (vl-catch-all-error-p xl)
      (progn (princ "\nat-stat: 未检测到 Excel，无法创建。") nil)
      (progn
        (setq i 0)
        (foreach row data
          (setq i (1+ i))
          (at-stat:excel-set-row xl row i))
        (vlax-invoke (vlax-get-property xl 'ActiveWorkbook) 'SaveAs file)
        (vlax-invoke (vlax-get-property xl 'ActiveWorkbook) 'Close)
        (vlax-invoke xl 'Quit)
        (vlax-release-object xl)
        (princ (strcat "\nat-stat: 已写入 " file))
        file)))

(defun at-stat:write-table-file (data file / res ext)
  "二维表写文件：.csv 写 CSV；其他扩展名优先 Excel COM，失败回退 CSV。
  data: 二维矩阵（第一行表头）；file: 路径。"
  "实际写入的文件路径；失败返回 nil"
  "示例: (at-stat:write-table-file '((\"A\" 1)) \"d:/stat.xlsx\")"
  (if (null data)
      (progn (princ "\nat-stat: 无数据，未写文件。") nil)
      (progn
        (setq ext (at-stat:ext file))
        (cond
         ((or (null ext) (= "CSV" ext))
          (at-stat:to-csv data (if ext file (strcat file ".csv"))))
         (t
          (progn
            (setq res (vl-catch-all-apply 'at-stat:write-xlsx (list data file)))
            (if (or (vl-catch-all-error-p res) (null res))
                (progn
                  (princ "\nat-stat: Excel 不可用，回退 CSV 输出。")
                  (at-stat:to-csv data
                                     (strcat (vl-string-left-truncate
                                              (+ (strlen ext) 1)
                                              file)
                                             ".csv")))
                res)))))))

;; ---------- 选择辅助 ----------

(defun at-stat:ss-ents (ss)
  "选择集转图元 ename 列表（ss 为 nil 返回空表）。"
  "图元列表"
  "示例: (at-stat:ss-ents (ssget '((0 . \"*\"))))"
  (if ss (pickset:to-list ss) nil))

(defun at-stat:pick-ents (prompt filter / ss)
  "交互选择图元（ssget），返回 ename 列表，取消返回 nil。"
  "图元列表"
  "示例: (at-stat:pick-ents \"选择图元: \" '((0 . \"*\")))"
  (setq ss (ssget filter))
  (if ss (pickset:to-list ss) nil))

(defun at-stat:to-ents (x)
  "选择集(pickset)或列表转图元 ename 列表（其他返回 nil）。"
  "图元列表"
  "示例: (at-stat:to-ents (ssget \"x\" '((0 . \"*\"))))"
  (cond
   ((null x) nil)
   ((eq 'pickset (type x)) (pickset:to-list x))
   ((listp x) x)
   (t nil)))

(defun at-stat:trim (str)
  "去除字符串两端空白（空格/制表符/换行）。"
  "字符串"
  "示例: (at-stat:trim \"  abc \") => \"abc\""
  (if (and str (= 'str (type str)))
      (vl-string-trim " \t\r\n" str)
      ""))

(defun at-stat:str-replace (old new str / res i p)
  "把 str 中所有 old 替换为 new（精确匹配，未找到返回原串）。"
  "字符串"
  "示例: (at-stat:str-replace \"a\" \"b\" \"abc\") => \"bbc\""
  (if (or (null str) (null old) (= 0 (strlen old))
          (null (vl-string-search old str)))
      str
      (progn
        (setq res "" i 0)
        (while (setq p (vl-string-search old str i))
          (setq res (strcat res (substr str (+ i 1) (- p i)) new))
          (setq i (+ p (strlen old))))
        (if (< i (strlen str))
            (setq res (strcat res (substr str (+ i 1) (- (strlen str) i)))))
        res)))

(defun at-stat:clean-text (str)
  "规范文字内容：换行符(\\n \\r \\P)转空格并去除两端空白。"
  "字符串"
  "示例: (at-stat:clean-text \"a\\nb\") => \"a b\""
  (at-stat:trim
   (at-stat:str-replace "\P" " "
    (at-stat:str-replace "\r\n" " "
     (at-stat:str-replace "\n" " "
      (if (and str (= 'str (type str))) str ""))))))
