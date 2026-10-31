;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-stat 计数统计（count）
;; 块统计表 / 文字统计表 / 同形状统计 / 长度分层统计表 / 面积分层统计表
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------- 数据汇总 ----------

(defun at-stat:layer-ents (layers / ss)
  "取模型空间指定图层的图元（layers: 图层名列表，nil = 全部图层）。"
  "图元列表"
  "示例: (at-stat:layer-ents '(\"WALL\" \"DOOR\"))"
  (if (null string:from-list) (require 'string:*))
  (setq ss (ssget "x"
                  (list (cons 8
                             (if (null layers)
                                 "*"
                                 (string:from-list layers ","))))))
  (at-stat:ss-ents ss))

(defun at-stat:block-data (ents / res ent name box rec)
  "块统计数据：按块名汇总 INSERT（数量 + 合并外尺寸）。
  ents: INSERT 图元列表。"
  "二维矩阵: ((名称 数量 尺寸WxH) ...)，按数量降序"
  "示例: (at-stat:block-data (at-stat:pick-ents \"选: \" '((0 . \"INSERT\"))))"
  (if (null block:get-effectivename) (require 'block:*))
  (setq res nil)
  (foreach ent ents
    (if (= "INSERT" (entity:getdxf ent 0))
        (progn
          (setq name (block:get-effectivename ent))
          (setq box (entity:getbox ent 0))
          (if (setq rec (assoc name res))
              (progn
                (setq res (subst (list name (1+ (cadr rec))
                                        (at-stat:merge-box (caddr rec) box)) rec res)))
              (setq res (cons (list name 1 box) res))))))
  (mapcar (function (lambda (r)
                      (list (car r) (cadr r)
                            (at-stat:box-size-str (caddr r)))))
          (sort res (function (lambda (a b) (> (cadr a) (cadr b)))))))

(defun at-stat:text-data (ents / res ent content style height rec)
  "文字统计数据：按内容汇总 TEXT/MTEXT（数量/样式/字高）。
  ents: 文字图元列表。"
  "二维矩阵: ((内容 数量 样式 字高) ...)，按数量降序"
  "示例: (at-stat:text-data (at-stat:pick-ents \"选: \" '((0 . \"TEXT,MTEXT\"))))"
  (if (null text:get-mtext) (require 'text:*))
  (setq res nil)
  (foreach ent ents
    (cond
     ((= "TEXT" (entity:getdxf ent 0))
      (setq content (at-stat:trim (entity:getdxf ent 1))
            style (if (entity:getdxf ent 7) (entity:getdxf ent 7) "Standard")
            height (if (entity:getdxf ent 40) (entity:getdxf ent 40) 0.0)))
     ((= "MTEXT" (entity:getdxf ent 0))
      (setq content (at-stat:clean-text (text:get-mtext ent))
            style (if (entity:getdxf ent 7) (entity:getdxf ent 7) "Standard")
            height (if (entity:getdxf ent 40) (entity:getdxf ent 40) 0.0)))
     (t nil))
    (if content
        (if (setq rec (assoc content res))
            (setq res (subst (list (nth 0 rec) (1+ (cadr rec)) (nth 2 rec) (nth 3 rec)) rec res))
            (setq res (cons (list content 1 style height) res)))))
  (sort res (function (lambda (a b) (> (cadr a) (cadr b))))))

(defun at-stat:shape-data (ents tol / res ent typ dim key rec)
  "同形状统计数据：按类型(面积/长度) + 尺寸(面积值或长度值)汇总。
  ents: 图元列表；tol: 分组容差（0 或 nil = 精确）。"
  "二维矩阵: ((类型 尺寸 数量) ...)，按数量降序"
  "示例: (at-stat:shape-data (at-stat:pick-ents \"选: \" '((0 . \"*\"))) 0.1)"
  (setq res nil)
  (foreach ent ents
    (cond
     ((null ent) nil)
     ((at-stat:closed-p ent)
      (if (setq dim (at-stat:ent-area ent))
          (setq typ "面积")))
     (t
      (if (setq dim (at-stat:ent-length ent))
          (setq typ "长度"))))
    (if (and typ dim)
        (progn
          (setq key (if (and (numberp tol) (> tol 0.0))
                        (round (* dim (/ 1.0 tol)))
                        dim))
          (if (setq rec (assoc key res))
              (setq res (subst (list (nth 0 rec) (nth 1 rec) (nth 2 rec) (1+ (nth 3 rec))) rec res))
              (setq res (cons (list key typ dim 1) res))))))
  (mapcar (function (lambda (r) (list (cadr r) (caddr r) (nth 3 r))))
          (sort res (function (lambda (a b) (> (nth 3 a) (nth 3 b)))))))

(defun at-stat:length-by-layer (layers / res ent layer dim rec)
  "长度分层数据：按图层汇总线型实体长度。
  layers: 图层名列表，nil = 全部图层。"
  "二维矩阵: ((图层 长度) ...)，按长度降序"
  "示例: (at-stat:length-by-layer '(\"WALL\"))"
  (setq res nil)
  (foreach ent (at-stat:layer-ents layers)
    (if (setq dim (at-stat:ent-length ent))
        (progn
          (setq layer (entity:getdxf ent 8))
          (if (setq rec (assoc layer res))
              (setq res (subst (list layer (+ (cadr rec) dim)) rec res))
              (setq res (cons (list layer dim) res))))))
  (sort res (function (lambda (a b) (> (cadr a) (cadr b))))))

(defun at-stat:area-by-layer (layers factor / res ent layer a rec)
  "面积分层数据：按图层汇总封闭实体面积 × 换算系数。
  layers: 图层名列表，nil = 全部；factor: 换算系数（nil = 0.0001，即 mm2→m2）。"
  "二维矩阵: ((图层 面积) ...)，按面积降序"
  "示例: (at-stat:area-by-layer nil 1.0)"
  (if (null factor) (setq factor 0.0001))
  (setq res nil)
  (foreach ent (at-stat:layer-ents layers)
    (if (and (at-stat:closed-p ent) (setq a (at-stat:ent-area ent)))
        (progn
          (setq layer (entity:getdxf ent 8))
          (if (setq rec (assoc layer res))
              (setq res (subst (list layer (+ (cadr rec) a)) rec res))
              (setq res (cons (list layer a) res))))))
  (mapcar (function (lambda (r)
                      (list (car r) (* (cadr r) factor))))
          (sort res (function (lambda (a b) (> (cadr a) (cadr b)))))))

;; ---------- 交互辅助 ----------

(defun at-stat:split-str (str sep / i pos res seg)
  "按分隔符（单字符）分割字符串，跳过空段。"
  "字符串列表"
  "示例: (at-stat:split-str \"a,b, c ,\" \",\") => (\"a\" \"b\" \"c\")"
  (if (or (null str) (= 0 (strlen str)))
      nil
      (progn
        (setq res nil i 0)
        (while (setq pos (vl-string-search sep str i))
          (setq seg (at-stat:trim (substr str (+ i 1) (- pos i))))
          (if (> (strlen seg) 0) (setq res (cons seg res)))
          (setq i (+ pos 1)))
        (if (> (strlen str) i)
            (progn
              (setq seg (at-stat:trim (substr str (+ i 1)
                                                 (- (strlen str) i))))
              (if (> (strlen seg) 0) (setq res (cons seg res)))))
        (reverse res))))

(defun at-stat:pick-layers (/ str)
  "交互输入图层名（逗号分隔；空 = 全部图层；Esc = 取消）。"
  "图层名列表；取消返回 nil"
  "示例: (at-stat:pick-layers)"
  (setq str (getstring t "\n输入图层名(逗号分隔, 空=全部): "))
  (if str
      (at-stat:split-str str ",")
      nil))

;; ---------- 交互入口 ----------

(defun at-stat:block-list (/ ents data pt)
  "块统计表：选择块后按名称汇总（数量 + 外尺寸），绘制表格。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择块: " '((0 . "INSERT"))))
  (if ents
      (progn
        (setq data (at-stat:block-data ents))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "块统计表"
                                  '("块名" "数量" "尺寸(WxH)")
                                  data pt))))
  (princ))

(defun at-stat:text-list (/ ents data pt)
  "文字统计表：选择文字后按内容汇总（数量/样式/字高），绘制表格。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择文字(TEXT/MTEXT): "
                                  '((0 . "TEXT,MTEXT"))))
  (if ents
      (progn
        (setq data (at-stat:text-data ents))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "文字统计表"
                                  '("内容" "数量" "样式" "字高")
                                  data pt))))
  (princ))

(defun at-stat:shape-list (/ ents tol data pt)
  "同形状统计：选择图元后按类型 + 尺寸(长度/面积)汇总，绘制表格。"
  ""
  ""
  (setq ents (at-stat:pick-ents "选择图元: " '((0 . "*"))))
  (if ents
      (progn
        (setq tol (getreal "\n分组容差<0=精确>: "))
        (setq data (at-stat:shape-data ents (if tol tol 0.0)))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "同形状统计表"
                                  '("类型" "尺寸" "数量")
                                  data pt))))
  (princ))

(defun at-stat:total-length (/ layers data pt)
  "长度分层统计表：输入图层后按图层汇总线型实体长度，绘制表格。"
  ""
  ""
  (if (setq layers (at-stat:pick-layers))
      (progn
        (setq data (at-stat:length-by-layer layers))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "长度分层统计表"
                                  '("图层" "长度")
                                  data pt))))
  (princ))

(defun at-stat:by-layer-area (/ layers factor data pt)
  "面积分层统计表：输入图层后按图层汇总封闭实体面积(×换算系数)，绘制表格。"
  ""
  ""
  (if (setq layers (at-stat:pick-layers))
      (progn
        (setq factor (getreal "\n面积换算系数(0.0001=mm2→m2)<0.0001>: "))
        (setq data (at-stat:area-by-layer layers (if factor factor 0.0001)))
        (if (and data (setq pt (getpoint "\n指定表格插入点: ")))
            (at-stat:make-table "面积分层统计表"
                                  '("图层" "面积")
                                  data pt))))
  (princ))

;; ---------- AI 入口 ----------

(defun at-stat:ai-block-list (ss top-n out-file / ents data)
  "AI 调用入口：块统计表（按块名汇总，数量降序），无交互、不弹窗。
  ss: 选择集或 INSERT 图元列表；top-n: 只取前 n（nil = 全部）；
  out-file: 输出文件路径（.csv/.xlsx，nil = 不写文件）。
  return: 二维矩阵 ((名称 数量 尺寸WxH) ...)；无数据返回 nil。
  示例: (at-stat:ai-block-list (ssget \"x\" '((0 . \"INSERT\"))) 10 \"d:/blk.csv\")"
  (setq ents (at-stat:to-ents ss))
  (if ents
      (progn
        (setq data (at-stat:block-data ents))
        (if (and (numberp top-n) (> top-n 0))
            (setq data (at-stat:take data top-n)))
        (if (and out-file data)
            (at-stat:write-table-file
             (cons '("块名" "数量" "尺寸(WxH)") data) out-file))
        (if data data nil))
      nil))

(defun at-stat:ai-text-list (ss out-file / ents data)
  "AI 调用入口：文字统计表（TEXT/MTEXT 按内容汇总），无交互、不弹窗。
  ss: 选择集或文字图元列表；out-file: 输出文件路径（nil = 不写文件）。
  return: 二维矩阵 ((内容 数量 样式 字高) ...)；无数据返回 nil。
  示例: (at-stat:ai-text-list (ssget \"x\" '((0 . \"TEXT,MTEXT\"))) \"d:/txt.csv\")"
  (setq ents (at-stat:to-ents ss))
  (if ents
      (progn
        (setq data (at-stat:text-data ents))
        (if (and out-file data)
            (at-stat:write-table-file
             (cons '("内容" "数量" "样式" "字高") data) out-file))
        (if data data nil))
      nil))

(defun at-stat:ai-shape-list (ss tol / ents data)
  "AI 调用入口：同形状统计（按类型 + 尺寸汇总），无交互、不弹窗。
  ss: 选择集或图元列表；tol: 分组容差（0 = 精确）。
  return: 二维矩阵 ((类型 尺寸 数量) ...)；无数据返回 nil。
  示例: (at-stat:ai-shape-list (ssget \"x\" '((0 . \"*\"))) 0.1)"
  (setq ents (at-stat:to-ents ss))
  (if ents
      (at-stat:shape-data ents (if (and (numberp tol) tol) (and (numberp tol) tol) 0.0))
      nil))

(defun at-stat:ai-total-length (layers out-file / data)
  "AI 调用入口：长度分层统计（按图层汇总线型实体长度），无交互、不弹窗。
  layers: 图层名列表（nil = 全部图层）；out-file: 输出文件路径（nil = 不写文件）。
  return: 二维矩阵 ((图层 长度) ...)；无数据返回 nil。
  示例: (at-stat:ai-total-length '(\"WALL\") nil)"
  (setq data (at-stat:length-by-layer layers))
  (if (and out-file data)
      (at-stat:write-table-file (cons '("图层" "长度") data) out-file))
  (if data data nil))

(defun at-stat:ai-by-layer-area (layers factor out-file / data)
  "AI 调用入口：面积分层统计（按图层汇总封闭实体面积 × factor），无交互、不弹窗。
  layers: 图层名列表（nil = 全部）；factor: 换算系数（nil = 0.0001）；
  out-file: 输出文件路径（nil = 不写文件）。
  return: 二维矩阵 ((图层 面积) ...)；无数据返回 nil。
  示例: (at-stat:ai-by-layer-area nil 1.0 \"d:/area.csv\")"
  (setq data (at-stat:area-by-layer layers factor))
  (if (and out-file data)
      (at-stat:write-table-file (cons '("图层" "面积") data) out-file))
  (if data data nil))
