;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-hvac AI 入口：风机统计、风机参数查询、分集水器平衡分析、设备 BOM
;; 供 MCP eval_lisp / Agent 调用，不弹窗、不取点、不调用 grread。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @hvac:sel-list (sel)
  "把选择集、单个 ename 或 ename 列表统一为 ename 列表。"
  "ename 列表或 nil"
  "(@hvac:sel-list (ssget))"
  (cond
    ((null sel) nil)
    ((p:picksetp sel) (pickset:to-list sel))
    ((p:enamep sel) (list sel))
    ((p:ename-listp sel) sel)
    (t nil)))

(defun @hvac:block-attrib (blk tag / att val)
  "按标记读取普通块属性值，找不到返回 nil。"
  "字符串或 nil"
  "(@hvac:block-attrib (car (entsel)) \"型号\")"
  (setq att (entnext blk))
  (while (and att (= "ATTRIB" (entity:getdxf att 0)))
    (if (= tag (entity:getdxf att 2))
        (setq val (entity:getdxf att 1)))
    (setq att (entnext att)))
  val)

(defun @hvac:block-model (blk / v)
  "读取风机的型号：优先动态属性“型号”，其次块属性“型号”/MODEL。"
  "字符串或 nil"
  "(@hvac:block-model (car (entsel)))"
  (if (p:enamep blk)
      (progn
        (setq v (vl-catch-all-apply 'block:get-dynprop (list blk "型号")))
        (if (vl-catch-all-error-p v) (setq v nil))
        (if (null v) (setq v (@hvac:block-attrib blk "型号")))
        (if (null v) (setq v (@hvac:block-attrib blk "MODEL")))
        (if (and (stringp v) (> (strlen v) 0)) v nil))
      nil))

(defun @hvac:ai-fengji-stat (sel / ents models)
  "AI 入口：统计风机型号数量，返回 ((型号 . 数量) ...)；未知型号保留为 \"未知型号\"。
  sel 为 nil 时统计全图“方壁式轴流风机”块。
  示例: (@hvac:ai-fengji-stat (ssget '((0 . \"INSERT\"))))"
  "点对表或 nil"
  "(@hvac:ai-fengji-stat nil)"
  (setq ents
        (if (null sel)
            (pickset:to-list
             (ssget "_X" '((0 . "INSERT") (2 . "方壁式轴流风机"))))
            (@hvac:sel-list sel)))
  (setq models
        (mapcar '(lambda (x / m)
                   (setq m (@hvac:block-model x))
                   (if m m "未知型号"))
                ents))
  (if models
      (vl-sort (stat:stat models) '(lambda (a b) (< (car a) (car b))))
      nil))

(defun @hvac:ai-fengji-list ()
  "AI 入口：返回内置风机盘管型号参数列表。
  每项为 ((model . 型号) (name . 全称) (cool . 冷量W) (heat . 热量W) (power . 功率W) (air . 风量m3/h))。"
  "点对表列表或 nil"
  "(@hvac:ai-fengji-list)"
  (if @hvac:*fengji*
      (mapcar
       '(lambda (x)
          (list (cons "model" (nth 0 x))
                (cons "name" (nth 1 x))
                (cons "cool" (nth 2 x))
                (cons "heat" (nth 3 x))
                (cons "power" (nth 4 x))
                (cons "air" (nth 5 x))))
       @hvac:*fengji*)
      nil))

(defun @hvac:ai-fengji-info (model / x)
  "AI 入口：按型号返回风机参数；未找到返回 nil。
  示例: (@hvac:ai-fengji-info \"FP85\")"
  "点对表或 nil"
  "(@hvac:ai-fengji-info \"FP85\")"
  (if (and model @hvac:*fengji*)
      (progn
        (setq x (assoc model @hvac:*fengji*))
        (if x
            (list (cons "model" (nth 0 x))
                  (cons "name" (nth 1 x))
                  (cons "cool" (nth 2 x))
                  (cons "heat" (nth 3 x))
                  (cons "power" (nth 4 x))
                  (cons "air" (nth 5 x)))
            nil))
      nil))

(defun @hvac:manifold-stat (ent / box pipes lens total pmax pmin diff)
  "统计一个采暖分集水器的地暖回路长度，返回点对表；失败 nil。"
  "点对表或 nil"
  "(@hvac:manifold-stat (car (entsel)))"
  (if (and (p:enamep ent)
           (setq box (entity:getbox ent 100))
           (car box) (cadr box))
      (progn
        (setq pipes
              (pickset:to-list
               (ssget "c" (car box) (cadr box)
                      '((0 . "LWPOLYLINE") (8 . "*地暖*")))))
        (setq lens (mapcar '(lambda (x) (* 0.001 (curve:length x))) pipes))
        (if lens
            (progn
              (setq total (apply '+ lens))
              (setq pmax (apply 'max lens))
              (setq pmin (apply 'min lens))
              (setq diff (- pmax pmin))
              (list (cons "circuits" (length lens))
                    (cons "total" total)
                    (cons "max" pmax)
                    (cons "min" pmin)
                    (cons "diff" diff)
                    (cons "over" (if (> diff 15.0) T nil))))
            nil))
      nil))

(defun @hvac:ai-equip-balance (sel / ents manifolds res i)
  "AI 入口：分集水器平衡分析，返回每个采暖分集水器的回路数/总长/最长/最短/差值/超限。
  sel 为 nil 时取全图 TCH_DBHEQUIPMENT。
  示例: (@hvac:ai-equip-balance nil)"
  "点对表列表或 nil"
  "(@hvac:ai-equip-balance nil)"
  (setq ents
        (if (null sel)
            (pickset:to-list (ssget "_X" '((0 . "TCH_DBHEQUIPMENT"))))
            (@hvac:sel-list sel)))
  (setq manifolds
        (vl-remove-if-not
         '(lambda (x / v)
            (setq v (vl-catch-all-apply
                     'vlax-get-property (list (e2o x) 'hvac_s5)))
            (and (not (vl-catch-all-error-p v))
                 (equal v "采暖分集水器")))
         ents))
  (setq res nil i 0)
  (foreach m manifolds
    (setq i (1+ i))
    (setq res (cons (cons (cons "index" i)
                          (@hvac:manifold-stat m))
                    res)))
  (if res (reverse res) nil))

(defun @hvac:ai-equip-bom (sel / ents manifolds models bom)
  "AI 入口：分集水器设备 BOM，返回 ((型号 . 数量) ...)，型号格式 FPQ-回路数。
  sel 为 nil 时取全图 TCH_DBHEQUIPMENT。
  示例: (@hvac:ai-equip-bom nil)"
  "点对表或 nil"
  "(@hvac:ai-equip-bom nil)"
  (setq ents
        (if (null sel)
            (pickset:to-list (ssget "_X" '((0 . "TCH_DBHEQUIPMENT"))))
            (@hvac:sel-list sel)))
  (setq manifolds
        (vl-remove-if-not
         '(lambda (x / v)
            (setq v (vl-catch-all-apply
                     'vlax-get-property (list (e2o x) 'hvac_s5)))
            (and (not (vl-catch-all-error-p v))
                 (equal v "采暖分集水器")))
         ents))
  (setq models
        (vl-remove nil
                   (mapcar
                    '(lambda (m / st)
                       (setq st (@hvac:manifold-stat m))
                       (if st (strcat "FPQ-" (itoa (cdr (assoc "circuits" st)))) nil))
                    manifolds)))
  (setq bom (if models (stat:stat models) nil))
  (if bom (vl-sort bom '(lambda (a b) (< (car a) (car b)))) nil))
