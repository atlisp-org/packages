;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-elec AI 入口：无交互的负荷统计、设备统计、回路编号、电缆标注
;; 供 MCP eval_lisp / Agent 直接调用，不依赖 ssget/getstring/entsel。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 选择集/图元统一转成 ename 列表
(defun @elec:sel-list (sel)
  "把选择集、单个 ename 或 ename 列表统一为 ename 列表。"
  "ename 列表或 nil"
  "(at-elec:sel-list (ssget))"
  (cond
    ((null sel) nil)
    ((p:picksetp sel) (pickset:to-list sel))
    ((p:enamep sel) (list sel))
    ((p:ename-listp sel) sel)
    (t nil)))

(defun @elec:draw-scale (/ s)
  "取当前出图比例，缺失时返回 1.0。"
  "Real"
  "(@elec:draw-scale)"
  (setq s (@:get-config '@::draw-scale))
  (if (and (numberp s) (> s 0.0)) s 1.0))

(defun @elec:block-attrib (blk tag / att val)
  "按标记读取普通块属性值，找不到返回 nil。"
  "字符串或 nil"
  "(@elec:block-attrib (car (entsel)) \"功率\")"
  (setq att (entnext blk))
  (while (and att (= "ATTRIB" (entity:getdxf att 0)))
    (if (= tag (entity:getdxf att 2))
        (setq val (entity:getdxf att 1)))
    (setq att (entnext att)))
  val)

(defun @elec:block-watt (blk / v)
  "读取设备块的功率：优先动态属性“功率”，其次普通块属性“功率”。"
  "Real 或 nil"
  "(at-elec:block-watt (car (entsel)))"
  (if (p:enamep blk)
      (progn
        (setq v (vl-catch-all-apply 'block:get-dynprop (list blk "功率")))
        (if (vl-catch-all-error-p v) (setq v nil))
        (if (null v) (setq v (@elec:block-attrib blk "功率")))
        (if (null v) (setq v (@elec:block-attrib blk "POWER")))
        (cond
          ((null v) nil)
          ((numberp v) v)
          ((and (stringp v) (> (strlen v) 0)) (atof v))
          (t nil)))
      nil))

(defun @elec:stat-load (ents / total count watt k)
  "统计设备块负荷，返回 ((\"count\" . n) (\"total\" . w) (\"demand\" . w) (\"factor\" . k))。"
  "点对表"
  "(at-elec:stat-load (pickset:to-list (ssget '((0 . \"INSERT\")))))"
  (setq k (@:get-config '@elec:demand-factor))
  (if (or (null k) (not (numberp k))) (setq k 0.7))
  (setq total 0.0 count 0)
  (foreach blk (if (p:ename-listp ents) ents nil)
    (if (setq watt (@elec:block-watt blk))
        (progn
          (setq total (+ total watt))
          (setq count (1+ count)))))
  (list (cons "count" count)
        (cons "total" total)
        (cons "demand" (* total k))
        (cons "factor" k)))

(defun @elec:stat-equip (ents / names)
  "按块真实名称统计设备数量，返回 ((名称 . 数量) ...)，按名称排序。"
  "点对表"
  "(at-elec:stat-equip (pickset:to-list (ssget '((0 . \"INSERT\")))))"
  (setq names (vl-remove nil
                         (mapcar 'block:get-effectivename
                                 (if (p:ename-listp ents) ents nil))))
  (if names
      (vl-sort (stat:stat names) '(lambda (a b) (< (car a) (car b))))
      nil))

(defun @elec:ai-load-stat (sel / ents)
  "AI 入口：统计设备块负荷，sel 为选择集/ename/ename 列表；nil 表示全图 INSERT。
  返回 ((\"count\" . n) (\"total\" . w) (\"demand\" . w) (\"factor\" . k))。
  示例: (@elec:ai-load-stat (ssget '((0 . \"INSERT\"))))"
  "点对表"
  "(@elec:ai-load-stat nil)"
  (setq ents
        (if (null sel)
            (pickset:to-list (ssget "_X" '((0 . "INSERT"))))
            (@elec:sel-list sel)))
  (@elec:stat-load ents))

(defun @elec:ai-equip-stat (sel / ents)
  "AI 入口：按块名统计设备数量；sel 为 nil 时统计全图 INSERT。
  返回 ((名称 . 数量) ...)，无设备返回 nil。"
  "点对表或 nil"
  "(@elec:ai-equip-stat nil)"
  (setq ents
        (if (null sel)
            (pickset:to-list (ssget "_X" '((0 . "INSERT"))))
            (@elec:sel-list sel)))
  (@elec:stat-equip ents))

(defun @elec:ai-circut-numbering (sel prefix start / ents num pt txt style)
  "AI 入口：给曲线回路编号，返回编号数量。
  sel 为选择集/ename/ename 列表；prefix 为 nil 时取配置；start 为 nil 时取配置。
  示例: (@elec:ai-circut-numbering (ssget '((0 . \"*line\"))) \"WL\" 1)"
  "整数"
  "(@elec:ai-circut-numbering (ssget '((0 . \"*line\"))) \"WL\" 1)"
  (setq ents (@elec:sel-list sel))
  (if (null prefix) (setq prefix (@:get-config '@elec:circut-prefix)))
  (if (null start) (setq start (@:get-config '@elec:circut-start)))
  (if (null start) (setq start 1))
  (if (or (null ents) (null prefix))
      (progn (princ "at-elec: 没有可编号的曲线") 0)
      (progn
        (setq num start)
        (setq style (if (tblsearch "style" "mm") "mm" (getvar "textstyle")))
        (foreach ent ents
          (if (and (p:enamep ent)
                   (member (entity:getdxf ent 0)
                           '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE"
                             "CIRCLE" "ELLIPSE" "MLINE")))
              (progn
                (setq pt (curve:midpoint ent))
                (if pt
                    (progn
                      (setq txt
                            (entity:make-text
                             (strcat prefix "-"
                                     (string:number-format (itoa num) 2 0 "0"))
                             (polar pt (* 0.5 pi) 200)
                             (* (@elec:draw-scale) 2.5)
                             0 0.72 0 style))
                      (if txt (setq num (1+ num))))))))
        (@:set-config '@elec:circut-start num)
        (- num start))))

(defun @elec:ai-cable-label (ent cable-type / pts pt ang len txt style)
  "AI 入口：给一条曲线标注电缆型号，返回文字 ename；失败 nil。
  示例: (@elec:ai-cable-label (car (entsel)) \"YJV-3x2.5+1x1.5\")"
  "ename 或 nil"
  "(@elec:ai-cable-label (car (entsel)) \"YJV-3x2.5\")"
  (if (and (p:enamep ent) cable-type (setq pts (curve:get-points ent)))
      (progn
        (if (null (tblsearch "layer" (@:get-config '@elec:cable-layer)))
            (layer:make (@:get-config '@elec:cable-layer) 3 nil nil))
        (setq ang (if (cadr pts) (angle (car pts) (cadr pts)) 0.0))
        (setq len (if (cadr pts) (distance (car pts) (cadr pts)) 0.0))
        (setq pt (polar (car pts) ang (* 0.3 len)))
        (setq style (if (tblsearch "style" "mm") "mm" (getvar "textstyle")))
        (setq txt
              (entity:make-text cable-type pt
                                (* (@elec:draw-scale) 2.0)
                                ang 0.72 0 style))
        (if txt
            (progn
              (entity:putdxf txt 8 (@:get-config '@elec:cable-layer))
              txt)
            nil))
      (progn (princ "at-elec: 无效图元或电缆型号为空") nil)))
