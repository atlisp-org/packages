;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-structure AI 入口：钢筋面积查询、梁截面统计
;; 纯计算/批量统计，不取点、不弹窗。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun at-structure:ai-steel-area (steelbar-str / area)
  "AI 入口：解析钢筋文字并返回面积（mm²）；无法解析返回 nil。
  示例: (at-structure:ai-steel-area \"%%1328@100\")"
  "Real 或 nil"
  "(at-structure:ai-steel-area \"%%1328@100\")"
  (setq area (vl-catch-all-apply
              'at-structure:get-steel-area (list steelbar-str)))
  (if (or (vl-catch-all-error-p area) (null area) (not (numberp area)))
      nil
      area))

(defun at-structure:ai-steel-area-text (ent / txt parts res)
  "AI 入口：解析单行文字中的多组钢筋（用 ; 或 ； 分隔），返回 ((文字 . 面积) ...)。"
  "点对表列表或 nil"
  "(at-structure:ai-steel-area-text (car (entsel)))"
  (if (and (p:enamep ent)
           (member (entity:getdxf ent 0) '("TEXT" "TCH_TEXT"))
           (setq txt (text:get-mtext ent)))
      (progn
        (setq txt (text:remove-fmt txt))
        (setq parts (string:parse-by-lst txt '(";" "；")))
        (setq res
              (mapcar '(lambda (x / a)
                         (setq a (at-structure:ai-steel-area x))
                         (if a (cons x a) nil))
                      parts))
        (setq res (vl-remove nil res))
        (if res res nil))
      nil))

(defun at-structure:ai-stat-beam-size (sel / ents txts sections lay ss)
  "AI 入口：统计梁平法文字中的截面尺寸，返回去重排序后的字符串列表。
  sel 为 nil 时扫描当前图中梁图层文字；也可传 TEXT ename 列表。
  示例: (at-structure:ai-stat-beam-size nil)"
  "字符串列表或 nil"
  "(at-structure:ai-stat-beam-size nil)"
  (setq ents
        (cond
          ((null sel)
           (setq lay (@:get-config '@structure:layer-beam))
           (if lay
               (progn
                 (setq ents nil)
                 (foreach l (string:to-list lay ",")
                   (setq ss (ssget "_X" (list '(0 . "*TEXT") (cons 8 l))))
                   (if ss (setq ents (append ents (pickset:to-list ss)))))
                 ents)
               (pickset:to-list (ssget "_X" '((0 . "*TEXT"))))))
          ((p:picksetp sel) (pickset:to-list sel))
          ((p:enamep sel) (list sel))
          ((p:ename-listp sel) sel)
          (t nil)))
  (setq txts
        (vl-remove nil
                   (mapcar '(lambda (x)
                              (if (and (p:enamep x)
                                       (member (entity:getdxf x 0)
                                               '("TEXT" "TCH_TEXT"))
                                       (text:get-mtext x))
                                  (text:remove-fmt (text:get-mtext x))
                                  nil))
                           ents)))
  (setq sections
        (if txts
            (apply 'append
                   (mapcar '(lambda (x)
                              (re:match "/\\d+[xX]\\d+/g" x))
                           txts))
            nil))
  (setq sections (list:remove-duplicates sections))
  (if sections
      (vl-sort sections '<)
      nil))
