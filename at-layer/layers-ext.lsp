;; layers-ext.lsp — at-layer 图层扩展函数
;; 复现 iMini 工具集的图层增强命令：图层状态管理、隐藏实体、独立显示、
;; 锁定实体(xdata 标记)、双$层清理、层名大小写、按颜色分层、
;; 按图层调整绘图顺序、图层提取器。
;; ai- 前缀函数为无交互入口：参数全走形参，失败返回 nil，提示仅用 princ。

;; ============================================================
;; 公共辅助
;; ============================================================

(defun @layer:ents-ify (ents)
  "将实体参数(ename/ename 列表/选择集)规范化为 ename 列表"
  "ename 列表，失败返回 nil"
  "(@layer:ents-ify (ssget))"
  (cond
   ((null ents) nil)
   ((p:enamep ents) (list ents))
   ((p:ename-listp ents) ents)
   ((= (type ents) 'pickset)
    (pickset:to-list ents))
   (t nil)))

(defun @layer:layers-by-ents (ents / out en lay)
  "从 ename 列表获取所在图层列表(去重)"
  "图层名列表"
  "(@layer:layers-by-ents (pickset:to-list (ssget)))"
  (foreach en ents
    (setq lay (cdr (assoc 8 (entget en))))
    (if (not (member lay out))
        (setq out (cons lay out))))
  (reverse out))

(defun @layer:join (lst sep / out x)
  "用分隔符连接字符串列表"
  "结果字符串"
  "(@layer:join '(\"a\" \"b\") \", \")"
  (if lst
      (setq out (car lst))
      (setq out ""))
  (foreach x (cdr lst)
    (setq out (strcat out sep x)))
  out)

(defun @layer:current-states (/ out lay)
  "读取当前所有图层的开/冻结/锁定状态"
  "((\"层名\" . (on frozen locked)) ...)"
  "(@layer:current-states)"
  (vlax-for lay (std:layers)
    (setq out (cons
               (cons (vla-get-name lay)
                     (list (vla-get-layeron lay)
                           (vla-get-freeze lay)
                           (vla-get-lock lay)))
               out)))
  (reverse out))

(defun @layer:replace-str (s old new / out i)
  "将字符串 s 中所有子串 old 替换为 new"
  "结果字符串"
  "(@layer:replace-str \"aXbXc\" \"X\" \",\")"
  (setq out "")
  (while (setq i (vl-string-search old s))
    (setq out (strcat out (substr s 1 i) new))
    (setq s (substr s (+ i (strlen old) 1))))
  (strcat out s))

(defun @layer:set-layer-on (lay on)
  "设置图层开/关状态(容错：锁定层等失败时不中断)"
  "t 设置成功，nil 失败"
  "(@layer:set-layer-on (std:layers) t)"
  (if (vl-catch-all-error-p
       (vl-catch-all-apply 'vla-put-layeron
                           (list lay (if on :vlax-true :vlax-false))))
      nil
      t))

(defun @layer:csv-quote (s)
  "CSV 字段加引号处理(含逗号/引号/换行时加引号并转义引号)"
  "处理后的字符串"
  "(@layer:csv-quote \"a,b\")"
  (if (or (vl-string-position 44 s)
          (vl-string-position 34 s)
          (vl-string-position 10 s)
          (vl-string-position 13 s))
      (strcat "\""
              (@layer:replace-str s "\"" "\"\"")
              "\"")
      s))

;; ============================================================
;; 1. 图层状态管理(*layer-states*)
;; ============================================================

(defun @layer:ai-save-state (name)
  "AI 入口：保存当前所有图层的开/冻结/锁定状态到 *layer-states*"
  "成功返回 t，失败返回 nil"
  "(@layer:ai-save-state \"work\")"
  (or name (setq name "default"))
  (setq *layer-states*
        (cons (cons name (@layer:current-states))
              (vl-remove-if
               (lambda (x) (= (car x) name))
               *layer-states*)))
  (princ (strcat "已保存图层状态: " name))
  t)

(defun @layer:save-state (name)
  "保存当前所有图层的开/冻结/锁定状态到 *layer-states*(按名称区分多份)"
  "成功返回 t，失败返回 nil"
  "(@layer:save-state \"work\")"
  (if (null name)
      (progn
        (setq name (getstring "状态名称 <default>: "))
        (or name (setq name "default"))))
  (@layer:ai-save-state name))

(defun @layer:ai-restore-state (name / st lay rec on frozen locked)
  "AI 入口：按名称恢复图层状态(来自 *layer-states*)并应用到各图层"
  "成功返回 t，失败返回 nil"
  "(@layer:ai-restore-state \"work\")"
  (or name (setq name "default"))
  (if (null (setq st (cdr (assoc name *layer-states*))))
      (progn
        (princ (strcat "未保存的图层状态: " name))
        nil)
      (progn
        (vlax-for lay (std:layers)
          (if (setq rec (assoc (vla-get-name lay) st))
              (progn
                (setq on (car (cdr rec)))
                (setq frozen (cadr (cdr rec)))
                (setq locked (caddr (cdr rec)))
                ;; 目标为解锁时先解锁，允许解冻已冻结图层
                (if (not locked)
                    (if (vla-get-lock lay)
                        (vla-put-lock lay :vlax-false)))
                (if (vlax-write-enabled-p lay)
                    (vla-put-freeze lay
                                     (if frozen :vlax-true :vlax-false)))
                (@layer:set-layer-on lay on)
                (if locked
                    (vla-put-lock lay :vlax-true)))))
        (princ (strcat "已恢复图层状态: " name))
        t)))

(defun @layer:restore-state (name / res)
  "按名称恢复图层状态(开/冻结/锁定，来自 *layer-states*)"
  "成功返回 t，失败返回 nil"
  "(@layer:restore-state \"work\")"
  (if (null name)
      (progn
        (if (null *layer-states*)
            (setq name (getstring "无已保存状态，输入状态名称: "))
            (setq name (getstring
                        (strcat "状态名称 <"
                                (car (mapcar 'car *layer-states*))
                                ">: "))))
        (or name (setq name "default"))))
  (if (null (assoc name *layer-states*))
      (progn
        (princ (strcat "未保存的图层状态: " name))
        nil)
      (progn
        (command "_.UNDO" "Begin")
        (setq res (@layer:ai-restore-state name))
        (command "_.UNDO" "End")
        res)))

(defun @layer:ai-state-list ()
  "AI 入口：列出已保存的图层状态名称"
  "状态名列表"
  "(@layer:ai-state-list)"
  (mapcar 'car *layer-states*))

(defun @layer:state-list ()
  "列出已保存的图层状态名称"
  "状态名列表"
  "(@layer:state-list)"
  (if (null *layer-states*)
      (princ "无已保存图层状态")
      (princ (strcat "已保存状态: "
                     (@layer:join (mapcar 'car *layer-states*) ", "))))
  (mapcar 'car *layer-states*))

;; ============================================================
;; 2. 隐藏实体(*layer-hidden*)
;; ============================================================

(defun @layer:ai-hide (ents / res layers lay)
  "AI 入口：关闭选中实体所在图层并记录到 *layer-hidden*(不重复记录)"
  "被关闭的图层名列表，失败返回 nil"
  "(@layer:ai-hide (pickset:to-list (ssget)))"
  (if (null (setq ents (@layer:ents-ify ents)))
      (progn
        (princ "请选择对象")
        nil)
      (if (null (setq layers (@layer:layers-by-ents ents)))
          (progn
            (princ "对象未找到所在图层")
            nil)
          (progn
            (setq res nil)
            (vlax-for lay (std:layers)
              (if (and (member (vla-get-name lay) layers)
                       (vla-get-layeron lay))
                  (if (@layer:set-layer-on lay nil)
                      (progn
                        (if (not (member (vla-get-name lay) *layer-hidden*))
                            (setq *layer-hidden*
                                  (append *layer-hidden*
                                         (list (vla-get-name lay)))))
                        (setq res (cons (vla-get-name lay) res))))))
            (if (member (getvar "clayer") res)
                (setvar "clayer" "0"))
            (princ (strcat "关闭 " (itoa (length res)) " 个图层: "
                           (@layer:join (reverse res) ", ")))
            (reverse res)))))

(defun @layer:hide-ents (ss / res)
  "关闭选中实体所在图层，记录到 *layer-hidden* 供恢复"
  "被关闭的图层名列表，失败返回 nil"
  "(@layer:hide-ents)"
  (if (null ss)
      (setq ss (ssget)))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-hide (if ss (pickset:to-list ss) nil)))
  (command "_.UNDO" "End")
  res)

(defun @layer:ai-show-hidden ()
  "AI 入口：恢复 *layer-hidden* 中的图层并清空列表"
  "被恢复的图层名列表"
  "(@layer:ai-show-hidden)"
  (if (null *layer-hidden*)
      (progn
        (princ "无已隐藏图层可恢复")
        nil)
      (progn
        (layer:on *layer-hidden* t)
        (setq res *layer-hidden*)
        (setq *layer-hidden* nil)
        (princ (strcat "恢复 " (itoa (length res)) " 个图层: "
                       (@layer:join res ", ")))
        res)))

(defun @layer:show-hidden ()
  "恢复 *layer-hidden* 中记录的图层并清空列表"
  "被恢复的图层名列表"
  "(@layer:show-hidden)"
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-show-hidden))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 3. 独立显示(*layer-iso*)
;; ============================================================

(defun @layer:ai-isolate (ents / res layers lay)
  "AI 入口：关闭除选中实体所在图层外的所有图层并记录到 *layer-iso*"
  "被关闭的图层名列表，失败返回 nil"
  "(@layer:ai-isolate (pickset:to-list (ssget)))"
  (if (null (setq ents (@layer:ents-ify ents)))
      (progn
        (princ "请选择对象")
        nil)
      (if (null (setq layers (@layer:layers-by-ents ents)))
          (progn
            (princ "对象未找到所在图层")
            nil)
          (progn
            (if (not (member (getvar "clayer") layers))
                (setvar "clayer" (car layers)))
            (setq res nil)
            (vlax-for lay (std:layers)
              (if (and (not (member (vla-get-name lay) layers))
                       (vla-get-layeron lay))
                  (if (@layer:set-layer-on lay nil)
                      (progn
                        (if (not (member (vla-get-name lay) *layer-iso*))
                            (setq *layer-iso*
                                  (append *layer-iso*
                                         (list (vla-get-name lay)))))
                        (setq res (cons (vla-get-name lay) res))))))
            (princ (strcat "关闭 " (itoa (length res)) " 个图层，仅显示: "
                           (@layer:join layers ", ")))
            (reverse res)))))

(defun @layer:isolate (ss / res)
  "独立显示：关闭除选中实体所在图层外的所有图层并记录供恢复"
  "被关闭的图层名列表，失败返回 nil"
  "(@layer:isolate)"
  (if (null ss)
      (setq ss (ssget)))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-isolate (if ss (pickset:to-list ss) nil)))
  (command "_.UNDO" "End")
  res)

(defun @layer:ai-un-isolate ()
  "AI 入口：恢复 *layer-iso* 中的图层并清空列表"
  "被恢复的图层名列表"
  "(@layer:ai-un-isolate)"
  (if (null *layer-iso*)
      (progn
        (princ "未处于独立显示状态")
        nil)
      (progn
        (layer:on *layer-iso* t)
        (setq res *layer-iso*)
        (setq *layer-iso* nil)
        (princ (strcat "恢复 " (itoa (length res)) " 个图层: "
                       (@layer:join res ", ")))
        res)))

(defun @layer:un-isolate ()
  "取消独立显示：恢复被独立显示关闭的图层"
  "被恢复的图层名列表"
  "(@layer:un-isolate)"
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-un-isolate))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 4. 锁定实体(xdata 标记 LAYERLOCK)
;; ============================================================

(defun @layer:ensure-app (name / r)
  "注册 xdata 应用名(已存在时静默通过)"
  "t"
  "(@layer:ensure-app \"LAYERLOCK\")"
  (setq r (vl-catch-all-apply 'regapp (list name)))
  t)

(defun @layer:is-locked? (ent)
  "判断实体是否带有 LAYERLOCK 锁定标记"
  "t 或 nil"
  "(@layer:is-locked? (car (entsel)))"
  (if (and ent (assoc -3 (entget ent (list "LAYERLOCK")))) t nil))

(defun @layer:ai-lock (ents / res en ed xd)
  "AI 入口：给选中实体打 LAYERLOCK xdata 标记(锁定实体标识)"
  "被标记的实体名列表，失败返回 nil"
  "(@layer:ai-lock (list (car (entsel))))"
  (if (null (setq ents (@layer:ents-ify ents)))
      (progn
        (princ "请选择对象")
        nil)
      (progn
        (@layer:ensure-app "LAYERLOCK")
        (setq res nil)
        (foreach en ents
          (if (not (@layer:is-locked? en))
              (progn
                (setq ed (entget en)
                      xd (assoc -3 ed))
                (if xd
                    (entmod (subst (append xd
                                           (list (list "LAYERLOCK"
                                                       (cons 1000 "1"))))
                                   xd ed))
                    (entmod (append ed
                                    (list (list -3
                                                (list "LAYERLOCK"
                                                      (cons 1000 "1")))))))
                (setq res (cons en res)))))
        (princ (strcat "锁定 " (itoa (length res)) " 个实体"))
        (reverse res))))

(defun @layer:lock-ents (ss / res)
  "给选中实体打 LAYERLOCK xdata 标记(供其他包/脚本识别锁定实体)"
  "被标记的实体名列表，失败返回 nil"
  "(@layer:lock-ents)"
  (if (null ss)
      (setq ss (ssget)))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-lock (if ss (pickset:to-list ss) nil)))
  (command "_.UNDO" "End")
  res)

(defun @layer:unlock-one (en / ed xd)
  "移除单个实体的 LAYERLOCK 标记"
  "t 或 nil"
  "(@layer:unlock-one (car (entsel)))"
  (setq ed (entget en (list "LAYERLOCK"))
        xd (assoc -3 ed))
  (if (null xd)
      nil
      (progn
        ;; 本平台 entmod 不删除 xdata，改为把该应用的数据置空
        (entmod (list (cons -1 en)
                      (list -3 (list "LAYERLOCK"))))
        t)))

(defun @layer:ai-unlock (ents / res en)
  "AI 入口：移除选中实体的 LAYERLOCK 标记"
  "被移除标记的实体名列表，失败返回 nil"
  "(@layer:ai-unlock (list (car (entsel))))"
  (if (null (setq ents (@layer:ents-ify ents)))
      (progn
        (princ "请选择对象")
        nil)
      (progn
        (setq res nil)
        (foreach en ents
          (if (@layer:unlock-one en)
              (setq res (cons en res))))
        (princ (strcat "解锁 " (itoa (length res)) " 个实体"))
        (reverse res))))

(defun @layer:unlock-ents (ss / res)
  "移除选中实体的 LAYERLOCK 标记"
  "被移除标记的实体名列表，失败返回 nil"
  "(@layer:unlock-ents)"
  (if (null ss)
      (setq ss (ssget)))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-unlock (if ss (pickset:to-list ss) nil)))
  (command "_.UNDO" "End")
  res)

(defun @layer:locked-list (/ ss)
  "获取图中全部带 LAYERLOCK 锁定标记的实体"
  "ename 列表"
  "(@layer:locked-list)"
  (if (setq ss (ssget "_X" (list (cons -3 (list "LAYERLOCK")))))
      (pickset:to-list ss)
      nil))

(defun @layer:ai-unlock-all (/ res en ss)
  "AI 入口：移除图中所有实体的 LAYERLOCK 标记"
  "被移除标记的实体数量"
  "(@layer:ai-unlock-all)"
  (if (setq ss (ssget "_X" (list (cons -3 (list "LAYERLOCK")))))
      (progn
        (setq res (pickset:to-list ss))
        (foreach en res
          (@layer:unlock-one en))
        (princ (strcat "全部解锁: " (itoa (length res)) " 个实体"))
        (length res))
      0))

(defun @layer:unlock-ents-all (/ res)
  "移除图中所有实体的 LAYERLOCK 标记"
  "被移除标记的实体数量"
  "(@layer:unlock-ents-all)"
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-unlock-all))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 5. 清理双$图层
;; ============================================================

(defun @layer:ai-kill-dollar (keep / layers lay ss n en)
  "AI 入口：删除名称含 $$ 的图层及其上的实体"
  "被删除的图层数量"
  "keep=1 时保留实体并移入 0 层"
  "(@layer:ai-kill-dollar nil)"
  (if (null (setq layers (vl-remove-if-not
                          (lambda (x) (wcmatch x "*$$*"))
                          (layer:list))))
      (progn
        (princ "无名称含 $$ 的图层")
        0)
      (progn
        (if (member (getvar "clayer") layers)
            (setvar "clayer" "0"))
        (setq n 0)
        (foreach lay layers
          (if (setq ss (ssget "_X" (list (cons 8 lay))))
              (if (= keep 1)
                  (foreach en (pickset:to-list ss)
                    (entity:putdxf en 8 "0"))
                  (while (sslength ss)
                    (entdel (ssname ss 0)))))
          (if (layer:delete lay)
              (setq n (1+ n))))
        (princ (strcat "删除 " (itoa n) " 个图层: "
                       (@layer:join layers ", ")))
        n)))

(defun @layer:kill-dollar (keep / res)
  "删除名称含 $$ 的图层及其上的实体"
  "被删除的图层数量"
  "keep=1 时保留实体并移入 0 层"
  (if (null keep)
      (if (= "1" (getkword "层上实体保留 [保留(1)/删除(0)] <删除>: "))
          (setq keep 1)
          (setq keep 0)))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-kill-dollar keep))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 6. 层名大小写
;; ============================================================

(defun @layer:ai-strcase (mode layers / res old new existing l rec ss)
  "AI 入口：层名统一大写/小写(新建目标层→移实体→删旧层)"
  "已处理的(旧层 . 新层)列表，失败返回 nil"
  "mode 为 \"uc\"/\"lc\"；layers 为 nil 时处理全部图层"
  "(@layer:ai-strcase \"uc\" nil)"
  (cond
   ((= mode 1) (setq mode 1))
   ((= mode 2) (setq mode 2))
   ((and (p:stringp mode) (= (strcase mode) "UC")) (setq mode 1))
   ((and (p:stringp mode) (= (strcase mode) "LC")) (setq mode 2))
   (t (progn
        (princ "mode 须为 uc 或 lc")
        nil)))
  (if (and layers (= (type layers) 'STR))
      (setq layers (list layers)))
  (if (null (setq layers (if layers layers (layer:list))))
      (progn
        (princ "无图层可处理")
        nil)
      (progn
        (setq res nil)
        (foreach old layers
          (setq new (strcase old mode))
          (if (not (string= new old))
              (progn
                ;; 查找已存在的目标层(忽略大小写)
                (setq existing nil)
                (foreach l (layer:list)
                  (if (and (not (string= l old))
                           (string= (strcase l mode) new))
                      (setq existing l)))
                (if (null existing)
                    (progn
                      (if (setq rec (entget (tblobjname "layer" old)))
                          (layer:make new
                                      (cdr (assoc 62 rec))
                                      (cdr (assoc 6 rec))
                                      (cdr (assoc 70 rec))))))
                (if (string= (getvar "clayer") old)
                    (setvar "clayer" (if existing existing new)))
                (if (setq ss (ssget "_X" (list (cons 8 old))))
                    (while (sslength ss)
                      (entity:putdxf (ssname ss 0)
                                     8
                                     (if existing existing new))))
                (layer:delete old)
                (setq res (cons (cons old (if existing existing new)) res)))))
        (princ (strcat "处理 " (itoa (length res)) " 个图层"))
        (reverse res))))

(defun @layer:strcase (mode layers / res)
  "层名统一大写/小写(新建目标层→移实体→删旧层)"
  "已处理的(旧层 . 新层)列表，失败返回 nil"
  "(@layer:strcase)"
  (if (null mode)
      (if (= "uc" (getkword "转大写 [大写(uc)/小写(lc)] <大写>: "))
          (setq mode "uc")
          (setq mode "lc")))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-strcase mode layers))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 7. 按颜色分层
;; ============================================================

(defun @layer:ai-by-color (ents prefix / res en e color name skipped)
  "AI 入口：选中实体按颜色拆到不同图层(层名 前缀+颜色号)"
  "生成的图层名列表，失败返回 nil"
  "BYLAYER/BYBLOCK 实体跳过"
  "(@layer:ai-by-color (pickset:to-list (ssget)) \"颜色\")"
  (if (null (setq ents (@layer:ents-ify ents)))
      (progn
        (princ "请选择对象")
        nil)
      (progn
        (or prefix (setq prefix "颜色"))
        (setq res nil)
        (setq skipped 0)
        (foreach en ents
          (setq e (entget en))
          (setq color (if (cdr (assoc 62 e)) (cdr (assoc 62 e)) (cdr (assoc 63 e))))
          (if (or (null color)
                  (member color '(256 0 255)))
              (setq skipped (1+ skipped))
              (progn
                (setq name (strcat prefix (itoa color)))
                (if (not (member name (layer:list)))
                    (layer:make name color "continuous" nil))
                (entity:putdxf en 8 name)
                (entity:putdxf en 62 256)
                (entity:deldxf en 420)
                (if (not (member name res))
                    (setq res (cons name res))))))
        (princ (strcat "处理 " (itoa (length ents)) " 个实体，"
                       (itoa (length res)) " 个图层，跳过 "
                       (itoa skipped)))
        (reverse res))))

(defun @layer:by-color (ss prefix / res)
  "选中实体按颜色拆到不同图层(层名 颜色N，BYLAYER/BYBLOCK 跳过)"
  "生成的图层名列表，失败返回 nil"
  "(@layer:by-color)"
  (if (null ss)
      (setq ss (ssget)))
  (if (null prefix)
      (progn
        (setq prefix (getstring "层名前缀 <颜色>: "))
        (or prefix (setq prefix "颜色"))))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-by-color (if ss (pickset:to-list ss) nil) prefix))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 8. 按图层调整绘图顺序
;; ============================================================

(defun @layer:ai-ldorder (layers order / res lay ss en r fail)
  "AI 入口：按图层把实体移到绘图顺序最前/最后"
  "已处理的图层名列表，失败返回 nil"
  "layers 为 nil 时处理全部图层；order 为 \"front\"/\"back\""
  "(@layer:ai-ldorder nil \"front\")"
  (cond
   ((= order "front") (setq order "front"))
   ((= order "back") (setq order "back"))
   ((and (p:stringp order) (= (strcase order) "FRONT")) (setq order "front"))
   ((and (p:stringp order) (= (strcase order) "BACK")) (setq order "back"))
   (t (progn
        (princ "order 须为 front 或 back")
        nil)))
  (if (and layers (= (type layers) 'STR))
      (setq layers (list layers)))
  (foreach lay (if layers layers (layer:list))
    (if (setq ss (ssget "_X" (list (cons 8 lay))))
        (progn
          (setq fail 0)
          (while (sslength ss)
            (setq en (ssname ss 0))
            (setq r (vl-catch-all
                     (if (= order "front")
                         (vla-MoveToFront
                          (vlax-ename->vla-object en))
                         (vla-MoveToBack
                          (vlax-ename->vla-object en)))))
            (if (vl-catch-all-error-p r)
                (setq fail (1+ fail)))
            (ssdel en ss))
          (if (> fail 0)
              (princ (strcat "图层 " lay " 有 " (itoa fail)
                             " 个实体跳过")))
          (setq res (cons lay res)))))
  (princ (strcat "处理 " (itoa (length res)) " 个图层"))
  (reverse res))

(defun @layer:ldorder (layers order / res)
  "按图层把实体移到绘图顺序最前/最后"
  "已处理的图层名列表，失败返回 nil"
  "(@layer:ldorder)"
  (if (null order)
      (if (= "front"
             (getkword "绘图顺序 [前置(front)/后置(back)] <前置>: "))
          (setq order "front")
          (setq order "back")))
  (command "_.UNDO" "Begin")
  (setq res (@layer:ai-ldorder layers order))
  (command "_.UNDO" "End")
  res)

;; ============================================================
;; 9. 图层提取器
;; ============================================================

(defun @layer:ai-extract (out-file / f rec name color flag onp frp lkp lt
                          lw cnt ss)
  "AI 入口：导出全部图层信息(名/颜色/线型/线宽/开/冻结/锁定/实体数)为 CSV"
  "输出文件路径，失败返回 nil"
  "(@layer:ai-extract \"C:/layers.csv\")"
  (or out-file
      (setq out-file
            (strcat (getvar "dwgprefix")
                    (getvar "dwgname")
                    "_layers.csv")))
  (if (null (setq f (open out-file "w")))
      (progn
        (princ (strcat "无法创建文件: " out-file))
        nil)
      (progn
        (princ "图层名,颜色,线型,线宽,打开,冻结,锁定,实体数\n" f)
        (while (setq rec (tblnext "LAYER" (null rec)))
          (setq name (cdr (assoc 2 rec)))
          (setq color (abs (if (cdr (assoc 62 rec)) (cdr (assoc 62 rec)) 7)))
          (setq flag (if (cdr (assoc 70 rec)) (cdr (assoc 70 rec)) 0))
          (setq onp (>= (if (cdr (assoc 62 rec)) (cdr (assoc 62 rec)) 7) 1))
          (setq frp (/= (rem flag 2) 0))
          (setq lkp (= (rem (lsh flag -2) 2) 1))
          (setq lt (if (cdr (assoc 6 rec)) (cdr (assoc 6 rec)) "Continuous"))
          (setq lw (if (cdr (assoc 370 rec)) (cdr (assoc 370 rec)) -1))
          (setq cnt (if (setq ss (ssget "_X"
                                         (list (cons 8 name))))
                        (sslength ss)
                        0))
          (princ
           (strcat
            (@layer:csv-quote name)
            "," (itoa color) ","
            (@layer:csv-quote lt)
            "," (itoa lw) ","
            (if onp "是" "否") ","
            (if frp "是" "否") ","
            (if lkp "是" "否") ","
            (itoa cnt) "\n")
           f))
        (close f)
        (princ (strcat "已导出图层信息: " out-file))
        out-file)))

(defun @layer:extract (out-file)
  "导出全部图层信息(名/颜色/线型/线宽/开/冻结/锁定/实体数)为 CSV 文件"
  "输出文件路径，失败返回 nil"
  "(@layer:extract)"
  (if (null out-file)
      (progn
        (setq out-file
              (getstring
               (strcat "输出文件 <"
                       (getvar "dwgprefix")
                       (getvar "dwgname")
                       "_layers.csv>: ")))
        (or out-file
            (setq out-file
                  (strcat (getvar "dwgprefix")
                          (getvar "dwgname")
                          "_layers.csv"))))
  (@layer:ai-extract out-file)))
