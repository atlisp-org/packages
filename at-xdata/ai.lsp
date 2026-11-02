;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-xdata AI 入口：无交互读取/写入/删除扩展数据
;; 供 MCP eval_lisp / Agent 直接调用，不依赖 DCL 或选择集。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(xdata:* entity:*))

(defun at-xdata:ai-get (ent)
  "AI 入口：读取图元的全部扩展数据。
  返回 ((APPID . ((码 . 值) ...)) ...)，无扩展数据返回 nil。"
  "扩展数据 alist 或 nil"
  "(at-xdata:ai-get (car (entsel)))"
  (if ent
      (cdr (assoc -3 (entget ent '("*"))))
      nil))

(defun at-xdata:ai-set (ent appid values)
  "AI 入口：向图元写入扩展数据（已有 APPID 时追加）。
  values 可以是单值或值列表（字符串/整数/实数）。
  返回写入后的扩展数据 alist，失败返回 nil。"
  "扩展数据 alist 或 nil"
  "(at-xdata:ai-set (car (entsel)) \"MYAPP\" '(\"hello\" 42 3.5))"
  (if (and ent appid)
      (progn
        (xdata:put ent appid values)
        (at-xdata:ai-get ent))
      (progn
        (princ "at-xdata:ai-set ent/appid 不能为空")
        nil)))

(defun at-xdata:ai-remove (ent appid)
  "AI 入口：删除图元指定 APPID 的扩展数据，返回剩余扩展数据 alist。"
  "扩展数据 alist 或 nil"
  "(at-xdata:ai-remove (car (entsel)) \"MYAPP\")"
  (if (and ent appid)
      (progn
        (xdata:remove-appid ent appid)
        (at-xdata:ai-get ent))
      nil))

(defun at-xdata:ai-remove-all (ent)
  "AI 入口：删除图元全部扩展数据，返回 t。"
  "T 或 nil"
  "(at-xdata:ai-remove-all (car (entsel)))"
  (if ent
      (progn (xdata:remove-all ent) t)
      nil))

(defun at-xdata:ai-appids (ent / data)
  "AI 入口：返回图元上所有扩展数据 APPID 字符串列表。"
  "字符串列表或 nil"
  "(at-xdata:ai-appids (car (entsel)))"
  (setq data (at-xdata:ai-get ent))
  (if data (mapcar 'car data) nil))
