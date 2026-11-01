;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-layer 图层创建扩展
;; 1) 从 TXT 文件读取图层名并批量建层
;; 2) 以选中的文字内容作为图层名建层
;; 需求来源：明经论坛 thread-87384 / thread-42764
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(entity:* pickset:*))

(@:add-menu "图层" "由TXT建层" "(@layer:layers-from-file)")
(@:add-menu "图层" "文字建层" "(@layer:layer-from-text)")

(defun @layer:sanitize-name (s / r)
  "把字符串整理成合法图层名：去掉首尾空白，非法字符替换为下划线。"
  "图层名字符串"
  "(@layer:sanitize-name \"A/B\") => \"A_B\""
  (setq r (vl-string-trim " \t\r\n" (if s s "")))
  ;; vl-string-translate 按位映射，替换串长度必须与源字符集一致（12 个非法字符）
  (setq r (vl-string-translate "<>/\\\":;?*|,=" "____________" r))
  (if (> (strlen r) 250) (setq r (substr r 1 250)))
  r)

(defun @layer:ensure (name)
  "确保图层存在，返回图层名；name 为空返回 nil。"
  "图层名或 nil"
  "(@layer:ensure \"WALL\")"
  (if (and name (/= name ""))
      (progn
        (if (null (tblsearch "LAYER" name))
            (layer:make name nil nil nil))
        (if (tblsearch "LAYER" name) name nil))
      nil))

(defun @layer:ai-layers-from-file (file prefix / fp line name res)
  "AI 调用入口：从 TXT 文件逐行读取图层名并创建图层。
  空行、以 # 或 ; 开头的行忽略；prefix 非空时加到每个层名前。
  返回创建的图层名列表（已存在的也返回），失败返回 nil。"
  "图层名列表或 nil"
  "(@layer:ai-layers-from-file \"D:/layers.txt\" nil)"
  (setq fp (if file (open file "r") nil))
  (if (null fp)
      (progn
        (princ "at-layer:ai-layers-from-file 无法打开文件")
        nil)
      (progn
        (while (setq line (read-line fp))
          (setq line (vl-string-trim " \t\r\n" line))
          (if (and (> (strlen line) 0)
                   (/= (substr line 1 1) "#")
                   (/= (substr line 1 1) ";"))
              (progn
                (setq name (@layer:sanitize-name
                            (strcat (if (and prefix (/= prefix "")) prefix "")
                                    line)))
                (if (and name (/= name "") (not (member name res)))
                    (if (@layer:ensure name)
                        (setq res (cons name res)))))))
        (close fp)
        (reverse res))))

(defun @layer:ent-list (ents / res)
  "内部：ename / 选择集 / ename 列表统一成 ename 列表。"
  "ename 列表"
  "(@layer:ent-list (ssget))"
  (cond
   ((= 'ename (type ents)) (setq res (list ents)))
   ((= 'pickset (type ents)) (setq res (pickset:to-entlist ents)))
   ((listp ents) (setq res ents)))
  res)

(defun @layer:ai-layer-from-text (ents prefix / res e ty txt name)
  "AI 调用入口：以 TEXT/MTEXT 的文字内容创建图层。
  返回创建的图层名列表，失败返回 nil。"
  "图层名列表或 nil"
  "(@layer:ai-layer-from-text (pickset:to-entlist (ssget '((0 . \"TEXT\")))) nil)"
  (foreach e (@layer:ent-list ents)
    (setq ty (if e (entity:getdxf e 0) nil))
    (if (member ty '("TEXT" "MTEXT"))
        (progn
          (setq txt (entity:getdxf e 1))
          (setq txt (if (stringp txt) txt ""))
          ;; MTEXT 可能带格式码，只取可打印内容并不做复杂解析
          (setq name (@layer:sanitize-name
                      (strcat (if (and prefix (/= prefix "")) prefix "")
                              txt)))
          (if (and name (/= name "")
                   (not (member name res))
                   (@layer:ensure name))
              (setq res (cons name res))))))
  (reverse res))

(defun @layer:layers-from-file (/ file prefix res)
  "由 TXT 建层：选择文本文件 → 输入前缀 → 批量建层。"
  "图层名列表或 nil"
  "(@layer:layers-from-file)"
  (setq file (getfiled "选择图层名文本" "" "txt;csv" 0))
  (if file
      (progn
        (setq prefix (getstring "\n图层名前缀(回车=无): "))
        (setq res (@layer:ai-layers-from-file file
                                              (if (= prefix "") nil prefix)))
        (if res
            (princ (strcat "\n已处理 " (itoa (length res)) " 个图层。"))
            (princ "\n未读到有效图层名。"))))
  (princ))

(defun @layer:layer-from-text (/ ss ents prefix res)
  "文字建层：选择文字，以其内容为图层名建层。"
  "图层名列表或 nil"
  "(@layer:layer-from-text)"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
      (progn
        (setq ents (pickset:to-entlist ss))
        (setq prefix (getstring "\n图层名前缀(回车=无): "))
        (setq res (@layer:ai-layer-from-text
                   ents (if (= prefix "") nil prefix)))
        (if res
            (princ (strcat "\n已处理 " (itoa (length res)) " 个图层。"))
            (princ "\n未读到有效文字。"))))
  (princ))
