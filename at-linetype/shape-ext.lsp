;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-linetype —— shape-ext.lsp
;; 制作形文件 / 线型快捷命令
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------- 1. 制作形文件 ----------

(defun at-linetype:join-str (lst sep / s x)
  "用分隔符拼接字符串列表。
  参数: lst:字符串列表; sep:分隔符"
  "字符串"
  "(at-linetype:join-str '(\"a\" \"b\") \",\")"
  (setq s nil)
  (foreach x lst
    (setq s (strcat (if s (strcat s sep) "") x)))
  s)

(defun at-linetype:compile-shp-file (shp-path / cm)
  "编译 .shp 为 .shx（COMPILE 命令）。
  参数: shp-path:.shp 文件路径"
  "成功 t，失败 nil"
  "(at-linetype:compile-shp-file \"c:/a/tree.shp\")"
  (if (findfile shp-path)
      (progn
        (setq cm (getvar "cmdecho"))
        (setvar "cmdecho" 0)
        (command "_.compile" shp-path)
        (while (> (getvar "cmdactive") 0) (command ""))
        (setvar "cmdecho" cm)
        (princ (strcat "\n已编译: " (vl-filename-base shp-path) ".shx"))
        t)
      nil))

(defun at-linetype:mkshape-core (ent name out-file / verts n closed i p1 p2 b segs pts x0 y0 x1 y1 ext scale grid g2 q dx dy tokens db shp-path fp lin-str)
  "核心：多段线 → .shp 形文件 + .lin 线型片段。
  弧段拆 8 段弦，归一化到 64 单位整数网格。
  参数: ent:多段线; name:形名/线型名; out-file:.shp 输出路径（无扩展名自动补 .shp）"
  "列表 (shp路径 .lin片段)，失败 nil"
  "(at-linetype:mkshape-core (car (entsel)) \"TREE\" \"c:/a/tree.shp\")"
  (if (or (null ent) (null name))
      (progn
        (princ "\n缺少参数")
        nil)
      (progn
        (setq verts (at-linetype:pline-verts ent))
        (setq n (length verts))
        (setq closed (at-linetype:pline-closed-p ent))
        (if (< n 2)
            (progn
              (princ "\n多段线顶点少于 2 个")
              nil)
            (progn
              ;; 弧段展开为 8 段弦
              (setq pts (list (car (car verts))))
              (setq i 0)
              (while (< i (if closed n (1- n)))
                (setq p1 (car (nth i verts)))
                (setq p2 (car (nth (rem (1+ i) n) verts)))
                (setq b (nth 1 (nth i verts)))
                (if (setq segs (at-linetype:bulge2pts p1 b p2 8))
                    (setq pts (append pts segs)))
                (setq pts (append pts (list p2)))
                (setq i (1+ i)))
              (setq x0 1e300)
              (setq y0 1e300)
              (setq x1 -1e300)
              (setq y1 -1e300)
              (foreach p pts
                (setq x0 (min x0 (car p)))
                (setq y0 (min y0 (cadr p)))
                (setq x1 (max x1 (car p)))
                (setq y1 (max y1 (cadr p))))
              (setq ext (max (- x1 x0) (- y1 y0)))
              (if (< ext 1e-9)
                  (progn
                    (princ "\n形状尺寸过小")
                    nil)
                  (progn
                    (setq scale (/ 64.0 ext))
                    (setq grid nil)
                    (foreach p pts
                      (setq grid (append grid
                                   (list (fix (+ 0.5 (* scale (- (car p) x0))))
                                         (fix (+ 0.5 (* scale (- (cadr p) y0))))))))
                    (setq g2 nil)
                    (foreach q grid
                      (if (or (null g2)
                              (not (equal q (car (reverse g2)) 1e-9)))
                          (setq g2 (append g2 (list q)))))
                    (if (< (length g2) 2)
                        (progn
                          (princ "\n形状退化为点")
                          nil)
                        (progn
                          (setq tokens nil)
                          (setq db 0)
                          (setq i 0)
                          (while (< (1+ i) (length g2))
                            (setq dx (- (car (nth (1+ i) g2)) (car (nth i g2))))
                            (setq dy (- (cadr (nth (1+ i) g2)) (cadr (nth i g2))))
                            (if (= dy 0)
                                (progn
                                  (setq tokens (append tokens (list (itoa dx))))
                                  (setq db (1+ db)))
                                (progn
                                  (setq tokens (append tokens
                                               (list (strcat "(" (itoa dx) ","
                                                             (itoa dy) ")"))))
                                  (setq db (+ db 2))))
                            (setq i (1+ i)))
                          (setq shp-path (if (vl-string-search ".shp"
                                                               (strcase out-file))
                                             out-file
                                             (strcat out-file ".shp")))
                          (setq fp (open shp-path "w"))
                          (if fp
                              (progn
                                (princ (strcat "*1," (itoa (1+ db)) ","
                                               (strcase name) "\n")
                                       fp)
                                (princ (strcat (at-linetype:join-str tokens ",")
                                               ",0\n")
                                       fp)
                                (close fp)
                                (setq lin-str (strcat "*" (strcase name) ", 形线型\nA,0,["
                                                      (strcase name) ","
                                                      (vl-filename-base shp-path)
                                                      ".shx,s=1]\n"))
                                (list shp-path lin-str))
                              (progn
                                (princ (strcat "\n无法打开文件: " shp-path))
                                nil)))))))))))

(defun at-linetype:mkshape (/ ent name path res ans)
  "制作形文件：多段线转 .shp 形文件并输出 .lin 线型片段。
  交互命令，无返回值。可立即编译 .shp 为 .shx。"
  "nil"
  "(at-linetype:mkshape)"
  (setq ent (car (entsel '((0 . "*POLYLINE")))))
  (if ent
      (progn
        (setq name (getstring "\n输入形名（建议大写，如 TREE）: "))
        (if (and name (/= name ""))
            (progn
              (setq path (getfiled "保存 .shp 文件" "" "shp" 1))
              (if path
                  (progn
                    (setq res (at-linetype:mkshape-core ent name path))
                    (if res
                        (progn
                          (princ (strcat "\n形文件已生成: " (car res)))
                          (princ (strcat "\n线型片段（复制到 .lin 文件并编译形文件）:\n"
                                         (cadr res)))
                          (setq ans (getkword
                                      "\n现在编译 .shp 为 .shx 吗? [Yes/No]<No>: "))
                          (if (= ans "Yes")
                              (at-linetype:compile-shp-file (car res))))))
                  (princ "\n未选择输出文件")))
            (princ "\n未输入形名")))
      (princ "\n请先选择一条多段线"))
  (princ))

(defun at-linetype:ai-mkshape (pline-ent name out-file)
  "AI 调用入口：制作形文件（不自动编译）。
  参数: pline-ent:多段线; name:形名/线型名; out-file:.shp 输出路径"
  "列表 (shp路径 .lin片段)，失败 nil"
  "(at-linetype:ai-mkshape (car (entsel)) \"TREE\" \"c:/a/tree.shp\")"
  (if (or (null pline-ent) (null name) (null out-file))
      (progn
        (princ "\n缺少参数")
        nil)
      (at-linetype:mkshape-core pline-ent name out-file)))

;; ---------- 2. 线型快捷命令 ----------

(defun at-linetype:lt-key-path ()
  "线型快捷键映射文件路径。
  无参数。"
  "字符串"
  "(at-linetype:lt-key-path)"
  (if @:*prefix*
      (strcat @:*prefix* "mini-lt-key.txt")
      "mini-lt-key.txt"))

(defun at-linetype:lt-key-load (/ fp lst s parts)
  "加载快捷键映射（优先文件，失败退回全局 *mini-lt-key-map*）。
  无参数。"
  "列表 (键 . 线型)"
  "(at-linetype:lt-key-load)"
  (setq lst nil)
  (if (setq fp (open (at-linetype:lt-key-path) "r"))
      (progn
        (while (setq s (read-line fp))
          (if (> (strlen s) 0)
              (progn
                (setq parts (at-linetype:split-str s "="))
                (if (= (length parts) 2)
                    (setq lst (append lst
                                 (list (cons (strcase (car parts) t)
                                             (cadr parts)))))))))
        (close fp))
      (if *mini-lt-key-map*
          (setq lst *mini-lt-key-map*)))
  lst)

(defun at-linetype:lt-key-save (map / fp kv)
  "保存快捷键映射到文件，失败退回全局变量。
  参数: map:列表 (键 . 线型)"
  "成功 t，失败 nil"
  "(at-linetype:lt-key-save '((\"A\" . \"DASHED\")))"
  (setq fp (open (at-linetype:lt-key-path) "w"))
  (if fp
      (progn
        (foreach kv map
          (princ (strcat (car kv) "=" (cdr kv) "\n") fp))
        (close fp)
        t)
      (progn
        (setq *mini-lt-key-map* map)
        (princ "\n文件不可写，已存入全局变量 *mini-lt-key-map*")
        nil)))

(defun at-linetype:lt-key-get (key)
  "取快捷键对应的线型。
  参数: key:快捷键"
  "线型名，未定义 nil"
  "(at-linetype:lt-key-get \"A\")"
  (cdr (assoc (strcase key t) (at-linetype:lt-key-load))))

(defun at-linetype:ai-lt-key (map / merged kv)
  "AI 调用入口：线型快捷命令。
  参数: map:成对列表 (键 . 线型)；nil 时返回当前映射（只读）"
  "完整映射列表 (键 . 线型)，失败 nil"
  "(at-linetype:ai-lt-key '((\"A\" . \"DASHED\")))"
  (if (null map)
      (at-linetype:lt-key-load)
      (progn
        (setq merged (at-linetype:lt-key-load))
        (foreach kv map
          (if (and (consp kv) (stringp (car kv)) (stringp (cdr kv)))
              (progn
                (setq merged (vl-remove (assoc (strcase (car kv) t) merged)
                                      merged))
                (setq merged (append merged
                                 (list (cons (strcase (car kv) t) (cdr kv))))))))
        (if (at-linetype:lt-key-save merged)
            merged
            (progn
              (princ "\n快捷键映射保存失败")
              nil)))))

(defun at-linetype:lt-key (/ str parts lst)
  "线型快捷命令：定义 键=线型 映射（逐行输入，回车结束）。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:lt-key)"
  (princ "\n逐行输入 键=线型（如 A=DASHDOT），直接回车结束:")
  (setq str (getstring))
  (setq lst nil)
  (while str
    (setq parts (at-linetype:split-str str "="))
    (if (= (length parts) 2)
        (setq lst (append lst (list (cons (strcase (car parts) t)
                                          (cadr parts)))))
        (princ (strcat "\n忽略无效行: " str)))
    (setq str (getstring)))
  (if lst
      (progn
        (if (at-linetype:ai-lt-key lst)
            (princ (strcat "\n已保存 " (itoa (length lst)) " 个快捷键"))))
  (princ)))
