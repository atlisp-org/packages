;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; at-linetype —— ltype-ext.lsp
;; 批量改线型 / 局部变虚线 / 反转线型方向 / 输出线型定义
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------- 1. 批量改线型 ----------

(defun at-linetype:ltype (/ name entlist)
  "批量改线型：输入目标线型后选择实体（留空=当前线型）。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:ltype)"
  (setq name (getstring "\n输入目标线型名（留空=当前线型）: "))
  (if (null name)
      (setq name (getvar "celtype")))
  (if (at-linetype:check-ltype name nil)
      (progn
        (command "_.UNDO" "Begin")
        (setq entlist (at-linetype:collect-ents (ssget) nil))
        (if entlist
            (progn
              (at-linetype:apply-ltype entlist name)
              (princ (strcat "\n共 " (itoa (length entlist)) " 个实体改为 " name))))
        (command "_.UNDO" "End")))
  (princ))

(defun at-linetype:ai-ltype (ltype-name ents layers lin-file / enlist)
  "AI 调用入口：批量改线型。
  参数: ltype-name:目标线型名（nil=当前 celtype）; ents:实体列表或选择集（与 layers 均可为 nil）;
       layers:图层名列表（ents 为 nil 时按层取）; lin-file:自定义 .lin 路径（线型缺失时先加载）"
  "被修改的 ename 列表，失败 nil"
  "(at-linetype:ai-ltype \"DASHED\" (ssget) nil nil)"
  (if (null ltype-name)
      (setq ltype-name (getvar "celtype")))
  (if (at-linetype:check-ltype ltype-name lin-file)
      (progn
        (setq enlist (at-linetype:collect-ents ents layers))
        (if enlist
            (progn
              (at-linetype:apply-ltype enlist ltype-name)
              enlist)
            nil))
      nil))

;; ---------- 2. 局部变虚线 ----------

(defun at-linetype:split-str (str sep / pos lst s)
  "按单字符分隔符拆分字符串。
  参数: str:原串; sep:分隔符（1 字符字符串）"
  "子串列表"
  "(at-linetype:split-str \"a,b,c\" \",\")"
  (if (= "" str)
      (list "")
      (progn
        (setq pos (vl-string-search sep str))
        (if (null pos)
            (list str)
            (progn
              (setq lst (list (substr str 1 pos)))
              (setq s (substr str (1+ pos)))
              (append lst (at-linetype:split-str s sep)))))))

(defun at-linetype:parse-idx (str m / tok i a b tmp lst)
  "解析段号串为 0 基索引列表（用户输入 1 基，逗号分隔，可写 a-b 区间）。
  参数: str:输入串; m:总段数（越界忽略）"
  "0 基索引列表"
  "(at-linetype:parse-idx \"1,3,5-7\" 10)"
  (setq lst nil)
  (foreach tok (at-linetype:split-str (strcase str) ",")
    (if (> (strlen tok) 0)
        (progn
          (setq i (vl-string-search "-" tok))
          (if i
              (progn
                (setq a (atoi (substr tok 1 i)))
                (setq b (atoi (substr tok (1+ i))))
                (if (> a b)
                    (progn
                      (setq tmp a)
                      (setq a b)
                      (setq b tmp)))
                (if (<= 1 a)
                    (progn
                      (if (> b m)
                          (setq b m))
                      (setq i a)
                      (while (<= i b)
                        (setq lst (append lst (list (1- i))))
                        (setq i (1+ i))))))
              (progn
                (setq a (atoi tok))
                (if (and (<= 1 a) (<= a m))
                    (setq lst (append lst (list (1- a))))))))))
  lst)

(defun at-linetype:jb-vd-core (ent edges ltype / verts closed n m marks all-dash s i a k idxs run-pts run-bulges en enames src-lt)
  "核心：把多段线指定段拆出为新多段线并设虚线线型，其余段保持原线型，删除原线。
  参数: ent:多段线; edges:0 基段索引列表; ltype:虚线线型名"
  "新虚线多段线 ename 列表，失败或无虚线段 nil"
  "(at-linetype:jb-vd-core (car (entsel)) '(0 2) \"DASHED\")"
  (setq verts (at-linetype:pline-verts ent))
  (setq closed (at-linetype:pline-closed-p ent))
  (setq n (length verts))
  (setq m (if closed n (1- n)))
  (if (or (< m 1) (null edges))
      nil
      (progn
        (setq marks nil)
        (setq i 0)
        (while (< i m)
          (setq marks (append marks (list (if (member i edges) 1 0))))
          (setq i (1+ i)))
        (setq all-dash (= (length (vl-remove 0 marks)) m))
        (if (zerop (length (vl-remove 0 marks)))
            nil
            (progn
              (command "_.UNDO" "Begin")
              (if all-dash
                  (progn
                    (setq en (at-linetype:make-lwpl (mapcar 'car verts)
                                          (mapcar '(lambda (v) (nth 1 v)) verts)
                                          nil (if closed 1 0) ent))
                    (if en
                        (progn
                          (at-linetype:set-ltype en ltype)
                          (entdel ent)
                          (list en))
                        nil))
                  (progn
                    ;; 闭合时旋转到段边界起点，避免首尾同色段被拆开
                    (setq s 0)
                    (if (and closed
                             (equal (car marks) (car (reverse marks))))
                        (progn
                          (setq i 1)
                          (while (and (< i m)
                                      (equal (nth i marks) (nth (1- i) marks)))
                            (setq i (1+ i)))
                          (setq s i)))
                    (setq enames nil)
                    (setq i 0)
                    (while (< i m)
                      (setq a i)
                      (setq i (1+ i))
                      (while (and (< i m)
                                  (equal (nth i marks) (nth a marks)))
                        (setq i (1+ i)))
                      ;; 段区间 [a, i-1]
                      (setq idxs nil)
                      (setq k a)
                      (while (<= k i)
                        (setq idxs (append idxs (list (rem (+ s k) n))))
                        (setq k (1+ k)))
                      (setq run-pts (mapcar '(lambda (j) (car (nth j verts))) idxs))
                      (setq run-bulges (mapcar '(lambda (j) (nth 1 (nth j verts))) idxs))
                      (setq en (at-linetype:make-lwpl run-pts run-bulges nil 0 ent))
                      (if en
                          (progn
                            (if (equal (nth a marks) 1)
                                (progn
                                  (at-linetype:set-ltype en ltype)
                                  (setq enames (append enames (list en))))
                                (progn
                                  (setq src-lt (cdr (assoc 6 (entget ent))))
                                  (if src-lt
                                      (entity:putdxf en 6 src-lt))))))
                      )
                    (entdel ent)
                    (if enames enames nil)))
              (command "_.UNDO" "End"))))))

(defun at-linetype:jb-vd (/ ent str edges ltype enames)
  "局部变虚线：把多段线指定段改为虚线线型（段号从 1 开始）。
  交互命令，无返回值。支持逗号分隔和区间：如 1,3,5-7。"
  "nil"
  "(at-linetype:jb-vd)"
  (setq ent (car (entsel '((0 . "*POLYLINE")))))
  (if ent
      (progn
        (setq str (getstring "\n输入段号（从 1 开始，逗号分隔，可用区间，如 1,3,5-7）: "))
        (setq edges (at-linetype:parse-idx str (at-linetype:pline-edge-count ent)))
        (if edges
            (progn
              (setq ltype (getstring "\n虚线线型名 <DASHED>: "))
              (if (null ltype)
                  (setq ltype "DASHED"))
              (if (at-linetype:check-ltype ltype nil)
                  (progn
                    (setq enames (at-linetype:jb-vd-core ent edges ltype))
                    (if enames
                        (princ (strcat "\n生成虚线多段线 " (itoa (length enames)) " 条"))
                        (princ "\n没有有效段号")))
                  (princ "\n线型不存在")))
            (princ "\n没有输入有效段号")))
      (princ "\n请先选择一条多段线"))
  (princ))

(defun at-linetype:ai-jb-vd (pline-ent edges ltype)
  "AI 调用入口：局部变虚线。
  参数: pline-ent:多段线图元; edges:0 基段索引列表（如 (0 2 3)）; ltype:虚线线型名（nil 默认 DASHED）"
  "新虚线多段线 ename 列表，失败 nil"
  "(at-linetype:ai-jb-vd (car (entsel)) '(0 2) \"DASHED\")"
  (if (or (null pline-ent) (null edges))
      (progn
        (princ "\n缺少参数")
        nil)
      (progn
        (if (null ltype)
            (setq ltype "DASHED"))
        (if (at-linetype:check-ltype ltype nil)
            (at-linetype:jb-vd-core pline-ent edges ltype)
            nil))))

;; ---------- 3. 反转线型方向 ----------

(defun at-linetype:reverse-entity (ent / data typ pts buls w0s w1s base n i j lst a1 a2 verts r-pts r-buls r-ws new-en d)
  "反转单个图元方向（LINE/LWPOLYLINE/POLYLINE/ARC），SPLINE 暂不支持。
  参数: ent:图元"
  "成功 t，失败 nil"
  "(at-linetype:reverse-entity (car (entsel)))"
  (setq data (entget ent))
  (setq typ (cdr (assoc 0 data)))
  (cond
   ((= "LINE" typ)
    (setq a1 (cdr (assoc 10 data)))
    (setq a2 (cdr (assoc 11 data)))
    (entmod (append (vl-remove (assoc 10 data) (vl-remove (assoc 11 data) data))
                    (list (cons 10 a2) (cons 11 a1))))
    (entupd ent)
    t)
   ((= "LWPOLYLINE" typ)
    (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 10)) data)))
    (setq buls (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 42)) data)))
    (setq w0s (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 40)) data)))
    (setq w1s (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 41)) data)))
    (setq base (vl-remove-if '(lambda (g) (member (car g) '(10 40 41 42))) data))
    (setq n (length pts))
    (setq i 0)
    (setq lst nil)
    (while (< i n)
      (setq j (- n 1 i))
      (setq lst (append lst
                   (list (cons 10 (nth j pts))
                         (cons 40 (if (and w1s (< j (length w1s))) (nth j w1s) 0.0))
                         (cons 41 (if (and w0s (< j (length w0s))) (nth j w0s) 0.0))
                         (cons 42 (if (and buls (< j (length buls)))
                                      (- 0.0 (nth j buls))
                                      0.0)))))
      (setq i (1+ i)))
    (entmod (append base lst))
    (entupd ent)
    t)
   ((= "POLYLINE" typ)
    (setq verts (at-linetype:pline-verts ent))
    (setq n (length verts))
    (setq i 0)
    (setq r-pts nil)
    (setq r-buls nil)
    (setq r-ws nil)
    (while (< i n)
      (setq j (- n 1 i))
      (setq r-pts (append r-pts (list (car (nth j verts)))))
      (setq r-buls (append r-buls (list (nth 1 (nth j verts)))))
      (setq r-ws (append r-ws
                    (list (list (caddr (nth j verts)) (nth 2 (nth j verts))))))
      (setq i (1+ i)))
    (setq new-en (at-linetype:make-lwpl r-pts r-buls r-ws
                             (if (at-linetype:pline-closed-p ent) 1 0) ent))
    (if new-en
        (progn
          (entdel ent)
          t)
        nil))
   ((= "ARC" typ)
    (setq a1 (cdr (assoc 50 data)))
    (setq a2 (cdr (assoc 51 data)))
    (entmod (append (vl-remove (assoc 50 data) (vl-remove (assoc 51 data) data))
                    (list (cons 50 a2) (cons 51 a1))))
    (entupd ent)
    t)
   ((= "SPLINE" typ)
    (princ "\n样条线暂不支持反转（见 PLAN.md P2）")
    nil)
   (t
    (princ (strcat "\n不支持的图元类型: " typ))
    nil)))

(defun at-linetype:reverse (/ ss n ok en)
  "反转线型方向：交换 LINE/LWPOLYLINE/POLYLINE/ARC 的起止端。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:reverse)"
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
  (if ss
      (progn
        (command "_.UNDO" "Begin")
        (setq n 0)
        (setq ok 0)
        (foreach en (pickset:to-list ss)
          (if (at-linetype:reverse-entity en)
              (setq ok (1+ ok)))
          (setq n (1+ n)))
        (command "_.UNDO" "End")
        (princ (strcat "\n共反转 " (itoa ok) "/" (itoa n) " 个实体"))))
  (princ))

(defun at-linetype:ai-reverse (ents / lst ok en)
  "AI 调用入口：反转实体方向（LINE/LWPOLYLINE/POLYLINE/ARC）。
  参数: ents:实体列表或选择集"
  "成功反转的 ename 列表，失败 nil"
  "(at-linetype:ai-reverse (ssget '((0 . \"*line\"))))"
  (setq lst (at-linetype:collect-ents ents nil))
  (if lst
      (progn
        (command "_.UNDO" "Begin")
        (setq ok nil)
        (foreach en lst
          (if (at-linetype:reverse-entity en)
              (setq ok (append ok (list en)))))
        (command "_.UNDO" "End")
        (if ok ok nil))
      nil))

;; ---------- 4. 输出线型定义 ----------

(defun at-linetype:ltype-record (name / rec found)
  "读取线型表记录（含 49 组码线型元素列表）。
  参数: name:线型名"
  "表记录（assoc 列表），不存在 nil"
  "(at-linetype:ltype-record \"DASHED\")"
  (setq found nil)
  (while (and (null found)
              (setq rec (tblnext "LTYPE" (null rec))))
    (if (and (assoc 2 rec)
             (= (strcase (cdr (assoc 2 rec))) (strcase name)))
        (setq found rec)))
  found)

(defun at-linetype:elems2str (rec / elems e s)
  "线型记录的 49 组码列表转 .lin 元素串（首个为总长）。
  参数: rec:线型表记录"
  "字符串"
  "(at-linetype:elems2str (at-linetype:ltype-record \"DASHED\"))"
  (setq elems (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 49)) rec)))
  (setq s nil)
  (foreach e elems
    (setq s (strcat (if s (strcat s ",") "") (rtos e 2 3))))
  (if s s "0"))

(defun at-linetype:write-ltype-fp (name fp / rec elems has-shape)
  "把一个线型的定义写入已打开的文件。
  参数: name:线型名; fp:文件指针"
  "成功 t，失败 nil"
  "(at-linetype:write-ltype-fp \"DASHED\" (open \"c:/a.lin\" \"w\"))"
  (if (setq rec (at-linetype:ltype-record name))
      (progn
        (setq elems (mapcar 'cdr (vl-remove-if-not '(lambda (g) (= (car g) 49)) rec)))
        (setq has-shape (vl-some '(lambda (e) (equal e 0.0 1e-9)) elems))
        (if has-shape
            (progn
              (princ "; 注意: 该线型含形元素(0)，形字体信息无法自动导出，请手工补全 [形名,shx,s=..]\n" fp)
              (princ (strcat "\n警告: " name " 含形元素，需手工补全形定义\n"))))
        (princ (strcat "*" (cdr (assoc 2 rec)) ", " (if (cdr (assoc 3 rec)) (cdr (assoc 3 rec)) "") "\n") fp)
        (princ (strcat "A," (at-linetype:elems2str rec) "\n") fp)
        t)
      nil))

(defun at-linetype:lt-out-core (names out-file / fp lst n name)
  "把线型定义写入 .lin 文件（文件已存在则覆盖）。
  参数: names:线型名列表; out-file:输出路径"
  "写入的线型数，失败 nil"
  "(at-linetype:lt-out-core '(\"DASHED\") \"c:/out.lin\")"
  (setq lst nil)
  (foreach name names
    (if (at-linetype:ltype-record name)
        (setq lst (append lst (list name)))
        (princ (strcat "\n线型不存在，跳过: " name))))
  (if (null lst)
      nil
      (progn
        (setq fp (open out-file "w"))
        (if fp
            (progn
              (setq n 0)
              (foreach name lst
                (if (at-linetype:write-ltype-fp name fp)
                    (setq n (1+ n))))
              (close fp)
              n)
            (progn
              (princ (strcat "\n无法打开文件: " out-file))
              nil)))))

(defun at-linetype:used-ltypes (/ ss lst out s)
  "收集当前图使用的线型（所有实体的显式 6 组码 + 当前线型）。
  无参数。"
  "线型名列表（去重）"
  "(at-linetype:used-ltypes)"
  (setq ss (ssget "_X"))
  (setq lst (list (getvar "celtype")))
  (foreach en (pickset:to-list ss)
    (if (entity:getdxf en 6)
        (setq lst (append lst (list (entity:getdxf en 6))))))
  (setq out nil)
  (foreach s (reverse lst)
    (if (not (member s out))
        (setq out (cons s out))))
  out)

(defun at-linetype:ents-ltypes (ents / lst s)
  "收集实体列表的显式线型（6 组码）。
  参数: ents:实体列表"
  "线型名列表（去重）"
  "(at-linetype:ents-ltypes (ssget))"
  (setq lst nil)
  (foreach en ents
    (if (setq s (entity:getdxf en 6))
        (if (not (member s lst))
            (setq lst (append lst (list s))))))
  lst)

(defun at-linetype:lt-out (/ ss names path n)
  "输出线型定义：选择实体，把它们的线型 .lin 定义写到文件。
  交互命令，无返回值。"
  "nil"
  "(at-linetype:lt-out)"
  (setq ss (ssget))
  (if ss
      (progn
        (setq names (at-linetype:ents-ltypes (pickset:to-list ss)))
        (if names
            (progn
              (setq path (getfiled "选择 .lin 输出文件" "" "lin" 1))
              (if path
                  (progn
                    (setq n (at-linetype:lt-out-core names path))
                    (if n
                        (princ (strcat "\n已写入 " (itoa n) " 个线型到 " path))))))
            (princ "\n所选实体没有显式线型")))
      (princ "\n请先选择实体"))
  (princ))

(defun at-linetype:ai-lt-out (out-file names)
  "AI 调用入口：输出线型定义到 .lin 文件。
  参数: out-file:输出路径; names:线型名列表（nil=图内使用的线型）"
  "写入的线型数，失败 nil"
  "(at-linetype:ai-lt-out \"c:/out.lin\" '(\"DASHED\"))"
  (if (null out-file)
      (progn
        (princ "\n缺少参数 out-file")
        nil)
      (at-linetype:lt-out-core (if names names (at-linetype:used-ltypes))
                                 out-file)))
