;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layout-draw.lsp — 布局视口工具 主文件（菜单注册 + 公共辅助）
;; 菜单注册 + 公共辅助（布局/视口 VLA 封装）
;; 复现 iMini 工具集的布局与视口命令
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(vl-load-com)

;; 依赖函数库（按需加载，已加载则跳过）
(if (null layout:list) (require 'layout:*))
(if (null string:sort-by-number) (require 'string:*))
(if (null pickset:to-list) (require 'pickset:*))
(if (null p:picksetp) (require 'p:*))

;; 向系统中添加菜单
(@:add-menus
 '(("布局视口工具"
    ("布局改名增减排序" "(@layout:tab-sort)")
    ("布局合并" "(@layout:layout-merge)")
    ("布局分图" "(@layout:layout-isolate)")
    ("布局选字重命名" "(@layout:layout-rename)")
    ("局部取图" "(@layout:jbqt)")
    ("编辑视口比例..." "(@layout:vp-scale)")
    ("同步对齐视口" "(@layout:vp-sync)")
    ("在模型空间绘制视口外框" "(@layout:vp-outline)")
    ("视口锁定或解锁" "(@layout:vport-lock)")
    ("视口图层状态编辑" "(@layout:vplayer)")
    ("视口选层变灰" "(@layout:vport-grey)")
    ("还原视口颜色" "(@layout:vport-ungrey)")
    ("图纸空间1：1布图" "(@layout:ps-scale)")
    ("模型空间做块布图" "(@layout:ms-scale)")
    ("还原炸开布图块" "(@layout:ms-exp)")
    ("快速局部重生成" "(@layout:regen)"))))

;; ===================== 公共辅助 =====================

(defun @layout:layout-p (name)
  "判断 name 是否为已存在的布局名"
  "t / nil"
  "示例: (@layout:layout-p \"布局1\")"
  (if (and (stringp name) (/= "" name))
      (member name (layout:list))
      nil))

(defun @layout:layout-obj (name)
  "获取指定布局名 name 的布局对象"
  "布局对象 / nil"
  "示例: (@layout:layout-obj \"布局1\")"
  (if (@layout:layout-p name)
      (vla-item *layouts* name)
      nil))

(defun @layout:ensure-layout (name)
  "确保布局存在，不存在则创建并切换"
  "布局名"
  "示例: (@layout:ensure-layout \"合并布局\")"
  (if (@layout:layout-p name)
      name
      (progn
        (vla-add *layouts* name)
        (setvar "ctab" name)
        name)))

(defun @layout:del-layout (name)
  "删除布局（不能删当前布局或最后一个布局）"
  "t / nil"
  "示例: (@layout:del-layout \"布局2\")"
  (if (and (@layout:layout-p name)
           (/= name (getvar "ctab"))
           (> (length (layout:list)) 1))
      (progn
        (vla-delete (vla-item *layouts* name))
        t)
      nil))

(defun @layout:unique-name (base / names name i)
  "在布局名基础上生成一个不重复的新布局名"
  "布局名"
  "示例: (@layout:unique-name \"局部图\") => \"局部图\" 或 \"局部图-1\""
  (setq names (layout:list)
        name base
        i 1)
  (while (member name names)
    (setq i (1+ i)
          name (strcat base "-" (itoa i))))
  name)

(defun @layout:unique-block (base / name i)
  "在 base 基础上生成一个不重复的块名"
  "块名"
  "示例: (@layout:unique-block \"ML_MS_布局1\")"
  (setq name (@layout:sanitize-name base)
        i 1)
  (while (tblobjname "BLOCK" name)
    (setq i (1+ i)
          name (strcat name "_" (itoa i))))
  name)

(defun @layout:sanitize-name (str / res i ch)
  "清理布局名/块名中的非法字符并去首尾空白"
  "字符串"
  "示例: (@layout:sanitize-name \"一/二层 平面图\")"
  (setq res ""
        i 1)
  (while (<= i (strlen str))
    (setq ch (substr str i 1))
    (if (not (member ch '("/" "\\" ":" "*" "?" "\"" "<" ">" "|")))
        (setq res (strcat res ch)))
    (setq i (1+ i)))
  (setq res (vl-string-trim " \t\r\n" res))
  (if (= "" res)
      "图"
      res))

(defun @layout:text-of (ename / dat t0 s)
  "读取 TEXT/MTEXT 实体的文字内容（MTEXT 去格式化）"
  "字符串 / nil"
  "示例: (@layout:text-of (car (entsel)))"
  (setq dat (entget ename)
        t0 (cdr (assoc 0 dat)))
  (if (and (member t0 '("TEXT" "MTEXT"))
           (setq s (cdr (assoc 1 dat)))
           (stringp s))
      (vl-string-trim " \t\r\n"
                      (if (= t0 "MTEXT")
                          (@layout:mtext-clean s)
                          s))
      nil))

(defun @layout:mtext-clean (s / res i len ch j c2 found)
  "去除 MTEXT 文字中的 \\<...\\> 与 \\...; 格式码"
  "字符串"
  "示例: (@layout:mtext-clean \"{\\H2x;A}B\")"
  (setq res ""
        i 1
        len (strlen s))
  (while (<= i len)
    (setq ch (substr s i 1))
    (cond
     ((or (= ch "{") (= ch "}"))
      (setq res (strcat res ch)))
     ((= ch "\\")
      ;; 跳过 \...; 或 \\...\\> 整段
      (setq j (1+ i)
            found nil)
      (while (and (<= j len) (not found))
        (setq c2 (substr s j 1))
        (cond
         ((= c2 ";") (setq found t))
         ((= c2 ">")
          (if (= (substr s (1- j) 1) "\\")
              (setq found t))))
        (setq j (1+ j)))
      (setq i (min j len)))
     (t
      (setq res (strcat res ch))))
    (setq i (1+ i)))
  res)

(defun @layout:layout-ents (layout / ss lst)
  "返回指定布局图纸空间内所有实体 ename 列表（不含视口）"
  "ename 列表"
  "示例: (@layout:layout-ents \"布局1\")"
  (setq ss (ssget "x"
                  (list (cons 410 layout)
                        (cons -4 "<=")
                        (cons 0 "VIEWPORT")
                        (cons -4 ">")))
        lst nil)
  (if ss
      (progn
        (repeat (sslength ss)
          (setq lst (cons (ssname ss (1- (sslength ss))) lst)))
        lst)
      nil))

(defun @layout:vp-list (layout / ss lst)
  "返回指定布局内所有视口 ename 列表（图纸空间从左到右、从上到下）"
  "ename 列表"
  "示例: (@layout:vp-list (getvar \"ctab\"))"
  (setq ss (ssget "x"
                  (list (cons 410 layout)
                        (cons 0 "viewport")))
        lst nil)
  (if ss
      (progn
        (repeat (sslength ss)
          (setq lst (cons (ssname ss (1- (sslength ss))) lst)))
        (vl-sort lst
                 (function
                  (lambda (a b / ra rb)
                    (setq ra (@layout:vp-rect a)
                          rb (@layout:vp-rect b))
                    (if (= (caddr ra) (caddr rb))
                        (< (car ra) (car rb))
                        (> (caddr ra) (caddr rb)))))))
      nil))

(defun @layout:vports ()
  "返回当前布局内所有视口 ename 列表"
  "ename 列表"
  "示例: (@layout:vports)"
  (@layout:vp-list (getvar "ctab")))

(defun @layout:vp-p (e)
  "判断 e 是否为视口实体"
  "t / nil"
  "示例: (@layout:vp-p (car (@layout:vports)))"
  (and (@layout:ename-p e)
       (= "VIEWPORT" (cdr (assoc 0 (entget e))))))

(defun @layout:vp-rect (vp / dat)
  "返回视口在图纸空间的矩形 (minx miny maxx maxy)"
  "(minx miny maxx maxy)"
  "示例: (@layout:vp-rect (car (@layout:vports)))"
  (setq dat (entget vp))
  (list (cadr (assoc 10 dat))
        (caddr (assoc 10 dat))
        (+ (cadr (assoc 10 dat)) (cdr (assoc 40 dat)))
        (+ (caddr (assoc 10 dat)) (cdr (assoc 41 dat)))))

(defun @layout:vp-view (vp / dat obj)
  "返回视口的模型视图 (mx my paper-w paper-h scale)"
  "(mx my paper-w paper-h scale)"
  "示例: (@layout:vp-view (car (@layout:vports)))"
  (setq dat (entget vp)
        obj (e2o vp))
  (list (cadr (assoc 12 dat))
        (caddr (assoc 12 dat))
        (cdr (assoc 40 dat))
        (cdr (assoc 41 dat))
        (if obj (vla-get-customscale obj) 1.0)))

(defun @layout:vp-model-rect (vp / vw hw hh)
  "返回视口在模型空间的矩形 (minx miny maxx maxy)"
  "(minx miny maxx maxy)"
  "示例: (@layout:vp-model-rect (car (@layout:vports)))"
  (setq vw (@layout:vp-view vp)
        hw (/ (nth 2 vw) (* 2.0 (nth 4 vw)))
        hh (/ (nth 3 vw) (* 2.0 (nth 4 vw))))
  (list (- (car vw) hw)
        (- (cadr vw) hh)
        (+ (car vw) hw)
        (+ (cadr vw) hh)))

(defun @layout:make-vp (layout pt-corner w h mx my scale / ps vp dat)
  "在布局中创建视口。
pt-corner 为图纸空间左下角，w/h 为图纸空间宽/高，
mx/my 为模型目标中心，scale 为 customscale（1/N 即 1/N）。"
  "视口 ename / nil"
  "示例: (@layout:make-vp \"布局1\" '(0 0 0) 100 70 0 0 0.02)"
  (if (and layout (not (@layout:layout-p layout)))
      (progn
        (vla-add *layouts* layout)
        (setvar "ctab" layout)))
  (if (and layout (/= layout (getvar "ctab")))
      (setvar "ctab" layout))
  (if (not (and (numberp scale) (> scale 0.0)))
      (setq scale 1.0))
  (setq ps (vla-get-paperspace *doc*)
        vp (vla-addpviewport ps
                             (vlax-3d-point (list (float (car pt-corner))
                                                  (float (cadr pt-corner))
                                                  0.0))
                             (float w)
                             (float h)))
  (if vp
      (progn
        (setq vp (o2e vp))
        ;; 设置模型目标点（DXF 12）
        (setq dat (entget vp))
        (setq dat (subst (cons 12 (list (float mx) (float my) 0.0))
                         (assoc 12 dat)
                         dat))
        (entmod dat)
        ;; 设置比例
        (vla-put-customscale (e2o vp) scale)
        ;; 同步 DXF 15（模型空间中心，保持数据一致）
        (setq dat (entget vp))
        (setq dat (subst (cons 15 (list (float (+ mx (/ w (* 2.0 scale))))
                                         (float (+ my (/ h (* 2.0 scale))))
                                         0.0))
                         (assoc 15 dat)
                         dat))
        (entmod dat)
        ;; 清除激活位（DXF 70 bit 32），避免新视口带框
        (setq dat (entget vp))
        (setq dat (subst (cons 70 (logand (cdr (assoc 70 dat))
                                          (lnot 32)))
                         (assoc 70 dat)
                         dat))
        (entmod dat)
        vp)
      (progn
        (princ "\n创建视口失败")
        nil)))

(defun @layout:switch-space (space)
  "切换空间：\"model\" 到模型空间，\"paper\" 到图纸空间"
  "t"
  "示例: (@layout:switch-space \"model\")"
  (if (= (strcase space) "MODEL")
      (command "_.-MSPACE")
      (command "_.-PSPACE"))
  t)

(defun @layout:in-model ()
  "判断当前是否在模型空间（cvport=1）"
  "t / nil"
  "示例: (@layout:in-model)"
  (= 1 (getvar "cvport")))

(defun @layout:box (pt1 pt2 / xs ys)
  "把两点归一化为 (minx miny maxx maxy)"
  "(minx miny maxx maxy)"
  "示例: (@layout:box '(3 2) '(1 5))"
  (setq xs (list (car pt1) (car pt2))
        ys (list (cadr pt1) (cadr pt2)))
  (list (apply 'min xs)
        (apply 'min ys)
        (apply 'max xs)
        (apply 'max ys)))

(defun @layout:find-layer (name / en)
  "确保图层存在并返回其图层 ename"
  "ename / nil"
  "示例: (@layout:find-layer \"ML_OUTLINE\")"
  (setq en (tblobjname "LAYER" name))
  (if (null en)
      (progn
        (vla-add (vla-get-layers *doc*) name)
        (setq en (tblobjname "LAYER" name))))
  en)

(defun @layout:ename-p (x)
  "判断 x 是否为合法 ename"
  "t / nil"
  "示例: (@layout:ename-p (car (entsel)))"
  (and (listp x)
       (= 'intg (type (car x)))))

(defun @layout:entlist (v)
  "把选择集/ename 列表/单个 ename 归一化为 ename 列表"
  "ename 列表"
  "示例: (@layout:entlist (ssget))"
  (cond
   ((null v) nil)
   ((p:picksetp v) (pickset:to-list v))
   ((@layout:ename-p v) (list v))
   ((listp v) v)
   (t nil)))

(defun @layout:ss-to-list (ss / lst)
  "选择集转 ename 列表"
  "ename 列表"
  "示例: (@layout:ss-to-list (ssget))"
  (setq lst nil)
  (if ss
      (progn
        (repeat (sslength ss)
          (setq lst (cons (ssname ss (1- (sslength ss))) lst)))
        lst)
      nil))

(defun @layout:erase-ents (entlist)
  "删除实体列表中的实体"
  "t"
  "示例: (@layout:erase-ents (@layout:vports))"
  (foreach e entlist
    (if (and e (@layout:ename-p e))
        (vlax-invoke (e2o e) 'erase)))
  t)

(princ "\n[at-layout] 布局视口工具已加载")
(princ)
