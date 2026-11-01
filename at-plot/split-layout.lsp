;;; 按布局拆分打印
;;; 为每个布局复制当前图形、删除其它布局后另存为独立 dwg，
;;; 可选把每个布局批量打印为 PDF（打印为尽力而为，取决于打印驱动）。

(@:add-menu "通用打印" "按布局拆分" "(@plot:split-layout)")

(defun @plot:layouts-names (doc / a lst)
  "返回文档 doc 的全部布局名列表。"
  (vlax-for a (vla-get-layouts doc)
    (setq lst (cons (vla-get-name a) lst)))
  (reverse lst))

(defun @plot:delete-other-layouts (doc keep / layouts a todo)
  "删除文档 doc 中除 keep 之外的全部布局。"
  (setq layouts (vla-get-layouts doc))
  (setq todo nil)
  (vlax-for a layouts
    (if (not (= (vla-get-name a) keep))
        (setq todo (cons a todo))))
  (vla-put-activelayout doc (vla-item layouts keep))
  (foreach a todo
    (vl-catch-all-apply 'vla-Delete (list a))))

(defun @plot:safe-name (name / chars)
  "把文件名中的非法字符替换为下划线。"
  (setq chars '(92 47 60 62 58 63 33 124 42 34))
  (foreach c chars
    (setq name (vl-string-subst (chr c) "_" name)))
  name)

(defun @plot:iso-media (w h)
  "按纸幅尺寸匹配 ISO 标准 canonical media 名。"
  (cond
   ((and (equal w 1189.0 2.0) (equal h 841.0 2.0)) "ANSER-ISO-A0-1189.00x841.00-MILLIMETERS")
   ((and (equal w 841.0 2.0) (equal h 594.0 2.0)) "ANSER-ISO-A1-841.00x594.00-MILLIMETERS")
   ((and (equal w 594.0 2.0) (equal h 420.0 2.0)) "ANSER-ISO-A2-594.00x420.00-MILLIMETERS")
   ((and (equal w 420.0 2.0) (equal h 297.0 2.0)) "ANSER-ISO-A3-420.00x297.00-MILLIMETERS")
   ((and (equal w 297.0 2.0) (equal h 210.0 2.0)) "ANSER-ISO-A4-297.00x210.00-MILLIMETERS")
   (t (strcat "ANSER-ISO-" (itoa (fix w)) "x" (itoa (fix h)) "-MILLIMETERS"))))

(defun @plot:print-layout (file pdf / acad doc layout plot size media ret)
  "打开 file 并把其唯一布局整幅打印为 pdf，成功返回 t，失败 nil。"
  (setq acad (vlax-get-acad-object))
  (setq doc (vl-catch-all-apply 'vla-Open (list acad file)))
  (if (vl-catch-all-error-p doc)
      (progn
        (princ (strcat "\n[at-plot] 打开失败: " file))
        nil))
  (progn
    (setq layout (vla-get-activelayout doc))
    (setq plot (vla-get-plot doc))
    (setq size (vlax-safearray->list
                (vlax-variant-value (vla-get-size layout))))
    (setq media (@plot:iso-media (car size) (cadr size)))
    (setq ret (vl-catch-all-apply
               '(lambda ()
                  (vla-put-canonicalmedianame layout media)
                  (vla-put-plottype layout acWindow)
                  (vla-put-standardscale layout acScaleToFit)
                  (vla-put-plotrotation layout ac0degrees)
                  (vla-put-centerplot layout :vlax-false)
                  (vla-SetWindowToPlot layout
                    (vlax-make-variant
                     (vlax-safearray-fill
                      (vlax-make-safearray vlax-vbDouble '(0 . 1))
                      (list 0.0 0.0 0.0)))
                    (vlax-make-variant
                     (vlax-safearray-fill
                      (vlax-make-safearray vlax-vbDouble '(0 . 1))
                      (list (car size) (cadr size) 0.0))))
                  (vla-PlotToFile plot pdf))))
    (vl-catch-all-apply 'vla-Close (list doc :vlax-true))
    (if (vl-catch-all-error-p ret)
        (progn
          (princ (strcat "\n[at-plot] 打印失败: " (vl-catch-all-error-message ret)))
          nil)
      t)))

(defun @plot:ai-split-by-layout
  (out-dir print / names i acad doc src name copy doc2 out pdf lst)
  "AI 调用入口：按布局把当前图形拆分为独立 dwg，返回生成的文件列表，失败返回 nil。
out-dir: 输出目录（不存在则创建）
print: T 时同时把每个布局整幅打印为 PDF（尽力而为）
示例: (@plot:ai-split-by-layout \"D:/Layouts\" nil)"
  (if (null out-dir)
      (progn (princ "\n[at-plot] 请指定输出目录。") nil))
  (setq out-dir (system:dir out-dir))
  (if (null (vl-file-directory-p out-dir))
      (vl-directory-make out-dir t))
  (setq acad (vlax-get-acad-object))
  (setq doc (vla-get-activedocument acad))
  (setq src (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (if (null (findfile src))
      (progn
        (princ "\n[at-plot] 请先保存当前图形再拆分。")
        nil))
  (setq names (@plot:layouts-names doc))
  (if (null names)
      (progn (princ "\n[at-plot] 未找到布局。") nil))
  (setq lst nil)
  (setq i 0)
  (foreach name names
    (progn
      (setq copy (strcat out-dir (strcat "_split_" (itoa i) ".dwg")))
      (if (vl-file-copy src copy)
          (progn
            (setq doc2 (vl-catch-all-apply 'vla-Open (list acad copy)))
            (if (vl-catch-all-error-p doc2)
                (princ (strcat "\n[at-plot] 打开失败: " copy))
              (progn
                (@plot:delete-other-layouts doc2 name)
                (setq out (strcat out-dir (@plot:safe-name name) ".dwg"))
                (if (findfile out) (vl-file-delete out))
                (vla-SaveAs doc2 out)
                (vl-catch-all-apply 'vla-Close (list doc2 :vlax-true))
                (if print
                    (progn
                      (setq pdf (strcat out-dir (@plot:safe-name name) ".pdf"))
                      (vl-catch-all-apply
                       (function (lambda () (@plot:print-layout out pdf))))))
                (setq lst (cons out lst)))))
          (princ (strcat "\n[at-plot] 复制失败: " copy))))
    (setq i (1+ i)))
  (vl-catch-all-apply 'vla-Activate (list doc))
  (princ (strcat "\n[at-plot] 拆分完成，共生成 " (itoa (length lst)) " 个文件。"))
  (if lst (reverse lst)))

(defun @plot:split-layout (/ out-dir print)
  "交互：按布局拆分当前图形为独立 dwg。"
  (@:help "为每个布局生成一个独立 dwg 文件，可选同时打印为 PDF。")
  (setq out-dir (system:get-folder "选择拆分输出目录"))
  (if out-dir
      (progn
        (initget 0 1)
        (setq print (getint "\n是否同时打印为 PDF (0/1)<0>: "))
        (if (null print)
            (setq print 0))
        (@plot:ai-split-by-layout out-dir (= print 1))))
  (princ))
