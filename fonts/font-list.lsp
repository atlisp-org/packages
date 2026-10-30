;; 字体库清单
;; iMini 功能复现：font_list（列出可用 SHX 字体文件与当前图文字样式）

(defun fonts:support-paths (/ acad)
  "返回 CAD 支持搜索路径列表（拆分 ACAD 环境变量）。"
  "路径字符串列表"
  "示例: (fonts:support-paths)"
  (if (setq acad (getenv "ACAD"))
      (string:to-list acad ";")
    nil))

(defun fonts:collect-shx (/ path files shx-lst)
  "遍历支持路径收集 .shx 字体文件名。"
  "shx 文件名列表（大写）"
  "示例: (fonts:collect-shx)"
  (setq shx-lst nil)
  (foreach path (fonts:support-paths)
    (if (and path (not (equal path "")))
        (progn
          (setq files (vl-catch-all-apply 'vl-directory-files
                                           (list path "*.shx" 1)))
          (if (not (vl-catch-all-error-p files))
              (foreach f files
                (setq shx-lst (cons (strcase f) shx-lst)))))))
  (vl-sort (list:remove-duplicates shx-lst) '<))

(defun fonts:collect-styles (/ st res)
  "返回当前图所有文字样式的 (名称 主字体 大字体)。"
  "((样式名 主字体 大字体) ...)"
  "示例: (fonts:collect-styles)"
  (setq st (tblnext "style" t))
  (setq res nil)
  (while st
    (setq res (cons (list (cdr (assoc 2 st))
                          (cdr (assoc 3 st))
                          (cdr (assoc 4 st)))
                    res))
    (setq st (tblnext "style")))
  (reverse res))

(defun fonts:style-line (s / main big)
  "把样式元组转成可读字符串（空字体显示为空）。"
  "描述字符串"
  "示例: (fonts:style-line '(\"HZ\" \"tssdeng.shx\" \"hztxt.shx\"))"
  (setq main (if (cadr s) (cadr s) "")
        big (if (caddr s) (caddr s) ""))
  (strcat (car s) " | 主字体:" main " | 大字体:" big))

(defun fonts:ai-font-list (out-file / shx-lst styles fp)
  "AI 入口：列出可用 SHX 字体文件与当前图文字样式字体；可写报告文件。
out-file: 可选输出文件路径（nil 时不写文件）。"
  "无 out-file 返回 (shx文件列表 样式表)；有 out-file 返回文件路径；失败 nil"
  "示例: (fonts:ai-font-list nil)"
  (setq shx-lst (fonts:collect-shx)
        styles (fonts:collect-styles))
  (if out-file
      (progn
        (setq fp (open out-file "w"))
        (if (null fp)
            (progn (princ "fonts:ai-font-list: 无法打开输出文件。") nil)
          (progn
            (write-line "=== SHX 字体文件 ===" fp)
            (foreach f shx-lst (write-line f fp))
            (write-line "" fp)
            (write-line "=== 当前图文字样式 ===" fp)
            (foreach s styles (write-line (fonts:style-line s) fp))
            (close fp)
            out-file)))
    (list shx-lst styles)))

(defun fonts:font-list (/ kw out-file res)
  "字体库清单：列出可用 SHX 字体文件与当前图文字样式，可写文件。"
  "字体列表与样式表；写文件时返回文件路径"
  "示例: 运行 fonts:font-list 后按提示选择是否写文件"
  (initget "Yes No")
  (setq kw (getkword "\n写出到文件?[是(Y)/否(N)]<否>:"))
  (if (= kw "Yes")
      (progn
        (setq out-file (getfiled "输出字体清单" "font-list.txt" "txt" 1))
        (if out-file
            (setq res (fonts:ai-font-list out-file))
          (princ "font-list: 未选择输出文件。")))
    (progn
      (setq res (fonts:ai-font-list nil))
      (princ "\n=== SHX 字体文件 ===")
      (foreach f (car res) (princ (strcat "\n" f)))
      (princ "\n=== 当前图文字样式 ===")
      (foreach s (cadr res) (princ (strcat "\n" (fonts:style-line s))))))
  (princ))
