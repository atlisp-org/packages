;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mini-text 字体类命令：自动替换缺失字库 fix-fonts、全部字体用我的字库 use-myfonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @text:font-exists-p (font / p)
  "判断字体文件是否存在（CAD 搜索路径 + 系统字体目录）。
  返回 t/nil。"
  "t 或 nil"
  (if (findfile font)
    t
    (if (setq p (getenv "WINDIR"))
        (findfile (strcat p "\\fonts\\" font))
        (if (setq p (getenv "FONTDIR"))
            (findfile (strcat p "/" font))
            nil))))

(defun @text:fix-fonts-impl (font big-font dry-run / res st name f b)
  "缺失字库替换内部实现：扫描文字样式，字体/大字体文件找不到的
  替换为指定字体（dry-run 为 t 时只报告）。
  返回 (样式名 . 旧字体) 列表。"
  "list"
  (setq res nil)
  (setq st (tblnext "style" t))
  (while st
    (if (and (= "STYLE" (cdr (assoc 0 st)))
             (setq name (cdr (assoc 2 st))))
        (progn
          (setq f (cdr (assoc 4 st)))
          (setq b (cdr (assoc 41 st)))
          (if (and f (/= "" f) (not (@text:font-exists-p f)))
              (progn
                (princ (strcat "\n  " name " : " f " → " font))
                (setq res (cons (cons name f) res))
                (if (not dry-run)
                    (entmod (list-subst (cons 4 font) st)))))
          (if (and b (/= "" b) (not (@text:font-exists-p b)))
              (progn
                (princ (strcat "\n  " name " : " b "(大字体) → " big-font))
                (setq res (cons (cons (strcat name "#BIG") b) res))
                (if (not dry-run)
                    (entmod (list-subst (cons 41 big-font) st))))))
    )
    (setq st (tblnext st)))
  (if (and res (not dry-run))
    (command "_.REGEN" "All"))
  (reverse res))

(defun @text:ai-fix-fonts (repl-font dry-run / res)
  "自动替换缺失字库（AI 入口，无交互）。
  扫描全部文字样式，字体或大字体文件找不到的替换为指定字体
  （大字体缺失时替换为空）。
  repl-font: 替代字体文件名 (nil=simplex.shx)；
  dry-run: t=只报告不修改。
  返回 (样式名 . 旧字体) 列表（空列表表示无缺失），失败返回 nil。"
  "list 或 nil"
  (progn
    (setq res (@text:fix-fonts-impl (or repl-font "simplex.shx") "" dry-run))
    (if (null res)
        (princ "\n未发现缺失字库。")
        (princ (strcat "\n" (if dry-run "发现（未修改） " "已替换 ")
                       (itoa (length res)) " 项缺失字库")))
    res))

(defun @text:fix-fonts (/ font big dry)
  "自动替换缺失字库：扫描文字样式，把字体缺失的样式换成指定字体。
  返回 nil。"
  "nil"
  (setq font (getstring "\n替代字体文件 <simplex.shx>: "))
  (setq big (getstring "\n替代大字体文件 (回车清空): "))
  (setq dry (wcmatch (getkword "\n立即执行? [Y执行/N只报告] <Y>: ") "*N*"))
  (if (@text:fix-fonts-impl (if (= "" font) "simplex.shx" font)
                                big dry)
    (princ "\n完成，明细见上方输出。")
    (princ "\n未发现缺失字库。"))
  (princ))

(defun @text:ai-use-myfonts (font big-font / res st name f st2)
  "全部字体用我的字库（AI 入口，无交互）。
  把所有文字样式的字体替换为指定字体。
  font: 字体文件名 (nil=simplex.shx)；
  big-font: 大字体文件名 (nil=保持原大字体)。
  返回 (样式名 . 旧字体) 列表（空列表表示无需修改）。"
  "list"
  (if (null font) (setq font "simplex.shx"))
  (setq res nil)
  (setq st (tblnext "style" t))
  (while st
    (if (and (= "STYLE" (cdr (assoc 0 st)))
             (setq name (cdr (assoc 2 st))))
        (progn
          (setq f (cdr (assoc 4 st)))
          (if (or (/= (strcase  (@text:str f)) (strcase font))
                  (and big-font
                       (/= (strcase  (@text:str (cdr (assoc 41 st))))
                           (strcase big-font))))
              (progn
                (setq st2 (list-subst (cons 4 font) st))
                (if big-font
                    (setq st2 (list-subst (cons 41 big-font) st2)))
                (entmod st2)
                (princ (strcat "\n  " name " : "  (@text:str f) " → " font))
                (setq res (cons (cons name  (@text:str f)) res))))))
    (setq st (tblnext st)))
  (if res
    (progn
      (command "_.REGEN" "All")
      (princ (strcat "\n已修改 " (itoa (length res)) " 个文字样式。"))))
  (reverse res))

(defun @text:use-myfonts (/ font big)
  "全部字体用我的字库：所有文字样式字体替换为指定字体。
  返回 nil。"
  "nil"
  (setq font (getstring "\n字体文件 <simplex.shx>: "))
  (setq big (getstring "\n大字体文件 (回车保持原大字体): "))
  (if (@text:ai-use-myfonts (if (= "" font) "simplex.shx" font)
                                (if (= "" big) nil big))
    (princ "\n完成。")
    (princ "\n没有需要修改的文字样式。"))
  (princ))
