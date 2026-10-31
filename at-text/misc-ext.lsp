;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mini-text 杂项命令：输出文字到粘贴板 text2clipboard、
;; 词库输入 text-lib、多文件查找替换 find
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ============ 输出文字到粘贴板 ============

(defun @text:ai-text2clipboard (ents sep / infos s s2)
  "输出文字到粘贴板（AI 入口，无交互）。
  把每个文字的内容按顺序用分隔符拼接后写入系统粘贴板。
  ents: 文字图元 ename 列表；sep: 分隔符 (nil=换行)。
  返回拼接后的字符串，失败返回 nil。"
  "string 或 nil"
  (if (null ents)
      (progn (princ "@text:ai-text2clipboard: ents 为空") nil)
      (progn
        (setq infos (@text:ents-to-infos ents))
        (if (null infos)
            (progn (princ "@text:ai-text2clipboard: 无有效文字") nil)
            (progn
              (if (null sep) (setq sep "\n"))
              (setq s (cdr (assoc 'content (car infos))))
              (foreach s2 (cdr infos)
                (setq s (strcat s sep (cdr (assoc 'content s2)))))
              (if (vl-catch-all-error-p
                   (vl-catch-all-apply 'clipboard:setdata (list s)))
                  (progn
                    (princ "@text:ai-text2clipboard: 写入粘贴板失败")
                    nil)
                  (progn
                    (princ (strcat "\n已输出 " (itoa (length infos))
                                   " 个文字到粘贴板"))
                    s)))))))

(defun @text:text2clipboard (/ ss sep)
  "输出文字到粘贴板：把选中文字内容按顺序拼接后写入系统粘贴板。
  返回 nil。"
  "nil"
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
      (progn
        (setq sep (getstring "\n分隔符 (回车=换行): "))
        (if (@text:ai-text2clipboard (pickset:to-list ss)
                                          (if (= "" sep) nil sep))
            (princ "\n完成。")
            (princ "\n输出失败。"))
      )
      (princ "\n未选择文字，命令取消。"))
  (princ))

;; ============ 词库输入 ============

(defun @text:word-lib-path (name)
  "取包内置词库文件路径（存在时返回完整路径）。
  name: 文件名（如 floor.txt）。
  返回 string 或 nil。"
  "string 或 nil"
  (if (boundp '@::*prefix*)
      (findfile (strcat @::*prefix* "packages/at-text/" name))
      (findfile (strcat "packages/at-text/" name))))

(defun @text:strip-line (s)
  "清理词库行：去掉 \r 和首尾空白。
  返回 string，空行返回 nil。"
  "string 或 nil"
  (if s
      (progn
        (setq s (@text:trim (vl-string-translate "\r" "" s)))
        (if (/= "" s) s nil))
      nil))

(defun @text:load-word-lib (file / contents res w)
  "读取词库文件（UTF-8，一行一词）。
  返回词列表，文件不可读返回 nil。"
  "list 或 nil"
  (if (setq contents (@::get-file-contents file))
      (progn
        (setq res nil)
        (foreach w (@::string-to-list contents "\n")
          (if (setq w (@text:strip-line w))
              (setq res (cons w res))))
        (reverse res))
      nil))

(defun @text:ai-text-lib (word pt style height / st ent)
  "词库输入（AI 入口，无交互）：用指定词插入单行文字。
  word: 词内容；pt: 插入点 (x y z)；
  style: 文字样式 (nil=当前 textstyle)；height: 字高 (nil=2.5)。
  返回插入的 TEXT ename，失败返回 nil。"
  "ename 或 nil"
  (if (and word pt (/= "" (@text:str word)))
      (progn
        (setq st (if (and style (/= "" (@text:str style)))
                     (@text:str style)
                     (getvar "textstyle")))
        (if (setq ent (entity:make-text (@text:str word) pt
                                         (if (and (numberp height)
                                                  (> height 0))
                                             (float height) 2.5)
                                         0 1 0 "LB"))
            (progn
              (entmod (append (entget ent)
                              (list (cons 7 st)
                                    (cons 8 (getvar "clayer")))))
              (entupd ent)
              (princ (strcat "\n已插入: " (@text:str word)))
              ent)
            nil)
      (progn (princ "@text:ai-text-lib: 缺少词或插入点") nil))))

(defun @text:text-lib (/ name file words w i n pt)
  "词库输入：从词库选词，在指定点插入文字。
  内置词库：floor（楼层名称）、room（房间名称），
  也可输入外部词库文件路径（UTF-8，一行一词）。
  返回 nil。"
  "nil"
  (setq name (getstring "\n词库 (floor/room 或词库文件路径，回车取消): "))
  (if (/= "" name)
      (progn
        (cond
          ((wcmatch (strcase name) "*FLOOR*")
           (setq file (@text:word-lib-path "floor.txt")))
          ((wcmatch (strcase name) "*ROOM*")
           (setq file (@text:word-lib-path "room.txt")))
          (t
           (setq file (findfile name))))
        (if file
            (progn
              (setq words (@text:load-word-lib file))
              (if (null words)
                  (princ (strcat "\n词库无有效词条: " file))
                  (progn
                    (princ (strcat "\n词表 (共 " (itoa (length words)) " 词):"))
                    (setq i 1)
                    (foreach w words
                      (princ (strcat "\n  " (itoa i) ". " w))
                      (setq i (1+ i)))
                    (if (and (setq n (getint (strcat "\n词编号 <1-"
                                                     (itoa (length words))
                                                     ">: ")))
                             (> n 0) (<= n (length words)))
                        (if (setq pt (getpoint "\n插入点: "))
                            (@text:ai-text-lib (nth (1- n) words) pt nil nil)
                            (princ "\n取消。"))
                        (princ "\n编号无效。"))))
              )
            (princ (strcat "\n词库文件不存在: " name))))
      (princ "\n取消。"))
  (princ))

;; ============ 多文件查找替换 ============

(defun @text:find-in-doc (doc findstr repl / cur-doc ss n i e content res)
  "在指定文档中查找替换文字（TEXT/MTEXT/ATTRIB），结束后恢复原活动文档。
  doc: VLA 文档对象；findstr: 查找串；repl: 替换串 (空=删除)。
  返回替换的文字数量。"
  "number"
  (if (and doc findstr)
      (progn
        (setq cur-doc (vla-get-activedocument (vlax-get-acad-object)))
        (vla-activate doc)
        (command "_.UNDO" "Begin")
        (setq res 0)
        (if (setq ss (ssget "X" '((0 . "TEXT,MTEXT,ATTRIB"))))
            (progn
              (setq n (sslength ss))
              (setq i 0)
              (while (< i n)
                (setq e (ssname ss i))
                (if (setq content (@text:entity-content e))
                    (if (vl-string-search findstr content)
                        (progn
                          (@text:set-content e
                                                 (string:subst-all repl
                                                                   findstr
                                                                   content))
                          (setq res (1+ res)))))
                (setq i (1+ i)))
        (command "_.UNDO" "End")
        (if cur-doc (vla-activate cur-doc))
        res)
            nil))
      nil))

(defun @text:ai-find (findstr repl files / docs i d name n res)
  "多文件查找替换（AI 入口，无交互）。
  findstr: 查找串（区分大小写，不能为空）；repl: 替换串 (nil/空=删除)；
  files: 文件名过滤列表（不区分大小写，支持通配符，匹配完整路径），
  nil=当前全部已打开文档。
  返回 (文件名 . 替换数量) 列表，无匹配返回 nil。"
  "list 或 nil"
  (if (or (null findstr) (= "" findstr))
      (progn (princ "@text:ai-find: 查找串不能为空") nil)
      (progn
        (if (null repl) (setq repl ""))
        (setq docs (vla-get-documents (vlax-get-acad-object)))
        (setq res nil)
        (setq i 0)
        (while (< i (vla-get-count docs))
          (setq d (vlax-vla-item docs i))
          (setq name (@text:str (vla-get-filename d)))
          (if (or (null files)
                  (vl-some
                   (function (lambda (f)
                               (wcmatch
                                (strcase (vl-filename-as-tstring name))
                                (strcase (vl-filename-as-tstring
                                          (@text:str f))))))
                   files))
              (if (setq n (@text:find-in-doc d findstr repl))
                  (progn
                    (princ (strcat "\n" (vl-filename-base name) ": "
                                   (itoa n) " 个文字已替换"))
                    (setq res (cons (cons (vl-filename-base name) n) res)))))
          (setq i (1+ i)))
        (if res
            (reverse res)
            (progn
              (princ "\n未找到匹配的文字。")
              nil)))))

(defun @text:find (/ findstr repl f)
  "多文件查找替换：在当前打开的图纸中批量查找替换文字内容。
  返回 nil。"
  "nil"
  (if (setq findstr (getstring "\n查找串: "))
      (progn
        (setq repl (getstring "\n替换串 (回车删除): "))
        (setq f (getstring "\n文件过滤 (回车=全部打开图纸，如 *.dwg): "))
        (if (@text:ai-find findstr (if (= "" repl) nil repl)
                                (if (= "" f) nil (list f)))
            (princ "\n完成，明细见上方输出。")
            (princ "\n未找到。"))
      )
      (princ "\n取消。"))
  (princ))
