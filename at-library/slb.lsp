;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @图库 (at-library) — 幻灯片库 (.slb) 名称读取
;; 用于盘点旧图库/幻灯片库：不打开 CAD 图形即可列出库内全部幻灯片名称。
;; 代码思路来自明经论坛 thread-113336（gxl-slb2sldnames），按 SLB 目录结构重写：
;;   31 字节文件头 + N × (32 字节名称 + 4 字节偏移)，N = (文件大小 - 31) / 36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(entity:*))

(@:add-menu "@图库" "幻灯片库名称" "(at-library:slb-list)")

(defun at-library:slb-read-bytes (fp n / s i ch)
  "内部：从文件读 n 个字节，返回字符串（忽略 0 字节与 EOF）。"
  "字符串"
  "(at-library:slb-read-bytes fp 32)"
  (setq s "" i 0)
  (while (< i n)
    (setq ch (read-char fp))
    (if (and (numberp ch) (>= ch 0) (/= ch 0))
        (setq s (strcat s (chr ch))))
    (setq i (1+ i)))
  s)

(defun at-library:slb-names (file / fp size count i name res)
  "读取 AutoCAD 幻灯片库 (.slb) 中的幻灯片名称列表。
  参数：file 为 .slb 全路径（或支持路径下可找到的文件名）。
  返回名称字符串列表，失败返回 nil。"
  "字符串列表或 nil"
  "(at-library:slb-names \"D:/lib/old.slb\")"
  (setq file (if file (findfile file) nil))
  (if (null file)
      (progn (princ "at-library:slb-names 找不到文件") nil)
      (progn
        (setq size (vl-file-size file))
        (setq fp (open file "r"))
        (if (or (null fp) (null size) (< size 67))
            (progn
              (if fp (close fp))
              (princ "at-library:slb-names 文件不是有效的幻灯片库")
              nil)
            (progn
              ;; 跳过 31 字节文件头
              (at-library:slb-read-bytes fp 31)
              (setq count (fix (/ (- size 31) 36)))
              (setq i 0 res nil)
              (while (< i count)
                (setq name (vl-string-trim " \t"
                                           (at-library:slb-read-bytes fp 32)))
                ;; 读掉 4 字节偏移
                (at-library:slb-read-bytes fp 4)
                (if (> (strlen name) 0)
                    (setq res (cons name res)))
                (setq i (1+ i)))
              (close fp)
              (reverse res))))))

(defun at-library:ai-slb-names (file)
  "AI 调用入口：读取幻灯片库 (.slb) 的名称列表（无交互）。
  示例: (at-library:ai-slb-names \"D:/lib/old.slb\")"
  "名称字符串列表，失败返回 nil"
  "(at-library:ai-slb-names \"D:/lib/old.slb\")"
  (at-library:slb-names file))

(defun at-library:slb-list (/ file names)
  "幻灯片库名称：选择一个 .slb，列出库内全部幻灯片名称。"
  "名称字符串列表"
  "(at-library:slb-list)"
  (setq file (getfiled "选择幻灯片库" "" "slb" 0))
  (if file
      (progn
        (setq names (at-library:slb-names file))
        (if names
            (progn
              (princ (strcat "\n幻灯片库: " file))
              (foreach n names (princ (strcat "\n  " n)))
              (princ (strcat "\n共 " (itoa (length names)) " 个幻灯片。")))
            (princ "\n未读到幻灯片，文件可能不是 .slb 或已损坏。")))
      (princ "\n未选择文件。"))
  (princ))
