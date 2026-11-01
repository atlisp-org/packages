;;; 批量打印：按图框实际尺寸识别图幅/横竖后输出 PDF，可选用 pdftk 合并成一个文件。

(@:add-menu "通用打印" "批打输出" "(@plot:plot-all)")
(@:add-menu "通用打印" "批打并合并" "(@plot:ai-plot-all nil T)")

(defun @plot:dir-normalize (dir / sep)
  "目录字符串规范化：去掉尾部斜杠，再按当前平台补一个分隔符。"
  "string - 以分隔符结尾的目录；dir 为 nil 时返回 nil"
  "(@plot:dir-normalize \"D:/out\") => 以分隔符结尾的目录字符串"
  (if (null dir)
      nil
    (progn
      (while (and (> (strlen dir) 0)
                  (member (last (vl-string->list dir)) '(47 92)))
        (setq dir (substr dir 1 (1- (strlen dir)))))
      (setq sep (if (and (boundp '$platform) (eq $platform 'linux)) "/" "\\"))
      (strcat dir sep))))

(defun @plot:ensure-dir (dir)
  "确保目录存在（逐级创建），返回规范化后的目录；无法创建返回 nil。"
  "string - 以分隔符结尾的目录；失败返回 nil"
  "(@plot:ensure-dir \"D:/out\") => 已存在的目录（以分隔符结尾）"
  (if (null dir)
      nil
    (progn
      (setq dir (@plot:dir-normalize dir))
      (if (null (vl-file-directory-p dir))
          (progn
            (vl-catch-all-apply 'vl-directory-make (list dir))
            (if (null (vl-file-directory-p dir))
                (vl-catch-all-apply '@::mkdir (list (@::path dir))))))
      (if (vl-file-directory-p dir) dir nil))))

(defun @plot:sort-frames (recs)
  "图框按左上角排序：先上后下（y 降序），同一行再从左到右（x 升序）。
recs 的元素为 (四点表 . 外框)，外框为 (xmin ymin xmax ymax)。"
  "list - 排序后的图框表"
  "(@plot:sort-frames @plot:frames)"
  (vl-sort recs
           '(lambda (a b / ba bb)
              (setq ba (cdr a) bb (cdr b))
              (if (equal (nth 3 ba) (nth 3 bb) 1.0)
                  (< (car ba) (car bb))
                (> (nth 3 ba) (nth 3 bb))))))

(defun @plot:collect-frames (/ ss recs en pts)
  "收集 temp-frames 图层上的图框（标记后的矩形多段线），按左上角排序。"
  "list - ((四点表 . 外框) ...)；没有图框返回 nil"
  "(@plot:collect-frames)"
  (setq recs nil)
  (if (setq ss (ssget "x" '((0 . "LWPOLYLINE")
                            (90 . 4)
                            (70 . 1)
                            (8 . "temp-frames"))))
      (foreach en (pickset:to-list ss)
        (setq pts (@:get-rec-points en))
        (if (and pts (>= (length pts) 4))
            (setq recs (cons (cons pts (@plot:pts->box pts)) recs)))))
  (@plot:sort-frames (reverse recs)))

(defun @plot:safe-title (title / c)
  "把图名里不能出现在文件名中的字符替换为下划线。"
  "string - 可用于文件名的图名"
  "(@plot:safe-title \"a/b:c\") => \"a_b_c\""
  (foreach c '(92 47 60 62 58 63 33 124 42 34)
    (setq title (vl-string-translate (chr c) "_" title)))
  (vl-string-trim " " title))

(defun @plot:map-title (box / res title)
  "取图框内最可能的图名，返回带前导连字符的 \"-图名\"；没有图名返回空串。
box: 图框外框 (xmin ymin xmax ymax)。图名识别复用 at-pm，未安装时返回空串。"
  "string - \"-图名\" 或 \"\""
  "(@plot:map-title '(0 0 42000 29700))"
  (setq res nil)
  (if @pm:pickout-maptitle-first
      (setq res (vl-catch-all-apply
                 '@pm:pickout-maptitle-first
                 (list (list (list (car box) (cadr box))
                             (list (nth 2 box) (nth 3 box)))))))
  (if (vl-catch-all-error-p res) (setq res nil))
  (if res
      (progn
        (setq title (if (listp res) (car res) res))
        ;; 两个候选时 @pm:pickout-maptitle-first 返回 ((图名 . 概率) ...)
        (if (and title (/= 'str (type title)))
            (setq title (car title)))
        (if (= 'str (type title))
            (progn
              (setq title (@plot:safe-title title))
              (if (/= "" title) (strcat "-" title) ""))
          ""))
    ""))

(defun @plot:frame-paper (box / w h ft)
  "按图框外框识别图幅/横竖，识别失败才回退配置默认值。
返回 (图幅名 方向)，方向 L=横 P=竖。"
  "list - (图幅名 方向)"
  "(@plot:frame-paper '(0 0 42000 29700)) => (\"A3\" \"L\")"
  (setq w (- (nth 2 box) (car box))
        h (- (nth 3 box) (cadr box)))
  (setq ft (@plot:get-frame-type w h))
  (if ft
      (list (car ft) (cdr ft))
    (list (@:get-config '@plot:default-frame)
          (@:get-config '@plot:default-orient))))

(defun @plot:plot-frames (frames dir / i pdf pdfs paper box res)
  "把图框逐个打印为 PDF，文件名「序号-图名.pdf」，返回成功生成的路径表（按打印顺序）。
frames: (@plot:collect-frames) 的返回值；dir: 已存在且以分隔符结尾的目录。"
  "list - PDF 路径表；全部失败返回 nil"
  "(@plot:plot-frames (@plot:collect-frames) \"D:/out\")"
  (setq i 0)
  (setq pdfs nil)
  (setvar "cmdecho" 0)
  (foreach rec frames
    (setq box (cdr rec))
    (setq paper (@plot:frame-paper box))
    (setq pdf (strcat dir
                      (itoa (setq i (1+ i)))
                      (@plot:map-title box)
                      ".pdf"))
    (setq res (vl-catch-all-apply
               'plot:to-pdf
               (list (car paper) (cadr paper)
                     (list (car box) (cadr box))
                     (list (nth 2 box) (nth 3 box))
                     pdf)))
    (if (vl-catch-all-error-p res)
        (princ (strcat "\n[at-plot] 打印出错: "
                       (vl-catch-all-error-message res))))
    (if (findfile pdf)
        (setq pdfs (cons pdf pdfs))
      (princ (strcat "\n[at-plot] 打印失败: " pdf))))
  (setvar "cmdecho" 1)
  (reverse pdfs))

(defun @plot:pdftk-exe (/ exe cands path)
  "查找 pdftk 可执行文件：优先 @lisp 的 bin 目录，其次系统 PATH。找不到返回 nil。"
  "string - pdftk 路径；找不到返回 nil"
  "(@plot:pdftk-exe)"
  (setq exe nil)
  (setq cands (list
               (if (boundp '@::*prefix*)
                   (strcat @::*prefix* "bin\\pdftk.exe"))
               (if (boundp '@::*prefix*)
                   (strcat @::*prefix* "bin/pdftk"))
               "bin\\pdftk.exe"
               "bin/pdftk"))
  (foreach path cands
    (if (and (null exe) path (findfile path))
        (setq exe (findfile path))))
  (if (null exe)
      (setq exe (system:which "pdftk")))
  exe)

(defun @plot:run-shell (cmdline / res)
  "执行命令行并立即返回（Windows 走 @lisp 的 shell-bg）。启动失败返回 nil。"
  "T 或 nil"
  "(@plot:run-shell \"pdftk a.pdf b.pdf cat output c.pdf\")"
  (if (null cmdline)
      nil
    (if (and (boundp '@::platform-is-linux-p) (@::platform-is-linux-p))
        nil
      (progn
        (if @::ensure-start-enabled (@::ensure-start-enabled))
        (setq res (vl-catch-all-apply '@::cmd (list "shell-bg" cmdline)))
        (not (vl-catch-all-error-p res))))))

(defun @plot:pdf-merge (files out-file / exe cmd n)
  "按 files 顺序用 pdftk 合并为 out-file。
成功返回 out-file；pdftk 不可用或合并失败返回 nil（调用方保留单独 PDF）。"
  "string - 合并后的 PDF 路径；失败返回 nil"
  "(@plot:pdf-merge (list \"D:/out/1.pdf\" \"D:/out/2.pdf\") \"D:/out/merge-all.pdf\")"
  (setq exe (@plot:pdftk-exe))
  (if (null exe)
      (progn
        (princ "\n[at-plot] 未找到 pdftk，跳过合并（PDF 已分别输出）。")
        nil)
    (progn
      (if (findfile out-file) (vl-file-delete out-file))
      (setq cmd (strcat "\"" exe "\""))
      (foreach f files
        (setq cmd (strcat cmd " \"" f "\"")))
      (setq cmd (strcat cmd " cat output \"" out-file "\""))
      (if (@plot:run-shell cmd)
          (progn
            ;; shell-bg 是后台执行，轮询等待输出文件（最多 60 秒）
            (setq n 0)
            (while (and (< n 120) (null (findfile out-file)))
              (sleep 0.5)
              (setq n (1+ n)))
            (if (findfile out-file)
                out-file
              (progn
                (princ "\n[at-plot] pdftk 合并未完成，已保留单独 PDF。")
                nil)))
        (progn
          (princ "\n[at-plot] 无法执行 pdftk，已保留单独 PDF。")
          nil)))))

(defun @plot:ai-plot-all (out-dir merge-p / frames dir pdfs merged)
  "AI 调用入口：无交互批量打印 temp-frames 图层上的全部图框，返回 PDF 路径。
out-dir: PDF 输出目录；nil 时用配置 @plot:export-path（不存在会自动创建）
merge-p: T 时按图框左上角排序后用 pdftk 合并为一个 PDF
返回: 合并成功返回合并文件路径；否则返回 PDF 路径表；没有图框或全部失败返回 nil
示例: (@plot:ai-plot-all \"D:/out\" T)"
  "string 或 list - 合并文件路径 / PDF 路径表；失败 nil"
  "(@plot:ai-plot-all \"D:/out\" T)"
  (setq frames (@plot:collect-frames))
  (if (null frames)
      (progn
        (princ "\n[at-plot] temp-frames 图层上没有图框，请先识别并标记图框。")
        nil)
    (progn
      (if (null out-dir)
          (setq out-dir (@:get-config '@plot:export-path)))
      (setq dir (@plot:ensure-dir out-dir))
      (if (null dir)
          (progn
            (princ "\n[at-plot] 无法创建输出目录，请检查 out-dir。")
            nil)
        (progn
          (setq pdfs (@plot:plot-frames frames dir))
          (if (null pdfs)
              (progn
                (princ "\n[at-plot] 没有生成任何 PDF。")
                nil)
            (progn
              (princ (strcat "\n[at-plot] 已输出 " (itoa (length pdfs))
                             " 个 PDF 到 " dir))
              (if merge-p
                  (progn
                    (setq merged
                          (@plot:pdf-merge pdfs (strcat dir "merge-all.pdf")))
                    (if merged
                        (progn
                          (princ (strcat "，已合并为 " merged "。"))
                          merged)
                      (progn
                        (princ "；合并未完成，返回单独 PDF 列表。")
                        pdfs)))
                pdfs))))))))

(defun @plot:plot-all (/ frames dir pdfs)
  "交互入口：打印 temp-frames 图层上已标记的图框（图幅/横竖自动识别）。
输出目录取配置 @plot:export-path，文件名「序号-图名.pdf」。"
  "nil"
  "(@plot:plot-all)"
  (@:help "打印标记的图框")
  (setq frames (@plot:collect-frames))
  (if (null frames)
      (princ "\n[at-plot] 未找到标记图框，请先执行 AI识别图框。")
    (progn
      (@:log "INFO" "打印标记的图框")
      (setq dir (@plot:ensure-dir (@:get-config '@plot:export-path)))
      (if (null dir)
          (princ "\n[at-plot] 打印输出路径无效，请检查配置 @plot:export-path。")
        (progn
          (setq pdfs (@plot:plot-frames frames dir))
          (princ (strcat "\n[at-plot] 已输出 "
                         (itoa (length pdfs))
                         " 个 PDF 到 "
                         dir))))))
  (princ))
