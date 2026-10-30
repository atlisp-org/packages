;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 填充 (at-hatch) — 图案文件命令
;; 填充图案提取输出、自定义填充图案（生成并载入 .pat）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun at-hatch:pat-path (file name / ext dir)
  "整理 .pat 输出路径（补扩展名，无目录时放图纸目录）"
  ".pat 文件全路径"
  "(at-hatch:pat-path \"a\" \"x\")"
  (if (or (null file) (= file "")) (setq file name))
  (setq dir (getvar "DWGPREFIX"))
  (if (or (null dir) (= dir "")) (setq dir ""))
  (if (not (vl-filename-directory file))
    (setq file (strcat dir file)))
  (setq ext (strcase (if (vl-filename-extension file) (vl-filename-extension file) "")))
  (if (/= ext ".PAT") (setq file (strcat file ".pat")))
  file)

(defun at-hatch:write-pat-multi (file entries / f r d)
  "把多个图案定义写入一个 .pat 文件"
  "写出的文件全路径或 nil"
  "(at-hatch:write-pat-multi \"/tmp/a.pat\" '((\"A\" ((0 0 0 0 10 1 -1)))))"
  (setq f (open file "w"))
  (if f
    (progn
      (foreach r entries
        (write-line (strcat "*" (car r) "," (car r)) f)
        (foreach d (cadr r)
          (if d (write-line (at-hatch:pat-line->str d) f)))
        (write-line "" f))
      (close f)
      file)
    nil))

(defun at-hatch:pat-entries (/ ss i e d nm defs res)
  "扫描图中所有填充，收集唯一的图案名与定义行"
  "((图案名 定义行表) ...)"
  "(at-hatch:pat-entries)"
  (setq ss (ssget "_X" '((0 . "HATCH"))) i 0 res nil)
  (while (and ss (< i (sslength ss)))
    (setq e (ssname ss i) d (entget e))
    (setq nm (cdr (assoc 2 d)) defs (at-hatch:dxf-pattern-lines d))
    (if (and nm defs (/= (strcase nm) "SOLID") (null (assoc nm res)))
      (setq res (append res (list (list nm defs)))))
    (setq i (1+ i)))
  res)

;; ---------------------------------------------------------------------------
;; 10. 填充图案提取输出
;; ---------------------------------------------------------------------------

(defun at-hatch:ai-hatch-out (out-file / entries res)
  "AI 入口：扫描图中填充的图案名，把图案定义写出到 .pat 文件"
  "写出的 .pat 文件全路径或 nil"
  "(at-hatch:ai-hatch-out \"/tmp/my-patterns.pat\")"
  (setq entries (at-hatch:pat-entries))
  (if (null entries)
    (progn (princ "\n图中没有可提取的图案填充。") nil)
    (progn
      (setq out-file (at-hatch:pat-path out-file "patterns"))
      (setq res (at-hatch:write-pat-multi out-file entries))
      (if res
        (progn
          (princ (strcat "\n已写出 " (itoa (length entries)) " 个图案定义: " res))
          res)
        (progn (princ "\n写文件失败。") nil)))))

(defun at-hatch:hatch-out (/ file res)
  "填充图案提取输出：扫描图中填充图案并写出 .pat 文件"
  ".pat 文件全路径或 nil"
  "(at-hatch:hatch-out)"
  (setq file (getfiled "输出图案文件" "patterns" "pat" 1))
  (if file
    (setq res (at-hatch:ai-hatch-out file))
    (progn (princ "\n已取消。") (setq res nil)))
  res)

;; ---------------------------------------------------------------------------
;; 11. 自定义填充图案
;; ---------------------------------------------------------------------------

(defun at-hatch:lines->defs (lines / defs l d)
  "把定义行输入（字符串或数值表）转成定义行表"
  "定义行表或 nil"
  "(at-hatch:lines->defs '(\"0,0,0,0,10,1,-1\"))"
  (setq defs nil)
  (foreach l lines
    (if (= (type l) 'STR)
      (setq d (at-hatch:parse-pat-line l))
      (if (and (listp l) (> (length l) 4))
        (setq d l)
        (setq d nil)))
    (if d (setq defs (append defs (list d)))))
  defs)

(defun at-hatch:ai-custom-pattern (name lines pat-file / defs file)
  "AI 入口：生成自定义填充图案 .pat 文件并载入支持路径。
lines 为定义行表（字符串 \"0,0,0,0,10,1,-1\" 或数值表 (0 0 0 0 10 1 -1)）；
pat-file 为 nil 时写到图纸目录 <name>.pat"
  ".pat 文件全路径或 nil"
  "(at-hatch:ai-custom-pattern \"斜线\" '(\"0,0,0,0,10,1,-1\") \"/tmp/x.pat\")"
  (setq defs (at-hatch:lines->defs lines))
  (if (or (null name) (null defs))
    (progn (princ "\n图案名或定义行无效。") nil)
    (progn
      (setq file (at-hatch:pat-path pat-file name))
      (setq file (at-hatch:write-pat file name defs))
      (if file
        (progn
          (at-hatch:load-pat file)
          (princ (strcat "\n已生成图案文件: " file))
          file)
        (progn (princ "\n写文件失败（目录可能不可写）。") nil)))))

(defun at-hatch:custom-pattern (/ name line lines file res bss r)
  "自定义填充：输入图案名与定义行，生成 .pat 并可用于填充"
  ".pat 文件全路径或 nil"
  "(at-hatch:custom-pattern)"
  (setq name (getstring "\n新图案名: "))
  (if (= name "")
    (progn (princ "\n未输入图案名。") (setq res nil))
    (progn
      (princ "\n输入图案定义行（格式: 角度,基点X,基点Y,偏移X,偏移Y,线型...），回车结束:")
      (setq lines nil)
      (while (setq line (getstring "\n定义行 <回车结束>: "))
        (if (/= line "") (setq lines (append lines (list line)))))
      (if (null lines)
        (progn (princ "\n未输入定义行。") (setq res nil))
        (progn
          (setq file (at-hatch:ai-custom-pattern name lines nil))
          (if (null file)
            (setq res nil)
            (progn
              (setq bss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE,SPLINE,REGION"))))
              (if bss
                (progn
                  (at-hatch:undo-start)
                  (setq r (at-hatch:ai-hatch (pickset:to-list bss) name 1.0 0.0 nil nil))
                  (at-hatch:undo-end)
                  (if r (princ "\n已用新图案填充所选边界。")))
                nil)
              res)))))))
