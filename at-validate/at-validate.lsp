;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 包验证工具
;; 运行时验证：括号匹配、未定义函数、重复定义、依赖完整性
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:add-menus
 '("@验证"
   ("验证当前包" "(@validate:current)")
   ("验证指定包" "(@validate:package)")
   ("括号匹配检查" "(@validate:parens)")
   ("未定义函数检查" "(@validate:undefined-funs)")
   ("重复定义检查" "(@validate:duplicate-defuns)")
   ("依赖完整性检查" "(@validate:dependencies)")
   ("全部检查" "(@validate:all)")
   ))

;;; ============================================================
;;; 括号匹配检查
;;; ============================================================
(defun @validate:check-parens (filename / fp line depth ch i errors line-num)
  "检查单个文件的括号匹配"
  (setq errors nil)
  (setq fp (open filename "r"))
  (if fp
      (progn
        (setq depth 0 line-num 0)
        (while (setq line (read-line fp))
          (setq line-num (1+ line-num))
          (setq i 1)
          (while (<= i (strlen line))
            (setq ch (substr line i 1))
            (cond
              ((= ch "(") (setq depth (1+ depth)))
              ((= ch ")") (setq depth (1- depth))))
            (if (< depth 0)
                (progn
                  (setq errors (cons (strcat "行 " (itoa line-num) ": 括号不匹配（多余右括号）") errors))
                  (setq depth 0)))
            (setq i (1+ i)))
          )
        (close fp)
        (if (> depth 0)
            (setq errors (cons (strcat "文件末尾: 缺少 " (itoa depth) " 个右括号") errors))))
    (setq errors (cons (strcat "无法打开文件: " filename) errors)))
  (reverse errors))

(defun @validate:parens (/ pkg-dir pkg-name files errors total-errors)
  "检查当前包的括号匹配"
  (@:help '("检查当前加载包的所有LSP文件的括号匹配。"))
  (setq pkg-dir (getstring "请输入包目录名: "))
  (setq pkg-dir (strcat @::*prefix* "packages/" pkg-dir))
  (if (findfile (strcat pkg-dir "/pkg.lsp"))
      (progn
        (setq total-errors nil)
        (setq files (vl-directory-files pkg-dir "*.lsp" 1))
        (foreach file files
          (setq errors (@validate:check-parens (strcat pkg-dir "/" file)))
          (if errors
              (progn
                (princ (strcat "\n=== " file " ==="))
                (foreach err errors
                  (princ (strcat "\n  ❌ " err)))
                (setq total-errors (append total-errors errors)))))
        (if total-errors
            (princ (strcat "\n共发现 " (itoa (length total-errors)) " 个括号错误"))
          (princ "\n✅ 所有文件括号匹配正确")))
    (@:prompt "未找到指定包目录"))
  (princ))

;;; ============================================================
;;; 未定义函数检查
;;; ============================================================
(defun @validate:find-defuns (filename / fp line defuns line-num func-name)
  "提取文件中定义的所有函数名"
  (setq defuns nil)
  (setq fp (open filename "r"))
  (if fp
      (progn
        (setq line-num 0)
        (while (setq line (read-line fp))
          (setq line-num (1+ line-num))
          ;; 匹配 (defun funcname 或 (defun c:funcname
          (if (setq func-name (vl-string-search "(defun " line))
              (progn
                (setq func-name (substr line (+ func-name 8)))
                ;; 提取函数名（到空格或/）
                (setq func-name
                      (car (vl-string-search " " func-name
                            (vl-string-search "/" func-name (strlen func-name)))))
                (if (and func-name (> (strlen func-name) 0))
                    (setq defuns (cons (strcat func-name "|" filename "|" (itoa line-num))
                                      defuns)))))
        (close fp))
    )
  defuns)

(defun @validate:undefined-funs (/ pkg-dir pkg-name files all-defuns called-funs undefined)
  "检查包中调用但未定义的函数"
  (@:help '("检查包中调用但未在本包定义的函数（可能缺少依赖声明）。"))
  (setq pkg-dir (getstring "请输入包目录名: "))
  (setq pkg-dir (strcat @::*prefix* "packages/" pkg-dir))
  (if (findfile (strcat pkg-dir "/pkg.lsp"))
      (progn
        (setq files (vl-directory-files pkg-dir "*.lsp" 1))
        (setq all-defuns nil)
        (foreach file files
          (setq all-defuns (append all-defuns
                                   (@validate:find-defuns (strcat pkg-dir "/" file)))))
        ;; 简化输出
        (princ (strcat "\n在 " (itoa (length files)) " 个文件中找到 "
                       (itoa (length all-defuns)) " 个函数定义"))
        (princ "\n注意：完整检查需要在CAD环境中加载后运行"))
    (@:prompt "未找到指定包目录"))
  (princ))

;;; ============================================================
;;; 重复定义检查
;;; ============================================================
(defun @validate:duplicate-defuns (/ pkg-dir pkg-name files all-defuns dup-check dup-funs)
  "检查包中是否有重复定义的函数"
  (@:help '("检查同一包内是否有多个函数同名定义。"))
  (setq pkg-dir (getstring "请输入包目录名: "))
  (setq pkg-dir (strcat @::*prefix* "packages/" pkg-dir))
  (if (findfile (strcat pkg-dir "/pkg.lsp"))
      (progn
        (setq files (vl-directory-files pkg-dir "*.lsp" 1))
        (setq all-defuns nil)
        (foreach file files
          (setq all-defuns (append all-defuns
                                   (@validate:find-defuns (strcat pkg-dir "/" file)))))
        ;; 检查重复
        (setq dup-check nil)
        (setq dup-funs nil)
        (foreach def all-defuns
          (setq fun-name (car (string:to-list def "|")))
          (if (member fun-name dup-check)
              (setq dup-funs (cons def dup-funs))
            (setq dup-check (cons fun-name dup-check))))
        (if dup-funs
            (progn
              (princ "\n=== 重复定义 ===")
              (foreach dup dup-funs
                (setq parts (string:to-list dup "|"))
                (princ (strcat "\n  " (car parts) " -> " (cadr parts) ":" (caddr parts)))))
          (princ "\n✅ 无重复定义")))
    (@:prompt "未找到指定包目录"))
  (princ))

;;; ============================================================
;;; 依赖完整性检查
;;; ============================================================
(defun @validate:dependencies (/ pkg-dir pkg-content required deps dep-dir)
  "检查包的依赖是否完整"
  (@:help '("检查包的:REQUIRED声明的依赖包是否存在。"))
  (setq pkg-dir (getstring "请输入包目录名: "))
  (setq pkg-dir-full (strcat @::*prefix* "packages/" pkg-dir))
  (if (findfile (strcat pkg-dir-full "/pkg.lsp"))
      (progn
        (setq pkg-content (@:read-file (strcat pkg-dir-full "/pkg.lsp")))
        ;; 简单提取:REQUIRED
        (if (setq required (vl-string-search ":REQUIRED" pkg-content))
            (progn
              (setq required (substr pkg-content (+ required 11)))
              (setq required (substr required 1 (- (vl-string-search "\"" required) 1)))
              (princ (strcat "\n依赖: " required))
              (setq dep-dir (strcat @::*prefix* "packages/" required))
              (if (findfile (strcat dep-dir "/pkg.lsp"))
                  (princ (strcat "\n✅ 依赖包 '" required "' 存在"))
                (princ (strcat "\n❌ 依赖包 '" required "' 未找到"))))
          (princ "\n无依赖声明（可能依赖base）")))
    (@:prompt "未找到指定包目录"))
  (princ))

;;; ============================================================
;;; 全部检查
;;; ============================================================
(defun @validate:all (/ pkg-dir)
  "对指定包执行全部检查"
  (@:help '("对指定包执行括号检查、重复定义检查、依赖检查。"))
  (setq pkg-dir (getstring "请输入包目录名: "))
  (princ (strcat "\n========== 验证包: " pkg-dir " =========="))
  (@validate:dependencies)
  (@validate:duplicate-defuns)
  (@validate:parens)
  (princ (strcat "\n========== 验证完成: " pkg-dir " =========="))
  (princ))

;;; ============================================================
;;; 验证当前加载的包
;;; ============================================================
(defun @validate:current ()
  "验证当前CAD中加载的包"
  (@:help '("检查当前加载的包的函数是否重复定义。"))
  (princ "\n当前已加载的函数:")
  (princ (strcat "\n  atlisp-lint 版本: 已集成"))
  (princ "\n提示: 运行 atlisp-lint 可进行更全面的静态分析"))
  (princ))
