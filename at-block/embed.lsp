;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 块定义 → Lisp 源码（clean-room 重构，不依赖第三方 VLX）
;; 把普通块定义里的图元 DXF 数据生成为 (entmake '(...))，便于把符号库
;; 打包进 Lisp 程序。支持常见图元；不支持的图元会跳过并在结果中列出。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:add-menu "Block2" "块转Lisp源码" "(@block:block->lisp)")

;; 可以安全 entmake 的块内图元类型
(setq @block:*embed-types*
      '("LINE" "LWPOLYLINE" "POLYLINE" "VERTEX" "SEQEND"
        "CIRCLE" "ARC" "ELLIPSE" "TEXT" "MTEXT" "POINT"
        "SOLID" "3DFACE" "INSERT" "ATTDEF" "HATCH" "SPLINE"
        "DIMENSION"))

(defun @block:lisp-string (s / i c out)
  "把字符串转成可嵌入 Lisp 源码的双引号形式。"
  "字符串"
  "(@block:lisp-string \"A\\\"B\")"
  (if (null s)
      "\"\""
      (progn
        (setq out "\"" i 1)
        (while (<= i (strlen s))
          (setq c (substr s i 1))
          (cond
            ((= c "\\") (setq out (strcat out "\\\\")))
            ((= c "\"") (setq out (strcat out "\\\"")))
            (t (setq out (strcat out c))))
          (setq i (1+ i)))
        (strcat out "\""))))

(defun @block:clean-dxf (dxf / res x code)
  "去掉句柄/所有者/XData 等不应进入 entmake 的组码。"
  "DXF 点对表"
  "(@block:clean-dxf (entget (car (entsel))))"
  (setq res nil)
  (foreach x dxf
    (setq code (car x))
    (if (not (or (member code '(-1 5 330 360 -2 102 -3))
                 (and (numberp code) (>= code 1000) (<= code 1071))))
        (setq res (cons x res))))
  (reverse res))

(defun @block:block-record (name / bd dxf)
  "取块表记录并清理为 entmake 可用的 DXF 表。"
  "DXF 点对表或 nil"
  "(@block:block-record \"MYBLOCK\")"
  (if (setq bd (tblobjname "block" name))
      (progn
        (setq dxf (@block:clean-dxf (entget bd)))
        (if (not (assoc 3 dxf))
            (setq dxf (append dxf (list (cons 3 name)))))
        dxf)
      nil))

(defun @block:block-entities (name / e res dxf typ)
  "取块定义内所有图元的清理后 DXF 表。"
  "DXF 表列表"
  "(@block:block-entities \"MYBLOCK\")"
  (if (tblobjname "block" name)
      (progn
        (setq e (entnext (tblobjname "block" name)))
        (setq res nil)
        (while (and e (/= "ENDBLK" (entity:getdxf e 0)))
          (setq dxf (@block:clean-dxf (entget e)))
          (setq typ (cdr (assoc 0 dxf)))
          (if (and dxf typ (/= typ "ATTRIB"))
              (setq res (cons dxf res)))
          (setq e (entnext e)))
        (reverse res))
      nil))

(defun @block:block-deps (name / res typ)
  "取块定义内引用的嵌套块名列表。"
  "字符串列表"
  "(@block:block-deps \"MYBLOCK\")"
  (setq res nil)
  (foreach dxf (@block:block-entities name)
    (setq typ (cdr (assoc 0 dxf)))
    (if (and (member typ '("INSERT" "DIMENSION"))
             (setq typ (cdr (assoc 2 dxf)))
             (tblobjname "block" typ))
        (if (not (member typ res))
            (setq res (cons typ res)))))
  res)

(defun @block:collect-embed (name seen / deps)
  "递归收集块及其嵌套块，依赖在前、自身在后。"
  "块名列表"
  "(@block:collect-embed \"MYBLOCK\" nil)"
  (cond
    ((null name) seen)
    ((not (tblobjname "block" name)) seen)
    ((member name seen) seen)
    (t
     (setq deps (@block:block-deps name))
     (foreach dep deps
       (setq seen (@block:collect-embed dep seen)))
     (append seen (list name)))))

(defun @block:embed-code (name idx / rec ents deps dep out)
  "生成一个块定义函数的源码行列表。"
  "字符串列表"
  "(@block:embed-code \"MYBLOCK\" 1)"
  (setq rec (@block:block-record name))
  (setq ents (@block:block-entities name))
  (setq deps (@block:block-deps name))
  (setq out nil)
  (setq out
        (append out
                (list (strcat "(defun at-block:embed-" (itoa idx) " ()")
                      (strcat "  \"重建块 " name "。\"")
                      "  \"块名\"")))
  (foreach dep deps
    (setq out
          (append out
                  (list (strcat "  (at-block:ai-embed-block "
                                (@block:lisp-string dep) ")")))))
  (setq out
        (append out
                (list (strcat "  (if (not (tblsearch \"block\" "
                                (@block:lisp-string name) "))")
                      "      (progn")))
  (if rec
      (setq out
            (append out
                    (list (strcat "        (entmake '"
                                  (vl-prin1-to-string rec) ")")))))
  (foreach dxf ents
    (setq out
          (append out
                  (list (strcat "        (entmake '"
                                (vl-prin1-to-string dxf) ")")))))
  (setq out
        (append out
                (list "        (entmake '((0 . \"ENDBLK\") (100 . \"AcDbEntity\") (8 . \"0\") (100 . \"AcDbBlockEnd\")))"
                      "        )"
                      "      )"
                      (strcat "  " (@block:lisp-string name) ")"))))
  out)

(defun @block:embed-source (blocks / mapping i out b)
  "为块名列表生成完整 Lisp 源码。"
  "字符串"
  "(@block:embed-source '(\"A\" \"B\"))"
  (setq mapping nil i 0)
  (foreach b blocks
    (setq i (1+ i))
    (setq mapping (cons (cons b i) mapping)))
  (setq out
        (list ";;; 由 at-block:ai-block->lisp 生成的块定义重建源码。"
              ";;; 用法: (at-block:embed-block \"块名\") 或 (at-block:ai-embed-block \"块名\")"
              ""
              "(defun at-block:embed-block (name)"
              "  \"按块名重建块定义并返回块名；失败 nil。\""
              "  (cond"))
  (foreach b blocks
    (setq out (append out
                      (list (strcat "    ((= name " (@block:lisp-string b) ") (at-block:embed-"
                                    (itoa (cdr (assoc b mapping))) "))")))))
  (setq out (append out (list "    (t nil)))" "")))
  (foreach b blocks
    (setq out (append out
                      (@block:embed-code b (cdr (assoc b mapping)))
                      (list ""))))
  (setq out
        (append out
                (list
                 "(defun at-block:ai-embed-block (name)"
                 "  \"确保块及嵌套块已定义，返回块名；失败 nil。\""
                 "  (if (tblsearch \"block\" name)"
                 "      name"
                 "    (at-block:embed-block name)))"
                 "")))
  (apply 'strcat (mapcar '(lambda (x) (strcat x "\n")) out)))

(defun @block:embed-skipped (blocks / res typ)
  "收集将被跳过的图元类型。"
  "字符串列表"
  "(@block:embed-skipped '(\"MYBLOCK\"))"
  (setq res nil)
  (foreach b blocks
    (foreach dxf (@block:block-entities b)
      (setq typ (cdr (assoc 0 dxf)))
      (if (and typ (not (member typ @block:*embed-types*)) (not (member typ res)))
          (setq res (cons typ res)))))
  res)

(defun @block:ai-block->lisp (blkname outfile / name blocks src skipped fp)
  "AI 入口：把普通块定义生成为可重建的 Lisp 源码。
  blkname 可以是块名字符串或块参照 ename；outfile 为 nil 时返回源码字符串。
  返回 ((\"file\" . 路径) (\"blocks\" . 数量) (\"skipped\" . 类型列表)) 或 nil。
  示例: (@block:ai-block->lisp \"MYBLOCK\" \"C:/tmp/myblock.lsp\")"
  "点对表或 nil"
  "(@block:ai-block->lisp \"MYBLOCK\" nil)"
  (cond
    ((p:enamep blkname)
     (if (= "INSERT" (entity:getdxf blkname 0))
         (setq name (block:get-effectivename blkname))
         (setq name nil)))
    ((stringp blkname) (setq name blkname))
    (t (setq name nil)))
  (if (or (null name) (null (tblobjname "block" name)))
      (progn (princ "at-block: 找不到块定义") nil)
      (progn
        (setq blocks (@block:collect-embed name nil))
        (setq skipped (@block:embed-skipped blocks))
        (setq src (@block:embed-source blocks))
        (if outfile
            (progn
              (setq fp (open outfile "w"))
              (if fp
                  (progn
                    (write-line src fp)
                    (close fp)
                    (list (cons "file" outfile)
                          (cons "blocks" (length blocks))
                          (cons "skipped" skipped)))
                  (progn (princ "at-block: 无法写入文件") nil)))
            (list (cons "source" src)
                  (cons "blocks" (length blocks))
                  (cons "skipped" skipped))))))

(defun @block:block->lisp (/ ent name path res)
  "交互：选择块参照，生成重建该块定义的 Lisp 源码。"
  "点对表或 nil"
  "(@block:block->lisp)"
  (@:help "选择块参照，生成可重建该块的 Lisp 源码")
  (setq ent (car (entsel "请选择块参照:")))
  (if (and ent (= "INSERT" (entity:getdxf ent 0)))
      (progn
        (setq name (block:get-effectivename ent))
        (setq path (getfiled "保存块 Lisp 源码" (strcat name ".lsp") "lsp" 1))
        (setq res (if path (@block:ai-block->lisp name path) nil))
        (if res
            (progn
              (@:prompt (strcat "已生成: " (cdr (assoc "file" res))))
              res)
            (progn (@:prompt "生成失败") nil)))
      (progn (@:prompt "未选择块参照") nil)))
