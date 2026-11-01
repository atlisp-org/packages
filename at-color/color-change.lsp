;;------------------------=={ 全能变色 / 按层变色 }==------------------------------;;
;;  at-color 变色补充（对应 iMini 的 一键变色 mini_yjbh）：
;;   - color-change  全能变色：所选实体 变指定颜色 / 颜色映射 / 变随层
;;   - color-by-layer 按层变色：指定图层的全部实体批量变色（可通配，可随层）
;;  AI 入口（无交互）：at-color:ai-change / at-color:ai-by-layer-color
;;----------------------------------------------------------------------;;

(@:add-menu (_ "Color") (_ "全能变色") "(c:color-change)")
(@:add-menu (_ "Color") (_ "按层变色") "(c:color-by-layer)")

;; 修改单个图元的颜色
;; 0~255=ACI 索引色（写 62，删 420 真彩色，67=0）；-1=随层；-2=随块；>255=真彩色整数（写 420，62=256，67=1）

(defun @color:set-ent-color (en color)
  "修改单个图元颜色：0~255 设 ACI 索引色（写 62、删 420、67=0）；-1=随层 -2=随块；>255 按真彩色整数写入（62=256、420、67=1）"
  "成功 T，失败 nil"
  "(@color:set-ent-color (car (entsel)) 1)"
  (if (and en (numberp color) (>= color -2))
      (if (> color 255)
          (progn
            (entity:putdxf en 420 color)
            (entity:putdxf en 62 256)
            (entity:putdxf en 67 1)
            t)
          (progn
            (entity:putdxf en 62 color)
            (if (entity:getdxf en 420)
                (entity:deldxf en 420))
            (entity:putdxf en 67 0)
            t))
      nil))

;; 判断字符串是否为非负整数

(defun @color:int-p (s)
  "判断字符串是否为非负整数（颜色 0 合法）"
  "T 或 nil"
  "(@color:int-p \"0\") => T"
  (and s (vl-regex-search "^[0-9][0-9]*$" s t)))

;; 解析颜色映射表达式 "1=2,3=4"（支持全角逗号）

(defun @color:parse-map (str / res item pos a b bad)
  "解析颜色映射表达式（1=2,3=4，支持全角逗号）"
  "((原色 . 新色) ...)，格式无效返回 nil"
  "(@color:parse-map \"1=2,3=4\") => ((1 . 2) (3 . 4))"
  (if (and str (vl-string-search "=" str))
      (progn
        (setq res nil bad nil)
        (foreach item (at-color:split-str (vl-string-subst "，" "," str) ",")
          (if (and (setq pos (vl-string-search "=" item))
                   (setq a (vl-string-trim " \t" (substr item 1 pos)))
                   (setq b (vl-string-trim " \t" (substr item (+ pos 1))))
                   (@color:int-p a)
                   (@color:int-p b))
              (setq res (cons (cons (atoi a) (atoi b)) res))
              (setq bad t)))
        (if bad nil (reverse res)))
      nil))

;; AI 入口：全能变色

(defun at-color:ai-change (ents mode param / n en res)
  "AI 入口：全能变色（对应 iMini 一键变色）
   ents：选择集或 ename 列表，nil=全图
   mode：\"color\"=变指定颜色 / \"map\"=按映射变色 / \"bylayer\"=变随层
   param：mode=color 时颜色数（0~255，>255 真彩色，-1 随层）；
   mode=map 时映射表 ((原色 . 新色) ...)，如 ((1 . 2) (3 . 4))；mode=bylayer 传 nil
   \"改动的图元数，失败返回 nil\"
   \"(at-color:ai-change nil \"color\" 1)\""
  (if (p:picksetp ents)
      (setq ents (pickset:to-list ents)))
  (if (null ents)
      (setq ents (pickset:to-list (ssget "_X"))))
  (if (and mode (not (p:stringp mode)))
      (if (symbolp mode)
          (setq mode (symbol-name mode))
          (setq mode nil)))
  (if (and ents (p:stringp mode))
      (progn
        (setq mode (strcase (vl-string-trim " \t" mode)))
        (if (wcmatch mode "COLOR,MAP,BYLAYER")
            (progn
              (if (and (= mode "COLOR") (not (numberp param)))
                  (progn
                    (princ "at-color:ai-change: param 须为颜色数")
                    nil)
                  (progn
                    (command "_.UNDO" "Begin")
                    (setq n 0)
                    (foreach en ents
                      (if (cond
                            ((= mode "COLOR")
                             (@color:set-ent-color en param))
                            ((= mode "MAP")
                             (and (listp param)
                                  (if (setq res (assoc (entity:getdxf en 62) param))
                                      (@color:set-ent-color en (cdr res))
                                      nil)))
                            (t
                             (@color:set-ent-color en -1)))
                          (setq n (1+ n))))
                    (command "_.UNDO" "End")
                    n)))
            (progn
              (princ (strcat "at-color:ai-change: 未知模式 " mode))
              nil)))
      (progn
        (princ "at-color:ai-change: ents/mode 无效")
        nil)))

;; AI 入口：按层变色

(defun at-color:ai-by-layer-color (layer color / found df ss n en)
  "AI 入口：按层变色：把指定图层上的全部实体批量变色（color nil=变随层）
   layer：图层名（支持通配符，如 \"WALL*\"）
   color：颜色数（0~255，>255 真彩色），nil=变随层
   \"改动的图元数，图层不存在返回 nil\"
   \"(at-color:ai-by-layer-color \"WALL\" 1)\""
  (if (and layer (not (p:stringp layer)))
      (setq layer (symbol-name layer)))
  (if (and color (not (numberp color)))
      (progn
        (princ "at-color:ai-by-layer-color: color 须为颜色数或 nil")
        nil)
      (progn
        (if (null color)
            (setq color -1))
        (setq found nil)
        (while (setq df (tblnext "layer" (not df)))
          (if (wcmatch (cdr (assoc 2 df)) layer)
              (setq found t)))
        (if found
            (if (setq ss (ssget "_X" (list (cons 8 (vl-string-trim " \t" layer)))))
                (progn
                  (command "_.UNDO" "Begin")
                  (setq n 0)
                  (foreach en (pickset:to-list ss)
                    (if (@color:set-ent-color en color)
                        (setq n (1+ n))))
                  (command "_.UNDO" "End")
                  n)
                (progn
                  (princ (strcat "at-color:ai-by-layer-color: 图层 " layer " 上没有图元"))
                  0))
            (progn
              (princ (strcat "at-color:ai-by-layer-color: 图层不存在 " layer))
              nil)))))

;; 交互命令：全能变色

(defun c:color-change (/ what color ss map res)
  "全能变色：所选实体 变指定颜色/颜色映射/变随层（对应 iMini 一键变色）"
  "无返回值（交互命令，结果输出到命令行）"
  "(c:color-change)"
  (princ "\n选择要变色的图元（回车=全图）：")
  (setq ss (ssget))
  (setq what (getkword "\n变色方式 [指定/映射/随层] <指定>："))
  (cond
    ((or (null what) (wcmatch (strcase what) "COLOR,指定"))
     (if (setq color (acad_colordlg 1))
         (if (setq res (at-color:ai-change ss "color" color))
             (princ (strcat "\n共改变 " (itoa res) " 个图元的颜色")))))
    ((wcmatch (strcase what) "MAP,映射")
     (princ "\n输入颜色映射（1=2,3=4，回车取消）：")
     (if (setq map (@color:parse-map (getstring t "映射：")))
         (if (setq res (at-color:ai-change ss "map" map))
             (princ (strcat "\n共改变 " (itoa res) " 个图元的颜色")))))
    ((wcmatch (strcase what) "BYLAYER,随层")
     (if (setq res (at-color:ai-change ss "bylayer" nil))
         (princ (strcat "\n共改变 " (itoa res) " 个图元的颜色")))))
  (princ))

;; 交互命令：按层变色

(defun c:color-by-layer (/ layer color n)
  "按层变色：指定图层的全部实体批量变色（颜色回车=随层）"
  "无返回值（交互命令，结果输出到命令行）"
  "(c:color-by-layer)"
  (princ "\n输入图层名（支持通配符，如 WALL*，回车取消）：")
  (if (setq layer (getstring t "图层名："))
      (progn
        (princ "\n颜色数（回车=随层）：")
        (setq color (getint "颜色数："))
        (if (setq n (at-color:ai-by-layer-color layer color))
            (princ (strcat "\n共改变 " (itoa n) " 个图元的颜色")))))
  (princ))

(princ)

(defun at-color:split-str (str seps / res cur i n c)
  "按分隔符集合拆分字符串（seps 中任一字符都算分隔符），忽略空段"
  "字符串列表"
  "(at-color:split-str \"a,b;c\" \",;\")"
  (setq res '() cur "" i 1 n (strlen str))
  (while (<= i n)
    (setq c (substr str i 1))
    (if (vl-string-search c seps)
        (progn
          (if (/= cur "") (setq res (append res (list cur))))
          (setq cur ""))
        (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (if (/= cur "") (setq res (append res (list cur))))
  res)
