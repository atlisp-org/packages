;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vport.lsp — 视口工具
;; 编辑视口比例 / 同步对齐视口 / 模型空间绘视口外框 / 视口锁定或解锁
;; 视口图层状态编辑 / 视口选层变灰 / 还原视口颜色
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (null layer:on) (require 'layer:*))
(if (null xdata:put) (require 'xdata:*))

;; ===================== 1. 编辑视口比例 vp-scale =====================

(defun @layout:ai-vp-scale (vports scale / lst cs e ok)
  "AI 调用入口：给一组视口设置比例（N 表示 1:N；0<N<1 按 customscale 处理如 2:1）。
vports 为视口 ename 列表（或选择集），scale 为比例 N。
成功返回已修改视口 ename 列表，失败返回 nil。
示例: (@layout:ai-vp-scale (@layout:vports) 100)"
  "已修改视口 ename 列表 / nil"
  "示例: (@layout:ai-vp-scale (@layout:vports) 100)"
  (setq lst (@layout:entlist vports)
        cs 0.0)
  (if (or (null lst) (not (and (numberp scale) (> scale 0.0))))
      (progn (princ "\n错误: 参数无效(视口列表或比例N)") nil)
      (progn
        (if (>= scale 1.0)
            (setq cs (/ 1.0 scale))
            (setq cs scale))
        (setq ok nil)
        (foreach e lst
          (if (@layout:vp-p e)
              (progn
                (vla-put-customscale (e2o e) cs)
                (setq ok (cons e ok)))))
        (if ok
            (progn
              (princ (strcat "\n已把 " (itoa (length ok))
                             " 个视口比例设为 1:" (rtos scale 2 1)))
              (reverse ok))
            (progn (princ "\n错误: 列表中没有视口实体") nil)))))

(defun @layout:vp-scale (/ ss n)
  "编辑视口比例（交互）：选择视口，设置统一比例 1:N"
  "t"
  "示例: (@layout:vp-scale)"
  (setq ss (ssget (list (cons 0 "VIEWPORT")))
        n (getreal 1 "\n视口比例 1:<100>: "))
  (if ss
      (progn
        (command "_.UNDO" "Begin")
        (@layout:ai-vp-scale (@layout:ss-to-list ss)
                                 (if n n 100.0))
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 2. 同步对齐视口 vp-sync =====================

(defun @layout:ai-vp-sync (vports rows cols gap / lst n r c i maxw maxh
                               e vw moved)
  "AI 调用入口：把一组视口按网格(rows × cols, 间距 gap)对齐排布。
按列表顺序从原点开始放置，保持各视口自身尺寸与模型视图。
rows/cols 为 0 或 nil 表示自动(cols=上取整 sqrt(n))，gap 缺省 500。
成功返回已移动视口 ename 列表，失败返回 nil。
示例: (@layout:ai-vp-sync (@layout:vports) 2 3 500)"
  "已移动视口 ename 列表 / nil"
  "示例: (@layout:ai-vp-sync (@layout:vports) 0 0 500)"
  (setq lst (@layout:entlist vports))
  (if (null lst)
      (progn (princ "\n错误: 没有视口") nil)
      (progn
        (setq n (length lst))
        (if (and (numberp cols) (> (fix cols) 0))
            (setq cols (fix cols))
            (progn
              (setq cols (fix (sqrt (float n))))
              (if (< cols 1) (setq cols 1))))
        (if (and (numberp rows) (> (fix rows) 0))
            (setq r (fix rows))
            (setq r (fix (/ (+ n - 1) cols))))
        (if (not (and (numberp gap) (> gap 0.0)))
            (setq gap 500.0))
        ;; 计算最大宽/高，保证行列对齐
        (setq maxw 0.0
              maxh 0.0)
        (foreach e lst
          (setq vw (@layout:vp-view e))
          (if (> (nth 2 vw) maxw) (setq maxw (nth 2 vw)))
          (if (> (nth 3 vw) maxh) (setq maxh (nth 3 vw))))
        (setq i 0
              moved nil)
        (foreach e lst
          (setq c (mod i cols))
          (setq vw (entget e))
          (setq vw (subst (cons 10 (list (* (+ c 1) (+ maxw gap))
                                         (* (+ (fix (/ i cols)) 1)
                                            (+ maxh gap))
                                         0.0))
                          (assoc 10 vw)
                          vw))
          (entmod vw)
          (setq moved (cons e moved))
          (setq i (1+ i)))
        (princ (strcat "\n已对齐 " (itoa (length moved)) " 个视口"
                       " (" (itoa r) " 行 × " (itoa cols) " 列)"))
        (reverse moved))))

(defun @layout:vp-sync (/ ss cols gap)
  "同步对齐视口（交互）：选择视口，按网格对齐"
  "t"
  "示例: (@layout:vp-sync)"
  (setq ss (ssget (list (cons 0 "VIEWPORT")))
        cols (getint "\n每列视口个数,0=自动 <0>: ")
        gap (getreal "\n视口间距 <500>: "))
  (if ss
      (progn
        (command "_.UNDO" "Begin")
        (@layout:ai-vp-sync (@layout:ss-to-list ss)
                                0
                                (if cols (fix cols) 0)
                                (if gap gap 500.0))
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 3. 模型空间绘制视口外框 vp-outline =====================

(defun @layout:ai-vp-outline (layout layer / lay lname vps ms i v vw
                                   x1 y1 x2 y2 th ln tx res)
  "AI 调用入口：在模型空间为布局内每个视口画带编号的外框(4条线+编号文字)。
layout 为 nil 时取当前布局，layer 为 nil 时用 \"ML_OUTLINE\"。
成功返回所创建实体 ename 列表，失败返回 nil。
示例: (@layout:ai-vp-outline nil nil)"
  "所创建实体 ename 列表 / nil"
  "示例: (@layout:ai-vp-outline nil \"0\")"
  (if (and (stringp layout) (/= "" layout))
      (setq lay layout)
      (setq lay (getvar "ctab")))
  (if (and (stringp layer) (/= "" layer))
      (setq lname layer)
      (setq lname "ML_OUTLINE"))
  (if (not (@layout:layout-p lay))
      (progn (princ (strcat "\n错误: 布局 [" lay "] 不存在")) nil)
      (progn
        (setq vps (@layout:vp-list lay)
              i 0
              res nil)
        (if (null vps)
            (progn (princ (strcat "\n错误: 布局 [" lay "] 没有视口")) nil)
            (progn
              (@layout:find-layer lname)
              (setq ms (vla-get-modelspace *doc*))
              (foreach v vps
                (setq i (1+ i)
                      vw (@layout:vp-view v)
                      x1 (- (car vw) (/ (nth 2 vw) (* 2.0 (nth 4 vw))))
                      y1 (- (cadr vw) (/ (nth 3 vw) (* 2.0 (nth 4 vw))))
                      x2 (+ (car vw) (/ (nth 2 vw) (* 2.0 (nth 4 vw))))
                      y2 (+ (cadr vw) (/ (nth 3 vw) (* 2.0 (nth 4 vw))))
                      th (* 0.02 (/ (nth 3 vw) (nth 4 vw))))
                (setq ln (vla-addline ms
                                      (vlax-3d-point (list x1 y1 0.0))
                                      (vlax-3d-point (list x2 y1 0.0))))
                (vla-put-layer ln lname)
                (setq res (cons (o2e ln) res))
                (setq ln (vla-addline ms
                                      (vlax-3d-point (list x2 y1 0.0))
                                      (vlax-3d-point (list x2 y2 0.0))))
                (vla-put-layer ln lname)
                (setq res (cons (o2e ln) res))
                (setq ln (vla-addline ms
                                      (vlax-3d-point (list x2 y2 0.0))
                                      (vlax-3d-point (list x1 y2 0.0))))
                (vla-put-layer ln lname)
                (setq res (cons (o2e ln) res))
                (setq ln (vla-addline ms
                                      (vlax-3d-point (list x1 y2 0.0))
                                      (vlax-3d-point (list x1 y1 0.0))))
                (vla-put-layer ln lname)
                (setq res (cons (o2e ln) res))
                (setq tx (vla-addtext ms (itoa i)
                                      (vlax-3d-point (list x1
                                                           (- y1 th 1.0)
                                                           0.0))
                                      th))
                (vla-put-layer tx lname)
                (setq res (cons (o2e tx) res)))
              (princ (strcat "\n已在模型空间绘制 " (itoa (length vps))
                             " 个视口外框(图层 " lname
                             "，如不可见请切到模型空间)"))
              (reverse res))))))

(defun @layout:vp-outline (/ lay lname)
  "在模型空间绘制视口外框（交互）：为当前布局每个视口画带编号外框"
  "t"
  "示例: (@layout:vp-outline)"
  (setq lay (getstring 1
                       (strcat "\n布局名 <当前 " (getvar "ctab") ">: ")))
  (setq lname (getstring 1 "\n外框图层 <ML_OUTLINE>: "))
  (command "_.UNDO" "Begin")
  (@layout:ai-vp-outline (if (= "" lay) nil lay)
                             (if (= "" lname) nil lname))
  (command "_.UNDO" "End")
  (princ))

;; ===================== 4. 视口锁定或解锁 vport-lock =====================

(defun @layout:ai-vport-lock (vports lock / lst e dat ok)
  "AI 调用入口：锁定(lock=t)或解锁(lock=nil)一组视口(DXF 70 bit 16)。
vports 为视口 ename 列表（或选择集）。
成功返回已修改视口 ename 列表，失败返回 nil。
示例: (@layout:ai-vport-lock (@layout:vports) t)"
  "已修改视口 ename 列表 / nil"
  "示例: (@layout:ai-vport-lock (@layout:vports) t)"
  (setq lst (@layout:entlist vports))
  (if (null lst)
      (progn (princ "\n错误: 没有视口") nil)
      (progn
        (setq ok nil)
        (foreach e lst
          (if (@layout:vp-p e)
              (progn
                (setq dat (entget e))
                (if lock
                    (setq dat (subst (cons 70 (logior (cdr (assoc 70 dat))
                                                      16))
                                     (assoc 70 dat)
                                     dat))
                    (setq dat (subst (cons 70 (logand (cdr (assoc 70 dat))
                                                      (lnot 16)))
                                     (assoc 70 dat)
                                     dat)))
                (entmod dat)
                (setq ok (cons e ok)))))
        (if ok
            (progn
              (princ (strcat "\n已" (if lock "锁定" "解锁") " "
                             (itoa (length ok)) " 个视口"))
              (reverse ok))
            (progn (princ "\n错误: 列表中没有视口实体") nil)))))

(defun @layout:vport-lock (/ ss opt)
  "视口锁定或解锁（交互）：选择视口，锁定/解锁"
  "t"
  "示例: (@layout:vport-lock)"
  (setq ss (ssget (list (cons 0 "VIEWPORT")))
        opt (getkword "\n锁定(L)或解锁(U) [L]: L"))
  (if ss
      (progn
        (command "_.UNDO" "Begin")
        (@layout:ai-vport-lock (@layout:ss-to-list ss)
                                   (= opt "L"))
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 5. 视口图层状态编辑 vplayer =====================

(defun @layout:vplayer-encode (pairs / res)
  "把 (图层名 . 状态) 对列表编码为 图层=状态|图层=状态| 形式字符串"
  "字符串"
  "示例: (@layout:vplayer-encode '((\"A\" . \"off\") (\"B\" . \"freeze\")))"
  (setq res "")
  (foreach p pairs
    (setq res (strcat res (car p) "=" (cdr p) "|")))
  res)

(defun @layout:vplayer-parse (str / res x)
  "把 图层=状态|图层=状态| 形式字符串解析为 (图层名 . 状态) 对列表"
  "对列表"
  "示例: (@layout:vplayer-parse \"A=off|B=freeze|\")"
  (setq res nil)
  (foreach x (@layout:split-str (vl-string-trim "|" str) "|")
    (if (vl-string-search "=" x)
        (progn
          (setq x (@layout:split-str x "="))
          (setq res (cons (cons (vl-string-trim " " (car x))
                                (vl-string-trim " " (cadr x)))
                          res)))))
  (reverse res))

(defun @layout:layer-apply-state (name state)
  "把状态(on/off/freeze/thaw)应用到图层表（对整图生效）"
  "t / nil"
  "示例: (@layout:layer-apply-state \"0\" \"off\")"
  (if (null (tblobjname "LAYER" name))
      nil
      (progn
        (cond
         ((= (strcase state) "ON")
          (layer:on (list name) t))
         ((= (strcase state) "OFF")
          (layer:on (list name) nil))
         ((= (strcase state) "FREEZE")
          (layer:freeze (list name) t))
         ((= (strcase state) "THAW")
          (layer:freeze (list name) nil)))
        t)))

(defun @layout:ai-vplayer (vport layers-state / lst pairs x ok)
  "AI 调用入口：设置视口的图层状态并保存到视口 xdata(APPID MINILAYOUT)，
同时立即应用到图层表。
vport 为视口 ename，layers-state 为 (图层名 . 状态) 对列表，
状态取 on/off/freeze/thaw。
成功返回已应用 (图层名 . 状态) 对列表，失败返回 nil。
示例: (@layout:ai-vplayer (car (@layout:vports)) '((\"A\" . \"off\")))"
  "已应用 (图层名 . 状态) 对列表 / nil"
  "示例: (@layout:ai-vplayer (car (@layout:vports)) '((\"A\" . \"off\")))"
  (if (not (@layout:vp-p vport))
      (progn (princ "\n错误: 参数不是视口") nil)
      (progn
        (if (listp layers-state)
            (setq lst layers-state)
            (setq lst (list layers-state)))
        (setq pairs nil)
        (foreach x lst
          (if (and (listp x) (= 2 (length x))
                   (stringp (car x))
                   (member (strcase (cadr x))
                           '("ON" "OFF" "FREEZE" "THAW")))
              (setq pairs (cons (cons (car x)
                                      (strcase (cadr x)))
                                pairs))))
        (if (null pairs)
            (progn
              (princ "\n错误: 图层状态列表无效(格式 (\"图层\" . \"off/on/freeze/thaw\"))")
              nil)
            (progn
              (setq pairs (reverse pairs))
              ;; 先删旧状态再写入
              (xdata:remove-appid vport "MINILAYOUT")
              (xdata:put vport "MINILAYOUT" (@layout:vplayer-encode pairs))
              (setq ok nil)
              (foreach p pairs
                (if (@layout:layer-apply-state (car p) (cdr p))
                    (setq ok (cons p ok))))
              (princ (strcat "\n视口已保存并应用 " (itoa (length ok))
                             " 个图层状态(作用于图层表)"))
              (reverse ok))))))

(defun @layout:ai-vplayer-query (vport / xdata rec str)
  "AI 调用入口：读取视口上已保存的图层状态"
  "(图层名 . 状态) 对列表 / nil"
  "示例: (@layout:ai-vplayer-query (car (@layout:vports)))"
  (if (not (@layout:vp-p vport))
      (progn (princ "\n错误: 参数不是视口") nil)
      (progn
        (setq xdata (cdr (assoc -3 (entget vport '("*"))))
              rec (assoc "MINILAYOUT" xdata))
        (if rec
            (progn
              (setq str (cdr (assoc 1000 (cdr rec))))
              (if str (@layout:vplayer-parse str) nil))
            nil))))

(defun @layout:ai-vplayer-apply (vport / pairs ok)
  "AI 调用入口：把视口上已保存的图层状态应用到图层表"
  "已应用 (图层名 . 状态) 对列表 / nil"
  "示例: (@layout:ai-vplayer-apply (car (@layout:vports)))"
  (if (not (@layout:vp-p vport))
      (progn (princ "\n错误: 参数不是视口") nil)
      (progn
        (setq pairs (@layout:ai-vplayer-query vport))
        (if (null pairs)
            (progn (princ "\n该视口没有已保存的图层状态") nil)
            (progn
              (setq ok nil)
              (foreach p pairs
                (if (@layout:layer-apply-state (car p) (cdr p))
                    (setq ok (cons p ok))))
              (princ (strcat "\n已应用 " (itoa (length ok)) " 个图层状态"))
              (reverse ok))))))

(defun @layout:vplayer (/ ss i n e str)
  "视口图层状态编辑（交互）：逐个视口设置 图层=状态|... 并保存+应用"
  "t"
  "示例: (@layout:vplayer)"
  (setq ss (ssget (list (cons 0 "VIEWPORT")))
        i 0
        n (if ss (sslength ss) 0))
  (if ss
      (progn
        (command "_.UNDO" "Begin")
        (while (< i n)
          (setq e (ssname ss i))
          (setq str (getstring 1
                               (strcat "\n[" (itoa (1+ i)) "/" (itoa n)
                                      "] 图层状态(格式 图层=状态|...,"
                                      "状态=on/off/freeze/thaw,回车跳过): ")))
          (if (/= "" str)
              (@layout:ai-vplayer e (@layout:vplayer-parse str)))
          (setq i (1+ i)))
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 6. 视口选层变灰 vport-grey =====================

(defun @layout:vp-grey-layers (vport / vw x1 y1 x2 y2 ss e layers)
  "收集视口模型视图范围内实体的图层名列表"
  "图层名列表"
  "示例: (@layout:vp-grey-layers (car (@layout:vports)))"
  (if (not (@layout:vp-p vport))
      nil
      (progn
        (if (not (@layout:in-model))
            (command "_.-MSPACE"))
        (setq vw (@layout:vp-view vport)
              x1 (car (@layout:vp-model-rect vport))
              y1 (cadr (@layout:vp-model-rect vport))
              x2 (caddr (@layout:vp-model-rect vport))
              y2 (cadddr (@layout:vp-model-rect vport)))
        (setq ss (ssget "w" (list x1 y1) (list x2 y2)))
        (setq layers nil)
        (if ss
            (progn
              (repeat (sslength ss)
                (setq e (ssname ss (1- (sslength ss))))
                (setq layers (cons (cdr (assoc 8 (entget e))) layers)))
              (vl-remove-duplicates layers))
            nil))))

(defun @layout:ai-vport-grey (vport layers gray / names nm en color ok)
  "AI 调用入口：把一组图层置灰（原颜色记在图层 xdata 供还原）。
vport 为视口 ename（layers 为 nil 时取该视口视图内实体所在图层），
layers 为图层名列表（nil=自动），gray 为灰色号（缺省 8）。
成功返回已置灰图层名列表，失败返回 nil。
示例: (@layout:ai-vport-grey (car (@layout:vports)) nil 8)"
  "已置灰图层名列表 / nil"
  "示例: (@layout:ai-vport-grey (car (@layout:vports)) '(\"A\" \"B\") 8)"
  (if (not (@layout:vp-p vport))
      (progn (princ "\n错误: 第一个参数不是视口") nil)
      (progn
        (if (null layers)
            (setq names (@layout:vp-grey-layers vport))
            (if (listp layers)
                (setq names layers)
                (setq names (list layers))))
        (if (not (and (numberp gray) (> gray 0)))
            (setq gray 8))
        (if (null names)
            (progn (princ "\n错误: 没有可置灰的图层") nil)
            (progn
              (setq ok nil)
              (foreach nm names
                (setq en (tblobjname "LAYER" nm))
                (if en
                    (progn
                      (setq color (cdr (assoc 62 (entget en))))
                      (xdata:remove-appid en "MINILAYOUT-GRAY")
                      (xdata:put en "MINILAYOUT-GRAY" color)
                      (entmod (subst (cons 62 gray)
                                     (assoc 62 (entget en))
                                     (entget en)))
                      (setq ok (cons nm ok)))))
              (if ok
                  (progn
                    (princ (strcat "\n已置灰 " (itoa (length ok))
                                   " 个图层(颜色 " (itoa gray) ")"))
                    (reverse ok))
                  (progn (princ "\n错误: 图层不存在") nil)))))))

(defun @layout:vport-grey (/ sel str color)
  "视口选层变灰（交互）：选视口，把其中图层（或指定图层）置灰"
  "t"
  "示例: (@layout:vport-grey)"
  (setq sel (entsel "\n选择视口: "))
  (if sel
      (progn
        (setq str (getstring 1 "\n图层名,逗号分隔,回车=视口内全部: "))
        (setq color (getint "\n灰色号 <8>: "))
        (command "_.UNDO" "Begin")
        (@layout:ai-vport-grey (car sel)
                                   (if (= "" str)
                                       nil
                                       (@layout:sanitize-lst str))
                                   (if color color 8))
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 7. 还原视口颜色 vport-ungrey =====================

(defun @layout:ai-vport-ungrey (layers / names en obj color ok rec xdata
                                    i n)
  "AI 调用入口：还原被 vport-grey 置灰的图层（读图层 xdata 中的原颜色）。
layers 为图层名列表；nil = 还原所有带 MINILAYOUT-GRAY 标记的图层。
成功返回已还原图层名列表，失败返回 nil。
示例: (@layout:ai-vport-ungrey nil)"
  "已还原图层名列表 / nil"
  "示例: (@layout:ai-vport-ungrey '(\"A\" \"B\"))"
  (if (null layers)
      (progn
        ;; 找出所有带标记的图层
        (setq names nil
              n (vla-get-count (vla-get-layers *doc*))
              i 0)
        (while (< i n)
          (setq obj (vla-item (vla-get-layers *doc*) i))
          (setq xdata (cdr (assoc -3 (entget (o2e obj) '("*")))))
          (if (assoc "MINILAYOUT-GRAY" xdata)
              (setq names (cons (vla-get-name obj) names)))
          (setq i (1+ i))))
      (if (listp layers)
          (setq names layers)
          (setq names (list layers))))
  (setq ok nil)
  (foreach nm names
    (setq en (tblobjname "LAYER" nm))
    (if en
        (progn
          (setq xdata (cdr (assoc -3 (entget en '("*")))))
          (setq rec (assoc "MINILAYOUT-GRAY" xdata))
          (if rec
              (progn
                (setq color (cdr (assoc 1070 (cdr rec))))
                (if color
                    (progn
                      (entmod (subst (cons 62 color)
                                     (assoc 62 (entget en))
                                     (entget en)))
                      (xdata:remove-appid en "MINILAYOUT-GRAY")
                      (setq ok (cons nm ok)))))))))
  (if ok
      (progn
        (princ (strcat "\n已还原 " (itoa (length ok)) " 个图层颜色"))
        (reverse ok))
      (progn (princ "\n没有可还原的图层") nil)))

(defun @layout:vport-ungrey (/ str)
  "还原视口颜色（交互）：还原被 vport-grey 置灰的图层"
  "t"
  "示例: (@layout:vport-ungrey)"
  (setq str (getstring 1 "\n图层名,逗号分隔,回车=全部已置灰图层: "))
  (command "_.UNDO" "Begin")
  (@layout:ai-vport-ungrey (if (= "" str)
                                   nil
                                   (@layout:sanitize-lst str)))
  (command "_.UNDO" "End")
  (princ))

(princ)

(defun @layout:split-str (str seps / res cur i n c)
  "按分隔符集合拆分字符串（seps 中任一字符都算分隔符），忽略空段"
  "字符串列表"
  "(@layout:split-str \"a,b;c\" \",;\")"
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
