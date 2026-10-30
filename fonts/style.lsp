;; 文字样式管理
;; iMini 功能复现：style_any（字形快速设置）、style_hz / style_khz（HZ/KHZ 系列字形）

(@:add-menu "@lisp字体库" "字形快速设置" "(fonts:style-any)")
(@:add-menu "@lisp字体库" "HZ字体系列" "(fonts:style-hz)")
(@:add-menu "@lisp字体库" "KHZ字体系列" "(fonts:style-khz)")

(defun fonts:make-style (name shx ttf height width oblique / doc textstyles obj main big)
  "创建或修改文字样式。ttf 优先；shx 支持 \"主.shx,大.shx\" 组合。
name: 样式名。shx: SHX 主字体（可带大字体）。ttf: TTF 字体名。
height/width/oblique: 字高/宽度系数/倾斜角(度)，nil 或 0 保持默认。"
  "文字样式 vla-object；失败 nil"
  "示例: (fonts:make-style \"HZ\" \"tssdeng.shx,hztxt.shx\" nil 0 0.8 0)"
  (if (or (null name) (and (null shx) (null ttf)))
      (progn (princ "fonts:make-style: 参数缺失。") nil)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            textstyles (vla-get-TextStyles doc))
      (setq obj (vl-catch-all-apply 'vla-item (list textstyles name)))
      (if (vl-catch-all-error-p obj)
          (progn
            (setq obj (vl-catch-all-apply 'vla-add (list textstyles name)))
            (if (vl-catch-all-error-p obj)
                (setq obj nil))))
      (if (null obj)
          (progn (princ (strcat "fonts:make-style: 无法创建样式 " name)) nil)
        (progn
          (cond
            ((and ttf (not (equal ttf "")))
             (vla-setfont obj ttf :vlax-false :vlax-false 134 2))
            (t
             (if (and shx (vl-string-search "," shx))
                 (progn
                   (setq main (substr shx 1 (vl-string-search "," shx))
                         big (substr shx (+ (vl-string-search "," shx) 2)))
                   (vla-put-fontfile obj main)
                   (vla-put-bigfontfile obj big))
               (if shx
                   (vla-put-fontfile obj shx)))))
          (if (and height (> height 0))
              (vla-put-height obj height))
          (if (and width (> width 0))
              (vla-put-width obj width))
          (if oblique
              (vla-put-obliqueangle obj (* pi (/ oblique 180.0))))
          obj)))))

(defun fonts:ai-style (name shx ttf height width oblique / obj)
  "AI 入口：创建/修改文字样式（SHX+TTF 组合、高度、宽度系数、倾斜角）。
name: 样式名。shx: SHX 主字体（可 \"主.shx,大.shx\"）。ttf: TTF 字体名（优先于 shx）。
height/width/oblique: 数值，nil 保持默认，oblique 单位为度。"
  "样式名字符串；失败返回 nil"
  "示例: (fonts:ai-style \"HZ\" \"tssdeng.shx,hztxt.shx\" nil 0 0.8 0)"
  (setq obj (fonts:make-style name shx ttf height width oblique))
  (if obj
      (progn (princ (strcat "fonts:ai-style: 样式 " name " 设置完成。"))
             name)
    nil))

(defun fonts:style-any (/ name shx ttf height width oblique res)
  "字形快速设置：创建/修改文字样式（SHX+TTF、字高、宽度系数、倾斜角）。"
  "样式名；失败 nil"
  "示例: 运行 fonts:style-any 后按提示输入"
  (if (setq name (getstring t "\n样式名:"))
      (progn
        (setq shx (getstring "\nSHX 主字体(如 tssdeng.shx,hztxt.shx，可空):"))
        (if (= shx "") (setq shx nil))
        (setq ttf (getstring "\nTTF 字体名(如 宋体，可空):"))
        (if (= ttf "") (setq ttf nil))
        (setq height (getreal "\n字高<0>:"))
        (if (null height) (setq height 0))
        (setq width (getreal "\n宽度系数<1.0>:"))
        (if (null width) (setq width 1.0))
        (setq oblique (getreal "\n倾斜角(度)<0>:"))
        (if (null oblique) (setq oblique 0))
        (startundo)
        (setq res (fonts:ai-style name shx ttf height width oblique))
        (endundo)
        res))
  (princ))

(defun fonts:series-hz ()
  "HZ 系列样式定义：(样式名 主字体 大字体)。"
  "((样式名 主字体 大字体) ...)"
  "示例: (fonts:series-hz)"
  '(("HZTXT" "tssdeng.shx" "hztxt.shx")
    ("HZDBT" "tssdeng.shx" "hzdbt.shx")
    ("HZTR" "tssdeng.shx" "hztr.shx")))

(defun fonts:series-khz ()
  "KHZ 系列样式定义：(样式名 主字体 大字体)。"
  "((样式名 主字体 大字体) ...)"
  "示例: (fonts:series-khz)"
  '(("KHT" "tssdeng.shx" "kht.shx")
    ("KDB" "tssdeng.shx" "kdb.shx")
    ("KHTZ" "tssdeng.shx" "khtz.shx")))

(defun fonts:apply-series (series / res item name obj)
  "批量应用字体系列（series 为 ((样式名 主字体 大字体) ...)）。"
  "成功创建的样式名列表；失败 nil"
  "示例: (fonts:apply-series (fonts:series-hz))"
  (setq res nil)
  (foreach item series
    (setq name (car item))
    (setq obj (fonts:make-style name
                                (strcat (cadr item) "," (caddr item))
                                nil nil nil nil))
    (if obj (setq res (cons name res))))
  (reverse res))

(defun fonts:ai-style-hz (/ res)
  "AI 入口：批量设置 HZ 系列文字样式（hztxt/hzdbt/hztr，主字体+大字体组合）。"
  "成功创建的样式名列表；失败返回 nil"
  "示例: (fonts:ai-style-hz)"
  (setq res (fonts:apply-series (fonts:series-hz)))
  (if res
      (princ (strcat "fonts:ai-style-hz: 已设置 "
                     (itoa (length res)) " 个样式。")))
  res)

(defun fonts:ai-style-khz (/ res)
  "AI 入口：批量设置 KHZ 系列文字样式（kht/kdb/khtz，主字体+大字体组合）。"
  "成功创建的样式名列表；失败返回 nil"
  "示例: (fonts:ai-style-khz)"
  (setq res (fonts:apply-series (fonts:series-khz)))
  (if res
      (princ (strcat "fonts:ai-style-khz: 已设置 "
                     (itoa (length res)) " 个样式。")))
  res)

(defun fonts:style-hz (/ res)
  "批量设置 HZ 系列文字样式（hztxt/hzdbt/hztr）。"
  "样式名列表；失败 nil"
  "示例: 运行 fonts:style-hz 一键设置"
  (startundo)
  (setq res (fonts:ai-style-hz))
  (endundo)
  res)

(defun fonts:style-khz (/ res)
  "批量设置 KHZ 系列文字样式（kht/kdb/khtz）。"
  "样式名列表；失败 nil"
  "示例: 运行 fonts:style-khz 一键设置"
  (startundo)
  (setq res (fonts:ai-style-khz))
  (endundo)
  res)
