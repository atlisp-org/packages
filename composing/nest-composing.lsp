;;; nest-composing.lsp — 全能材料排版（矩形嵌套）
;;; FFD（First-Fit Decreasing）算法：把一组矩形件排入多张板材，输出每件摆放坐标。
;;; 复现 iMini 的 mini_unest 核心功能（本包为纯 LISP 实现，iMini 为 C++ exe）。
;;; 注：部分 CAD（如 ZWCAD）无 startundo/endundo，统一用 UNDO Begin/End 命令实现。

(defun composing:str->nums (s / i len ch cur nums)
  "从字符串中提取所有数字（支持小数、负号），返回数字列表。"
  (setq i 1
        len (strlen s)
        nums nil
        cur "")
  (while (<= i len)
    (setq ch (substr s i 1))
    (if (or (vl-string-search ch "0123456789.")
            (and (= ch "-") (= cur "")))
        (setq cur (strcat cur ch))
      (if (/= cur "")
          (progn
            (setq nums (append nums (list (atof cur))))
            (setq cur ""))))
    (setq i (1+ i)))
  (if (/= cur "")
      (setq nums (append nums (list (atof cur)))))
  nums)

(defun composing:nest-candidate-points (placed gap / pts p x y w h)
  "生成候选摆放点：原点 + 每个已放件的右边缘、上边缘（含间距 gap）。"
  (setq pts (list (list 0.0 0.0)))
  (foreach p placed
    (setq x (car p)
          y (cadr p)
          w (caddr p)
          h (cadddr p))
    (setq pts (cons (list (+ x w gap) y) pts))
    (setq pts (cons (list x (+ y h gap)) pts)))
  pts)

(defun composing:nest-fits (x y w h placed gap sheet-w sheet-h / fits p ox oy ow oh)
  "判断矩形 (x y w h) 能否放入板材：不越界、不与已放件重叠（含间距 gap）。"
  (if (or (> (+ x w) sheet-w)
          (> (+ y h) sheet-h))
      nil
    (progn
      (setq fits T)
      (foreach p placed
        (setq ox (car p)
              oy (cadr p)
              ow (caddr p)
              oh (cadddr p))
        (if (and (< x (+ ox ow gap))
                 (< ox (+ x w gap))
                 (< y (+ oy oh gap))
                 (< oy (+ y h gap)))
            (setq fits nil)))
      fits)))

(defun composing:nest-place (w h placed gap sheet-w sheet-h rotate
                             / orients o ow oh pts pt x y p done)
  "尝试把件 (w h) 放入当前板材，返回 (x y 宽 高)，失败返回 nil。"
  "支持旋转 90°（rotate 非 nil 时先试原朝向，再试旋转）。"
  (setq orients (list (list w h)))
  (if rotate
      (setq orients (cons (list h w) orients)))
  (setq done nil)
  (foreach o orients
    (if (null done)
        (progn
          (setq ow (car o)
                oh (cadr o))
          (setq pts (composing:nest-candidate-points placed gap))
          (setq pts
                (vl-sort pts
                         (function
                          (lambda (a b)
                            (if (< (cadr a) (cadr b)) (< (cadr a) (cadr b)) (and (equal (cadr a) (cadr b) 1e-6)
                                     (< (car a) (car b))))))))
          (foreach pt pts
            (if (null done)
                (progn
                  (setq x (car pt)
                        y (cadr pt))
                  (if (composing:nest-fits x y ow oh placed gap sheet-w sheet-h)
                      (progn
                        (setq p (list x y ow oh))
                        (setq done T)))))))))
  p)

(defun composing:nest-rects (items sheet-w sheet-h gap rotate
                             / rects rec w h n i placed sheet-items sheets p
                             x y pw ph rot)
  "FFD 矩形嵌套：把 items ((宽 高 数量)...) 排入多张板材。"
  "返回 ((版次 (件号 x y 宽 高 旋转) ...) ...)，件号从 1 连续编号，旋转 1 表示旋转 90°。"
  (setq rects nil)
  (foreach rec items
    (setq w (car rec)
          h (cadr rec)
          n (caddr rec))
    (if (or (not (numberp n)) (< n 1))
        (setq n 1))
    (setq i 0)
    (while (< i n)
      (setq rects (cons (list w h) rects))
      (setq i (1+ i))))
  (setq rects
        (vl-sort rects
                 (function
                  (lambda (a b)
                    (> (* (car a) (cadr a)) (* (car b) (cadr b)))))))
  (setq placed nil
        sheet-items nil
        sheets nil
        i 1)
  (foreach rec rects
    (setq w (car rec)
          h (cadr rec))
    (setq p (composing:nest-place w h placed gap sheet-w sheet-h rotate))
    (if (null p)
        (progn
          (if sheet-items
              (setq sheets (append sheets (list sheet-items))))
          (setq placed nil
                sheet-items nil)
          (setq p (composing:nest-place w h nil gap sheet-w sheet-h rotate))))
    (if p
        (progn
          (setq x (car p)
                y (cadr p)
                pw (caddr p)
                ph (cadddr p))
          (setq rot (if (equal pw w 1e-6) 0 1))
          (setq placed (append placed (list (list x y pw ph))))
          (setq sheet-items
                (append sheet-items (list (list i x y pw ph rot))))
          (setq i (1+ i)))
      (princ (strcat "\n件 ( " (rtos w 2 0) " x " (rtos h 2 0)
                     " ) 超过板材尺寸，已跳过。"))))
  (if sheet-items
      (setq sheets (append sheets (list sheet-items))))
  sheets)

(defun composing:ai-nest (items sheet-w sheet-h gap rotate)
  "AI 调用入口：FFD 矩形嵌套排版，无交互，失败返回 nil。
参数: items ((宽 高 数量)...)、sheet-w/sheet-h 板尺寸、gap 件间距、
      rotate T 允许旋转 90° / nil 不允许。
示例: (composing:ai-nest '((400 300 5) (600 200 3)) 1220 2440 10 T)"
  (if (not (and (listp items)
                (numberp sheet-w)
                (numberp sheet-h)
                (> sheet-w 0)
                (> sheet-h 0)))
      (progn
        (princ "\ncomposing:ai-nest 参数错误：items/板尺寸无效。")
        nil)
    (progn
      (if (or (not (numberp gap)) (< gap 0))
          (setq gap 0))
      (composing:nest-rects items sheet-w sheet-h gap rotate))))

(defun composing:sheet-box (sheet gap / item x y w h maxx maxy)
  "计算一张板的包围盒尺寸（含件间距），返回 (宽 高)。"
  (setq maxx 0.0
        maxy 0.0)
  (foreach item sheet
    (setq x (cadr item)
          y (caddr item)
          w (cadddr item)
          h (nth 4 item))
    (if (> (+ x w) maxx)
        (setq maxx (+ x w)))
    (if (> (+ y h) maxy)
        (setq maxy (+ y h))))
  (list (+ maxx gap) (+ maxy gap)))

(defun composing:ai-nest-draw (nest gap / ents off box sw sh p0 p1 rect item
                               idx x y w h cpt txth)
  "AI 调用入口：把 ai-nest 排版结果画到 CAD，返回图元名列表。
参数: nest 为 ai-nest 返回结构、gap 为件间距；每板一矩形框 + 每件一矩形 + 件号文字。"
  (if (not (numberp gap))
      (setq gap 0))
  (setq ents nil
        off 0.0)
  (foreach sheet nest
    (setq box (composing:sheet-box sheet gap))
    (setq sw (car box)
          sh (cadr box))
    (setq p0 (list off 0.0 0.0)
          p1 (list (+ off sw) sh 0.0))
    (setq rect (entity:make-rectangle p0 p1))
    (if rect
        (setq ents (cons rect ents)))
    (foreach item sheet
      (setq idx (car item)
            x (cadr item)
            y (caddr item)
            w (cadddr item)
            h (nth 4 item))
      (setq p0 (list (+ off x) y 0.0)
            p1 (list (+ off x w) (+ y h) 0.0))
      (setq rect (entity:make-rectangle p0 p1))
      (if rect
          (setq ents (cons rect ents)))
      (setq cpt (list (+ off x (/ w 2.0)) (+ y (/ h 2.0)) 0.0))
      (setq txth (max 2.0 (min 20.0 (/ (min w h) 2.0))))
      (setq rect (entity:addtext (itoa idx) cpt txth 0 22))
      (if rect
          (setq ents (cons rect ents))))
    (setq off (+ off sw gap 50.0)))
  (reverse ents))

(defun composing:unest (/ sheet-w sheet-h gap rotate ans items ss ent nums w h n
                          txt result)
  "交互入口：矩形嵌套排版（FFD 算法）。输入板尺寸、间距、旋转选项与件清单。
件清单可从选中文字读取（每字含 \"宽 高 数量\" 三个数），也可手输 \"宽 高 数量\"。"
  (@::prompt "矩形嵌套排版（FFD）：把一组矩形件排入多张板材。")
  (setq sheet-w (getdist "\n板材宽度:"))
  (setq sheet-h (getdist "\n板材高度:"))
  (if (not (and sheet-w sheet-h (> sheet-w 0) (> sheet-h 0)))
      (progn
        (princ "\n板材尺寸无效，已取消。")
        nil)
    (progn
      (setq gap (getdist "\n件间距 <0>:"))
      (if (null gap)
          (setq gap 0))
      (initget "是 否")
      (setq ans (getkword "\n允许旋转 [是/否] <是>:"))
      (setq rotate (if (= ans "否") nil T))
      (setq items nil)
      (princ "\n选择含 \"宽 高 数量\" 的文字（回车跳过，改手输）:")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
      (foreach ent (pickset:to-list ss)
        (setq nums (composing:str->nums (entity:getdxf ent 1)))
        (while (>= (length nums) 2)
          (setq w (nth 0 nums)
                h (nth 1 nums)
                n (if (nth 2 nums) (fix (nth 2 nums)) 1)
                nums (cdddr nums))
          (if (and (> w 0) (> h 0) (> n 0))
              (setq items (append items (list (list w h n)))))))
      (if (null items)
          (progn
            (princ "\n手输每件 \"宽 高 数量\"，回车结束:")
            (while (and (setq txt (getstring T "\n宽 高 数量:"))
                        (/= txt ""))
              (setq nums (composing:str->nums txt))
              (if (>= (length nums) 2)
                  (progn
                    (setq w (nth 0 nums)
                          h (nth 1 nums)
                          n (if (nth 2 nums) (fix (nth 2 nums)) 1))
                    (if (and (> w 0) (> h 0) (> n 0))
                        (setq items (append items (list (list w h n))))
                      (princ "\n数据无效，跳过。")))))))
      (if (null items)
          (progn
            (princ "\n没有可排版的件。")
            nil)
        (progn
          (setq result
                (composing:nest-rects items sheet-w sheet-h gap rotate))
          (command "_.UNDO" "Begin")
          (composing:ai-nest-draw result gap)
          (command "_.UNDO" "End")
          (princ (strcat "\n排版完成：共 " (itoa (length result))
                         " 张板材，已绘制到模型空间原点。"))
          result)))))
