;;; 全能文字计算：选中数字/算式文字，求和、求积或逐项求值。
;;; 内置简易表达式解析器(支持 + - * / 括号与小数)，不使用 eval。
(@:define-config '@math:cale-layer "cale"
                 "全能计算结果文字所在图层")
(@:define-config '@math:cale-fontsize 3.5
                 "全能计算结果文字字高(绘图单位，实际值乘以绘图比例)")
(@:add-menu "数学" "全能计算" "(at-math:cale-txt)")

(defun @math:cale-tokenize (str / ch tok toks)
  "把算式字符串切分为记号列表：数字记号为实数，运算符/括号为字符串。
   str: 算式字符串。
   return: 记号列表，如 \"(2+3)*4\" => (\"(\" 2.0 \"+\" 3.0 \")\" \"*\" 4.0)。
   示例: (@math:cale-tokenize \"12.5*2\")"
  (setq str (vl-string-translate "{[]}（） \t\n" "(())     " str))
  (setq toks nil
        tok "")
  (while (/= "" str)
    (setq ch (substr str 1 1)
          str (substr str 2))
    (cond
      ((member ch '("+" "-" "*" "/" "(" ")"))
       (if (/= "" tok)
           (setq toks (cons (atof tok) toks)
                 tok ""))
       (setq toks (cons ch toks)))
      ((or (and (>= ch "0") (<= ch "9")) (= ch "."))
       (setq tok (strcat tok ch)))
      (t nil)))
  (if (/= "" tok)
      (setq toks (cons (atof tok) toks)))
  (reverse toks))

(defun @math:cale-parse-primary (toks / tok v)
  "解析基本项：数字、括号子式或一元正负号。
   toks: 记号列表。
   return: 点对 (值 . 剩余记号)；语法错误返回 nil。
   示例: (@math:cale-parse-primary '(3.0 \"+\" 2.0))"
  (setq tok (car toks))
  (cond
    ((and tok (= "(" tok))
     (setq v (@math:cale-parse-add (cdr toks)))
     (if (and v (= ")" (car (cdr v))))
         (cons (car v) (cdr (cdr v)))
         nil))
    ((and tok (= "-" tok))
     (setq v (@math:cale-parse-primary (cdr toks)))
     (if v (cons (- (car v)) (cdr v)) nil))
    ((and tok (= "+" tok))
     (@math:cale-parse-primary (cdr toks)))
    ((numberp tok)
     (cons tok (cdr toks)))
    (t nil)))

(defun @math:cale-parse-mul (toks / v op r)
  "解析乘除项(优先级高于加减)。
   toks: 记号列表。
   return: 点对 (值 . 剩余记号)。
   示例: (@math:cale-parse-mul '(3.0 \"*\" 6.0))"
  (setq v (@math:cale-parse-primary toks))
  (while (and v (member (car (cdr v)) '("*" "/")))
    (setq op (car (cdr v))
          r (@math:cale-parse-primary (cdr (cdr v))))
    (if (null r)
        (setq v nil)
        (setq v (cons (if (= "*" op)
                          (* (car v) (car r))
                          (/ (car v) (car r)))
                      (cdr r)))))
  v)

(defun @math:cale-parse-add (toks / v op r)
  "解析加减表达式(最低优先级)。
   toks: 记号列表。
   return: 点对 (值 . 剩余记号)。
   示例: (@math:cale-parse-add '(2.0 \"+\" 3.0))"
  (setq v (@math:cale-parse-mul toks))
  (while (and v (member (car (cdr v)) '("+" "-")))
    (setq op (car (cdr v))
          r (@math:cale-parse-mul (cdr (cdr v))))
    (if (null r)
        (setq v nil)
        (setq v (cons (if (= "+" op)
                          (+ (car v) (car r))
                          (- (car v) (car r)))
                      (cdr r)))))
  v)

(defun @math:cale-eval (str / toks v)
  "计算一个四则算式字符串，不使用 eval。
   str: 算式字符串，如 \"1200+3600\"、\"3*6\"、\"(2+3)*4\"。
   return: 数值；空串/非法表达式/除零返回 nil。
   示例: (@math:cale-eval \"(2+3)*4\") => 20.0"
  (if (and str (= 'str (type str)))
      (setq toks (@math:cale-tokenize str))
      (setq toks nil))
  (setq v (if toks
              (vl-catch-all-apply '@math:cale-parse-add (list toks))
              nil))
  (if (and v
           (not (vl-catch-all-error-p v))
           (null (cdr v)))
      (car v)
      nil))

(defun @math:cale-format (value / s)
  "把计算结果格式化为字符串：整数去小数点，实数保留必要小数。
   value: 数值。
   return: 字符串。
   示例: (@math:cale-format 4800.0) => \"4800\""
  (setq s (rtos value 2 6))
  (if (vl-string-search "." s)
      (progn
        (while (= "0" (substr s (strlen s) 1))
          (setq s (substr s 1 (1- (strlen s)))))
        (if (= "." (substr s (strlen s) 1))
            (setq s (substr s 1 (1- (strlen s)))))))
  s)

(defun @math:cale-make-text (value pt / en)
  "创建全能计算结果文字(数值→字符串)并放入结果图层。
   value: 数值；pt: 插入点。
   return: 文字 ename。
   示例: (@math:cale-make-text 20.0 (getpoint))"
  (if (null (tblsearch "layer" (@::get-config '@math:cale-layer)))
      (layer:make (@::get-config '@math:cale-layer) 2 nil nil))
  (setq en (entity:make-text
            (@math:cale-format value)
            pt
            (@::scale (@::get-config '@math:cale-fontsize))
            0 0.8 0 "LB"))
  (entity:putdxf en 8 (@::get-config '@math:cale-layer))
  en)

(defun @math:cale-side-point (ent / box p1 p2)
  "计算文字旁的结果插入点(文字包围盒右侧、垂直居中)。
   ent: 文字 ename。
   return: 插入点。
   示例: (@math:cale-side-point (car (entsel)))"
  (setq box (vl-catch-all-apply 'entity:getbox (list ent 0)))
  (if (and box (listp box) (= 2 (length box)))
      (progn
        (setq p1 (car box)
              p2 (cadr box))
        (list (+ (car p2) (* 0.5 (@::scale (@::get-config '@math:cale-fontsize))))
              (/ (+ (cadr p1) (cadr p2)) 2.0)
              0.0))
      (entity:getdxf ent 10)))

(defun @math:cale-txt-value (ent / str)
  "提取文字内容并求值(数字或四则算式)。
   ent: 文字 ename。
   return: 数值；无法解析返回 nil。
   示例: (@math:cale-txt-value (car (entsel)))"
  (setq str (vl-catch-all-apply 'text:get-mtext (list ent)))
  (if (and str (not (vl-catch-all-error-p str)))
      (@math:cale-eval (text:remove-fmt str))
      nil))

(defun at-math:cale-txt (/ ents op total en result-pt res vals miss)
  "交互命令：选中数字/算式文字，求和、求积或逐项求值并写结果。
   return: 无。
   示例: 在命令行执行 (at-math:cale-txt)"
  (@::prompt '("全能文字计算：选中数字/算式文字，求和、求积或逐项求值。"
               "支持 + - * / 括号与小数，如 \"1200+3600\"、\"3*6\"、\"(2+3)*4\"。"))
  (if (null (setq ents (cadr (ssgetfirst))))
      (setq ents (ssget '((0 . "*text")))))
  (if (null ents)
      (princ "\n[计算] 未选择文字。")
      (progn
        (setq ents (pickset:to-list ents))
        (initget "Sum Prod Eval")
        (setq op (getkword
                  "计算方式 [Sum(求和)/Prod(求积)/Eval(逐项求值)] <Sum>: "))
        (if (null op) (setq op "Sum"))
        (if (= "Eval" op)
            (progn
              (setq res nil
                    miss 0)
              (startundo)
              (foreach ent ents
                (setq total (@math:cale-txt-value ent))
                (if (numberp total)
                    (setq res (cons
                               (@math:cale-make-text
                                total (@math:cale-side-point ent))
                               res))
                    (setq miss (1+ miss))))
              (endundo)
              (if res
                  (princ (strcat "\n[计算] 已写 " (itoa (length res))
                                 " 个结果，跳过 " (itoa miss) " 个。"))
                  (princ "\n[计算] 没有可计算的结果。")))
            (progn
              (setq vals (vl-remove nil (mapcar '@math:cale-txt-value ents)))
              (if (null vals)
                  (princ "\n[计算] 没有可计算的数值。")
                  (progn
                    (setq total (if (= "Prod" op)
                                    (apply '* vals)
                                    (apply '+ vals)))
                    (setq result-pt
                          (getpoint "\n指定结果文字插入点(回车仅显示): "))
                    (if result-pt
                        (progn
                          (startundo)
                          (setq en (@math:cale-make-text total result-pt))
                          (endundo))
                        nil)
                    (princ (strcat "\n[计算] 结果: "
                                   (@math:cale-format total)))))))))
  (princ))

(defun at-math:ai-cale (ents op result-pt / lst vals total en res)
  "AI 调用入口：对选中文字做全能计算(求和/求积/逐项求值)，无交互、不弹窗。
   ents: 文字 ename 列表或选择集，内容为数字或四则算式。
   op: \"sum\" 求和 / \"prod\" 求积 / \"eval\" 逐项求值并把结果写在每项旁。
   result-pt: sum/prod 结果文字插入点(缺省 nil 不写图，仅返回数值)；eval 忽略此参数。
   return: sum/prod 返回 (结果值 结果文字ename 或 nil)；
           eval 返回 ((值 . 文字ename) ...) 列表；无有效值返回 nil。
   示例: (at-math:ai-cale (ssget '((0 . \"*text\"))) \"sum\" (getpoint))
         (at-math:ai-cale ents \"eval\" nil)"
  (setq lst (cond ((null ents) nil)
                  ((= 'pickset (type ents)) (pickset:to-list ents))
                  ((listp ents) ents)
                  (t (list ents))))
  (if (null lst)
      (progn (princ "\n[计算] 未提供文字对象。") nil)
      (progn
        (setq op (strcase (if (and op (= 'str (type op))) op "SUM")))
        (cond
          ((= "EVAL" op)
           (setq res nil)
           (startundo)
           (foreach ent lst
             (setq total (@math:cale-txt-value ent))
             (if (numberp total)
                 (setq res (cons (cons total
                                       (@math:cale-make-text
                                        total
                                        (@math:cale-side-point ent)))
                                 res))))
           (endundo)
           (reverse res))
          (t
           (setq vals (vl-remove nil (mapcar '@math:cale-txt-value lst)))
           (if (null vals)
               (progn (princ "\n[计算] 没有可计算的数值。") nil)
               (progn
                 (setq total (if (= "PROD" op)
                                 (apply '* vals)
                                 (apply '+ vals)))
                 (if (and result-pt (listp result-pt))
                     (progn
                       (startundo)
                       (setq en (@math:cale-make-text total result-pt))
                       (endundo)
                       (list total en))
                     (list total nil)))))))))
