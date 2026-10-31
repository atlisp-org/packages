;;------------------------=={ Quick / Getsel / Chain / Touch Selection }==-----------;;
;;  at-select 补充选择功能（对应 iMini 的 quick选择易/获得选择集/链式/触碰选择）：
;;   - quick  快速选择：按过滤表达式（图层=墙,块=门*,颜色=1）快速选择
;;   - getsel 获得选择集：过滤表达式另支持 类型=LINE
;;   - chain  链式选择：从一条线出发，沿首尾相接的链递归扩展（容差参数）
;;   - touch  触碰选择：选中实体，自动选中与其相交或端点相接的所有实体（递归扩展）
;;  quick/getsel 共用过滤引擎 @select:filter-ss。
;;  AI 入口（无交互）：at-select:ai-quick / ai-getsel / ai-chain / ai-touch
;;----------------------------------------------------------------------;;

;; 排除 关闭/冻结/锁定 图层的 ssget 过滤片段

(defun @select:excl-layer-filter (/ df ex)
  "生成排除关闭/冻结/锁定图层的 ssget 过滤片段"
  "列表（可拼入 ssget 过滤），无排除图层返回 nil"
  "(@select:excl-layer-filter)"
  (while (setq df (tblnext "layer" (not df)))
    (if (or (minusp (cdr (assoc 62 df)))
            (< 0 (logand 5 (cdr (assoc 70 df)))))
        (setq ex (cons (cons 8 (@select:chain-escape-wildcards (cdr (assoc 2 df)))) ex))))
  (if ex
      (append '(-4 . "<NOT") '(-4 . "<OR") ex '(-4 . "OR>") '(-4 . "NOT>"))
      nil))

;; 图中所有曲线实体（排除关闭/冻结/锁定图层上的图元）

(defun @select:curve-entlist (/ ss res)
  "列出图中所有曲线实体（LINE/ARC/CIRCLE/LWPOLYLINE/POLYLINE/SPLINE/ELLIPSE）"
  "ename 列表，无则 nil"
  "(@select:curve-entlist)"
  (if (setq ss (ssget "_X" (append
                            (list '(-4 . "<OR")
                                  '(0 . "LINE,ARC,CIRCLE,LWPOLYLINE,POLYLINE,SPLINE,ELLIPSE")
                                  '(-4 . "OR>")))
                          (@select:excl-layer-filter)))
      (setq res (vl-remove-if-not 'vlax-curve-is-curve (pickset:to-list ss))))
  (if res res nil))

;;--------------------------=={ 过滤引擎 }==---------------------------------;;

;; 过滤键名 -> dxf 组码

(defun @select:filter-code (key / k)
  "把过滤键名映射为 dxf 组码。支持 图层/块/颜色/类型（兼容英文 layer/blk/color/type，不区分大小写）"
  "组码（整数），未知键名返回 nil"
  "(@select:filter-code \"图层\") => 8"
  (setq k (strcase (vl-string-trim " \t" key)))
  (cond
    ((wcmatch k "图层,LAYER") 8)
    ((wcmatch k "块,BLK,BLOCK") 2)
    ((wcmatch k "颜色,COLOR") 62)
    ((wcmatch k "类型,TYPE") 0)))

;; 解析过滤表达式为条件列表

(defun @select:parse-filter (str / lst tok key code val acc last-type)
  "解析过滤表达式。格式：图层=墙,块=门*,颜色=1,类型=LINE（多条件为 AND，类型 值可含多个逗号分隔的类型）"
  "((组码 . 值) ...) 或 nil（表达式为空/无等号）"
  "(@select:parse-filter \"图层=墙,颜色=1\") => ((8 . \"墙\") (62 . \"1\"))"
  (if (and str (vl-string-search "=" str))
      (progn
        (setq lst (at-select:split-str (vl-string-subst "，" "," str) ","))
        (foreach tok lst
          (if (vl-string-search "=" tok)
              (progn
                (setq key (vl-string-trim " \t" (substr tok 1 (vl-string-search "=" tok)))
                      val (vl-string-trim " \t" (substr tok (+ (vl-string-search "=" tok) 1)))
                      code (@select:filter-code key))
                (if code
                    (progn
                      (setq acc (cons (cons code val) acc)
                            last-type (= code 0))
                      )
                    (setq last-type nil)))
              (if last-type
                  (setq acc (mapcar
                             '(lambda (x)
                                (if (= (car x) 0)
                                    (cons 0 (strcat (cdr x) "," (vl-string-trim " \t" tok)))
                                    x))
                             acc)))))
        (if acc (reverse acc) nil))
      nil))

;; 单图元是否满足全部过滤条件（AND）

(defun @select:filter-match (ent conds / d code val)
  "判断图元是否满足全部过滤条件（AND，通配符匹配）"
  "T 或 nil"
  "(@select:filter-match en '((8 . \"墙\") (62 . \"1\")))"
  (vl-every
   '(lambda (x)
      (progn
        (setq code (car x)
              val (cdr x)
              d (entget ent))
        (cond
          ((= code 62)
           (wcmatch
            (itoa (if (cdr (assoc 62 d)) (cdr (assoc 62 d)) 0))
            (if (wcmatch (strcase val) "BYLAYER,随层") "-1" val)))
          (t
           (wcmatch (if (cdr (assoc code d)) (cdr (assoc code d)) "") val)))))
   conds))

;; 在给定选择集（nil=全图）中按过滤表达式筛选

(defun @select:filter-ss (ss filter / res en conds)
  "在给定选择集（nil 表示全图，也可传 ename 列表）中按过滤表达式筛选"
  "ename 列表，无匹配返回 nil"
  "(@select:filter-ss (ssget \"_X\") \"图层=墙,颜色=1\")"
  (if (p:picksetp ss)
      (setq ss (pickset:to-list ss)))
  (if (null ss)
      (setq ss (pickset:to-list (ssget "_X"))))
  (setq conds (@select:parse-filter filter))
  (setq res nil)
  (foreach en ss
    (if (@select:filter-match en conds)
        (setq res (cons en res))))
  (if res (reverse res) nil))

;; AI 入口：快速选择

(defun at-select:ai-quick (filter / res)
  "AI 入口：按过滤表达式快速选择（图层=墙,块=门*,颜色=1，多条件 AND，通配符）
   filter：过滤表达式，nil 表示全图
   \"ename 列表，失败返回 nil\"
   \"(at-select:ai-quick \"图层=墙,颜色=1\")\""
  (if (null filter)
      (pickset:to-list (ssget "_X"))
      (progn
        (setq res (@select:filter-ss nil filter))
        (if res res nil))))

;; AI 入口：获得选择集（支持 类型=LINE）

(defun at-select:ai-getsel (filter / res)
  "AI 入口：按表达式获得选择集（同 quick 语法，另支持 类型=LINE,ARC 等）
   filter：过滤表达式，nil 表示全图
   \"ename 列表，失败返回 nil\"
   \"(at-select:ai-getsel \"类型=LINE,图层=墙\")\""
  (setq res (at-select:ai-quick filter))
  (if res res nil))

;; 交互命令：快速选择

(defun c:quick-sel (/ filter ss res)
  "快速选择：输入过滤表达式（图层=墙,块=门*,颜色=1）选择匹配图元"
  "无返回值（交互命令，结果输出到命令行）"
  "(c:quick-sel)"
  (prompt "\n输入过滤表达式（图层=墙,块=门*,颜色=1）：")
  (if (setq filter (getstring t "过滤表达式："))
      (progn
        (setq ss (ssget))
        (if (null ss)
            (setq ss (ssget "_X")))
        (if (setq res (@select:filter-ss ss filter))
            (progn
              (sssetfirst nil (pickset:from-list res))
              (princ (strcat "\n已选择 " (itoa (length res)) " 个图元")))
            (princ "\n没有匹配的图元"))))
  (princ))

;; 交互命令：获得选择集

(defun c:getsel (/ filter ss res)
  "获得选择集：按过滤表达式获得选择集（支持 类型=LINE）"
  "无返回值（交互命令，结果输出到命令行）"
  "(c:getsel)"
  (prompt "\n输入过滤表达式（图层=墙,块=门*,颜色=1,类型=LINE）：")
  (if (setq filter (getstring t "过滤表达式："))
      (progn
        (setq ss (ssget))
        (if (null ss)
            (setq ss (ssget "_X")))
        (if (setq res (@select:filter-ss ss filter))
            (progn
              (sssetfirst nil (pickset:from-list res))
              (princ (strcat "\n已选择 " (itoa (length res)) " 个图元")))
            (princ "\n没有匹配的图元"))))
  (princ))

;;--------------------------=={ 链式选择 }==---------------------------------;;

(defun @select:chain-from (start-ent tol / cands res frontier p c matched next new-ents en)
  "链式选择核心：从 start-ent 两端点出发，沿端点相接（容差 tol）递归扩展整条链
   候选实体为图中全部曲线（排除关闭/冻结/锁定图层）
   \"链上 ename 列表（含 start-ent），无效时 nil\"
   \"(@select:chain-from (car (entsel)) 0.001)\""
  (if (vlax-curve-is-curve start-ent)
      (progn
        (setq cands nil)
        (foreach en (@select:curve-entlist)
          (if (not (eq en start-ent))
              (setq cands (cons
                           (list (vlax-curve-getstartpoint en)
                                 (vlax-curve-getendpoint en)
                                 en)
                           cands))))
        (setq res (list start-ent)
              frontier (list (vlax-curve-getstartpoint start-ent)
                             (vlax-curve-getendpoint start-ent)))
        (while frontier
          (setq next nil new-ents nil)
          (foreach p frontier
            (setq matched
                  (vl-remove-if-not
                   '(lambda (x)
                      (if (equal (car x) p tol) (equal (car x) p tol) (equal (cadr x) p tol)))
                   cands))
            (foreach c matched
              (setq next (cons
                          (if (equal (car c) p tol) (cadr c) (car c))
                          next))
                    new-ents (cons (caddr c) new-ents))
            (setq cands (vl-remove-if
                         '(lambda (x) (member (caddr x) new-ents))
                         cands)))
          (if new-ents
              (progn
                (setq res (append res new-ents)
                      frontier (vl-remove-duplicates next))
              )
              (setq frontier nil)))
        res)
      nil))

;; AI 入口：链式选择

(defun at-select:ai-chain (start-ent / tol)
  "AI 入口：从给定实体出发，沿首尾相接的链选出整条链
   start-ent：起始曲线实体；tol：端点容差（nil 用 1e-6）
   \"链上 ename 列表（含起始实体），失败返回 nil\"
   \"(at-select:ai-chain (car (entsel)) 0.001)\""
  (if (or (null tol) (< tol 0))
      (setq tol 1e-6))
  (@select:chain-from start-ent tol))

;;--------------------------=={ 触碰选择 }==---------------------------------;;

;; 点列 -> 线段对列表

(defun @select:pt-segments (pts / res i)
  "点列转线段对列表"
  "((p1 p2) ...)"
  "(@select:pt-segments '((0 0 0) (1 0 0) (1 1 0)))"
  (setq i 0)
  (repeat (1- (length pts))
    (setq res (cons (list (nth i pts) (nth (+ i 1) pts)) res)
          i (1+ i)))
  (reverse res))

;; 点是否在线段上（容差）

(defun @select:pt-on-seg (pt p1 p2 tol / dx dy dd tt cp)
  "判断点是否在线段上（容差内）"
  "T 或 nil"
  "(@select:pt-on-seg '(0.5 0 0) '(0 0 0) '(1 0 0) 1e-6)"
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1))
        dd (+ (* dx dx) (* dy dy))
        tt (if (< dd 1e-24)
               0.0
               (/ (+ (* (- (car pt) (car p1)) dx)
                     (* (- (cadr pt) (cadr p1)) dy))
                  dd))
        cp (list (+ (car p1) (* tt dx))
                 (+ (cadr p1) (* tt dy))
                 0.0))
  (and (>= tt -0.01) (<= tt 1.01)
       (< (distance pt cp) (+ tol 1e-9))))

;; 两线段是否相交或端点相接（容差）

(defun @select:seg-intersect (p1 p2 p3 p4 tol / d1 d2 cross r s)
  "判断两线段是否相交或端点相接（容差内）"
  "T 或 nil"
  "(@select:seg-intersect '(0 0 0) '(1 0 0) '(0.5 0.5 0) '(0.5 -0.5 0) 1e-6)"
  (if (or (equal p1 p3 tol) (equal p1 p4 tol)
          (equal p2 p3 tol) (equal p2 p4 tol))
      t
      (progn
        (setq d1 (list (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))
              d2 (list (- (cadr p4) (cadr p3)) (- (car p4) (car p3)))
              cross (- (* (car d1) (cadr d2)) (* (cadr d1) (car d2))))
        (if (< (abs cross) 1e-12)
            (or (@select:pt-on-seg p1 p3 p4 tol)
                (@select:pt-on-seg p2 p3 p4 tol)
                (@select:pt-on-seg p3 p1 p2 tol)
                (@select:pt-on-seg p4 p1 p2 tol))
            (progn
              (setq r (/ (- (* (- (car p3) (car p1)) (cadr d2))
                            (* (- (cadr p3) (cadr p1)) (car d2)))
                        cross)
                    s (/ (- (* (- (car p3) (car p1)) (cadr d1))
                            (* (- (cadr p3) (cadr p1)) (car d1)))
                        cross))
              (and (> r 0.0) (< r 1.0) (> s 0.0) (< s 1.0)))))))

;; 两折线（点列）是否相交或端点相接

(defun @select:plines-intersect (pts1 pts2 tol / s1s s2s)
  "判断两折线（点列）是否相交或端点相接"
  "T 或 nil"
  "(@select:plines-intersect '((0 0 0) (1 0 0)) '((0.5 0.5 0) (0.5 -0.5 0)) 1e-6)"
  (and pts1 pts2
       (progn
         (setq s1s (@select:pt-segments pts1)
               s2s (@select:pt-segments pts2))
         (vl-some
          '(lambda (s1)
             (vl-some
              '(lambda (s2)
                 (@select:seg-intersect (car s1) (cadr s1) (car s2) (cadr s2) tol))
              s2s))
          s1s))))

;; 圆弧/圆采样为折线

(defun @select:sample-arc (c r a1 a2 closed / res i)
  "把圆弧/圆采样 16 段折线（闭环时首点追加到末尾）"
  "点列表"
  "(@select:sample-arc '(0 0 0) 10.0 0.0 pi t)"
  (repeat 16
    (setq res (cons (polar c r (+ a1 (* (/ (length res) 15.0) (- a2 a1)))) res)))
  (reverse (if closed (append res (list (car res))) res)))

;; 实体转点列（非几何实体返回 nil）

(defun @select:ent-to-pts (en / d res i e pts)
  "把实体转换为折线点列（LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE/SPLINE/ELLIPSE），闭环折线首点追加末尾"
  "点列表，非几何实体返回 nil"
  "(@select:ent-to-pts (car (entsel)))"
  (setq i 0)
  (setq d (entget en))
  (cond
    ((= "LINE" (cdr (assoc 0 d)))
     (list (vlax-curve-getstartpoint en) (vlax-curve-getendpoint en)))
    ((= "LWPOLYLINE" (cdr (assoc 0 d)))
     (setq res (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) d)))
     (if (and (cdr (assoc 70 d)) (= 1 (logand 1 (cdr (assoc 70 d)))))
         (setq res (append res (list (car res)))))
     res)
    ((= "POLYLINE" (cdr (assoc 0 d)))
     (setq res nil)
     (setq e (entnext en))
     (while (and e (wcmatch (cdr (assoc 0 (entget e))) "VERTEX"))
       (setq res (cons (cdr (assoc 10 (entget e))) res)
             e (entnext e)))
     (setq res (reverse res))
     (if (and (cdr (assoc 70 d)) (= 1 (logand 1 (cdr (assoc 70 d)))))
         (setq res (append res (list (car res)))))
     res)
    ((= "ARC" (cdr (assoc 0 d)))
     (@select:sample-arc
      (cdr (assoc 10 d)) (cdr (assoc 40 d))
      (/ (* (cdr (assoc 50 d)) pi) 180.0)
      (/ (* (cdr (assoc 51 d)) pi) 180.0) nil))
    ((= "CIRCLE" (cdr (assoc 0 d)))
     (@select:sample-arc (cdr (assoc 10 d)) (cdr (assoc 40 d)) 0.0 (* pi 2) t))
    ((wcmatch (cdr (assoc 0 d)) "SPLINE,ELLIPSE")
     (repeat 16
       (setq res (cons (vlax-curve-getPoint en (/ i 15.0)) res)
             i (1+ i)))
     (setq res (reverse res))
     (if (vlax-curve-isClosed en)
         (setq res (append res (list (car res)))))
     res)
    (t nil)))

;; 触碰选择核心：递归扩展

(defun @select:touch-from (ent tol / pts-map res done frontier next fp cm e pts)
  "触碰选择核心：从 ent 出发，递归选中与其相交或端点相接（容差 tol）的所有曲线实体"
  "ename 列表（含 ent），无效实体返回 nil"
  "(@select:touch-from (car (entsel)) 0.001)"
  (if (vlax-curve-is-curve ent)
      (progn
        (setq pts-map nil)
        (foreach e (@select:curve-entlist)
          (if (not (eq e ent))
              (if (setq pts (@select:ent-to-pts e))
                  (setq pts-map (cons (list e pts) pts-map)))))
        (if pts-map
            (progn
              (setq res (list ent)
                    done (list ent)
                    frontier (list (list ent (@select:ent-to-pts ent))))
              (while frontier
                (setq next nil)
                (foreach fp frontier
                  (foreach cm pts-map
                    (if (and (not (member (car cm) done))
                             (@select:plines-intersect (cadr fp) (cadr cm) tol))
                        (progn
                          (setq done (cons (car cm) done))
                          (setq next (cons cm next))))))
                (setq pts-map (vl-remove-if
                               '(lambda (x) (member (car x) done))
                               pts-map))
                (setq frontier next))
              (reverse done))
            (list ent)))
      nil))

;; AI 入口：触碰选择

(defun at-select:ai-touch (ent / tol)
  "AI 入口：选中与给定实体相交或端点相接的所有实体（递归扩展）
   ent：曲线实体；tol：容差（nil 用 0.001）
   \"ename 列表（含 ent），失败返回 nil\"
   \"(at-select:ai-touch (car (entsel)) 0.001)\""
  (if (or (null tol) (< tol 0))
      (setq tol 0.001))
  (@select:touch-from ent tol))

;; 交互命令：触碰选择

(defun c:touch-sel (/ ent tol res)
  "触碰选择：点选一个实体，自动选中与其相交或端点相接的所有实体"
  "无返回值（交互命令，结果输出到命令行）"
  "(c:touch-sel)"
  (if (setq ent (entsel "\n请选择一个实体："))
      (progn
        (setq tol (getreal "\n触碰容差（回车默认 0.001）："))
        (if (or (null tol) (< tol 0))
            (setq tol 0.001))
        (if (setq res (@select:touch-from (car ent) tol))
            (progn
              (sssetfirst nil (pickset:from-list res))
              (princ (strcat "\n已选择 " (itoa (length res)) " 个图元")))
            (princ "\n未找到相关实体"))))
  (princ))

(princ)

(defun at-select:split-str (str seps / res cur i n c)
  "按分隔符集合拆分字符串（seps 中任一字符都算分隔符），忽略空段"
  "字符串列表"
  "(at-select:split-str \"a,b;c\" \",;\")"
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
