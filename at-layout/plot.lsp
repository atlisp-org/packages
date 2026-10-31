;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plot.lsp — 布图工具
;; 图纸空间1:1布图 / 模型空间做块布图 / 还原炸开布图块 / 快速局部重生成
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (null point:mid) (require 'point:*))

;; ===================== 1. 图纸空间1：1布图 ps-scale =====================

(defun @layout:ai-ps-scale (corner1 corner2 dstpt / b w h center pt)
  "AI 调用入口：框选模型空间区域，在当前布局图纸空间建 1:1 视口。
corner1/corner2 为模型空间两角点，dstpt 为图纸空间插入点
（视口左下角，nil=(0,0,0)）。
成功返回视口 ename，失败返回 nil。
示例: (@layout:ai-ps-scale '(0 0 0) '(1000 800 0) '(10 10 0))"
  "视口 ename / nil"
  "示例: (@layout:ai-ps-scale '(0 0 0) '(1000 800 0) nil)"
  (if (not (and (listp corner1) (listp corner2)
                (> (length corner1) 1) (> (length corner2) 1)))
      (progn (princ "\n错误: 角点参数无效") nil)
      (progn
        (setq b (@layout:box corner1 corner2)
              w (- (caddr b) (car b))
              h (- (cadddr b) (cadr b)))
        (if (or (<= w 0) (<= h 0))
            (progn (princ "\n错误: 区域宽或高为零") nil)
            (progn
              (setq center (point:mid (list (car b) (cadr b))
                                      (list (caddr b) (cadddr b))))
              (if (and (listp dstpt) (> (length dstpt) 1))
                  (setq pt (list (float (car dstpt))
                                 (float (cadr dstpt))
                                 0.0))
                  (setq pt (list 0.0 0.0 0.0)))
              (@layout:make-vp (getvar "ctab")
                                   pt
                                   w h
                                   (car center) (cadr center)
                                   1.0))))))

(defun @layout:ps-scale (/ p1 p2 dst)
  "图纸空间1：1布图（交互）：模型空间框选区域，图纸空间建 1:1 视口"
  "t"
  "示例: (@layout:ps-scale)"
  (if (not (@layout:in-model))
      (@layout:switch-space "model"))
  (setq p1 (getpoint "\n模型空间第一角点: "))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n模型空间第二角点: "))
        (if p2
            (progn
              (@layout:switch-space "paper")
              (setq dst (getpoint "\n图纸空间插入点(回车=0,0): "))
              (command "_.UNDO" "Begin")
              (@layout:ai-ps-scale p1 p2 dst)
              (command "_.UNDO" "End")))))
  (princ))

;; ===================== 2. 模型空间做块布图 ms-scale =====================

(defun @layout:copy-to-block (blockdef ent / en r l1 l2 l3 l4)
  "把实体复制进块定义；视口改为画其图纸空间矩形框"
  "t / nil"
  "示例: (@layout:copy-to-block blockdef (car (entsel)))"
  (setq en (if (@layout:ename-p ent)
               ent
               (if (listp ent) (car ent) nil)))
  (if (not (@layout:ename-p en))
      nil
      (if (@layout:vp-p en)
          (progn
            (setq r (@layout:vp-rect en))
            (setq l1 (vla-addline blockdef
                                  (vlax-3d-point (list (car r) (cadr r) 0.0))
                                  (vlax-3d-point (list (caddr r) (cadr r) 0.0))))
            (vla-put-layer l1 "0")
            (setq l2 (vla-addline blockdef
                                  (vlax-3d-point (list (caddr r) (cadr r) 0.0))
                                  (vlax-3d-point (list (caddr r) (cadddr r) 0.0))))
            (vla-put-layer l2 "0")
            (setq l3 (vla-addline blockdef
                                  (vlax-3d-point (list (caddr r) (cadddr r) 0.0))
                                  (vlax-3d-point (list (car r) (cadddr r) 0.0))))
            (vla-put-layer l3 "0")
            (setq l4 (vla-addline blockdef
                                  (vlax-3d-point (list (car r) (cadddr r) 0.0))
                                  (vlax-3d-point (list (car r) (cadr r) 0.0))))
            (vla-put-layer l4 "0")
            t)
          (progn
            (vla-copy (e2o en) blockdef)
            t))))

(defun @layout:ai-ms-scale (layout basept scale / lay ss vps best
                                bestarea vw s bname blks bdef e pt ins)
  "AI 调用入口：把布局图纸空间实体打成块并插入模型空间（做块布图）。
layout 为源布局名（nil=当前布局），
basept 为模型空间插入点（nil=(0,0,0)），
scale 为块插入比例（nil=自动，取主视口 1:N 的 N，放大后打印尺寸不变）。
成功返回插入的块 ename，失败返回 nil。
示例: (@layout:ai-ms-scale \"布局1\" '(0 0 0) 100)"
  "插入的块 ename / nil"
  "示例: (@layout:ai-ms-scale nil nil nil)"
  (if (and (stringp layout) (/= "" layout))
      (setq lay layout)
      (setq lay (getvar "ctab")))
  (if (not (@layout:layout-p lay))
      (progn (princ (strcat "\n错误: 布局 [" lay "] 不存在")) nil)
      (progn
        (setq ss (ssget "x" (list (cons 410 lay))))
        (if (null ss)
            (progn (princ (strcat "\n错误: 布局 [" lay "] 没有图纸空间实体"))
                   nil)
            (progn
              ;; 自动比例：主视口(图纸面积最大)的 1:N
              (if (not (and (numberp scale) (> scale 0.0)))
                  (progn
                    (setq vps (@layout:vp-list lay)
                          best nil
                          bestarea 0.0
                          s 1.0)
                    (foreach v vps
                      (setq vw (@layout:vp-view v))
                      (if (> (* (nth 2 vw) (nth 3 vw)) bestarea)
                          (progn
                            (setq best vw)
                            (setq bestarea (* (nth 2 vw) (nth 3 vw))))))
                    (if best (setq s (/ 1.0 (nth 4 best))))
                    (setq scale s)))
              (setq bname (@layout:unique-block
                           (strcat "ML_MS_" (@layout:sanitize-name lay)))
                    blks (vla-get-blocks *doc*)
                    bdef (vla-add blks bname))
              (repeat (sslength ss)
                (setq e (ssname ss (1- (sslength ss))))
                (@layout:copy-to-block bdef e))
              ;; 插入模型空间
              (if (and (listp basept) (> (length basept) 1))
                  (setq pt (list (float (car basept))
                                 (float (cadr basept))
                                 0.0))
                  (setq pt (list 0.0 0.0 0.0)))
              (setq ins (vla-insertblock (vla-get-modelspace *doc*)
                                         (vlax-3d-point pt)
                                         bname
                                         (float scale)
                                         (float scale)
                                         (float scale)
                                         0.0))
              (if ins
                  (progn
                    (princ (strcat "\n已生成布图块 [" bname "] 插入模型空间"
                                   " (比例 " (rtos scale 2 1) ")"))
                    (o2e ins))
                  (progn (princ "\n插入块失败") nil)))))))

(defun @layout:ms-scale (/ lay s pt)
  "模型空间做块布图（交互）：当前布局图纸空间实体打块插入模型空间"
  "t"
  "示例: (@layout:ms-scale)"
  (setq lay (getstring 1
                       (strcat "\n源布局名 <当前 " (getvar "ctab") ">: ")))
  (setq s (getreal "\n布图比例(回车=按主视口1:N自动): "))
  (@layout:switch-space "model")
  (setq pt (getpoint "\n模型空间插入点(回车=0,0): "))
  (command "_.UNDO" "Begin")
  (@layout:ai-ms-scale (if (= "" lay) nil lay)
                           pt
                           (if s s nil))
  (command "_.UNDO" "End")
  (princ))

;; ===================== 3. 还原炸开布图块 ms-exp =====================

(defun @layout:ai-ms-exp (ents / lst e res)
  "AI 调用入口：炸开一组块插入（还原做块布图的结果）。
ents 为 INSERT ename 列表（或选择集）。
成功返回已炸开 ename 列表，失败返回 nil。
示例: (@layout:ai-ms-exp (@layout:ss-to-list (ssget \"i\" (list (cons 0 \"INSERT\")))))"
  "已炸开 ename 列表 / nil"
  "示例: (@layout:ai-ms-exp (list (car (entsel))))"
  (setq lst (@layout:entlist ents)
        res nil)
  (if (null lst)
      (progn (princ "\n错误: 没有块实体") nil)
      (progn
        (setvar "cmdecho" 0)
        (foreach e lst
          (if (and (@layout:ename-p e)
                   (= "INSERT" (cdr (assoc 0 (entget e)))))
              (progn
                (command "_.explode" e "")
                (setq res (cons e res)))))
        (setvar "cmdecho" 1)
        (if res
            (progn
              (princ (strcat "\n已炸开 " (itoa (length res)) " 个块"))
              (reverse res))
            (progn (princ "\n没有可炸开的块") nil)))))

(defun @layout:ms-exp (/ ss)
  "还原炸开布图块（交互）：选择布图块炸开"
  "t"
  "示例: (@layout:ms-exp)"
  (setq ss (ssget (list (cons 0 "INSERT"))))
  (if ss
      (progn
        (command "_.UNDO" "Begin")
        (@layout:ai-ms-exp (@layout:ss-to-list ss))
        (command "_.UNDO" "End")))
  (princ))

;; ===================== 4. 快速局部重生成 regen =====================

(defun @layout:ai-regen (corner1 corner2 / b vps v res x1 y1 x2 y2)
  "AI 调用入口：重生成当前布局中模型视图与所选区域相交的视口（局部重生成）。
corner1/corner2 为模型空间两角点。
成功返回已重生成视口 ename 列表，失败返回 nil。
示例: (@layout:ai-regen '(0 0 0) '(5000 5000 0))"
  "已重生成视口 ename 列表 / nil"
  "示例: (@layout:ai-regen '(0 0 0) '(5000 5000 0))"
  (if (not (and (listp corner1) (listp corner2)
                (> (length corner1) 1) (> (length corner2) 1)))
      (progn (princ "\n错误: 角点参数无效") nil)
      (progn
        (setq b (@layout:box corner1 corner2)
              vps (@layout:vports)
              res nil)
        (if (null vps)
            (progn (princ "\n当前布局没有视口") nil)
            (progn
              (foreach v vps
                (setq x1 (car (@layout:vp-model-rect v))
                      y1 (cadr (@layout:vp-model-rect v))
                      x2 (caddr (@layout:vp-model-rect v))
                      y2 (cadddr (@layout:vp-model-rect v)))
                (if (and (> x2 (car b)) (> (caddr b) x1)
                         (> y2 (cadr b)) (> (cadddr b) y1))
                    (progn
                      (vla-regen (e2o v))
                      (setq res (cons v res)))))
              (if res
                  (progn
                    (princ (strcat "\n已重生成 " (itoa (length res))
                                   " 个视口"))
                    (reverse res))
                  (progn (princ "\n所选区域内没有视口") nil)))))))

(defun @layout:regen (/ p1 p2)
  "快速局部重生成（交互）：模型空间框选区域，重生成其中视口"
  "t"
  "示例: (@layout:regen)"
  (if (not (@layout:in-model))
      (@layout:switch-space "model"))
  (setq p1 (getpoint "\n第一角点: "))
  (if p1
      (progn
        (setq p2 (getpoint p1 "\n第二角点: "))
        (if p2
            (progn
              (command "_.UNDO" "Begin")
              (@layout:ai-regen p1 p2)
              (command "_.UNDO" "End")))))
  (princ))

(princ)
