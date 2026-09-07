;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 电气设计工具包
;; 功能：回路编号、电缆标注、配电箱统计、负荷计算、设备插入等
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:define-config '@elec:circut-prefix "WL" "回路编号前缀")
(@:define-config '@elec:circut-start 1 "回路起始编号")
(@:define-config '@elec:cable-layer "ELEC-CABLE" "电缆标注图层")
(@:define-config '@elec:panel-blkname "配电箱" "配电箱图块名")

(@:add-menus
 '("@电气"
   ("电气设置" (@elec:setup))
   ("---")
   ("回路编号" (@elec:circut-numbering))
   ("电缆标注" (@elec:cable-label))
   ("负荷统计" (@elec:load-stat))
   ("设备统计" (@elec:equip-stat))
   ("插入设备" (@elec:menu-insert-equip))
   ("---")
   ("电气说明" (@elec:draw-readme))
   ("电气样例" (@elec:draw-plan-example))
   ))

(defun @elec:setup ()
  "电气设置对话框"
  (setq @::tmp-search-str "@elec")
  (@::edit-config-dialog))

;;; ============================================================
;;; 回路编号
;;; ============================================================
(defun @elec:circut-numbering (/ prefix start num ss pt)
  "对选中的配电箱块进行回路编号"
  (@:help '("选择配电箱块，按顺序为每个回路编号。"
            "编号格式: 前缀-序号, 如 WL-01, WL-02"))
  (setq prefix (@:get-config '@elec:circut-prefix))
  (setq start (@:get-config '@elec:circut-start))
  (@:prompt "请选择需要编号的回路线:")
  (setq ss (ssget '((0 . "*line"))))
  (if ss
      (progn
        (setq num start)
        (foreach ent (pickset:to-list ss)
          (setq pt (curve:midpoint ent))
          (entity:make-text
           (strcat prefix "-" 
                   (string:number-format (itoa num) 2 0 "0"))
           (polar pt (* 0.5 pi) 200)
           (* (@:get-config '@::draw-scale) 2.5)
           0 0.72 0 "mm")
          (setq num (1+ num)))
        (@:set-config '@elec:circut-start num)
        (princ (strcat "\n已编号 " (itoa (- num start)) " 个回路。")))
    (@:prompt "未选中任何线。"))
  (princ))

;;; ============================================================
;;; 电缆标注
;;; ============================================================
(defun @elec:cable-label (/ ent pt cable-type)
  "标注电缆型号和规格"
  (@:help '("选择一条线，标注其电缆型号。"
            "如: YJV-3x2.5+1x1.5, NH-BV-4x4"))
  (if (null (tblsearch "layer" (@:get-config '@elec:cable-layer)))
      (layer:make (@:get-config '@elec:cable-layer) 3 nil nil))
  (setq ent (car (entsel "请选择要标注的电缆线:")))
  (if (and ent
           (setq pt (curve:midpoint ent))
           (setq cable-type (getstring "\n请输入电缆型号 (如 YJV-3x2.5+1x1.5): ")))
      (progn
        (entity:putdxf
         (entity:make-text
          cable-type
          (polar pt (angle (car (curve:get-points ent))
                           (cadr (curve:get-points ent)))
                 (* 0.3 (distance (car (curve:get-points ent))
                                  (cadr (curve:get-points ent)))))
          (* (@:get-config '@::draw-scale) 2.0)
          (angle (car (curve:get-points ent))
                 (cadr (curve:get-points ent)))
          0.72 0 "mm")
         8 (@:get-config '@elec:cable-layer))
        (@:prompt (strcat "已标注: " cable-type)))
    (@:prompt "未选中有效图元或未输入型号。"))
  (princ))

;;; ============================================================
;;; 负荷统计
;;; ============================================================
(defun @elec:load-stat (/ ss total-watt count watt-str watt)
  "统计选中设备块的总负荷功率"
  (@:help '("选择设备块，统计其总安装功率。"
            "块的动态属性中需包含'功率'属性。"))
  (@:prompt "请选择需要统计的设备块:")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
      (progn
        (setq total-watt 0 count 0)
        (foreach blk (pickset:to-list ss)
          (setq watt-str (block:get-dynprop blk "功率"))
          (if (and watt-str (setq watt (atof watt-str)))
              (progn
                (setq total-watt (+ total-watt watt))
                (setq count (1+ count)))))
        (princ (strcat "\n共 " (itoa count) " 个设备"))
        (princ (strcat "\n总安装功率: " (rtos total-watt 2 1) " W"))
        (princ (strcat "\n计算负荷: " (rtos (* total-watt 0.7) 2 1) " W (Kd=0.7)")))
    (@:prompt "未选中任何块。"))
  (princ))

;;; ============================================================
;;; 设备统计
;;; ============================================================
(defun @elec:equip-stat (/ ss stats i bname)
  "统计所选设备块的型号和数量"
  (@:help '("选择设备块，按型号分类统计数量。"))
  (@:prompt "请选择需要统计的设备块:")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
      (progn
        (setq stats nil)
        (foreach blk (pickset:to-list ss)
          (setq bname (block:get-effectivename blk))
          (if bname
              (if (assoc bname stats)
                  (setq stats (subst (cons bname (1+ (cdr (assoc bname stats))))
                                     (assoc bname stats)
                                     stats))
                (setq stats (cons (cons bname 1) stats)))))
        ;; 排序并输出
        (setq stats (vl-sort stats '(lambda (a b) (< (car a) (car b)))))
        (princ "\n=== 设备统计 ===")
        (setq i 0)
        (foreach s stats
          (setq i (1+ i))
          (princ (strcat "\n" (itoa i) ". " (car s) " x " (itoa (cdr s)))))
        (princ (strcat "\n共 " (itoa (length stats)) " 种设备")))
    (@:prompt "未选中任何块。"))
  (princ))

;;; ============================================================
;;; 设备插入
;;; ============================================================
(defun @elec:menu-insert-equip (/ equip-list choice downfile)
  (@:help '("插入电气设备图块"))
  (@:prompt "请选择要插入的设备类型:")
  (setq equip-list
        '("开关" "插座" "灯具" "配电箱" "断路器" "电表"))
  (setq choice (ui:select "设备类型" equip-list))
  (if choice
      (progn
        (setq downfile (strcat "at-elec/" choice ".dwg"))
        (if (null (findfile (strcat "packages/" downfile)))
            (progn
              (@:load-module 'pkgman)
              (@:down-pkg-file (@:uri) downfile "stable")
              (@:alert (strcat "正在下载所需的dwg文件, 请稍候。"))
              (sleep 3)))
        (if (findfile (strcat "packages/" downfile))
            (progn
              (ui:dyndraw
               (block:insert choice
                             (@::package-path "at-elec")
                             '(0 0 0) 0 1)
               '(0 0 0)))
          (@:alert (strcat "未找到设备文件: " choice ".dwg"))))
    (@:prompt "未选择设备。"))
  (princ))

;;; ============================================================
;;; 说明/样例插入
;;; ============================================================
(defun @elec:draw-readme (/ readme-elec)
  (@:help '("插入电气设计说明。"))
  (if (findfile (strcat @::*prefix* "packages/at-elec/readme-elec.dwg"))
      (progn
        (setq readme-elec
              (block:insert
               "readme-elec"
               (strcat @::*prefix* "packages/at-elec/")
               (getpoint "请点击插入位置:")
               0 1))
        (if (string-equal "insert" (entity:getdxf readme-elec 0))
            (progn
              (vla-explode (e2o readme-elec))
              (vla-delete (e2o readme-elec)))))
    (@:prompt "未找到说明文件 readme-elec.dwg")))

(defun @elec:draw-plan-example (/ example-plan-elec)
  (@:help '("插入电气平面图样例。"))
  (if (findfile (strcat @::*prefix* "packages/at-elec/example-elec.dwg"))
      (progn
        (setq example-plan-elec
              (block:insert
               "example-plan-elec"
               (strcat @::*prefix* "packages/at-elec/")
               (getpoint "请点击插入位置:")
               0 1)))
    (@:prompt "未找到样例文件 example-elec.dwg")))
