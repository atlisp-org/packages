;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 电气设计工具包
;; 功能：回路编号、电缆标注、配电箱统计、负荷计算、设备插入等
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:define-config '@elec:circut-prefix "WL" "回路编号前缀")
(@:define-config '@elec:circut-start 1 "回路起始编号")
(@:define-config '@elec:cable-layer "ELEC-CABLE" "电缆标注图层")
(@:define-config '@elec:panel-blkname "配电箱" "配电箱图块名")
(@:define-config '@elec:demand-factor 0.7 "负荷计算需要系数 Kd")

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
(defun @elec:circut-numbering (/ prefix start ss)
  "对选中的回路线进行编号"
  (@:help '("选择回路线，按顺序为每个回路编号。"
            "编号格式: 前缀-序号, 如 WL-01, WL-02"))
  (setq prefix (@:get-config '@elec:circut-prefix))
  (setq start (@:get-config '@elec:circut-start))
  (@:prompt "请选择需要编号的回路线:")
  (setq ss (ssget '((0 . "*line"))))
  (if ss
      (princ (strcat "\n已编号 "
                     (itoa (@elec:ai-circut-numbering ss prefix start))
                     " 个回路。"))
    (@:prompt "未选中任何线。"))
  (princ))

;;; ============================================================
;;; 电缆标注
;;; ============================================================
(defun @elec:cable-label (/ ent cable-type)
  "标注电缆型号和规格"
  (@:help '("选择一条线，标注其电缆型号。"
            "如: YJV-3x2.5+1x1.5, NH-BV-4x4"))
  (setq ent (car (entsel "请选择要标注的电缆线:")))
  (if (and ent
           (setq cable-type (getstring "\n请输入电缆型号 (如 YJV-3x2.5+1x1.5): ")))
      (if (@elec:ai-cable-label ent cable-type)
          (@:prompt (strcat "已标注: " cable-type))
        (@:prompt "标注失败，请检查图元或文字样式。"))
    (@:prompt "未选中有效图元或未输入型号。"))
  (princ))

;;; ============================================================
;;; 负荷统计
;;; ============================================================
(defun @elec:load-stat (/ ss res)
  "统计选中设备块的总负荷功率"
  (@:help '("选择设备块，统计其总安装功率。"
            "块属性中需包含“功率”属性。"))
  (@:prompt "请选择需要统计的设备块:")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
      (progn
        (setq res (@elec:stat-load (pickset:to-list ss)))
        (princ (strcat "\n共 " (itoa (cdr (assoc "count" res))) " 个设备"))
        (princ (strcat "\n总安装功率: "
                       (rtos (cdr (assoc "total" res)) 2 1) " W"))
        (princ (strcat "\n计算负荷: "
                       (rtos (cdr (assoc "demand" res)) 2 1) " W (Kd="
                       (rtos (cdr (assoc "factor" res)) 2 2) ")")))
    (@:prompt "未选中任何块。"))
  (princ))

;;; ============================================================
;;; 设备统计
;;; ============================================================
(defun @elec:equip-stat (/ ss stats i)
  "统计所选设备块的型号和数量"
  (@:help '("选择设备块，按型号分类统计数量。"))
  (@:prompt "请选择需要统计的设备块:")
  (setq ss (ssget '((0 . "INSERT"))))
  (if ss
      (progn
        (setq stats (@elec:stat-equip (pickset:to-list ss)))
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
  (if (findfile (strcat @:*prefix* "packages/at-elec/readme-elec.dwg"))
      (progn
        (setq readme-elec
              (block:insert
               "readme-elec"
               (strcat @:*prefix* "packages/at-elec/")
               (getpoint "请点击插入位置:")
               0 1))
        (if (string-equal "insert" (entity:getdxf readme-elec 0))
            (progn
              (vla-explode (e2o readme-elec))
              (vla-delete (e2o readme-elec)))))
    (@:prompt "未找到说明文件 readme-elec.dwg")))

(defun @elec:draw-plan-example (/ example-plan-elec)
  (@:help '("插入电气平面图样例。"))
  (if (findfile (strcat @:*prefix* "packages/at-elec/example-elec.dwg"))
      (progn
        (setq example-plan-elec
              (block:insert
               "example-plan-elec"
               (strcat @:*prefix* "packages/at-elec/")
               (getpoint "请点击插入位置:")
               0 1)))
    (@:prompt "未找到样例文件 example-elec.dwg")))
