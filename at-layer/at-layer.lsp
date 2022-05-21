;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vitaltools -- 唯他工具集
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 图层工具集

(defun @:get-layer-by-object(ss / layer ti% ename e  )
  "根据所选对象生成图层表"
  (setq layer nil )
  (setq ti% 0)
  (if (/= ss nil)
      (progn 
        (while
            (<= ti% (- (sslength ss) 1))
          (setq ename (ssname ss ti%))
          (setq e (entget ename ))
          (if (=  (member (cdr (assoc 8 e)) layer) nil)
              (progn
                (if (= layer nil)
                    (setq layer (list (cdr (assoc 8 e))))
                  (setq layer (append layer (list (cdr (assoc 8 e)))))
                  )
                )
            )
          (setq ti%(+ 1 ti%))
          )))
  layer
  )
(@:add-menu "图层" "关闭其它" "(@:layer-off-other)")
(defun @:layer-off-other( /  ss  layer  lay-act-list )
  "关闭其它图层"
  (setq lay-act-list "")
  (setq ss (ssget ))
  (foreach layer (layer:list)
           ;;; 如果当前图层不在 所选对象中，设当前层为第一个当前对象层
           (if (= (member (getvar "clayer")  (@:get-layer-by-object ss)) nil)
               (setvar "clayer" (car  (@:get-layer-by-object ss)) )
             )
           (if (= (member layer (@:get-layer-by-object ss)) nil)
               (if (= lay-act-list "")
                   (setq lay-act-list layer)
                 (setq lay-act-list (strcat lay-act-list "," layer)
             )
           )))
  (command "-layer" "off" lay-act-list "")
  )
(@:add-menu "图层" "冻结其它" "(@:layer-frozen-other)")
(defun @:layer-frozen-other( /  ss  layer  lay-act-list )
  "冻结其它图层"
  (setq lay-act-list "")
  (setq ss (ssget ))
  (foreach layer (layer:list)
           ;;; 如果当前图层不在 所选对象中，设当前层为第一个当前对象层
           (if (= (member (getvar "clayer")  (@:get-layer-by-object ss)) nil)
               (setvar "clayer" (car  (@:get-layer-by-object ss)) )
             )
           (if (= (member layer (@:get-layer-by-object ss)) nil)
               (if (= lay-act-list "")
                   (setq lay-act-list layer)
                 (setq lay-act-list (strcat lay-act-list "," layer)
             )
           )))
  (command "-layer" "f" lay-act-list "")
  )

(@:add-menu "图层" "锁定其它" "(@:layer-lock-other)")
(defun @:layer-lock-other( /  ss  layer  lay-act-list )
  "锁定其它图层"
  (setq lay-act-list "")
  (setq ss (ssget ))
  (foreach layer (layer:list)
           ;;; 如果当前图层不在 所选对象中，设当前层为第一个当前对象层
           (if (= (member (getvar "clayer")  (@:get-layer-by-object ss)) nil)
               (setvar "clayer" (car  (@:get-layer-by-object ss)) )
             )
           (if (= (member layer (@:get-layer-by-object ss)) nil)
               (if (= lay-act-list "")
                   (setq lay-act-list layer)
                 (setq lay-act-list (strcat lay-act-list "," layer)
             )
           )))
  (command "-layer" "lo" lay-act-list "")
  )

(@:add-menu "图层" "解锁全部" "(@:layer-unlock)")
(defun @:layer-unlock( /  ss  layer  lay-act-list )
   "解锁全部图层"
  (setq lay-act-list "")
  (foreach layer (layer:list)
           (if (= lay-act-list "")
                   (setq lay-act-list layer)
                 (setq lay-act-list (strcat lay-act-list "," layer)
             )
           ))
  (command "-layer" "u" lay-act-list "")
  )

(@:add-menu "图层" "解冻全部" "(@:layer-thaw)")
(defun @:layer-thaw( /  layer  lay-act-list )
  "解冻全部图层"
  (setq lay-act-list "")
  (foreach layer (layer:list)
           (if (= lay-act-list "")
                   (setq lay-act-list layer)
                 (setq lay-act-list (strcat lay-act-list "," layer)
             )
           ))
  (command "-layer" "t" lay-act-list "")
  )
(@:add-menu "图层" "图层全开" "layon")

;; Local variables:
;; coding: gb2312
;; End: 
