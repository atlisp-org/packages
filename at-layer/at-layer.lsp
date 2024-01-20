;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vitaltools -- 唯他工具集
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 图层工具集

(@:add-menus '("图层"
	       ("关闭其它" "(@:layer-off-other)")
	       ("冻结其它" "(@:layer-frozen-other)")
	       ("锁定其它" "(@:layer-lock-other)")
	       ("解锁全部" "(@:layer-unlock)")
	       ("解冻全部" "(@:layer-thaw)")
	       ("图层全开" "layon")
	       ("图层恢复" "layerp")
	       ("特性随层" "laycur")
	       ("合并图层" "laymgr")
	       ("删除图层" "laydel")
	       ("图层漫游" "laywalk")
	       ("选图进层" "(@layer:ent-to-clayer)")

	       ))
;; (@:add-menu "图层" "常用命令" "(@layer:ent-to-clayer)")

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

(defun @layer:ent-to-clayer ()
  (if (null layer:list)(require 'layer:*))
  (if curr-layer
      (cond
       ((= 'int (type curr-layer))
	(setvar "clayer" (itoa curr-layer)))
       ((= 'str (type curr-layer))
	(if (null (member curr-layer (layer:list)))
	    (layer:make curr-layer nil nil nil))
	(setvar "clayer" curr-layer))
       ((atom curr-layer)
	(if (null (member (vl-symbol-name curr-layer) (layer:list)))
	    (layer:make (vl-symbol-name curr-layer) nil nil nil))
	(setvar "clayer" (vl-symbol-name curr-layer)))))
  (@:help (list
	   (strcat "选择对象到" (getvar "clayer") "层")))
  (if (setq ss-curr (cadr (ssgetfirst)))
      (foreach ent (pickset:to-list ss-curr)
	       (entity:putdxf ent 8 (getvar "clayer"))
	       (entity:deldxf ent 6 )
	       (entity:deldxf ent 48)
	       (entity:deldxf ent 62)
	       )
    (while (setq ent (car (entsel)))
      (entity:putdxf ent 8 (getvar "clayer"))
      (entity:deldxf ent 6 )
      (entity:deldxf ent 48)
      (entity:deldxf ent 62)
      ))
  (setq curr-layer nil)
  )
(defun c:ent2clayer ()
  (@layer:ent-to-clayer))

;; Local variables:
;; coding: gb2312
;; End: 
