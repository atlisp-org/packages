;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'at-purge:first 用于 应用包 at-purge 的 第一个配置项 first 
;; (@:get-config 'at-purge:first) ;; 获取配置顶的值
;; (@:set-config 'at-purge:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "实体"  "清理DGN" "(at-purge:remove-dgn)")
(defun at-purge:remove-dgn (/)
  (vl-load-com)
  (if (dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
      (princ "已修正 DGN线型 问题，请运行 purge 清理文件。")
      (princ "本图没有 DGN 问题"))
  ;;;(command "purge" "" "a" "" "N" "")
  (princ)
  )

(@:add-menu "实体" "分解重块" "(@:explode-minsert)")
(defun @:explode-minsert (/ en ent)
  "分解多重插入块 "
  (vlax-for blk *blks* (if(=""(vla-get-name blk)) (vla-put-name blk "ttt")))
  
  (setq en (entsel "n请选择多重插入块:"))
  (if en
      (if (= (cdr (assoc 0 (setq ent (cdr (entget (setq en (car en)))))))
	     "INSERT"
	     )
	  (progn
	    (setq ent (entmakex
		       (list '(0 . "INSERT")
			     
			     (assoc 2 ent)
			     (assoc 10 ent)
			     )
		       )
		  )
	    (command "_.explode" (entlast))
	    (entdel en)
	    )
	  )
    )
  (princ "\nOK，分解成功。")
  (princ)
  )

