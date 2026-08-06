;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
<<<<<<< HEAD
;; ËøôÊòØ‰ΩøÁî®ÂºÄÂèëÂ∑•ÂÖ∑ dev-tools Ëá™Âä®ÂàõÂª∫ÁöÑÁ®ãÂ∫èÊ∫êÊñá‰ª∂ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ÂÆö‰πâÈÖçÁΩÆÈ°π 'at-purge:first Áî®‰∫é Â∫îÁî®ÂåÖ at-purge ÁöÑ Á¨¨‰∏Ä‰∏™ÈÖçÁΩÆÈ°π first 
;; (@:get-config 'at-purge:first) ;; Ëé∑ÂèñÈÖçÁΩÆÈ°∂ÁöÑÂÄº
;; (@:set-config 'at-purge:first  "Êñ∞ËÆæÁöÑÂÄº") ;; ËÆæÁΩÆÈÖçÁΩÆÈ°∂ÁöÑÂÄº
;; ÂêëÁ≥ªÁªü‰∏≠Ê∑ªÂä†ËèúÂçï 
(@:add-menu "ÂÆû‰Ωì"  "Ê∏ÖÁêÜDGN" "(at-purge:remove-dgn)")
(defun at-purge:remove-dgn (/)
  (vl-load-com)
  (if (dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
      (progn
	(command "purge" "a" "*" "N")
	(princ "Â∑≤‰øÆÊ≠£ DGNÁ∫øÂûã ÈóÆÈ¢òÔºåÂπ∂ËøêË°å purge Ê∏ÖÁêÜ‰∫ÜÊñá‰ª∂„ÄÇ")
	)
      (princ "Êú¨ÂõæÊ≤°Êúâ DGN ÈóÆÈ¢ò"))
  ;;;
  (princ)
  )

(@:add-menu "ÂÆû‰Ωì" "ÂàÜËß£ÈáçÂùó" "(@:explode-minsert)")
(defun @:explode-minsert (/ en ent)
  "ÂàÜËß£Â§öÈáçÊèíÂÖ•Âùó "
  (vlax-for blk *blks* (if(=""(vla-get-name blk)) (vla-put-name blk "ttt")))
  
  (setq en (entsel "nËØ∑ÈÄâÊã©Â§öÈáçÊèíÂÖ•Âùó:"))
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
  (princ "\nOKÔºåÂàÜËß£ÊàêÂäü„ÄÇ")
  (princ)
  )
=======
;; ’‚ « π”√ø™∑¢π§æﬂ dev-tools ◊‘∂Ø¥¥Ω®µƒ≥Ã–Ú‘¥Œƒº˛
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ∂®“Â≈‰÷√œÓ 'at-purge:first ”√”⁄ ”¶”√∞¸ at-purge µƒ µ⁄“ª∏ˆ≈‰÷√œÓ first
;; (@:get-config 'at-purge:first) ;; ªÒ»°≈‰÷√∂•µƒ÷µ
;; (@:set-config 'at-purge:first  "–¬…Ëµƒ÷µ") ;; …Ë÷√≈‰÷√∂•µƒ÷µ
;; œÚœµÕ≥÷–ÃÌº”≤Àµ•
(@:add-menu " µÃÂ" "«Â¿ÌDGN" "(at-purge:remove-dgn)")
(defun at-purge:remove-dgn ()
  (vl-load-com)
  (if (dictremove (namedobjdict) "ACAD_DGNLINESTYLECOMP")
    (progn
      (command "purge" "a" "*" "N")
      (princ "“—–ﬁ’˝ DGNœﬂ–Õ Œ Ã‚£¨≤¢‘À–– purge «Â¿Ì¡ÀŒƒº˛°£"))
    (princ "±æÕº√ª”– DGN Œ Ã‚"))
  ;;;
  (princ))
(@:add-menu " µÃÂ" "∑÷Ω‚÷ÿøÈ" "(@:explode-minsert)")
(defun @:explode-minsert (/ en ent)
  "∑÷Ω‚∂‡÷ÿ≤Â»ÎøÈ "
  (vlax-for blk *blks* (if (= "" (vla-get-name blk)) (vla-put-name blk "ttt")))

  (setq en (entsel "n«Î—°‘Ò∂‡÷ÿ≤Â»ÎøÈ:"))
  (if en
    (if
      (=
        (cdr (assoc 0 (setq ent (cdr (entget (setq en (car en)))))))
        "INSERT")
      (progn
        (setq ent (entmakex
                    (list
                      '(0 . "INSERT")
                      (assoc 2 ent)
                      (assoc 10 ent))))
        (command "_.explode" (entlast))
        (entdel en))))
  (princ "\nOK£¨∑÷Ω‚≥…π¶°£")
  (princ))
>>>>>>> 584d6d7 (ÂØπÈΩê)
