(@:define-config '@structure:layer-beam "*BEAM*,*梁*" "梁图层")
;; (@:add-menu "结构" "*梁编组" "(at-structure:find-beam)")
(defun at-structure:find-beam (/ ss-beam lst-beam en-line1 en-line2) 
  "找出梁边(成对的平行线)"
  (setq ss-beam (pickset:to-list 
                  (ssget 
                    (list 
                      '(0 . "line")
                      (cons 8 (@:get-config 'at-structure:layer-beam))
                    )
                  )
                )
  )
  ;; (setq ss-beam
  ;; 	(vl-sort ss-beam
  ;; 		 '(lambda (x y) ;;按角度排序
  ;; 		   (< (angle (entity:getdxf x 10)(entity:getdxf x 11))
  ;; 		    (angle (entity:getdxf y 10)(entity:getdxf y 11))))))
  (setq lst-beam nil)
  (while (> (length ss-beam) 1) 
    (setq en-line1 (car ss-beam))
    (setq ss-beam (vl-sort 
                    (cdr ss-beam)
                    '(lambda (x y) 
                       (and 
                         ((geometry:angle en-line1) 
                           (< 
                             (geometry:dist-pt-line 
                               (geometry:segment-mid 
                                 (geometry:segment-by-line en-line1)
                               )
                               (geometry:segment-by-line x)
                             )
                             (geometry:dist-pt-line 
                               (geometry:segment-mid 
                                 (geometry:segment-by-line en-line1)
                               )
                               (geometry:segment-by-line y)
                             )
                           )
                         )
                       )
                     )
                  )
    )
    (setq lst-beam (append lst-beam (list (list en-line1 (car ss-beam)))))
    (entity:make-line 
      (geometry:segment-mid (geometry:segment-by-line en-line1))
      (geometry:segment-mid (geometry:segment-by-line (car ss-beam)))
    )
    (setq ss-beam (cdr ss-beam))
  )
  lst-beam
)

	     
(@:add-menu "PKPM" "次梁结果" '(at-structure:secondarybeam-result))
(defun at-structure:secondarybeam-result () 
  "找出梁边(成对的平行线)"
  (setq beamlines (pickset:to-list 
                    (ssget 
                      (list 
                        '(0 . "line")
                        (cons 8 "15104")
                      )
                    )
                  )
  )
  (foreach beamline beamlines 
    ;; 向上向左500，向下200取文字
    (setq pt-mid (curve:midpoint beamline))
    (setq ang (apply 'angle (entity:getdxf beamline '(10 11))))
    (setq uptext (ssget 
                   "c"
                   pt-mid
                   (polar 
                     (polar pt-mid ang 150)
                     (+ (* 0.5 pi) ang)
                     500
                   )
                   '((0 . "MTEXT"))
                 )
    )
    (if uptext 
      (progn 
        (foreach ent (pickset:to-list uptext) 
          (entity:putdxf ent 8 "次梁数据")
          (entity:putdxf 
            ent
            1
            (string:subst-all "\\C6" "\\C4" (entity:getdxf ent 1))
          )
          (entity:putdxf ent 62 6)
        )
      )
    )
    (setq downtext (ssget 
                     "c"
                     pt-mid
                     (polar 
                       (polar pt-mid ang 150)
                       (+ (* 1.5 pi) ang)
                       200
                     )
                     '((0 . "MTEXT"))
                   )
    )
    (if downtext 
      (progn 
        (foreach ent (pickset:to-list downtext) 
          (entity:putdxf ent 8 "次梁数据")
          (entity:putdxf 
            ent
            1
            (string:subst-all "\\C6" "\\C4" (entity:getdxf ent 1))
          )
          (entity:putdxf ent 62 6)
        )
      )
    )
  )
)

	     
  
  
  
