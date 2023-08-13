(@:define-config 'mark-order:matchs "*M*,*C*" "欲匹配的文字")
(@:define-config 'mark-order:notmatchs "*MM*,*CC*,*%C*" "不能匹配的文字")

(defun mark-order:check-str (str / matchs notmatchs) 
  (setq matchs (string:to-list (@:get-config 'mark-order:matchs) ","))
  (setq notmatchs (string:to-list (@:get-config 'mark-order:notmatchs) ","))
  (if (p:stringp str) 
    (and 
      (apply 'or (mapcar '(lambda (x) (wcmatch (strcase str) x)) matchs))
      (apply 
        'and
        (mapcar '(lambda (x) (wcmatch (strcase str) (strcat "~" x))) notmatchs)))))
(defun mark-order:check-text (ent dxf / str) 
  (mark-order:check-str (entity:getdxf ent dxf)))
(defun mark-order:findtext (/ entlst flag txts) 
  (setq txts (pickset:to-list (ssget '((0 . "*text,attrib,insert,tch_opening")))))
  (setq txts (vl-remove-if-not 
               '(lambda (txt) 
                  (cond 
                    ((= "TCH_OPENING" (entity:getdxf txt 0))
                     (mark-order:check-text txt 302)
                    )
                    ((/= "INSERT" (entity:getdxf txt 0))
                     (mark-order:check-text txt 1))
                    ((= "INSERT" (entity:getdxf txt 0))
                     (setq entlst (block:ent-list (entity:getdxf txt 2)))
                     (setq flag nil)
                     (while (and (setq ent (car entlst)) (null flag)) 
                       (if (wcmatch (entity:getdxf ent 0) "*TEXT") 
                         (setq flag (mark-order:check-text ent 1)))
                       (setq entlst (cdr entlst)))
                     ;; attributes
                     (or 
                       flag
                       (apply 
                         'or
                         (mapcar 
                           'mark-order:check-str
                           (mapcar 'cdr (block:get-attributes txt))))))))
               txts)))
(defun mark-order:main (ents / pi-mid ang h) 
  (setq i 0)
  (foreach ent ents 
    (cond 
      ((= "TCH_OPENING" (entity:getdxf ent 0))
       (setq pt-mid (entity:getdxf ent 11))
       (setq h (* (entity:getdxf ent 140) (entity:getdxf ent 147)))
       (setq ang (entity:getdxf ent 50))
       (entity:putdxf 
         (entity:make-text 
           (strcat (itoa (setq i (1+ i))) "#")
           (polar pt-mid (+ (* 0.5 pi) ang) h)
           (* 0.9 h)
           ang
           0.8
           0
           "MM")
         62
         1))

      ((/= "INSERT" (entity:getdxf ent 0))
       (setq pt-mid (apply 'point:mid (entity:getbox ent 0)))
       (setq h (entity:getdxf ent 40))
       (setq ang (entity:getdxf ent 50))
       (entity:putdxf 
         (entity:make-text 
           (strcat (itoa (setq i (1+ i))) "#")
           (polar pt-mid (+ (* 0.5 pi) ang) h)
           (* 0.9 h)
           ang
           0.8
           0
           "MM")
         62
         1))

      ((= "INSERT" (entity:getdxf ent 0))
       (setq pt-ins (entity:getdxf ent 10))
       (setq ang-ins (entity:getdxf ent 50))
       (setq scale-ins (entity:getdxf ent 41))
       (setq ent-attrib (entnext ent))
       (setq attribs nil)
       (while (= "ATTRIB" (entity:getdxf ent-attrib 0)) 
         (setq attribs (cons ent-attrib attribs))
         (setq ent-attrib (entnext ent-attrib)))
       (setq subents (block:ent-list (setq blkname (entity:getdxf ent 2))))
       (setq pt-base (entity:getdxf (tblobjname "block" blkname) 10))
       (setq subents (vl-remove-if-not 
                       '(lambda (x) 
                          (and 
                            (wcmatch (entity:getdxf x 0) "*TEXT")
                            (mark-order:check-text x 1)))
                       subents))
       (foreach subent subents 
         (setq pt-mid (apply 'point:mid (entity:getbox subent 0)))
         (setq pt-mid (block:bcs2wcs pt-mid pt-base pt-ins ang-ins scale-ins))
         (setq h (entity:getdxf subent 40))
         (setq ang (+ (entity:getdxf subent 50) ang-ins))
         (entity:putdxf 
           (entity:make-text 
             (strcat (itoa (setq i (1+ i))) "#")
             (polar pt-mid (+ (* 0.5 pi) ang) h)
             (* 0.9 h)
             ang
             0.8
             0
             "MM")
           62
           1))
       (setq attribs (vl-remove-if-not 
                       '(lambda (x) 
                          (and 
                            (mark-order:check-text x 1)))
                       attribs))
       (foreach subent attribs 
         (setq pt-mid (apply 'point:mid (entity:getbox subent 0)))
         ;; (setq pt-mid (block:bcs2wcs pt-mid pt-base pt-ins ang-ins scale-ins))
         (setq h (entity:getdxf subent 40))
         (setq ang (entity:getdxf subent 50))
         (entity:putdxf 
           (entity:make-text 
             (strcat (itoa (setq i (1+ i))) "#")
             (polar pt-mid (+ (* 0.5 pi) ang) h)
             (* 0.9 h)
             ang
             0.8
             0
             "MM")
           62
           1))))))
(@:add-menu 
  (_ "Demo")
  (_ "Number by string")
  '(mark-order:main (mark-order:findtext)))
