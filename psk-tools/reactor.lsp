(vl-load-com)
(if (not $editor-reactor-loaded)
  (setq $editor-reactor-loaded
         (vlr-editor-reactor
           nil
           (list '(:vlr-commandwillstart . psk-commandwillstart)
                 '(:vlr-commandended . psk-commandended)
           )
         )
  )
)

(defun psk-commandwillstart (objreactor lstcommand /)
  (if (= "COPY" (car lstcommand))
    (setq $entlast (entlast))
  )
)

(defun psk-commandended (objreactor lstcommand / ents uid uids)
  (if (and (= "COPY" (car lstcommand))
           $entlast
      )
    (progn
      (setq ents (p-ss->enames (p-enames-after $entlast nil)))
      (foreach ent ents
        (if (and
              (or (setq uid (cdar (p-xdata-get ent "PSK-DRAFT")))
                  (setq uid (cdar (p-xdata-get ent "PSK-DRAWID")))
              )
              (not (member uid uids))
            )
          (setq uids (cons uid uids))
        )
      )

      (setq uids (mapcar (function (lambda (e) (cons e (p-uid)))) uids))

      (foreach ent ents
        (if (and
              (setq uid (cdar (p-xdata-get ent "PSK-DRAWID")))
              (setq uid (p-get uids uid))
            )
          (p-xdata-set ent "PSK-DRAWID" (list (cons 1071 uid)))
          (if (and
                (setq uid (cdar (p-xdata-get ent "PSK-DRAFT")))
                (setq uid (p-get uids uid))
              )
            (p-xdata-set ent "PSK-DRAFT" (list (cons 1071 uid)))
          )
        )
      )

      (setq $entlast nil)
    )
  )
)