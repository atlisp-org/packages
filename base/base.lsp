(set '@::get-eval-code @::get-exec-permit)
(set '@::run-from-web @::load-remote)

(setq *linefile* (cond 
                   (is-bricscad
                    "default.lin")
                   (is-gcad
                    "gcad.lin")
                   (is-zwcad
                    "zwcad.lin")
                   (t
                    "acad.lin")))
(setq *isolinefile* (cond 
                      (is-bricscad
                       "iso.lin")
                      (is-gcad
                       "gcadiso.lin")
                      (is-zwcad
                       "zwcadiso.lin")
                      (t
                       "acadiso.lin")))
(setq *patfile* (cond 
                  (is-bricscad
                   "default.pat")
                  (is-gcad
                   "gcad.pat")
                  (is-zwcad
                   "zwcad.pat")
                  (t
                   "acad.pat")))
(setq *isopatfile* (cond 
                     (is-bricscad
                      "iso.pat")
                     (is-gcad
                      "gcadiso.pat")
                     (is-zwcad
                      "zwcadiso.pat")
                     (t
                      "acadiso.pat")))
