(@:define-config '@text:order-prefix "" "文字前缀内容")
(@:define-config '@text:order-suffix "" "文字后缀内容")
(@:define-config '@text:order-startnum  "1" "文字起始序号")

(defun @text:menu-add-order (/ *error* @dclfiled @dclfiledame @dclid ent errorsave k 
                             loop mod1 mod2 mod3 mod4 num num_ obj return# ss str1 str 
                             textaddstring1_1 textaddstring2_1 textaddstring3_1
                            ) 
  (vl-load-com)
  (setq errorsave *error*)
  (defun *error* (msg) 
    (unload_dialog @dclid)
    (vl-file-delete @dclfiledame)
    (setq *error* errorsave)
  )
  (defun get_dzmjjsbdc_main () 
    (setq textaddstring1_1 (get_tile "textaddstring1")
          textaddstring2_1 (get_tile "textaddstring2")
          textaddstring3_1 (get_tile "textaddstring3")
          mod1             (get_tile "x1")
          mod2             (get_tile "x2")
          mod3             (get_tile "x3")
          mod4             (get_tile "x4")
    )
  )
  (setq @dclfiledame (vl-filename-mktemp nil nil ".dcl")
        @dclfiled    (open @dclfiledame "w")
  )
  (write-line 
    "dzmjjsbdc:dialog { label = \"文本加前后缀\" ;
                            :row {:edit_box {key=\"textaddstring1\";label=\"前缀\";width=10;}
                                    :edit_box {key=\"textaddstring2\";label=\"后缀\";width=10;}
                                    :edit_box {key=\"textaddstring3\";label=\"起始序号\";width=10;value=1;}
                                   }
                              :spacer{hight=10;} :spacer{hight=10;}
                              :boxed_radio_column
                             {label=\"排序方式\";
                                :radio_button{label=\"序号 前缀 文字 后缀\";key=\"x1\";value=0;}
                                :radio_button{label=\"前缀 序号 文字 后缀\";key=\"x2\";value=0;}
                                :radio_button{label=\"前缀 文字 序号 后缀\";key=\"x3\";value=0;}
                                :radio_button{label=\"前缀 文字 后缀 序号\";key=\"x4\";value=1;}
                             }ok_cancel;}"

    @dclfiled
  )
  (close @dclfiled)
  (setq @dclid (load_dialog @dclfiledame))
  (new_dialog "dzmjjsbdc" @dclid)
  (action_tile "accept" "(get_dzmjjsbdc_main)(done_dialog 1)")
  (set_tile "textaddstring1" (@:get-config '@text:order-prefix))
  (set_tile "textaddstring2" (@:get-config '@text:order-suffix))
  (set_tile "textaddstring3" (@:get-config '@text:order-startnum))
  
  (setq return# (start_dialog))
  (if textaddstring1_1
    (progn
    (@:set-config '@text:order-prefix textaddstring1_1)
    (@:set-config '@text:order-suffix textaddstring2_1)
    (@:set-config '@text:order-startnum textaddstring3_1)))
  (list mod1 mod2 mod3 mod4)
  (cond 
    ((= return# 1)
     (if 
       (setq num (vl-remove nil 
                            (mapcar '(lambda (x) (if (< 47 x 59) x nil)) 
                                    (vl-string->list textaddstring3_1)
                            )
                 )
       )
       (setq num (atoi (apply 'strcat (mapcar 'chr num))))
     )
     (setq loop t)
     (while loop 
       (princ "\n选择文本<退出>")
       (cond 
         ((setq ss (ssget ":s" '((0 . "text"))))
          (repeat (setq k (sslength ss)) 
            (setq ent  (ssname ss (setq k (1- k)))
                  obj  (vlax-ename->vla-object ent)
                  str1 (vla-get-textstring obj)
                  num_ (if num (rtos num 2 0) "")
                  str  (cond 
                         ((= mod1 "1")
                          (list num_ textaddstring1_1 str1 textaddstring2_1)
                         )
                         ((= mod2 "1")
                          (list textaddstring1_1 num_ str1 textaddstring2_1)
                         )
                         ((= mod3 "1")
                          (list textaddstring1_1 str1 num_ textaddstring2_1)
                         )
                         ((= mod4 "1")
                          (list textaddstring1_1 str1 textaddstring2_1 num_)
                         )
                       )
            )
            (vla-put-textstring obj (apply 'strcat str))
            (if num 
              (setq num (1+ num))
            )
          )
         )
         (t (setq loop nil))
       )
     )
    )
  )
  (unload_dialog @dclid)
  (vl-file-delete @dclfiledame)
  (princ)
)