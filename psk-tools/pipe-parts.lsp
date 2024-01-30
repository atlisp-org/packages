(("FL-PL"
   ("TYPE" . "INLINE")
   ("KEY" . "FL-PL-{PN}-{DN:0}")
   ("DESC" . "板式平焊法兰")
   ("SIZEFILE" . "sizes\\Flange PN.csv")
   ("PARAM"
     ("PN" 1000
           "法兰压力等级"
           ""
           ("2.5" "6" "10" "16" "25" "40")
     )
     ("DN" 1070 "法兰公称直径" "" ())
     ("OD" "法兰外径")
     ("L" "法兰厚度")
   )
   ("DRAWER" (psk-draw-rectangle "L" "OD"))
 )
  ("GV41T-16"
    ("TYPE" . "INLINE")
    ("KEY" . "GV41T-16-{DN:0}")
    ("DESC" . "法兰截止阀 J41T-16")
    ("SIZEFILE" . "sizes\\J41T-16.csv")
    ("DRAWER" (psk-draw-global-valve-top "L" "D" "D0"))
  )
  ("BV71X-16"
    ("TYPE" . "INLINE")
    ("KEY" . "BV71X-16-{DN:0}")
    ("DESC" . "法兰对夹蝶阀 D71X-16")
    ("SIZEFILE" . "sizes\\D71X-16.csv")
    ("DRAWER" (psk-draw-global-valve-top "L" "D" "L0"))
  )
)