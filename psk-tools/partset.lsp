(("AXIAL-FAN-GROUP"
   ("TYPE" . "INLINE")
   ("DESC" . "管道风机组件（无软接头）")
   ("PACK"
     ("DUCT-STOR")
     ("AXIAL-FAN" ("L" . "L1"))
     ("DUCT-STOR" ("FLIP" . 1))
   )
   ("PARAM"
     ("W" 1070 "方风管宽度")
     ("H" 1070 "方风管高度")
     ("D" 1070 "风机直径")
     ("L" 1070 "变径长度")
     ("L1" 1070 "风机长度")
   )
 )
  ("FLANGE-VALVE-GROUP"
    ("TYPE" . "INLINE")
    ("DESC" . "法兰截止阀组件")
    ("PACK"
      ("FL-PL")
      ("VA-GLOBAL")
      ("FL-PL" ("FLIP" . 1))
    )
    ("PARAM"
      ("PN" 1000 "法兰压力等级")
      ("DN" 1070 "法兰公称直径")
    )
  )
)