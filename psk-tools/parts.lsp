;; KEY分为ATTACH 在管线上(1个连接点)
;;        INLINE 在管线上(2个连接点)
;;        END 在管端(1个连接点)
;; INLINE类型必须提供L值，特指管件在管道方向上占用的长度
(("DUCT-STOR"
   ("TYPE" . "INLINE")
   ("KEY" . "DUCT-STOR-{W}x{H}/{D}-{L}")
   ("DESC" . "方圆变径")
   ;; 创建KEY对象所用的参数
   ("PARAM"
     ("W" 1070 "方风管宽度")
     ("H" 1070 "方风管高度")
     ("D" 1070 "圆风管直径")
     ("L" 1040 "变径长度")
   )
   ;; 绘图所用的参数
   ("DRAWER" (psk-draw-stor "W" "D" "L" $PSK-DUCT-FLEXTEND))
   ("BOM")
 )
  ("AXIAL-FAN"
    ("TYPE" . "INLINE")
    ("KEY" . "AXIAL-FAN-{D}-{L}")
    ("DESC" . "轴流风机")
    ("PARAM"
      ("D" 1040 "风机直径")
      ("L" 1040 "风机长度")
    )
    ("DRAWER" (psk-draw-fan "L" "D"))
  )
  ("FDC"
    ("TYPE" . "INLINE")
    ("KEY" . "FDC-70-{W}x{H}-{L}")
    ("DESC" . "常闭防火阀")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
      ("L" 1040 "阀体长度")
    )
    ("DRAWER" (psk-draw-firedamper-c "L" "W" $PSK-DUCT-FLEXTEND))
  )
  ("FDO"
    ("TYPE" . "INLINE")
    ("KEY" . "FDO-70-{W}x{H}-{L}")
    ("DESC" . "常开防火阀")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
      ("L" 1040 "阀体长度")
    )
    ("DRAWER" (psk-draw-firedamper-o "L" "W" $PSK-DUCT-FLEXTEND))
  )
;;;  ("CAP"
;;;    ("TYPE" . "END")
;;;    ("KEY" . "")
;;;    ("DESC" . "盲板")
;;;    ("PARAM"
;;;      ("A" 1070 "厚")
;;;      ("B" 1070 "直径")
;;;    )
;;;    ("DRAWER" psk-draw-rectangle ("A" "B"))
;;;  )
  ("DUCT-END"
    ("TYPE" . "END")
    ("KEY" . "")
    ("DESC" . "风管封头")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
    )
    ("DRAWER" (psk-draw-ductend "W" $PSK-DUCT-FLEXTEND))
  )
  ("CV"
    ("TYPE" . "INLINE")
    ("KEY" . "CV")
    ("DESC" . "止回阀")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
      ("L" 1070 "阀体长度")
    )
    ("DRAWER" (psk-draw-checkvalve "L" "W" $PSK-DUCT-FLEXTEND))
  )
  ("DAMPERVALVE"
    ("TYPE" . "INLINE")
    ("KEY" . "DAMPERVALVE")
    ("DESC" . "调节阀")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
      ("L" 1070 "阀体长度")
    )
    ("DRAWER" (psk-draw-dvalve "L" "W" $PSK-DUCT-FLEXTEND))
  )
  ("DDAMPERVALVE"
    ("TYPE" . "INLINE")
    ("KEY" . "DAMPERVALVE")
    ("DESC" . "多叶调节阀")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
      ("L" 1070 "阀体长度")
    )
    ("DRAWER" (psk-draw-ddvalve "L" "W" $PSK-DUCT-FLEXTEND))
  )
  ("ND"
    ("TYPE" . "INLINE")
    ("KEY" . "ND")
    ("DESC" . "消音器")
    ("PARAM"
      ("W" 1070 "方风管宽度")
      ("H" 1070 "方风管高度")
      ("L" 1070 "消音器长度")
    )
    ("DRAWER" (psk-draw-noisereducer "L" "W" $PSK-DUCT-FLEXTEND))
  )
  ("SS"
    ("TYPE" . "ATTACH")
    ("KEY" . "PSK-SS{A}X{B}")
    ("DESC" . "单层百叶风口")
    ("PARAM"
      ("A" 1070 "风口长度")
      ("B" 1070 "风口宽度")
    )
    ("DRAWER" (psk-draw-singleshutter "A" "B"))
  )
  ("DS"
    ("TYPE" . "ATTACH")
    ("KEY" . "PSK-DS{A}X{B}")
    ("DESC" . "双层百叶风口")
    ("PARAM"
      ("A" 1070 "风口长度")
      ("B" 1070 "风口宽度")
    )
    ("DRAWER" (psk-draw-doubleshutter "A" "B"))
  )
;;;  ("FL-PL"
;;;    ("TYPE" . "INLINE")
;;;    ("KEY" . "FL-PL-{PN}-{DN}")
;;;    ("DESC" . "板式平焊法兰")
;;;    ("SIZEFILE" . "Flange PN.csv")
;;;    ("PARAM"
;;;      ("PN" 1000
;;;	    "法兰压力等级"
;;;	    ""
;;;	    ("0.25" "1.0" "1.6" "2.5" "4.0")
;;;      )
;;;      ("DN" 1070 "法兰公称直径" "" ())
;;;;;;		    ("OD" "法兰外径")
;;;;;;		    ("L" "法兰厚度")
;;;    )
;;;    ("DRAWER" psk-draw-rectangle ("L" "OD"))
;;;  )
)