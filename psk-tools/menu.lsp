
(@:add-menus
 '(("psk水平布矩形管"
    ("布置(&A)" "(C:AR)")
    ("送风风管" "DUCT SA")
    ("回风风管" "DUCT RA")
    ("新风风管" "DUCT OA")
    ("排风风管" "DUCT EA")
    ("人防送风风管" "DUCT RS")
    ("人防排风风管" "(C:DUCT) RP")
    ("消防排烟风管" "(C:DUCT) SE")
    ("加压送风风管" "(C:DUCT) PS")
    ("消防补风风管" "(C:DUCT) MA")
    ("排风兼排烟风管" "(C:DUCT) EA(SE)")
    ("圆形风管" "(C:DUCTR)")
    ("液体管道" "(C:PIPE)"))
   ("psk空调"
    ("冷冻水供水管" "(C:PIPE) CS")
    ("冷冻水回水管" "(C:PIPE) CR")
    ("热水供水管" "(C:PIPE) HS")
    ("热水回水管" "(C:PIPE) HR")
    ("冷、热水供水管" "(C:PIPE) CHS")
    ("冷、热水回水管" "(C:PIPE) CHR")
    ("冷却水供水管" "(C:PIPE) CTS")
    ("冷却水回水管" "(C:PIPE) CTR")
    ("冷媒管" "(C:PIPE) REF")
    ("冷凝水管" "(C:PIPE) CD")
    ("组合水管水空调" "PSKPG1")
    ("组合水管多联机" "PSKPG2"))
   ("psk管道连接打断" ;;"LIB"
    ("风管连接" "(C:DC)")
    ("风管重绘" "(C:DR)")
    ("插入部件" "(C:IV)")
    ("插入部件包" "(C:IKP)")
    ("插入分歧管" "(C:VRFBRANCH)")
    ("--" "--")
    ("查询编辑" "(C:CX)"))

   ("psk编辑标注"
    ("移动管道" "(C:MOVEPATH)")
    ("移动伸缩管道" "(C:MOVEPATHRESIZE)")
    ("标注" "(C:BZ)")
    ("水管组合标注" "(C:ZHBZ)")
    ("--" "--")
    ("布置设备" "(C:EQ)")
    ("设备连接管道" "(C:CE)")
    ("--" "--")
    ("管道分支属性汇总" "(C:TOT)")
    ("设置" "(C:PSKCONFIG)")
    ("关于" "(C:PSKABOUT)"))
   ("psk水风管计算"
    ("分支计算" "(C:SLJS)")
    ("负荷算流量" "(C:PSKFHLL)")
    ("负荷算冷凝水管径" "(C:PSKCALCDDN)")
    ("负荷算冷媒管径" "(C:PSKCALREFPIPESIZE)")
    ("流量算阻力" "PSKDN N ")
    ("重置管径" "PSKDN Y ")
    ("风管宽度" "(C:PSKFGKD)")
    ("风管阻力" "(C:PSKFGZL)")
    ("遍历分支" "(C:SHOWBRANCHS)")
    ("显示最不利分支" "(C:LARGESTBRANCH)")
    ("导出属性报表" "(C:COMPEXPORT)")
    ("材料清单" "(C:BOM)")
    )))
