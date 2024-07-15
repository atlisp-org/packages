(@:add-menus 
  '("@lisp选择"
    ("选择设置" "(@select:setup)")
    ("填充选块" "(at-select:select-blk-by-hatch)")
    ("线域选块" "(at-select:select-blk-by-lwpl)")
    ("线域选择" "(at-select:select-by-lwpl)")
    ("填充选择" "(at-select:select-by-hatch)")
    ("绘线指引" "(@select:line-to-ss)")
    ("ss1" "(c:ss1)")
    ("ss2" "(c:ss2)")
    ("ss3" "(c:ss3)")
    ("ss4" "(c:ss4)")
    ("ss5" "(c:ss5)")
    ("分堆加框" "(at-select:cluster-box)")
    ))
(@:add-menus 
  '("@lisp选择1"
    ("选择闭合线" (at-select:select-closed-lwpl))
    ("选择未闭合线" (at-select:select-unclosed-lwpl))
    ("选择短线" (at-select:select-shortlines))
    ("选择同类型" (at-select:select-sametype))
    
    ))
