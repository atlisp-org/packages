(@:add-menu "文本" "设置" '(@text:setup))
(@:add-menus
 '(("文本"
    ("文字排版" (@text:multi-text-align))
    ("插入时间戳" (@text:insert-time))
    ("左对齐" (@text:justifytext-left))
    ("右对齐" (@text:justifytext-right))
    ("居中对齐" (@text:justifytext-middle))
    ("属性变文本" (@text:a2t))
    ("单行转多" (@text:to-mtext))
    ("加前后缀" (@text:menu-add-prefix-or-suffix))
    ("批量加序号" (@text:menu-add-order))
    ("点哪加哪" (@text:inc-word))
    ("绘线查找" (@text:find-from-line))
    ("生成表格" (@text:text2table))
    ("格式数字" (@text:menu-format-number))
    ("文本转表格" (@text:string-to-table))
    ("朗读文本" (@text:menu-speak))
    )))
(@:define-hotkey "tts" "(@text:menu-speak)")
