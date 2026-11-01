# at-text 功能扩展计划

## 2026-10 iMini 复现（文字增强）

并入自原 mini-text，共 18 个 AI 无交互入口、16 个交互命令，分 4 个文件：

| 文件 | 内容 |
|---|---|
| `text-tools.lsp` | 菜单 + 公共辅助（文字统一读取、样式设置、MTEXT 构造） |
| `text-edit.lsp` | sedit / cht / text-order / join-text / text2mtext / exp-text / tbox |
| `font-ext.lsp` | fix-fonts / use-myfonts |
| `att-ext.lsp` | att2text / text2att / match-att / att-list |
| `misc-ext.lsp` | text2clipboard / text-lib / find（多文件查找替换） |

### iMini 命令映射

| iMini 命令 | 本包函数 | 状态 |
|---|---|---|
| mini_sedit / mini_cht | `@text:ai-sedit` / `@text:ai-cht` | ✅ |
| mini_text_order / mini_exp_text / mini_join_text / mini_text2mtext | 同名 ai 入口 | ✅ |
| mini_tbox | `@text:ai-tbox` / `ai-tbox-refresh` / `ai-tbox-refresh-all` | ✅ |
| mini_text_2_clipboard | `@text:ai-text2clipboard` | ✅ |
| mini_text_lib | `@text:ai-text-lib` | ✅ |
| mini_find | `@text:ai-find` | ✅ |
| mini_fix_fonts / mini_use_myfonts | `@text:ai-fix-fonts` / `@text:ai-use-myfonts` | ✅ |
| mini_att2text / mini_text2att / mini_MatchAttribs / mini_attrib_list | `@text:ai-att2text` / `ai-text2att` / `ai-match-att` / `ai-att-list` | ✅ |

### 未实现（backlog）

| 优先级 | 项目 | 说明 |
|---|---|---|
| P1 | mini_text2pl（文字变线） | 需要 TrueType 字形轮廓提取，LISP 层只能近似，且 iMini 用外部 text2svg |
| P1 | mini_shell_text（外部编辑器书写） | 依赖外部编辑器 + 临时文件回读 |
| P2 | mini_exedit（动态菜单编辑） | 依赖 iMini 自有数字面板机制 |
| P2 | mini_style_any/hz/khz | 已由 fonts 包 `fonts:ai-style*` 覆盖 |

### 已知限制

- 文字外框（tbox）为静态矩形，需手动或调用 `ai-tbox-refresh` 更新。
- `ai-find` 只处理已打开的文档，不遍历磁盘 dwg。
- 词库按行读取，UTF-8 无 BOM。
