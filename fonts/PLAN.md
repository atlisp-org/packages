# fonts 功能扩展计划

> @lisp 字体库：CAD 常用字体集合 + 文字样式管理。含天正、PKPM、探索者等字体，
> 自动配置 ACAD 环境变量，修复缺失字体。
> v1.0.30，文本类，独立字体包（无依赖）。

## 1. 概览

- 版本 1.0.30 / 分类 文本 / 无依赖
- 规模：4 个 .lsp（`fonts.lsp` 主程序 + `fix-font.lsp` 修复 + `style.lsp`/`font-list.lsp` 新增）
- 本轮 iMini 复现新增：`style_any`（字形快速设置）、`style_hz`/`style_khz`（HZ/KHZ 系列）、
  `font_list`（字体库清单）；缺失字库替换（`fix_fonts`/`use_myfonts`）已归 `at-text`，本包不重复

## 2. 已实现

| # | 命令 | 交互入口 | AI 入口 | 说明 |
|---|---|---|---|---|
| 1 | 字形快速设置 | `fonts:style-any` | `fonts:ai-style` | 创建/修改文字样式（SHX+TTF、字高、宽度系数、倾斜角） |
| 2 | HZ 字形设置 | `fonts:style-hz` | `fonts:ai-style-hz` | 批量设置 hztxt/hzdbt/hztr 样式（主字体+大字体） |
| 3 | KHZ 字形设置 | `fonts:style-khz` | `fonts:ai-style-khz` | 批量设置 kht/kdb/khtz 样式（主字体+大字体） |
| 4 | 字体库清单 | `fonts:font-list` | `fonts:ai-font-list` | 列出支持路径 SHX 文件 + 当前图文字样式 |

新增底层函数：`fonts:make-style`、`fonts:set-fonts`、`fonts:apply-series`、`fonts:series-hz`、
`fonts:series-khz`、`fonts:collect-shx`、`fonts:collect-styles`、`fonts:style-line`、`fonts:support-paths`。

## 3. 未实现（backlog）

- **P1 主字体可配置**：HZ/KHZ 系列主字体固定 `tssdeng.shx`；计划支持用户自定义主字体
  （如 `gbenor.shx`/`romans.shx`），并通过配置项或交互选择。
- **P1 缺失大字体回退**：`make-style` 直接设置 `BigFontFile`，不检查 `findfile`；当 hzdbt/hztr/
  kht/kdb/khtz 等字体未随包分发时，应回退到 `hztxt.shx` 或提示缺失。
- **P2 style_any 面板**：iMini `style_any` 为面板式（预览 + 多参数）；当前为命令行交互，
  计划 DCL/@lisp 面板。
- **P2 字体列表可视化**：`font-list` 目前仅命令行列表/写文件；计划输出表格并支持按样式/字体过滤。

## 4. 已知限制

- `fonts:make-style` 对 TTF 用 `vla-setfont`（charset 134/pitch 2，GB2312）；对 SHX 主/大字体
  改用 `fonts:set-fonts`（`entmod` 直写组码 4/3）。**原因**：实测 ZWCAD 24.1s LMS Tech 的
  VLA `FontFile`/`BigFontFile` 属性与 DXF 组码映射互换（`vla-put-fontfile` 把主字体写进了
  组码 3 大字体槽位，主/大字体写反）；`entmod` 直写组码符合 DXF 标准（4=主、3=大），
  AutoCAD 与各国产 CAD 均正确。设置 `FontFile` 前仍未校验字体文件是否存在于支持路径。
- `oblique` 参数统一按「度」输入，内部 `(* pi (/ oblique 180.0))` 转弧度。
- 本方言 `and`/`or` 只返回 `T`/`nil`；`(strcat nil ...)` 会报错，字体为空时已用空串兜底
  （`fonts:style-line`、`fonts:collect-styles` 的组码 3/4 可能为 nil）。
- 本包原无菜单组；本轮新增「@lisp字体库」菜单组（`style.lsp`/`font-list.lsp` 内联
  `@:add-menu`：字形快速设置/HZ字体系列/KHZ字体系列/字体库清单）。
