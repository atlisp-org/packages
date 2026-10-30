# @lisp 字体库

CAD 常用字体集合。包含天正、PKPM、探索者等字体。自动配置 ACAD 环境变量，修复缺失字体问题。

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `fonts:merge` | 无 | 归并字体样式，将中文字体替换为系统 TTF 字体 |
| `fonts:merge1` | 无 | 归并字体样式（旧版实现） |
| `fonts:nulltoFonts` | `shxx shxb ttf` | 替换空字体为指定字体文件 |
| `fonts:check` | 无 | 检查字体文件是否存在，返回缺失列表 |
| `fonts:check_fontfile` | `sty ttf wid` | 检查并修复单个字体样式 |
| `fonts:fix-fonts` | `tips` | 自动修复缺失字体 |
| `fonts:use_myfonts` | 无 | 批量替换字体为用户默认字体 |
| `fonts:style-any` | 无 | 字形快速设置（创建/修改文字样式） |
| `fonts:style-hz` | 无 | 批量设置 HZ 系列文字样式 |
| `fonts:style-khz` | 无 | 批量设置 KHZ 系列文字样式 |
| `fonts:font-list` | 无 | 字体库清单（SHX 文件 + 当前图样式） |

## 包含字体文件

| 字体文件 | 说明 |
|----------|------|
| `gbhzfs.shx` | 国标汉字仿宋字体 |
| `China.shx` | 中文字体 |
| `Gbcbig.shx` | 国标大字体 |
| `hztxt.shx` | 汉字字体 |
| `Pkpmeng.shx` | PKPM 英文字体 |
| `romans.shx` | 罗马字体 |
| `Tssdchn.shx` | 天正汉字字体 |
| `Tssdeng.shx` | 天正等线字体 |
| `Tssdeng2.shx` | 天正等线字体 2 |
| `yjkeng.shx` | 盈建科英文字体 |
| `yjkchn.shx` | 盈建科中文字体 |
| `txt.shx` | 标准文本字体 |

## 依赖

- 无（独立字体包）

## 安装

```lisp
(@:package-install "fonts")
```

## AI 调用

以下入口**无交互**（不弹窗、不取点），可由 MCP `eval_lisp` 直接调用；
成功返回结果，失败返回 `nil` 并用 `princ` 说明原因。

| 函数 | 参数 | 说明 / 示例 |
|---|---|---|
| `fonts:ai-style` | `name shx ttf height width oblique` | 创建/修改文字样式。`shx` 可 `"主.shx,大.shx"` 组合，`ttf` 优先。`(fonts:ai-style "HZ" "tssdeng.shx,hztxt.shx" nil 0 0.8 0)` |
| `fonts:ai-style-hz` | 无 | 批量设置 HZ 系列样式（hztxt/hzdbt/hztr）。`(fonts:ai-style-hz)` |
| `fonts:ai-style-khz` | 无 | 批量设置 KHZ 系列样式（kht/kdb/khtz）。`(fonts:ai-style-khz)` |
| `fonts:ai-font-list` | `out-file` | 列出 SHX 字体与当前图样式；无 `out-file` 返回 `(shx列表 样式表)`，有则写文件并返回路径。`(fonts:ai-font-list nil)` |

## 源文件

| 文件 | 说明 |
|------|------|
| `fonts.lsp` | 主程序源码 |
| `fix-font.lsp` | 字体修复工具 |
| `style.lsp` | 文字样式管理（style-any/hz/khz） |
| `font-list.lsp` | 字体库清单 |
| `*.shx` | 字体文件 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 1.0.30
- **分类**: 文本
- **网站**: http://atlisp.cn
