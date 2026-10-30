# at-math 功能扩展计划

> @lisp 数学工具（v1.0.58）。单行文本数学运算、求和求积、批量四则运算、插序号、统计图元、行列运算、公式计算、全能文字计算等。

## 1. 概览
- 版本 1.0.58 / 分类 通用 / 依赖 base
- 菜单：`数学`（求和/求积/均值/筛选/批量运算/插编号/多列运算/行列运算/公式求值/结果写图/全能计算）+ `数学2`（范围选数/线域标数/框选标数/提物理量/代数运算）
- 命名空间：`@math:` / `@m:` / `at-math:` / `@:`

## 2. 现状功能清单（按文件）
| 文件 | 函数 | 功能 |
|---|---|---|
| cale-txt.lsp（新增） | @math:cale-tokenize / cale-parse-primary / cale-parse-mul / cale-parse-add / cale-eval | 内置四则表达式解析器（+ - * / 括号、小数、一元正负号，递归下降，不使用 eval） |
| | @math:cale-format / cale-make-text / cale-side-point / cale-txt-value | 结果格式化与写图、旁置点计算、文字取值 |
| | at-math:cale-txt | 交互：选中数字/算式文字，求和/求积/逐项求值 |
| | at-math:ai-cale | AI 无交互入口，返回 (结果值 文字ename) / ((值 . ename) ...) |

## 3. 已知问题与扩展点

### P0
- （暂无）

### P1
- **表达式解析器能力扩展**：当前仅支持 + - * / 括号与小数；可扩展 `^`（幂）、`%`（取模）、科学计数法（如 `1.2e3`），参照 `formula.lsp` 的 `formula->lisp` 转换思路。
- **结果精度控制**：`@math:cale-format` 硬编码 6 位小数；可加配置项 `@math:cale-precision`，与 `@::num-precision` 联动。
- **逐项求值写图方向**：eval 模式下结果固定写在原文字右侧；可加偏移方向/对齐配置。

### P2
- **与 @m:cal 合并**：`@math:cale-eval` 与 `@m:cal`（走 geomcal.arx）能力重叠；可考虑统一入口，geomcal 不可用时回退到内置解析器。
- **选择集释放**：`at-math:cale-txt` 用 `ssget` 后未 `ssset` 释放选择集，与既有命令（如 @m:text-sum）一致，非阻塞问题。

## 4. 归属与复用
- 表达式解析器为原子能力，若后续被 ≥2 包复用，可提取到 `lib/src/<ns>/<fn>.lsp`（当前仅 at-math 使用，暂留包内）。
