# at-number 功能扩展计划

> @lisp 编号工具（v0.0.6）。给图形（矩形、圆、图块等相同图形）自动编号，新增全能编号（点/文字/块插入点按顺序批量编号）。

## 1. 概览
- 版本 0.0.6 / 分类 Common / 依赖 base
- 菜单：`编号工具`（编号设置/图形编号/全能编号/删除编号）
- 命名空间：`at-number:`；配置前缀 `@number:`

## 2. 现状功能清单（按文件）
| 文件 | 函数 | 功能 |
|---|---|---|
| at-number.lsp | at-number:setup / number-entity / delete-number | 配置对话框、图形自动编号（质心）、删除编号 |
| xuhao.lsp（新增） | at-number:number-string / insertion-point / sort-by-x / sort-by-y / sort-by-snake / default-offset | 编号串格式化、参考点、三种排序、默认偏移 |
| | at-number:xuhao | 交互：选中点/文字/块，按 x/y/snake 顺序批量编号（前缀/后缀/起始号/步长/偏移） |
| | at-number:ai-number | AI 无交互入口，返回文字 ename 列表 |

## 3. 已知问题与扩展点

### P0
- （暂无）

### P1
- **蛇形排序容差**：`at-number:sort-by-snake` 用 `@number:fontsize` 作为同行容差，对稀疏/密集布置可能需人工调参；可加 `@number:snake-row-tol` 配置（参照 `@block:sort-by-snake` 的 rowtol/minrow 思路）。
- **编号格式扩展**：当前为「前缀 + 整数序号 + 后缀」；可扩展补齐位数（如 `01`、`001`，参照 `string:number-format`）与字母序号（A、B、…）。
- **偏移方向**：默认文字放在对象右侧（offset 距离）；可加方向配置（上/下/左/右/四角）。

### P2
- **与 at-block 排序复用**：`@block:sort-by-snake` 为 `@block:` 命名空间且带车位专用「零星块后置」逻辑，不适配通用编号，故独立实现；若通用化可上提 lib。
- **选择集释放**：`at-number:xuhao` 用 `ssget` 后未 `ssset` 释放，与既有 `number-entity` 一致。

## 4. 归属与复用
- 排序（x/y/snake）为通用原子能力，若被 ≥2 包复用可提取到 `lib/src/<ns>/<fn>.lsp`（当前仅 at-number 使用，暂留包内）。
