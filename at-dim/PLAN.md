# at-dim 功能扩展计划

> @lisp标注。斜率标注、坐标标注（XY/AB/NE + 标高 + 多引线）、等分弧长标注、剖切符号（向视/剖视交互）、尺寸偏差、标注求和/面积、标注修复（Z 坐标/延伸线）、圆弧中心线。v0.0.27，自研（部分源自 Lee Mac / Paracadd），依赖 base + lib。

## 1. 概览
- 版本 0.0.27 / 分类 标注 / 依赖 base / 自研（centerline 基于 Lee Mac Centerline v1.2 MIT，dim-tools 源自 Paracadd）
- 规模：10 个 .lsp 共约 940 行 21 个函数；**`merge-dim.lsp` 未入 `:FILES`**（孤儿文件，4 个函数多 bug）
- 12 项菜单（"标注"组）；14 项 `@:define-config` 配置
- 复用 lib：`text:box`、`group:make`、`xdata:put`、`dcl:*`（base）、`string:number-format`、`m:fix-angle`、`curve:*`

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| @dim:setup | at-dim.lsp:33 | 标注配置对话框 |
| at-dim:set-env | at-dim.lsp:36 | 建"坡度标注"层/文字样式(complex 字体)/dimstyle |
| at-dim:menu-dim-slope | at-dim.lsp:52 | 选线标斜率入口（逐段 1:n 文本 + 短引线） |
| at-dim:dim-slope | at-dim.lsp:59 | 单线逐段斜率标注（角度归一 + 中点文本） |
| at-dim:set-scale | at-dim.lsp:94 | getint 设置标注比例 |
| at-dim:cutting-symbol | cutting-symbol.lsp:1 | 剖切符号交互式绘制（grread 循环：符号模式/详图号/向视/剖视，字母键改符号，DASHED 剖线 + A-B 详图框） |
| at-dim:area | area.lsp:1 | 闭合 lwpolyline 面积标注（m²，1e-6 固定换算） |
| at-dim:menu-zbbz | coord.lsp:1 | 坐标标注（XY/AB/NE 前缀、标高、自动/手动位置、多引线或文本+引线两种形式、编组） |
| at-dim:make-deviation | deviation.lsp:1 | 尺寸偏差：DIM 42 值 ± 输入值写覆盖文本 |
| at-dim:true-value | deviation.lsp:13 | 覆盖文本置空 → 显示尺寸原值 |
| at-dim:dimarc | dimarc.lsp:2 | vla-adddimarc 弧长标注 |
| at-dim:menu-dimarc-div | dimarc.lsp:5 | 圆弧按 n 等分逐段弧长标注 |
| @dim:fixdims | dim-tools.lsp:7 | 全图 DIMENSION/LEADER/LINE 的 10-16 组 Z 坐标压平 |
| @dim:dimext | dim-tools.lsp:41 | 标注延伸线显隐切换（1st/2nd/Both/None） |
| @dim:sum-dim | sum-dim.lsp:1 | 所选尺寸实测值求和（命令行输出） |
| c:centerline | centerline.lsp:12 | 圆弧/圆/椭圆十字中心线（Lee Mac 移植，未入菜单） |
| getxx / getmxy / Lsort / @dim:merge-dim | merge-dim.lsp:1/:18/:27/:41 | 合并连续标注（**未入 :FILES，多 bug**） |

## 3. 新功能（NEW）
- **F1 尺寸链/连续标注（P1）**：沿折线自动生成连续尺寸（DIMCONTINUE/DIMCHAIN 等价，VLA 落标注 + 偏移控制），现只能手工。
- **F2 对齐标注批量（P1）**：选一组平行线 → 批量对齐标注（DIMALIGN 等价，等距分布），工程图高频需求。
- **F3 弧长标注扩展（P2）**：menu-dimarc-div 仅限 ARC；扩展到圆（整圆弧长）、椭圆弧、lwpolyline 弧段（bulge 段）。
- **F4 中心线完善（P2）**：c:centerline 入菜单 + 改 at-dim: 命名空间；支持批量（ssget）+ 长度/方向可配（现每次手动 getpoint）。
- **F5 标注求和落图（P2）**：sum-dim 结果可写为文本/表格（table:make），支持按图层/块分组求和。
- **F6 标高批量（P2）**：坐标标注的单点标高 → 批量点标高（选点集 + 前缀 ± 相对标高基准），土方图场景。
- **F7 标注样式库（P2）**：dimstyle 模板保存/应用（现 set-env 只建"坡度标注"一种，硬编码组码）。

## 4. 改进（IMPROVE）
- **I1 true-value 逻辑错误（P1）**：`(entity:putdxf x 1 "")` 写空覆盖文本，显示为空而不是原值（deviation.lsp:15-18）；应删 1 组（`entity:deldxf`）。
- **I2 make-deviation 截断（P1）**：`(itoa (fix (+ ... (dxf 42))))`（deviation.lsp:8）截断小数（123.45→123），且覆盖后无 regen 提示；改 rtos + `@dim:units` 精度。
- **I3 斜率除零（P1）**：dim-slope 对竖直线段 `(- (cadr ...) (cadr ...))=0` 除零崩溃（at-dim.lsp:73-76）；竖线应标"无穷/90°"。
- **I4 等分弧角度回绕错误（P1）**：menu-dimarc-div 中 `(+ pi ang-all)`（dimarc.lsp:36-37）应为 `(+ (* 2 pi) ang-all)`，跨 0° 弧的等分位置全错。
- **I5 area 单位硬编码（P1）**：固定 `* 1e-6`（mm→m²）忽略 `@dim:draw-units`/`@dim:text-units` 配置（area.lsp:10）；文字样式 "MM" 硬编码。
- **I6 merge-dim 未打包且多 bug（P1）**：未入 `:FILES`/菜单；`getxx` 接收 entget 列表而非 ename（merge-dim.lsp:47 调用处）、`getmxy` 内 `SA` 未定义（:23）、`Lsort` i=2 永远走 x 排序（:29-32）——修好并入包或整删。
- **I7 cutting-symbol 拼写 bug（P1）**：`curttext1`（:130-131）、`curline1`（:325）、`cuttext11`（:331-332/:357）三处拼错 → 对应分支运行时报未定义符号；结尾清理 `:vlax-false(...)`（:381）缺空格被解析为单一符号必报错。
- **I8 cutting-symbol 全局状态（P2）**：`setvar "CELTYPE" "BYLAYER"` 不还原；DASHED 线型加载用 `*linefile*` 全局；文字样式 "MM" 硬编码。
- **I9 坐标标注 UX（P2）**：menu-zbbz 的 `(ssget pt-b)`（coord.lsp:37）以坐标点为窗角二次选择求相对方位，流程意外；应改为自动按象限或可选方位。
- **I10 set-scale 精度（P2）**：getint 限制比例只能整数（at-dim.lsp:95），配置默认 100.0 为浮点；改 getreal。
- **I11 多引线缩放（P2）**：`(vla-put-ScaleFactor ... (* 10 scale))`（coord.lsp:148）×10 系数可疑（scale=100 → 1000）；需实测核对。
- **I12 数字格式化参数（P2）**：`(string:number-format (rtos ...) 0 3 " 0")`（coord.lsp:84）中 int-n=0 会使内部 `(repeat (- int-n (strlen part-int)) ...)` 为负数（需实测是否报错）；填充 " 0" 语义为"空格前缀+0 后缀"（左对齐整数+补零小数），与 X/Y 同格式应统一。
- **I13 dimarc 输入健壮性（P2）**：n 为 nil（取消）时 `(/ ang-all nil)` 报错；`@dim:dimarc-gap 1000` 无 UI 只能改配置。
- **I14 小遗漏（P2）**：dim-slope 的 `i%` 全局泄漏；menu-dim-slope/area 缺 `(princ)`；c:centerline 的 `trans ... 1 0` 在 UCS 偏移布局下位置错误。

## 5. 重构（REFACTOR）
- **R1 标注文本模板（P1）**：area/dim-slope/coord 三处的 `rtos + make-text + putdxf(8/62)` 模式 → 提取 `(at-dim::label str pt height rot)`，统一样式/图层/精度（吃 `@dim:units`/`@dim:layer`）。
- **R2 cutting-symbol 状态机（P2）**：grread 大循环 280 行、cutmode 1/2/3 分支嵌套深 → 拆成 3 个模式处理函数 + 事件表分发；*error* 清理表与结尾清理表合一。
- **R3 set-env 数据化（P2）**：dimstyle 硬编码组码列表（at-dim.lsp:46）→ 配置项 + 默认模板，支持 F7 样式库。
- **R4 前缀统一（P2）**：`c:centerline` → `at-dim:centerline`；`@dim:`/`at-dim:` 双前缀并存。

## 6. lib 协同
- 复用（已用）：`text:box`、`group:make`、`xdata:put`、`string:number-format`、`string:from-list`、`m:fix-angle`、`curve:pline-2dpoints`/`put-points`、`point:to-ax`、`ui:input`、`table:make`（F5）
- 建议提取（≥2 包复用）：
  - `dim:make-aligned`/`dim:make-continuous`（F1/F2，at-structure/road-cross 也可能用）
  - `entity:label`（带样式/图层/精度的文本标注原语——与 at-curve PLAN 的 R2 同一候选，两包共用）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 true-value | P1 | 0.3h | — |
| I2 偏差截断 | P1 | 0.5h | — |
| I3 斜率除零 | P1 | 0.5h | — |
| I4 弧等分回绕 | P1 | 0.5h | — |
| I5 area 单位 | P1 | 0.5h | — |
| I6 merge-dim | P1 | 2h | 修或删二选一 |
| I7 cutting 拼写 | P1 | 1h | — |
| R1 文本模板 | P1 | 1.5h | — |
| F1 尺寸链 | P1 | 3h | — |
| F2 批量对齐 | P1 | 3h | — |
| F3 弧长扩展 | P2 | 2h | — |
| F4 中心线 | P2 | 1.5h | — |
| F5 求和落图 | P2 | 1.5h | — |
| F6 批量标高 | P2 | 2h | — |
| F7 样式库 | P2 | 2.5h | — |
| R2 状态机 | P2 | 3h | — |
| R3/R4 + I8-I14 | P2 | 4h | — |

合计 ≈32h
