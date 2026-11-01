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

## 8. 标注增强并入（v0.0.28，原 mini-dim 包）

原 `mini-dim`（迷你标注索引）包已并入本包：`mini-dim.lsp` → `dim-core.lsp`（公共工具 + 菜单），
命令层新增 `dim-tools-ext.lsp`。命名空间统一为 `at-dim:`，菜单组名由「迷你标注」改为「标注增强」。
`mini-dim/` 目录已删除。

### 8.1 iMini 命令映射表

| iMini 命令 | 状态 | 本包实现 | 说明 |
|---|---|---|---|
| dim_move | ✅ | `at-dim:dim-move` / `at-dim:ai-dim-move` | 文字移动（可投影到尺寸线） |
| dim_home | ✅ | `at-dim:dim-home` / `at-dim:ai-dim-home` | 文字归位（尺寸线中点） |
| dim_align | ✅ | `at-dim:dim-align` / `at-dim:ai-dim-align` | 文字沿基线对齐 |
| dim_merge | ✅ | `at-dim:dim-merge` / `at-dim:ai-dim-merge` | 共线标注合并为总标注（`merge-dim.lsp` 的 `@dim:merge-dim` 为旧实现，仍有 I6 缺陷） |
| dim_total | ✅ | `at-dim:dim-total` / `at-dim:ai-dim-total` | 包围盒总尺寸 |
| dim_check | ✅ | `at-dim:dim-check` / `at-dim:ai-dim-check` | 文字值与实测比对 |
| dimreassoc / dim_update | ✅ | `at-dim:dim-reassoc` / `at-dim:ai-dim-reassoc` | 按两点真实距离重建文字值 |
| dim_devide | ✅ | `at-dim:dim-devide` / `at-dim:ai-dim-devide` | 点表沿 X/Y 逐段 + 总标 |
| dim_line | ✅ | `at-dim:dim-line` / `at-dim:ai-dim-line` | 沿直线/多段线逐段标注 |
| dim_room | ✅ | `at-dim:dim-room` / `at-dim:ai-dim-room` | 房间周长 + 面积两行文字 |
| mleader_align | ✅ | `at-dim:mleader-align` / `at-dim:ai-mleader-align` | 多重引线文字对齐（走 10 组码，本 CAD 无 `TextLocation` 属性） |
| leader_2_mleader | ✅ | `at-dim:leader2mleader` / `at-dim:ai-leader2mleader` | 普通引线转多重引线 |
| bzmc | ✅ | `at-dim:bzmc` / `at-dim:ai-bzmc` | 块名/填充图案名标注 |
| index_name | ✅ | `at-dim:index-name` / `at-dim:ai-index-name` | 图名标注（框/下划线） |
| dim_fix_text | ⬜ **backlog** | — | 修正文字内容与格式（前缀/后缀/公差样式） |
| dim_flip | ⬜ **backlog** | — | 标注文字/箭头翻转（翻转尺寸线方向） |
| dim_trim | ⬜ **backlog** | — | 按边界修剪标注延伸线 |
| dim_win | ⬜ **backlog** | — | 门窗洞口尺寸批量标注 |
| dimpoly | ⬜ **backlog** | — | 多段线逐段标注（与 dim_devide/dim_line 重叠，需按 iMini 行为对齐） |
| dim_axis | ⬜ **backlog** | — | 轴网标注（轴线编号 + 尺寸链，需与 at-arch 轴网联动） |
| dim_wall | ⬜ **backlog** | — | 墙体厚度/墙段标注 |
| qnsy | ⬜ **backlog** | — | 墙身索引（引线 + 索引符号联动） |
| match_MLeader / mleader_mirror / mleader_list | ⬜ **backlog** | — | 引线格式刷 / 镜像 / 统计（统计可先做：按样式/字数汇总） |
| sbpdx | ⬜ **backlog** | — | 索引符号（详图索引圆） |
| indexer* / index_elev | ⬜ **backlog** | — | 剖切索引、索引文本、索引椭圆、标高索引 |

### 8.2 实现要点（供后续维护）

- **标注文字位置只能用 ActiveX**：本 CAD（AutoCAD 24.1 LMS Tech）对 DIMENSION 的 11/70 组码 `entmod`
  无效（静默失败），必须 `(vla-put-TextPosition obj (vlax-3d-point pt))`；封装见
  `at-dim:set-dim-text-pt` / `at-dim:dim-text-pt`。
- **旋转标注的 50 组码必须写在 `AcDbAlignedDimension` 段内**（在 `AcDbRotatedDimension` 标记之前），
  否则被忽略为 0（曾导致竖直标注实测值为 0）。
- **`and`/`or` 只返回 T/nil**（AutoLISP 语义），不能用于取值；需要取值用 `if`，
  位运算缺省值用 `at-dim:dxf0`。
- **`ssget "_C"`/"_W"` 只对当前视图内图元生效**，跨屏幕坐标取不到 → 用 `"_X"` + 距离过滤
  （见 `at-dim:leader-text`）。
- **MULTILEADER 无 `TextLocation` 属性**：文字位置改 10 组码 + `entmod` + `entupd`。
- `at-dim:dim-dir`：仅当实体含 `AcDbRotatedDimension` 时才用 50，否则用 13→14 方向
  （aligned 标注也带 50=0，误用会把竖直标注算成 0）。
