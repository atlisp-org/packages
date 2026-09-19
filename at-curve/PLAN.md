# at-curve 功能扩展计划

> @lisp curve。曲线工具集：双线互连、平滑路口（道路圆角）、曲线面积/长度标注、单线变双、交点编号、排线相连、样条转多段线、悬空线检查等。v0.1.35，自研，依赖 base + lib(curve/line/stat/ui/entity)。

## 1. 概览
- 版本 0.1.35 / 分类 Common / 依赖 base / 自研
- 规模：13 个 .lsp 共约 1160 行 26 个函数；`de-chamfer.lsp` 为未实现桩且未入 `:FILES`
- 两个菜单组："曲线工具"（15 项）、"曲线工具2"（3 项）；8 项 `@:define-config` 配置
- 核心几何计算大量复用 `lib/src/curve/`（pline-3dpoints/subsegment-*/o2bulge/optimize-lwpl/inters 等）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| @curve:setup | at-curve.lsp:35 | 曲线配置对话框（@::edit-config-dialog） |
| at-curve:join | at-curve.lsp:38 | 两条线按最近端点连接成一条 lwpline |
| at-curve:area | at-curve.lsp:63 | 闭合曲线面积文本标注（质心处，"mm" 文字样式） |
| at-curve:length | at-curve.lsp:84 | 曲线总长文本标注（中点处，"mb" 文字样式） |
| at-curve:per-length | at-curve.lsp:105 | 逐段长度标注（MLINE/LWPOLYLINE 分支，弧段中点偏移） |
| at-curve:dualline | at-curve.lsp:196 | 单线双向偏移成双线（DASHDOT 线型 + 直线/半圆封口） |
| at-curve:link-end | chain-line.lsp:15 | 取所选线的最左/最右端点各连一条竖向多段线 |
| @curve:left-points / right-points | chain-line.lsp:28/:34 | 端点 x 排序辅助（全局函数） |
| at-curve:de-chamfer | de-chamfer.lsp:1 | **桩**：仅提示"反倒角"，未实现，未入 :FILES/菜单 |
| at-curve:fillet | fillet-road.lsp:6 | 单条 lwpolyline 逐转角圆角（270 行，含闭口 repeat 3 分支） |
| at-curve:fillet-road | fillet-road.lsp:274 | 批量入口：仅选 **62=1（红色）** lwpolyline |
| @curve:inters-number | inters-number.lsp:1 | 一条曲线与多段线交点按曲线参数递增编号 |
| at-curve:join-by-number | join-by-number.lsp:1 | 按点旁数字文本（#,##,### 模式）排序连线 |
| @curve:link-obj | link-obj.lsp:4 | 源/目标物体用水平+垂直排线连接（1:1、1:n、n:1） |
| @curve:lw-to-3d | lw-to-3d.lsp:1 | lwpolyline 转 3D polyline（凸度拉直） |
| @curve:menu-lw2pl | lw-to-3d.lsp:17 | 菜单入口（提示含错字"将将行"） |
| @curve:spline2lwpl | sp2pl.lsp:1 | 样条按段数采样转 lwpolyline（可删原样条） |
| at-curve:optimize-lwpl | optimize-lwpl.lsp:2 | 共线/共圆顶点化简（调 lib curve:optimize-lwpl） |
| at-curve:stat | stat.lsp:1 | 曲线长度分类汇总（图层/颜色/线型/多线比例/组码） |
| at-curve:notch | notch.lsp:12 | 丁字交点开缺口（楔形，端点回缩 + 斜线） |
| at-curve:noclosed-endpt | unclosed.lsp:3 | 悬空端点检查（放大 zoom 找孤点 → 红圈标记 @temp 层） |
| at-curve:rm-flagpts | unclosed.lsp:35 | 删除标记圆 |

## 3. 新功能（NEW）
- **F1 多线连续连接（P1）**：join 仅限 2 条；支持按最近端点贪心链式连接 N 条线（含方向自动翻转 + 属性继承）。
- **F2 倒角/反倒角（P1）**：de-chamfer 桩落地——直线倒角（chamfer 距离参数）与"反倒角"（内角外扩），复用 curve:3pt2bulge 几何。
- **F3 缺口类型完善（P2）**：notch 现仅楔形（notch-type 配置了 0/1 但圆弧分支未实现）；补圆弧缺口 + 颜色/图层/线型/宽度继承原线（notch.lsp:6 已有 TODO）。
- **F4 曲线长度表（P2）**：per-length 标注 → 可选项落 at-stat 表格（段号/长度/累计长），支持按块/图层分组。
- **F5 路口圆角双幅道路（P2）**：fillet-road 目前单线；支持双线（已用 at-curve:dualline 生成的双幅）同步圆角。
- **F6 曲线拆分/分割（P1）**：按交点/点/段长把 lwpolyline 打断（curve:subsegment-points 已有取点能力，缺落图元）。

## 4. 改进（IMPROVE）
- **I1 悬空线检查崩溃（P0）**：noclosed-endpt 中 `n` 从未赋值，首次 foreach 执行 `(< ... (> n 0) ...)` 即 "bad argument" 报错（unclosed.lsp:16）；且 `n100` 死变量、`pts` 等全局泄漏。
- **I2 悬空线检查过滤缺陷（P1）**：过滤器 `(-4 . "&")(70 . 1)`（合取）要求图元必须带 70 组，导致 LINE/ARC/ELLIPSE 全部漏检——只有开口 LWPOLYLINE/POLYLINE 被检查；应改为 OR 分支（有 70 且 ≠1，或无 70 组）。
- **I3 spline2lwpl `(exit)`（P0）**：3 处 `(exit)`（sp2pl.lsp:10/:20/:31）在取消选择/段数=0/entmake 失败时直接终止 LISP 运行时，应改 `(princ)` 返回。
- **I4 spline2lwpl 顶点数不匹配（P1）**：`(cons 90 (1+ count))` 声明 count+1 个顶点，实际只写 1+segcount=count 个（sp2pl.lsp:27-31），entmake 顶点数错乱；闭口曲线还缺闭合点。
- **I5 join 健壮性（P1）**：仅取 car/cadr（选 1 条报 nil、选 3+ 条静默丢弃）；新线丢失原图层/颜色/线型（make-lwpline-bold 默认值）；`curves` 全局泄漏。
- **I6 fillet-road 颜色过滤硬编码（P1）**：`(62 . 1)` 只处理红色道路线（fillet-road.lsp:276）；改为过滤器配置（默认不过滤）。
- **I7 闭口分支条件重复（P2）**：fillet-road 闭口 repeat-3 首 cond 中 n2-n3 长度条件写了两次，第二次应为 n3-n4（fillet-road.lsp:204-210）。
- **I8 文字样式硬编码（P2）**：area/length/per-length 固定 "mm"/"mb" 样式，样式缺失即 entmake 失败；改 `@::text-style` 配置或跟随当前样式。
- **I9 stat 调试残留（P2）**：`stat.lsp:12` `(print dxfno)` 残留；MLINE 分支（per-length:112-133）对 MLINE 取 curve:get-points 不可靠，建议删分支。
- **I10 join-by-number 性能（P1）**：vl-sort 比较函数内对每对点做 2 次 ssget（O(n²) 选择集查询），大点数极慢；改预扫描一次建 点→号 映射再排序。
- **I11 dualline 可疑调用（P2）**：`(car (vla:get-value (vla-offset ...)))`（at-curve.lsp:205-206）对 SAFEARRAY 取元素非标准写法，需实测验证；封口半圆 bulge -1 方向需按端点相对位置取 ±（at-curve.lsp:224-231）。
- **I12 dualline 原线被改写（P2）**：原线被改成 DASHDOT+颜色 1，应保留原线属性或提供"原线保留"选项。

## 5. 重构（REFACTOR）
- **R1 fillet 去重（P1）**：fillet-road.lsp 三段几乎相同的圆角判定（初段/中段/闭口）→ 提取 `(at-curve::can-fillet n1 n2 n3)` + `(at-curve::apply-fillet ...)`，270 行 → 约 100 行；局部 defun 上提为包级 `at-curve::` 函数。
- **R2 标注函数模板（P2）**：area/length/per-length 的 `rtos+make-text+putdxf 62 1` 三连重复 5 处 → 提取 `(at-curve::label curve-str pt rotation)`。
- **R3 前缀统一（P2）**：`@curve:` / `at-curve:` 双前缀并存 + `@curve:left-points` 等全局辅助 → 统一 `at-curve:` 命令 + `at-curve::` 内部。
- **R4 link-obj 全局闭包（P2）**：obj-to-centerline 依赖外层 `pt-center`/`n-less` 全局（link-obj.lsp:126-151）→ 改为显式传参。

## 6. lib 协同
- 复用（已用）：`curve:pline-3dpoints`/`pline-convexity`/`subsegment-*`/`o2bulge`/`inters`/`optimize-lwpl`、`line:get-lwpoints`、`stat:classify`、`ui:input`/`ui:select`/`table:make`
- 建议提取（≥2 包复用）：
  - `curve:split-at-points`（按点集打断 lwpolyline，F6 与 at-dim 切割、road-cross 路网共用）
  - `curve:offset-2d`（dualline 的双向偏移+封口逻辑，at-dim 双线标注可能复用）
  - `entity:label`（带样式/颜色/旋转的文本标注原语，at-curve/at-dim/at-stat 重复模式）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 悬空检查崩溃 | P0 | 0.5h | — |
| I3 spline exit | P0 | 0.2h | — |
| I2 悬空检查过滤 | P1 | 1h | — |
| I4 顶点数 | P1 | 0.5h | — |
| I5 join 健壮 | P1 | 1h | — |
| I6 颜色过滤配置 | P1 | 0.5h | — |
| I10 排序性能 | P1 | 1.5h | — |
| R1 fillet 去重 | P1 | 2.5h | — |
| F1 多线连接 | P1 | 2h | — |
| F2 倒角 | P1 | 3h | — |
| F6 曲线拆分 | P1 | 3h | 依赖 lib 提取 |
| F3 缺口完善 | P2 | 2h | — |
| F4 长度表 | P2 | 1.5h | — |
| F5 双幅圆角 | P2 | 3h | — |
| R2/R3/R4 | P2 | 2h | — |
| I7-I9/I11/I12 | P2 | 2h | — |

合计 ≈32h
