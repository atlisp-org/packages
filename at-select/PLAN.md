# at-select 功能扩展计划

> @lisp 选择工具。填充/线域选块、选择集过滤、链式/触碰选择、快速选择。v0.1.9，自研，依赖 base。

## 1. 概览
- 版本 0.1.9 / 依赖 base / 自研
- 规模：7 个 lsp 文件 58 个函数（menus.lsp 为菜单注册）
- 功能域：填充选块、线域选块、填充选择、选择集记录(ss1-ss5)、绘线指引、属性选择（同类型/同层/同色/同线型/闭合/未闭合/短线/定长/相似）、链式选择、曲线对齐、分堆加框
- 2026-10 新增 quick-sel.lsp（对应 iMini：快速选择易 / 获得选择集 / 触碰连锁反应选择）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-select:select-blk-by-hatch | at-select.lsp | 选择一个填充，返回填充内的块 |
| at-select:select-blk-by-lwpl | at-select.lsp | 单环闭合多段线内选块 |
| at-select:select-by-lwpl | at-select.lsp | 单环闭合多段线内选图元 |
| at-select:select-by-hatch | at-select.lsp | 填充图形内选择 |
| c:ss1..c:ss5 | at-select.lsp | 记录/高亮选择集 ss1-ss5 |
| @select:line-to-ss | at-select.lsp | 绘制指向选择集的线 |
| at-select:select-sametype 等 9 个 | select.lsp | 同类型/同层/同色/同线型/闭合/未闭合/短线/定长线/相似线 |
| c:chain-sel | chain-sel.lsp | 链式选择（既有） |
| c:obj-align | object-align.lsp | 曲线对齐（既有） |
| at-select:cluster-box | cluster-box.lsp | 分堆加包围矩形框 |
| @select:parse-filter / @select:filter-ss | quick-sel.lsp | 快速选择过滤引擎：图层=/块=/颜色=/类型=，多条件 AND，通配符，颜色支持 bylayer/随层 |
| at-select:ai-quick / ai-getsel | quick-sel.lsp | AI 入口：按表达式选择，返回 ename 列表 |
| c:quick-sel / c:getsel | quick-sel.lsp | 交互：快速选择 / 获得选择集 |
| at-select:ai-chain / @select:chain-from | quick-sel.lsp | 链式选择：从曲线端点出发沿首尾相接递归扩展整条链 |
| at-select:ai-touch / @select:touch-from | quick-sel.lsp | 触碰选择：选中与实体相交或端点相接的所有实体（递归） |
| c:touch-sel | quick-sel.lsp | 交互：触碰选择 |

## 3. 新功能（NEW，iMini 未实现项）
- **F1 fastsel 高级选项（P2）**：iMini `mini_fastsel`（2020-01-05）有 [沿线触碰 F]、[沿线触碰及范围内 CP]、[范围内 WP] 选项；c:touch-sel 目前仅支持曲线相交/端点相接（递归）。
- **F2 链式选择支持更多类型（P2）**：iMini `mini_chain_Sel`（2016-01-16）增加支持标注 DIMENSION、solid、3dface；目前仅曲线类（LINE/ARC/CIRCLE/LWPOLYLINE/POLYLINE/SPLINE/ELLIPSE）。
- **F3 过滤引擎扩展（P2）**：更多过滤键（线型=、宽度=、半径= 等）、OR 条件、取反；目前仅 图层/块/颜色/类型 AND + 通配符。
- **F4 快速选择设置（P2）**：iMini 快速选择易带设置对话框；可记忆常用过滤表达式（配置存 base 包 `@:get-config` 族）。

## 4. 改进（IMPROVE）
- **I1 候选实体缓存（P2）**：`@select:chain-from` / `@select:touch-from` 每次调用都经 `@select:curve-entlist` 全图 ssget 一次并取端点；大图中多次调用应缓存候选端点表（会话级变量）。
- **I2 触碰选择性能（P2）**：`@select:touch-from` 对 frontier 每轮线性扫全部候选端点 O(n²)；端点哈希（按容差分格）可提速。
- **I3 README 菜单表补全（P2）**：README「选择菜单」表缺 链式选择(c:chain-sel) / 曲线对齐(c:obj-align) 两行（2026-10 已补）。

## 5. lib 协同
- 复用：`pickset:from-list`/`pickset:to-list`、`p:picksetp`/`p:stringp`（base）
- 暂无需提取（过滤引擎目前仅 at-select 使用）

## 6. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| F1 fastsel 选项 | P2 | 3h | 沿线触碰需投影判定 |
| F2 链式扩类型 | P2 | 2h | DIMENSION 需取端点逻辑 |
| F3 过滤扩展 | P2 | 2h | — |
| F4 设置记忆 | P2 | 1.5h | 依赖 base 配置 |
| I1/I2 性能 | P2 | 2h | 大图才明显 |

合计 ≈10h
