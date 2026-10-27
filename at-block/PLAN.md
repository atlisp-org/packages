# at-block 功能扩展计划

> @lisp 图块工具集（核心包 v1.1.9）。块编号、块替换、块爆炸（含可炸标志/循环爆炸）、重叠块定位/块视图、基点变换、xref 管理、跨块/跨图纸复制、分堆建块、按线转块等 15 个功能文件。

## 1. 概览
- 版本 1.1.9 / 分类 图块相关 / 依赖 base / 自研
- 规模：18 个 .lsp 共约 3100 行 95 个函数；配置 11 项（block-name/attribute-name/prefix/suffix/overlay-index/xref-layer/sort-order "xY"/sort-fuzz/size/overlap-tol/lib）
- 菜单两块：`_Block`（14 项）+ `_Block2`（11 项）

## 2. 现状功能清单（按文件）
| 文件 | 函数 | 功能 |
|---|---|---|
| at-block.lsp (575) | @block:config/setup | 编号配置对话框（块名+属性名+前后缀+覆盖策略） |
| | @block:set-number | 核心编号：ssget 同名块 → sort-order(xY)+fuzz 排序 → 属性递增，overlay-index 覆盖策略（支持多块名，连续编号） |
| | @block:set-any-block-number | 任意块编号（不限配置块名） |
| | @block:subst | 块替换（不支持动态块） |
| | @block:explodable / explode-disable | 块可炸标志（xdata 标记）设置/取消 |
| | @block:outline-dialog | 块视图 DCL（块定义浏览） |
| | @block:select-same | 选择同名块 |
| | @block:rename-noname | 匿名块重命名 |
| | @block:menu-change-base / menu-align-base | 改块基点 / 基点对齐 |
| | @block:menu-inserts | 连续插块 |
| | @block:explode-all / explode-chain / explode-loop | 一炸到底 / 链式 / 循环爆炸 |
| | @block:count-inblk / count-nested | 块内统计 / 嵌套块统计 |
| numbering-by-route.lsp | @block:numbering-by-route / find-on-curve / sort-by-curve / curve-fence-points / number-blocks / ai-number-by-route | 沿曲线路径编号；按距离采样围栏(支持弧段/圆/样条)、按投影距离排序、无交互 AI 入口 |
| auto-block-break.lsp (552) | c:auto-block-break 等 17 函数 | 第三方"自动断块"（mxv/trp/mxm 矩阵 + VLA） |
| copy-to-drawings.lsp (637) | c:copy-to-drawings 等 19 函数 | 跨图纸复制（DCL 文件浏览器，browseforfolder COM） |
| xref.lsp (207) | @block:xref-layer / xref-to-insert / help-xref-hk | xref 转插块 / xref 层管理 |
| overkill.lsp | @block:overblocks / overblocks2 / box-overlap? / overlap-pairs | 重叠块定位（size 阈值）/任意重叠；包围盒正面积重叠判定（overlap-tol 容差，并排块边线相接不算重叠） |
| explode.lsp | @block:explode-cliped | 炸剪裁块（按外框裁剪后炸） |
| 其余小文件 | insert-all / minsert2insert / tchpr2insert / block-cluster / set-clustergap / clip-to-blk / copy-by-blk / rotate-blk-by-line / write-file / match-att-style | 全图块插入 / 多重转普通 / 保护转块 / 分堆建块 / 设间隙 / 按块裁剪 / 块间复制 / 按线旋转 / 写块到库 / 属性样式刷 |

## 2.5 已修复
- **按路线编号漏选/报错（P0，已修复 v1.1.11）**：`@block:numbering-by-route` 用 `curve:get-points`（只返回端点/控制点）作 `ssget "F"` 围栏，带弧段的多段线/圆弧/圆/样条会退化成弦，沿弧的块漏选；圆路线只返回 1 个点直接报错。改为按曲线长度等距采样（`@block:curve-fence-points`，16~2000 点）。排序改为预计算投影距离 + 稳定次序键（原实现每次比较都在比较函数里 `defun` 重算，慢且污染全局）。未找到块时改为 `princ` 提示，不再弹 `alert`/误触发编号配置。编号串统一走 `@block:number-string`（支持前后缀与 `overlay-index`）。新增 AI 入口 `@block:ai-number-by-route` / `@block:find-on-curve`。
- **多块名编号（v1.1.11）**：`@block:block-name` 支持逗号/分号/空格分隔多个块名（如 `车位,车位-无障碍`）。新增 `@block:name-list` / `@block:block-names` / `@block:block-filter`，`set-number` 与按路线编号共用整套过滤器与编号串生成；多块名在同一次操作中共用一条连续编号（实测 52 块：车位 51 + 车位-无障碍 1，无障碍块按路径位置排到 21）。
- **蛇形排序（v1.1.11）**：新增 `@block:sort-by-snake`，`@block:sort-order` 填 `snake`/`S`/`蛇形` 即启用。按包围盒中心分行（`snake-row-tol`，默认 1200），整排（≥`snake-min-row`，默认 5）后换向，成 S 形；零星车位（墙边单列）排在最后，按列从左到右、列内自上而下。实测地库 140 个车位：主排蛇形 + 墙边列收尾，平均相邻步距 6.6m（原 `xY` 排序为跳跃式）。
- **AI 入口 `@block:ai-number-by-snake`（v1.1.11）**：无交互蛇形编号（`blkname`/`attname`/`start`/可选选择集，全图传 `nil`），自动全图筛选指定块名、按蛇形排序并写属性，返回按编号顺序的块列表。实测顶排 19 个车位：一次调用 203 ms 写入 01–19，写后已还原。
- **缺属性提示（v1.1.11）**：编号统一走 `@block:number-blocks`，对缺少目标属性的同名块（如 `ADFHADGHSA` 无 `CAR-NUMBER`）自动跳过并提示（`princ` + `@block:last-hint`），跳过不占号、编号连续。实测全图 `ded,ADFHADGHSA`：127 个 ded 编号 01–127、13 个无属性块被跳过并提示，0 空号；只编 ded 时无提示。
- **重叠块定位误判（P0，已修复 v1.1.10）**：overblocks/overblocks2 原用 `ssget "c"`（交叉窗口，含边界上的点）按块包围盒探测重叠，并排块边线重合（如两个车位块）会被误判为重叠。改为包围盒正面积重叠判定：X、Y 两方向重叠量均需大于 `@block:overlap-tol`（缺省 0.1），仅边线/角相接不算重叠。新增 `@block:box-overlap?` / `@block:overlap-pairs`（按 x 最小值排序的扫描，O(n·k) 比较）。

## 3. 新功能（NEW）
- **F1 动态块替换（P1）**：描述自述"不支持动态块"；subst 对动态块按"同名替换保留动作"或"退化为普通块替换"两策略，解决最大痛点。
- **F2 编号续号/跳号（P1）**：set-number 支持"从已有最大编号+1 续号"与"按现有属性值定位续排"（现 parking 包有 parking-curr-number 记忆，思路可上提）。
- **F3 块统计报表（P1）**：全图按块名统计数量/面积/分布 → ACAD_TABLE + CSV（count-nested 的推广），出图块明细表。
- **F4 块库管理（P2）**：write-file（写块到库）已有，补"从库浏览/插入/更新"（配合 at-pm 图框库；lib 配置项 `@block:lib` 已有路径）。
- **F5 xref 治理（P2）**：xref 缺失检测、批量转块（xref-to-insert 已有）、xref 版本检查报告。
- **F6 编号结果预览（P2）**：编号前 dyndraw 预览编号布局（复用 ui:dyndraw），确认后再写属性。

## 4. 改进（IMPROVE）
- **I0 重复定义（P0）**：`at-block/at-arch.lsp:11` 与 `at-block/at-block.lsp:94` 均定义 `@block:set-number`（参数表不同，后者覆盖前者）——文件 at-arch.lsp 疑似误放（内容属 at-arch 包的编号逻辑）；确认归属后删除其一，at-validate 应能捕获此类问题。
- **I1 备份文件（P0）**：`rotate-blk-by-line.lsp~` 残留，删除并确认 .gitignore 覆盖 `*~`。
- **I2 硬编码库路径（P1）**：`@block:lib "D:/Design/standard/lib/"` 默认值写死 D 盘；改为相对包路径或首次使用时 getfiled 设定。
- **I3 sort-order 容错（P1）**：配置 "xY" 任意两两组合，无输入校验；非法值（如 "xx"）静默错序；补 initget 校验。
- **I4 copy-to-drawings 的 COM 依赖（P1）**：browseforfolder 用 Shell COM（:534），部分环境无；回退 `getfiled` 目录选择。
- **I5 auto-block-break 第三方代码（P2）**：mxv/trp/mxm/c: 前缀无包名，污染全局；与原作者确认授权后加前缀或独立成包。

## 5. 重构（REFACTOR）
- **R1 at-block.lsp 拆分（P1）**：575 行 19 函数 → `numbering.lsp`（set-number/setup/config/numbering-by-route）、`subst.lsp`、`explode.lsp`（并入现有）、`basepoint.lsp`（change/align-base）。
- **R2 排序逻辑提取（P2）**：set-number 的 sort-order+fuzz 排序（xyXY 4 组合）抽为 `(block:sort-by-order lst order fuzz)` 纯函数入 lib（at-arch:parking-numbering 也需同类排序 → 跨包复用）。
- **R3 菜单语言混杂（P2）**：`_Block` 英文菜单 + `_Block2` 中文菜单并存；统一按 locale 走 `(_ "…")` 本地化（at-color 有 -chs/-cht lang 文件先例）。

## 6. lib 协同
- 复用：`pickset:sort`、`block:get-attributes`/`block:set-attributes`、`list:*`、`table:make`（F3）
- 候选提取：`block:sort-by-order`（R2，at-arch 复用）；`block:count-nested` 推广为 `block:stat`
- 跨包：at-pm（图框/图纸目录）、at-arch（车位编号）、mark-order（文本编号）共享"排序+递增编号"内核

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I0 重复定义 | P0 | 0.5h | 需确认 at-arch.lsp 归属 |
| I1 备份文件 | P0 | 0.1h | — |
| F1 动态块 | P1 | 4h | 核心痛点 |
| F2 续号/跳号 | P1 | 2h | — |
| I2 库路径 | P1 | 0.5h | — |
| I3 排序校验 | P1 | 0.5h | — |
| R1 拆分 | P1 | 2h | — |
| F3 统计报表 | P1 | 2.5h | — |
| I4 COM 回退 | P1 | 1h | — |
| R2 排序入 lib | P2 | 1.5h | 跨包 |
| F4/F5/F6 | P2 | 各 2-3h | — |
| I5/R3 | P2 | 1.5h | 需作者确认 |
