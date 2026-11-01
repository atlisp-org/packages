# at-elec 功能扩展计划

> @lisp建筑电气。建筑电气设计工具：回路编号、电缆标注、负荷统计、设备统计、设备图块插入、说明/样例图。v0.1.0，自研，依赖 base + at-pm。
> 注：pkg.lsp 显示名已为正常 UTF-8（"@lisp建筑电气"），无需乱码修复。

## 0. 2026-11-01 更新（Agent 适配）

- 新增 `ai.lsp`：`@elec:ai-load-stat`、`@elec:ai-equip-stat`、`@elec:ai-circut-numbering`、`@elec:ai-cable-label`，均无交互。
- 修复 **I1**：`@::*prefix*` → `@:*prefix*`（draw-readme / draw-plan-example）。
- 修复 **I6**：负荷需要系数配置化 `@elec:demand-factor`（默认 0.7）。
- 完成 **R2 部分**：统计逻辑提取为 `@elec:stat-load` / `@elec:stat-equip`，交互命令与 AI 入口共用。
- `@elec:block-watt` 支持动态属性“功率”与普通属性“功率”/`POWER`，普通块也能统计。
- 版本 0.1.1，已发布 stable。

## 1. 概览
- 版本 0.1.0 / 分类 电气 / 依赖 at-pm（下载 dwg 用）/ 自研
- 规模：at-elec.lsp 200 行 8 个函数 + 资源 readme-elec.dwg / example-elec.dwg
- 菜单组 "@电气"（9 项）；4 项 `@:define-config` 配置（其中 `@elec:panel-blkname` 未使用）
- 设备图块（开关/插座/灯具/配电箱/断路器/电表 .dwg）不在仓库内，运行时从服务器下载

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| @elec:setup | at-elec.lsp:25 | 电气配置对话框 |
| @elec:circut-numbering | :33 | 选回路线 → 中点上方 200 处标 "前缀-序号"（WL-01…），起始号自动递增保存 |
| @elec:cable-label | :61 | 选一条线 → 沿线 30% 处标电缆型号文本（ELEC-CABLE 层） |
| @elec:load-stat | :91 | 选 INSERT → 累加动态属性"功率"，输出总数/总功率/计算负荷（Kd=0.7 硬编码） |
| @elec:equip-stat | :115 | 选 INSERT → 按块名分类计数，命令行列表输出 |
| @elec:menu-insert-equip | :145 | 选 6 类设备 → 下载缺失 dwg → dyndraw 插入（服务器依赖） |
| @elec:draw-readme | :174 | 插入 readme-elec.dwg 块并炸开 |
| @elec:draw-plan-example | :190 | 插入 example-elec.dwg 块（块名 example-plan-elec） |

## 3. 新功能（NEW）
- **F1 回路表/负荷表落图（P1）**：load-stat 只打命令行 → 生成回路表（回路号/设备/功率/相别/L1-L3 分配）table:make 落图，或 Excel 导出（excel:* 已有）。
- **F2 配电箱回路自动统计（P1）**：以配电箱块为中心，收集关联回路线（按块属性/图层/几何距离）→ 自动出该箱回路清单（现"回路编号"与"配电箱"完全脱节，`@elec:panel-blkname` 配置形同虚设）。
- **F3 设备块库本地化（P1）**：6 类设备 dwg 入包 `:FILES`（替代运行时下载 + `sleep 3` 竞态），或至少入包一个内置最小库 + 服务器扩展。
- **F4 相线/回路颜色（P2）**：回路按相别（L1/L2/L3）自动着色（图层 ELEC-L1…），配合 F2 的回路清单。
- **F5 负荷计算深化（P2）**：Kd（需要系数）/Pj（计算系数）配置化，按设备类型（照明/插座/动力）分组系数，输出 Pjs/Qjs/Sjs/Ijs 全套。
- **F6 设备属性模板（P2）**：插入设备时自动带"功率/回路号"动态属性（现 load-stat 依赖属性存在，但插入流程不保证），ui:input 填参数。
- **F7 系统图生成（P2）**：由 F2 的配电箱-回路树自动生成电气系统图（框+线+标注，模板块）。

## 4. 改进（IMPROVE）
- **I1 `@::*prefix*` 未定义崩溃（P0）**：draw-readme/draw-plan-example 用 `@::*prefix*`（双冒号），全库只定义 `@:*prefix*`（单冒号）→ strcat nil 类型错误，两命令首行即崩（at-elec.lsp:176/:192）。
- **I2 dyndraw 用法错误（P1）**：menu-insert-equip 中 `(ui:dyndraw (block:insert ...) '(0 0 0))`（:162-166）——先把块插入 (0,0,0) 再把 ename 当"绘制函数"传给 dyndraw，块永远落在原点而非光标处；应 `'(lambda (pt) (block:insert ... pt ...))`。
- **I3 炸开后再查 ename（P1）**：draw-readme 先 `vla-explode` 删块引用，再对原 ename `entity:getdxf readme-elec 0`（:184-187）→ ename 已失效，报错；删掉这段无意义检查。
- **I4 下载竞态（P1）**：`(sleep 3)` 等待下载完成（:159）不可靠；改 `@:down-pkg-file` 返回值检查 + findfile 重试。
- **I5 编号/标注 UX（P1）**：circut-numbering 的 help 说"选择配电箱块"，实际选线（:35-39）；偏移 200 与角度固定，无方向选项；文本不随线编组、不入指定图层。
- **I6 Kd 硬编码（P2）**：load-stat 的 0.7（:108）配置化。
- **I7 文字样式硬编码（P2）**："mm" 样式假设存在（:51/:82），缺失即 entmake 失败；跟随当前样式或 `@::text-style`。
- **I8 重复计算（P2）**：cable-label 对同一 ent 调 4 次 `curve:get-points`（:75-81）；仅支持单线（entsel），无批量。
- **I9 未用配置（P2）**：`@elec:panel-blkname` 无引用；`@elec:circut-start` 被自动改写（副作用，提示语未说明）。
- **I10 无统计分组（P2）**：load-stat/equip-stat 不支持按图层/块属性过滤（如只统计某系统），无表格/Excel 输出。

## 5. 重构（REFACTOR）
- **R1 前缀统一（P2）**：`@elec:` / `@::` 混用 → 命令 `@elec:`、内部 `@elec::`；`@::*prefix*` 修正并入 R1。
- **R2 统计函数模板（P2）**：load-stat 与 equip-stat 的 ssget→遍历→累加→输出 结构重复 → 提取 `(at-elec::stat ss key-fn reduce-fn)`。
- **R3 设备定义数据化（P2）**：6 类设备列表（:149）+ 下载/插入流程 → 设备表 `(("开关" . "开关.dwg") ...)` 驱动，为 F3/F6 铺路。

## 6. lib 协同
- 复用：`block:get-dynprop`/`get-effectivename`/`insert`、`curve:midpoint`/`get-points`、`ui:select`/`dyndraw`/`input`、`table:make`（F1）、`stat:classify`（F5）、`excel:*`（F1 导出）
- 建议提取：无新增候选（电气为领域包）；设备库 dwg 资源管理方式建议与 at-arch/at-structure 的图块资源方案对齐（跨包主题，B10 复核）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 prefix 崩溃 | P0 | 0.3h | — |
| I2 dyndraw | P1 | 0.5h | — |
| I3 炸开检查 | P1 | 0.2h | — |
| I4 下载竞态 | P1 | 1h | — |
| I5 编号 UX | P1 | 1.5h | — |
| F1 表格落图 | P1 | 3h | — |
| F2 配电箱统计 | P1 | 4h | 核心功能补全 |
| F3 设备库本地化 | P1 | 2h | — |
| F4 相线着色 | P2 | 1.5h | — |
| F5 负荷深化 | P2 | 3h | — |
| F6 属性模板 | P2 | 2h | — |
| F7 系统图 | P2 | 4h | 依赖 F2 |
| R1-R3 + I6-I10 | P2 | 4h | — |

合计 ≈29h
