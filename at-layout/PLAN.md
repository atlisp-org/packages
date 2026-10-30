# at-layout 功能扩展计划

> 布局（图纸空间）工具包。原有 9 个文件负责布局设置/视口坐标/合并/分图/图框/清理；
> 2026-10 并入原 `mini-layout` 包（复现 iMini 布局视口命令），新增 4 个文件。

## 0. 包内组成

| 文件 | 来源 | 职责 |
|------|------|------|
| `at-layout.lsp` | 原有 | 菜单「布局」「布局2」+ 布局设置 / 布局到模型 |
| `pvp.lsp` / `merge.lsp` / `divide-map.lsp` / `stat-vp.lsp` / `add-mapsheet.lsp` / `lock-vp.lsp` / `purge.lsp` | 原有 | 视口坐标、布局合并、分图、统计、图框、锁定、清理 |
| `layout-draw.lsp` | 原 `mini-layout/mini-layout.lsp` | 菜单「布局视口工具」+ 公共辅助（布局/视口 VLA 封装） |
| `layout.lsp` | 原 `mini-layout/layout.lsp` | 布局改名增减排序/合并/分图/选字重命名/局部取图 |
| `vport.lsp` | 原 `mini-layout/vport.lsp` | 视口比例/同步对齐/外框/锁定/图层状态/变灰还原 |
| `plot.lsp` | 原 `mini-layout/plot.lsp` | 图纸空间 1:1 布图/模型空间做块布图/炸开/局部重生成 |

命名空间统一为 `@layout:`（原 `mini-layout:`）。

## 1. iMini 命令映射

原 mini-layout 的实现目标就是复现下列 iMini 布局/视口命令，映射关系如下
（交互入口 + 无交互 AI 入口）：

| iMini 命令 | at-layout 交互入口 | AI 入口 | 说明 |
|---|---|---|---|
| `mini_TabSort` | `@layout:tab-sort` | `@layout:ai-tab-rename` / `ai-tab-add` / `ai-tab-del` / `ai-tab-sort` | 布局改名/增减/排序 |
| `layoutmerge` | `@layout:layout-merge` | `@layout:ai-layout-merge` | 布局合并（主视口网格排布） |
| `layout_isolate` | `@layout:layout-isolate` | `@layout:ai-layout-isolate` | 布局分图（每视口一个布局） |
| `layout_rename` | `@layout:layout-rename` | `@layout:ai-layout-rename` | 布局选字重命名 |
| `jbqt` | `@layout:jbqt` | `@layout:ai-jbqt` | 局部取图（框选区域出新布局） |
| `regen` | `@layout:regen` | `@layout:ai-regen` | 快速局部重生成 |
| `ps_scale` | `@layout:ps-scale` | `@layout:ai-ps-scale` | 图纸空间 1:1 布图 |
| `ms_scale` | `@layout:ms-scale` | `@layout:ai-ms-scale` | 模型空间做块布图 |
| `ms_exp` | `@layout:ms-exp` | `@layout:ai-ms-exp` | 还原炸开布图块 |
| `vpscale` | `@layout:vp-scale` | `@layout:ai-vp-scale` | 编辑视口比例 |
| `vpsync` | `@layout:vp-sync` | `@layout:ai-vp-sync` | 同步对齐视口（网格） |
| `vp_outline` | `@layout:vp-outline` | `@layout:ai-vp-outline` | 模型空间绘制视口外框 |
| `vplayers` | `@layout:vplayer` | `@layout:ai-vplayer` / `ai-vplayer-query` / `ai-vplayer-apply` | 视口图层状态编辑（xdata 存储） |
| `vportlock` | `@layout:vport-lock` | `@layout:ai-vport-lock` | 视口锁定或解锁 |
| `vport_grey` | `@layout:vport-grey` | `@layout:ai-vport-grey` | 视口选层变灰 |
| `vport_ungrey` | `@layout:vport-ungrey` | `@layout:ai-vport-ungrey` | 还原视口颜色 |

## 2. 新增功能（NEW，来自原 mini-layout PLAN）

- **F1 视口关闭 `mini_vplayer_off`（P1）**：选择布局上部分视口，`putdisplay false`
  关闭以提升显示速度。接口：`(@layout:ai-vplayer-off vports)`，返回已关闭视口列表。
- **F2 视口打开 `mini_vplayer_on`（P1）**：与 F1 配对，`putdisplay true`
  打开已关闭视口。接口：`(@layout:ai-vplayer-on vports)`。
- **F3 视口分离 `mini_vplayer_iso`（P1）**：打开所选视口、关闭同布局其余视口
  （组合 F1+F2）。接口：`(@layout:ai-vplayer-iso vports)`。
- **F4 刷视口图层状态 `mini_vplayer_match`（P2）**：把源视口的图层状态 xdata
  复制到目标视口（复用 vplayer xdata + `ai-vplayer-apply`）。接口：
  `(@layout:ai-vplayer-match src-vp dst-vp)`。
- **F5 图层管理 `lman`（P2，依赖 ET 不实现）**：iMini 的 lman 依赖 ET(ExplorTools)
  外挂，本包不实现，仅记录。
- **F6 iMini 其余布局选项（P2）**：按需求逐个评估。
- **F7 收敛重复功能（P1）**：本包原有 `@layout:merge` / `divide-map` 与新并入的
  `@layout:layout-merge` / `layout-isolate` 功能相近（前者按图块/多视口拆图，后者按
  主视口合并/拆分），后续可合并交互入口，保留一套语义。

## 3. 改进（IMPROVE）

- **I1 vplayer 冻结态作用域（P1）**：当前 freeze/thaw 作用于图层表（整图生效），
  非真正“每视口独立冻结”。AutoCAD 无原生每视口冻结 API；可选方案：视口激活时按
  xdata 自动应用 + 离开时还原（需事件钩子，复杂度高）。
- **I2 ms-scale 块内视口框样式（P2）**：布图块中视口目前画为 0 图层 4 条线，
  可支持按原视口框颜色/线型输出。
- **I3 交互入口统一 DCL 化（P2）**：新并入的交互入口用 `getstring`/`getkword`/`getint`
  串接，可改为 DCL 对话框。
- **I4 已知限制（P3）**：`layout-merge`/`layout-isolate` 基于“主视口”（图纸面积最大者）
  搬运模型视图，多视口布局的次要视口内容不保留；merge 前会清空目标布局已有视口。
- **I5 文档（P3）**：README 顶部标题/包全名字段仍是历史 GBK 转码乱码，待 pkg.lsp
  文案修正时一并处理（修改前需确认已发布版本的显示名）。

## 4. 重构（REFACTOR）

- **R1 make-vp 参数校验（P2）**：`@layout:make-vp` 对 w/h 未做正数校验
  （<=0 时 `vla-addpviewport` 行为未定义），补 guard。
- **R2 提取视口矩形计算（P2）**：`vp-model-rect` 已被 vp-outline/regen/vp-grey-layers
  三处使用，`vp-view` 的 12/40/41 解析逻辑集中于此，保持单一数据源。

## 5. lib 协同

- 已复用：`layout:*`（list/set-position）、`string:sort-by-number`、`point:mid`、
  `layer:*`（on/off/freeze）、`xdata:*`（put/remove-appid）、`pickset:*`（to-list）、
  `p:*`（picksetp）。
- 候选提取：无（均为单包使用）。

## 6. 优先级与工作量

| 条目 | 优先级 | 预估 | 依赖 |
|---|---|---|---|
| F1/F2/F3 视口开关分离系列 | P1 | 1h | — |
| F7 收敛重复合并/分图入口 | P1 | 1h | — |
| I1 vplayer 冻结作用域 | P1 | 2h（事件钩子需验证） | — |
| F4 vplayer_match | P2 | 0.5h | F1/F3 |
| I2 块内视口框样式 | P2 | 0.5h | — |
| I3 DCL 交互 | P2 | 3h | — |
| R1 make-vp 校验 | P2 | 0.2h | — |
| F5 lman | P2 | 不实现（ET 依赖） | — |
| F6 iMini 其余布局选项 | P2 | 按需 | — |
| I4/I5 文档化限制与文案 | P3 | 0.2h | — |

> P0 无；P1 为视口开关分离系列（iMini 高频命令）、vplayer 作用域改进、重复入口收敛。
