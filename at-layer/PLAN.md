# at-layer 功能扩展计划

> @lisp图层。图层开关/冻结/锁定其它、解锁/解冻全部、选图进层，及 iMini 图层增强命令复现。
> v1.2.14，自研，依赖 base。

## 1. 概览
- 版本 1.2.14 / 分类 通用 / 依赖 base
- 规模：at-layer.lsp（菜单+6 个原有函数）+ layers-ext.lsp（27 个扩展函数，含 4 个辅助）
- 菜单组「图层」（27 项）

## 2. 现状功能清单（原有）
| 函数 | 功能 |
|---|---|
| @layer:off-other | 关闭其它图层 |
| @layer:frozen-other | 冻结其它图层 |
| @layer:lock-other | 锁定其它图层 |
| @layer:unlock-all | 解锁全部图层 |
| @layer:thaw-all | 解冻全部图层 |
| @layer:ent-to-clayer / c:ent2clayer | 选图进层（特性随层） |

## 3. 已实现（本轮新增，layers-ext.lsp）

复现 iMini 工具集图层增强命令，每个命令 = 交互入口 + `ai-` 无交互入口：

| # | iMini 命令 | 新增函数 | 说明 |
|---|---|---|---|
| 1 | mini_layerstates 图层状态管理 | save-state / restore-state / state-list + ai-save-state / ai-restore-state / ai-state-list + current-states | 状态存 `*layer-states*`（开/冻结/锁定），按名称区分多份；恢复时先解锁→设冻结→设开关→再锁定 |
| 2 | 隐藏实体 | hide-ents / show-hidden + ai-hide / ai-show-hidden | 被关层记 `*layer-hidden*`，重复不记录，当前层被关时切 0 层 |
| 3 | mini_IsolateObjects 独立显示 | isolate / un-isolate + ai-isolate / ai-un-isolate | 被关层记 `*layer-iso*` |
| 4 | mini_lockobjects 锁定实体 | lock-ents / unlock-ents / unlock-ents-all / is-locked? / locked-list + ai-lock / ai-unlock / ai-unlock-all + ensure-app | xdata 标记机制（应用名 LAYERLOCK + 1000 码） |
| 5 | 清理双$层 | kill-dollar + ai-kill-dollar | 删名称含 $$ 的图层及实体，keep=1 实体移 0 层 |
| 6 | 层名大小写 | strcase + ai-strcase | 新建目标层(复制颜色/线型/标志位)→移实体→删旧层；目标层已存在则并入 |
| 7 | mini_layer_bycolor 按颜色分层 | by-color + ai-by-color | 层名 前缀+颜色号（默认 颜色N），BYLAYER/BYBLOCK 跳过 |
| 8 | mini_ldorder 按图层调整绘图顺序 | ldorder + ai-ldorder | 逐实体 vla-MoveToFront/MoveToBack |
| 9 | lm_LayerExtract 图层提取器 | extract + ai-extract | CSV：层名/颜色/线型/线宽/开/冻结/锁定/实体数 |

辅助：ents-ify（实体参数规范化）、layers-by-ents、join、set-layer-on（容错开关层）、replace-str、csv-quote。

## 4. 未实现

| 项 | 优先级 | 说明 |
|---|---|---|
| layer_key 数字面板（mini_layer_key 新建图层快捷键面板） | P2 | 需 DCL 界面 + 用户配置，iMini 为独立面板，工作量较大 |
| lman 图层管理器（依赖 ET 电子表格控件） | P2 | 依赖 ActiveX 电子表格控件（ET），本环境不可用，明确不实现 |
| 锁定实体反应器（vlr-command-end 拦截编辑） | P2 | iMini 的 mini_lockobjects 用反应器拦截修改；本实现交付 xdata 标记 + 查询函数（@layer:is-locked? / @layer:locked-list），反应器在 MCP/agent 环境下易出问题，如需可基于标记二次开发 |
| 图层状态管理-切换（lxx_laman 式一键切换两份状态） | P2 | 现有 save/restore 组合可覆盖，暂不单做 |
| 视口级图层状态（mini_vplayers 等） | P2 | 视口冻结属 vport 域，超出本包范围 |

## 5. 验证记录
- 2026-10-29：atlisp-lint 0 错误（at-layer.lsp 60 警告/7 信息、layers-ext.lsp 12 警告/29 信息，均为风格类）；dialect 陷阱脚本通过。
- CAD 实测待做：`@I at-layer` 安装后按 README「AI 调用」表逐项冒烟（save/restore-state、isolate、lock/unlock、kill-dollar、strcase、by-color、ldorder、extract）。
