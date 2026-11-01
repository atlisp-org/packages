# at-purge 功能扩展计划

> 图面清理。DGN 线型修复、多重插入块分解、OVERKILL 清理、一键清理/短线清理/词典/合并重线。v0.0.7，自研，依赖 base。

## 1. 概览
- 版本 0.0.7 / 分类 文件 / 依赖 base / 自研
- 规模：3 个 lsp 文件 61 个函数
- 功能域：
  - at-purge.lsp：`at-purge:remove-dgn`（DGN 线型修复 + 全量 purge）、`@:explode-minsert`（分解 MINSERT）
  - overkill.lsp：AutoCAD OVERKILL 清理（DCL 对话框 + 模糊/忽略/多段线等模式，`c:overkill`/`c:-overkill`）
  - purge-extra.lsp（2026-10 新增，对应 iMini：一键清理 mini_purge / 清理短线点空字 mini_line_clean / 删除常规词典 mini_dicts / 合并重线 mini_line_union）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-purge:remove-dgn | at-purge.lsp | 删 ACAD_DGNLINESTYLECOMP 字典项后 `purge a * N` |
| @:explode-minsert | at-purge.lsp | 选择 MINSERT 转 INSERT 后分解 |
| c:overkill / c:-overkill | overkill.lsp | OVERKILL 清理（对话框：模糊、忽略图层/块、多段线/部分/端点模式） |
| at-purge:ai-quick | purge-extra.lsp | AI 入口：一键清理。nil=全量（`purge a * N`），"layer"/"blk"/"style"/"ltype"=按类清理（纯表操作，保护 0/当前层/*_*/ACAD_*/Standard/BYLAYER 等）+ 清空块 + 合并重复块 |
| at-purge:clean-empty-blks | purge-extra.lsp | 删除引用为 0 的空块定义 |
| at-purge:find-dup-blks / merge-dup-blks | purge-extra.lsp | 按块指纹（dxf 排序归一化）找/合并几何相同的重复块 |
| at-purge:ai-clean-lines | purge-extra.lsp | AI 入口：删除短线（<min-len 的 LINE/LWPOLYLINE 边）、孤立点、空文字 |
| at-purge:ai-del-dicts | purge-extra.lsp | AI 入口：删除常规词典（默认 ACAD_MTRANS/ACAD_LAYER_STATES/ACAD_MLEADERSTYLE，ACAD_GROUP 需显式指定） |
| at-purge:ai-union-lines | purge-extra.lsp | AI 入口：合并重线——同层同色同线型的重叠/相接共线线段合并为一条 + 多段线删冗余共线顶点 |
| c:purge-quick / c:clean-lines / c:del-dicts / c:union-lines | purge-extra.lsp | 交互命令（菜单「实体」组） |

## 3. 新功能（NEW，iMini 未实现项）
- **F1 不同属性合并重线（P2）**：iMini `mini_line_union2`（Lineunion_l）清理属性不相同的叠加线条；ai-union-lines 目前仅同层/同色/同线型组内合并。
- **F2 空选忽略属性合并（P2）**：iMini `mini_line_union`（2015-07-26）回车空选 = 忽略属性合并线条；c:union-lines 目前无此模式。
- **F3 打断并删除短线（P2）**：iMini `mini_break_erase` 框选打断并删除短线（先打断落点再删短段，保留断点）；ai-clean-lines 只删不拦。
- **F4 词典删除对话框（P2）**：iMini `mini_dicts`（2016-01-16）带勾选对话框；c:del-dicts 目前 getstring 输入 + 默认清单。
- **F5 扩展清理项（P2）**：未引用渲染材质、布局块、视口等清理（对应 PURGE 全选项）。

## 4. 改进（IMPROVE）
- **I1 使用计数一次遍历（P2）**：ai-quick 分类清理时 `@purge:used-attrs` 全图遍历一次，前后快照各遍历一次表；大图可合并。
- **I2 块指纹性能（P2）**：`@purge:blk-fingerprint` 收集块内全部图元全部 dxf；大块慢，可只取实体类型 + 关键组码归一化。
- **I3 overkill 与 ai-union-lines 去重（P2）**：overkill.lsp 已有重叠清理能力（DCL 交互），与 purge-extra 的合并重线功能有重叠；文档说明两者差异（overkill=交互全模式，ai-union-lines=无交互按属性组）。

## 5. lib 协同
- 复用：`entity:putdxf`/`entity:deldxf`、`pickset:from-list`/`pickset:to-list`、`p:stringp`（base）
- 候选提取（≥2 包复用才提）：`blk:merge-dup`（重复块合并，at-block 或 at-pm 有布块需求时可提）；暂不提取

## 6. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| F1 不同属性合并 | P2 | 2h | 需属性差异保留策略 |
| F2 空选忽略属性 | P2 | 1h | — |
| F3 打断删短线 | P2 | 3h | 打断逻辑可借 at-curve |
| F4 词典对话框 | P2 | 1.5h | DCL 勾选列表 |
| F5 扩展清理 | P2 | 2h | — |
| I1/I2 性能 | P2 | 2h | — |

合计 ≈11h
