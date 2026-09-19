# align-all 功能扩展计划

> 对齐图素（第三方，源自 print1985）。选中文字/尺寸/图块/表格/天正标高等图素，按 Y 降序排序后以指定行距逐行对齐排列（类似"调整行距"）。

## 1. 概览
- 版本 0.0.6 / 分类 通用 / 依赖 base / 第三方
- 规模：align-all.lsp 约 90 行 1 个公开函数；配置项 `align-all:hangju`（默认行距 2000）
- 支持对象：`*TEXT,DIMENSION,INSERT,ATTDEF,ACAD_TABLE,TCH_ELEVATION`

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| align-all:entity | align-all.lsp:13 | ssget → 按组码 10 的 Y 冒泡降序排序 → getdist 行距（记忆到 config）→ 从最上行开始逐行 move 到 x0 起点 → undo be/e 包裹 |

## 3. 新功能（NEW）
- **F1 列对齐（P1）**：X 方向对齐（按 X 升序，列距 config `align-all:liangju`）。接口：`(align-all:entity 'col)`，现函数加方向参数。
- **F2 自动行距（P1）**：不输入时按现有间距中位数/最小间距×1.2 自动推断，`getdist` 提示默认值。
- **F3 网格对齐（P2）**：行列双向对齐（行距+列距），适合说明文字排表。
- **F4 MTEXT 锚点修正（P2）**：多行文字用组码 10 基准点排序/移动，顶对齐时错位；按 `text:get-insert`（含对齐点）统一基准。
- **F5 相对保持（P2）**：对齐后保留与首行的相对 X 偏移选项（现在全部对齐到 x0，会破坏缩进结构）。

## 4. 改进（IMPROVE）
- **I1 排序算法（P1）**：align-all.lsp 的 O(n²) 冒泡（双层 while + subst 交换）大选择集下慢；改 `(vl-sort all '(lambda (a b) (> (cadr (cdr (assoc 10 a))) (cadr (cdr (assoc 10 b))))))`。
- **I2 subst 交换 hack（P1）**：`(subst 'aa ...)` 三行交换写法易错，vl-sort 后消除。
- **I3 osmode 恢复（P1）**：*error* 中 `(if snap (setvar "osmode" snap))` 被注释且 `snap` 从未赋值；函数开头也未保存原 osmode（只 set 了 16383/0）。用 push-var 保存恢复。
- **I4 undo 双包（P1）**：正常路径 `undo be`…`undo e`，*error* 里又 `undo e`——异常时嵌套 undo 行为未定义；统一为单一 undo group + error 中仅 pop。
- **I5 支持 TCH_SPACE/ACAD_MLEAD（P2）**：天正面积、引线标注同样常需行距对齐，扩充 ssget 过滤。

## 5. 重构（REFACTOR）
- **R1 选择集处理（P1）**：while 循环取 ssname → `(pickset:to-list ss)` 一行替代。
- **R2 提取纯函数（P1）**：`align-all:sort-by-y lst`、`align-all:layout lst hangju`（纯计算新坐标，无副作用）+ 主函数只做 entget/move——便于测试。

## 6. lib 协同
- 复用：`pickset:to-list`、`list:*`（排序/去重）
- 跨包：与 align-array（单体对齐）定位重叠但场景不同（行距 vs 图框排版），建议在 README 互相引用；`text:get-insert` 锚点逻辑与 at-text 包共用

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1+I2 排序 | P1 | 0.5h | — |
| I3/I4 变量与undo | P1 | 0.5h | — |
| F1 列对齐 | P1 | 1h | — |
| F2 自动行距 | P1 | 1h | — |
| R1/R2 | P1 | 1h | — |
| F3 网格 | P2 | 1.5h | — |
| F4/F5 | P2 | 各 1h | 需 at-text 锚点支持 |
| I5 对象扩展 | P2 | 0.5h | — |
