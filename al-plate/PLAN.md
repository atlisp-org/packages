# al-plate 功能扩展计划

> 铝板展开（机械类）。选中 A-铝板 层的轮廓多段线，按 A-板厚线 层判折点（内折扣板厚、外折不扣），计算各段展开净长，绘制展开图（底线/顶线/折弯线），ui:dyndraw 动态预览。

## 1. 概览
- 版本 0.0.4 / 分类 机械 / 依赖 base / 自研
- 规模：al-plate.lsp 约 110 行 3 个函数 + 配置 6 项
- 配置：al-layer(A-铝板) / thikness-layer(A-板厚线) / thikness(0.5) / length(200 绘制长度) / turned(0-2 转角) / symmetry(对称)

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| al-plate:setup | al-plate.lsp:27 | `@::edit-config-dialog` 配置对话框（tmp-search-str 过滤） |
| al-plate:draw-expand | :31 | 输入各段净长列表 → 累计坐标 → 底/顶双 polyline + 端线 + 折弯线 → `ui:dyndraw` 预览（不落图） |
| al-plate:draw1 | :77 | 选 lwpolyline（A-铝板层）→ 逐段判定：段中点沿下一段方向 2mm 处 `ssget "f"` 过滤 A-板厚线 层 → 命中则该端扣板厚 → 累加净长列表 → 调 draw-expand |

## 3. 新功能（NEW）
- **F1 确认落图 + 标注（P0）**：现只有 dyndraw 预览，无法真正出图。补"确认绘制"：落当前层 + 生成尺寸标注（每段净长 + 总长）+ 展开线/折弯线分色（复用 at-dim）。接口：`al-plate:draw1` 末尾 `(ui:confirm "确认绘制展开图?")`。
- **F2 展开数据表（P1）**：生成 ACAD_TABLE：段号 / 净长 / 累计 / 折弯方向（内/外），供下料单。配合 at-stat 表格风格。
- **F3 转角构件实现（P1）**：`turned`(1 一端/2 两端) 与 `symmetry` 配置已定义但**代码从未读取**（死配置）；实现转角段展开（斜边长度 = 板长/cosθ 类几何）。
- **F4 圆弧段支持（P1）**：lwpolyline 弧段展开长 = 弧长（`curve:get-length`），现只按直线 distance。
- **F5 下料清单导出（P2）**：多块铝板批量 → CSV/Excel 汇总（`excel:*`），按净长+数量去重统计。
- **F6 剖面符号标注（P2）**：在折弯位置画 45° 剖面符号（铝幕墙惯例）。

## 4. 改进（IMPROVE）
- **I1 死配置清理（P1）**：turned/symmetry 未使用——要么实现 F3，要么从 defpackage 导出与 config 中移除，避免误导。
- **I2 图层名硬编码（P1）**：:36-63 `"A-展开线"`、`"A-折弯线"` 写死；config 只有 al-layer/thikness-layer，补两个展开图层配置项。
- **I3 线型切换 hack（P1）**：draw1 中两次 `ssget "x"…putdxf 6 Continuous/BYLAYER` 改线型再还原（:72/:88）——直接按图层过滤 `ssget "f"` 不需要改线型；删除该 hack（若原意是过滤虚线则改过滤条件）。
- **I4 首段扣板厚逻辑（P1）**：flag-start 初始 0，首段前扣不扣取决于调用；确认设计意图并注释。
- **I5 错误处理（P1）**：无 `*error*`；选中非闭合/无板厚线时提示缺失。

## 5. 重构（REFACTOR）
- **R1 累计和纯函数（P1）**：draw-expand 中 `mapcar` 带 `(setq s ...)` 副作用的累计和（:32-36）→ 提取 `al-plate:accumulate lst` 纯函数（reduce 实现），可单测。
- **R2 段计算纯化（P1）**：draw1 中"判折+净长"循环抽为 `(al-plate:seg-lengths pts thikness-layer)` 纯函数，主函数只做选择与绘制。
- **R3 包一致性（P2）**：`(defpackage :al-plate …)` + `(in-package :al-plate)` 但函数实际定义在 autolisp 包内调用（al-plate: 前缀在 CL 包机制下与 @lisp 的 pkg:fn 约定冲突）；与 at-color 等 `@::def-pkg` 风格统一（保持包文件已有风格原则：本包用 `@::define-config`，建议整包迁移到 `@::def-pkg` 风格并去掉 defpackage）。

## 6. lib 协同
- 复用：`curve:get-points`/`curve:get-length`（F4）、`point:mid`、`entity:make-*`、`table:make`（F2）、`excel:*`（F5）
- 跨包：at-cnc（同属机械加工类）后续可共用"轮廓→加工数据"管线

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| F1 落图+标注 | P0 | 2h | 现功能不可交付 |
| I1 死配置 | P1 | 0.3h | — |
| I2 图层配置 | P1 | 0.5h | — |
| I3 hack 清理 | P1 | 0.5h | — |
| F2 数据表 | P1 | 2h | — |
| F3 转角 | P1 | 3h | 几何需确认 |
| F4 弧段 | P1 | 1h | — |
| R1/R2 | P1 | 1h | — |
| I4/I5 | P1 | 0.5h | — |
| F5/F6 | P2 | 各 2h | — |
| R3 包风格 | P2 | 1h | 需确认 CL 包影响 |
