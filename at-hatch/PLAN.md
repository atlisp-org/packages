# at-hatch 功能扩展计划

> @lisp填充。填充工具包：快捷/超级填充、填充参数修改、填充移动对齐、由填充生成边界、
> 批量分离、全能裁刀、SOLID 与填充互转、图案提取/自定义、修复填充显示。
> v0.1.0，通用类，依赖 `base`，由 `mini-hatch`（半成品，28 个辅助函数、0 个命令）整理重写而来。

## 1. 概览

- 版本 0.1.0 / 分类 通用 / 依赖 base
- 规模：4 个 .lsp 共约 1960 行；12 个交互命令 + 12 个 AI 入口 + 60 余个公共辅助函数
- 1 个菜单组「填充工具」（12 项）；8 个内置 `.pat` 资源（ASCII 文件名 + 中文图案名映射）
- 填充创建统一走 `entmake`（多段线路径 + 凸度），不产生临时边界图元；自定义图案把定义行写进填充，
  不依赖支持路径

## 2. 已实现（12 项）

| # | 命令 | 交互入口 | AI 入口 | 说明 |
|---|---|---|---|---|
| 1 | 快捷填充 | `at-hatch:quick-hatch` | `at-hatch:ai-hatch` | 边界图元/散线圆弧（自动链接）/点表/点表列表/区域内部点 → 填充 |
| 2 | 超级填充 | `at-hatch:superhatch` | `at-hatch:ai-superhatch` | 多区域各一个填充，孔洞按包含关系自动作内边界 |
| 3 | 选现有图案填充 | `at-hatch:hatch-select` | `at-hatch:ai-hatch-select` | 改图案/比例/角度/颜色；图案未载入时自动重建并嵌入定义行 |
| 4 | 填充移动对齐 | `at-hatch:hatch-move` | `at-hatch:ai-hatch-move` | move/rotate/scale，基点缺省取填充包围盒中心 |
| 5 | 由填充生成边界 | `at-hatch:hatch-boundary` | `at-hatch:ai-hatch-boundary` | 解析填充 DXF 边界（直线/圆弧/椭圆/样条边），生成带凸度的闭合多段线 |
| 6 | 批量分离填充 | `at-hatch:hatch-isolate` | `at-hatch:ai-hatch-isolate` | 每环一个独立填充，成功后删除原填充 |
| 7 | 全能裁刀裁填充 | `at-hatch:clip` | `at-hatch:ai-clip` | 面域布尔（0 并/1 交/2 差）裁切，保留内/外可选，图案属性继承 |
| 8 | SOLID 变填充 | `at-hatch:solid2hatch` | `at-hatch:ai-solid2hatch` | 保留图层与颜色，原 SOLID 删除 |
| 9 | SOLID 满铺多段线 | `at-hatch:pline2solid` | `at-hatch:ai-pline2solid` | 耳切三角化铺 SOLID，可选删除原边界 |
| 10 | 填充图案提取输出 | `at-hatch:hatch-out` | `at-hatch:ai-hatch-out` | 扫描图中填充图案名 + 定义行，写多图案 `.pat` |
| 11 | 自定义填充 | `at-hatch:custom-pattern` | `at-hatch:ai-custom-pattern` | 生成 `.pat`、加入支持路径、可直接用于填充 |
| 12 | 修复填充显示 | `at-hatch:fix-pattern` | `at-hatch:ai-fix-pattern` | 比例 ≤0 改比例、图案名空改实体、未载入图案补定义行或改 ANSI31 |

修复记录（相对 mini-hatch）：

- `mini-hatch.lsp` 括号不平衡（行 403 多余右括号、`triangulate` 里 `if` 结构与无用构造代码）——
  `triangulate` 整体重写为耳切法，去掉 `(eval (cons 'list ...))` 之类的死代码。
- `point-in-loop` 射线法比较写反（`(< (max y1 y2) py)`）已修正。
- 原 `loop-pts`/`loops->ptlists` 依赖不存在的 `vla-get-paths`，改为解析填充 DXF 边界组码
  （`92/93/72/10/11/40/50/51/73/97`），兼容多段线路径与边路径。
- 原 `pattern-file` 用 `foreach` 复用循环变量导致结果被覆盖，改为逐项匹配 + `.pat` 直查。
- 原 `load-pat` 对 `.pat` 调 `load`（LISP 加载）无效，改为把目录加入支持搜索路径。
- 原 `make-hatch` 依赖 `entity:addhatch`（比例被固定为 40），改为 entmake 全参数可控。

## 3. 未实现（backlog）

- **P1 `hatch_panel` 快捷填充面板**：非模态面板（选项按钮 + 图案预览 + 比例/角度/颜色），
  面板内切换图案直接预览在选中填充上；需要 DCL 或 @lisp 面板框架支持。
- **P1 `hatch_key` 快捷键**：把常用图案绑定到单键（如 `A` = ANSI31、`S` = SOLID），
  选填充后按键即换图案；需 hook `grread` 循环，交互态下与 MCP 调用冲突需隔离。
- **P2 iMini 其余填充选项**：
  - 填充图案“原点对齐/图案平移”（改填充原点而不是移动填充）；
  - 渐变填充（`AcDbHatch` 渐变组码 450-470）；
  - 填充边界编辑（增删边界环、孤岛检测模式 75=0/1/2 切换）；
  - 按填充面积/颜色/图层统计（可复用 `at-hatch:hatch-pattern-info`）。
- **P2 点取内部点求边界**：现用 `bpoly` + 必要时缩放到该点；在超大图纸/关闭显示时仍可能失败，
  计划改为 `-BOUNDARY` 命令行 + 失败回退提示。
- **P2 图案资源编码**：内置 `.pat` 内容为 GBK，图案名由 `at-hatch:builtin-patterns` 映射为中文；
  如需支持用户自带中文名 `.pat`，要加文件名编码探测。
- **P3 单元测试**：把 12 个 AI 入口的回归脚本固化为 CAD 内脚本（当前靠 MCP 手工回归）。

## 4. 已知限制

- 填充布尔裁切依赖 `AcDbRegion`，非闭合或不共面的边界无法裁切（返回 nil 并提示）。
- `pline2solid` 按弦处理弧段（凸度不参与三角化），圆弧边界会变成直边铺满。
- `ai-hatch-move` 的 `scale` 对带嵌图定义的自定义图案同样有效（缩放填充对象本身，不改图案定义）。
- 本方言 `and`/`or` 只返回 `T`/`nil`，代码中所有取值的写法均用 `if`/`cond`。
- 本方言 `(nth n nil)` 会报 “consp nil”，凸度表等可能为空的列表取值均已加长度判断。
