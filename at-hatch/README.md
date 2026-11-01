# @lisp 填充（at-hatch）

填充工具包：快捷填充、超级填充、填充参数修改、填充移动对齐、由填充生成边界、
批量分离填充、全能裁刀裁填充、SOLID 与填充互转、图案提取与自定义图案、修复填充不显示。
复现 iMini 填充类命令。

- 包名：`at-hatch`（菜单组「填充工具」，分类：通用，依赖 `base`）
- 版本：0.1.2

## 安装

CAD 命令行：`@I at-hatch`，或

```lisp
(@:package-install "at-hatch")
```

菜单：`填充工具`（12 项）+ `图案预览`（3 项），与下表命令一一对应。

## 命令

| 命令（菜单） | 交互入口 | 说明 |
|---|---|---|
| 快捷填充 | `at-hatch:quick-hatch` | 选边界（实体或区域内部点）→ 图案/比例/角度 → 生成填充 |
| 超级填充 | `at-hatch:superhatch` | 多个封闭区域各生成一个填充，区域内孔洞自动作内边界 |
| 选现有图案填充 | `at-hatch:hatch-select` | 选已有填充，重新指定图案/比例/角度（回车保持原值） |
| 填充移动对齐 | `at-hatch:hatch-move` | 选填充后平移 / 旋转 / 缩放（基点在提示中指定） |
| 由填充生成边界 | `at-hatch:hatch-boundary` | 由填充边界环生成闭合多段线（弧段保留凸度） |
| 批量分离填充 | `at-hatch:hatch-isolate` | 多环填充拆成每环一个独立填充 |
| 全能裁刀裁填充 | `at-hatch:clip` | 用闭合边界裁切填充（保留内/外可选），面域布尔实现 |
| SOLID 变填充 | `at-hatch:solid2hatch` | SOLID 实体转成图案填充（保留图层与颜色） |
| SOLID 满铺多段线 | `at-hatch:pline2solid` | 闭合多段线范围满铺 SOLID（耳切三角化） |
| 填充图案提取输出 | `at-hatch:hatch-out` | 扫描图中填充的图案名，把图案定义写成 `.pat` |
| 自定义填充 | `at-hatch:custom-pattern` | 输入图案名与定义行，生成 `.pat` 并可直接用于填充 |
| 修复填充显示 | `at-hatch:fix-pattern` | 修复图案未载入、比例为零、图案名为空导致的不显示 |
| 图案预览表 | `at-hatch:pattern-table` | 把内置/标准图案各画一个样块并标名称，直接在当前图预览 |
| PAT 文件预览 | `at-hatch:pat-preview` | 选任意 `.pat` → 选图案 → 两角点/比例/角度，按真实尺寸画到当前视口 |
| 清除预览 | `at-hatch:pat-erase` | 删除 `PAT-PREVIEW` 图层上的全部预览图元（含预览表与 PAT 预览） |

## 内置图案

包内自带 `.pat` 图案（资源文件名为 ASCII，避免不同代码页下找不到文件），
按图案名使用即可，图案定义会随填充一起写入，不依赖支持路径：

| 图案名 | 文件 |
|---|---|
| 大理石1 | `marble1.pat` |
| 石纹2 | `stone2.pat` |
| 木地板条1 | `woodfloor1.pat` |
| 木纹面1 | `woodgrain1.pat` |
| 人字砖面1 | `herringbone1.pat` |
| 有缝标准砖 | `brickjoint.pat` |
| PARQUET | `PARQUET.PAT` |
| ENDSHAKE | `ENDSHAKE.PAT` |

标准图案（`SOLID`/`ANSI31`~`ANSI63`/`BRICK`/`CONCRETE`/`GRID`/`LINES`/`DOTS`/
`GRASS`/`CROSS`/`DENSE`/`DASHED`）无需 `.pat` 文件，可直接使用。

## AI 调用

以下入口**无交互**（不弹窗、不取点），可直接由 MCP `eval_lisp` 调用；
成功返回图元名/图元表，失败返回 `nil` 并用 `princ` 说明原因。

| 函数 | 参数 | 说明 / 示例 |
|---|---|---|
| `at-hatch:ai-hatch` | `boundary pattern scale angle / layer color` | 由边界创建填充。`boundary` 可为闭合图元、图元表、选择集、点表、点表列表或区域内部点。`(at-hatch:ai-hatch (car (entsel)) "ANSI31" 2.0 45.0 nil 7)` |
| `at-hatch:ai-superhatch` | `boundary pattern scale` | 每个封闭区域各一个填充，孔洞自动作内边界，返回填充表。`(at-hatch:ai-superhatch (pickset:to-list (ssget)) "ANSI31" 2.0)` |
| `at-hatch:ai-hatch-select` | `hatch-ent pattern scale angle` | 改已有填充的图案/比例/角度（`nil` 保持原值），返回结果填充表。`(at-hatch:ai-hatch-select (car (entsel)) "ANSI37" 4.0 15.0)` |
| `at-hatch:ai-hatch-move` | `hatch-ent op param` | `op` 为 `"move"`（位移 `(dx dy)` 或两点）/`"rotate"`（角度或 `(基点 角度)`）/`"scale"`（比例或 `(基点 比例)`）。`(at-hatch:ai-hatch-move h "rotate" '((0 0) 45.0))` |
| `at-hatch:ai-hatch-boundary` | `hatch-ent / layer` | 由填充生成闭合多段线，返回多段线表（弧段保留凸度）。`(at-hatch:ai-hatch-boundary (car (entsel)) nil)` |
| `at-hatch:ai-hatch-isolate` | `hatch-ent` | 多环填充拆成每环一个独立填充（原填充删除）。`(at-hatch:ai-hatch-isolate (car (entsel)))` |
| `at-hatch:ai-clip` | `hatch-ent clip-boundary / inside` | 面域布尔裁切，`inside` 非 nil 保留内部，返回新填充。`(at-hatch:ai-clip h (car (entsel)) t)` |
| `at-hatch:ai-solid2hatch` | `solids pattern scale` | SOLID 转图案填充（保留图层/颜色，原 SOLID 删除）。`(at-hatch:ai-solid2hatch (pickset:to-list (ssget '((0 . "SOLID")))) "ANSI31" 1.0)` |
| `at-hatch:ai-pline2solid` | `pline-ent` | 闭合多段线满铺 SOLID（耳切三角化，弧段按弦处理，保留原边界）。`(at-hatch:ai-pline2solid (car (entsel)))` |
| `at-hatch:ai-hatch-out` | `out-file` | 扫描图中填充图案名，写出 `.pat`，返回文件全路径。`(at-hatch:ai-hatch-out "C:/tmp/patterns.pat")` |
| `at-hatch:ai-custom-pattern` | `name lines / pat-file` | 生成并载入自定义图案 `.pat`；`lines` 为定义行字符串表或数值表。`(at-hatch:ai-custom-pattern "斜线" '("0,0,0,0,10,1,-1" "90,0,0,0,10,1,-1") nil)` |
| `at-hatch:ai-fix-pattern` | `hatch-ent / min-scale` | 修复不显示的填充（补图案定义行/改比例/改实体），返回修复后的填充表。`(at-hatch:ai-fix-pattern (car (entsel)) 1.0)` |
| `at-hatch:ai-pattern-table` | `names pt cell-w cell-h cols scale` | 图案样块表（边框 + 填充 + 名称），不弹 DCL。`names` 为 nil 时画全部内置 + 标准图案。`(at-hatch:ai-pattern-table nil '(0 0) 90 60 6 20)` |
| `at-hatch:ai-pat-preview` | `file pattern p1 p2 scale angle` | 任意 `.pat` 中的图案按真实尺寸画到两角点矩形内，图层固定 `PAT-PREVIEW`、颜色随层，图案定义随填充写入（不依赖支持路径）。`pattern` 为 nil 取文件第一个图案。`(at-hatch:ai-pat-preview "/tmp/t.pat" "P1" '(0 0) '(100 100) 1.0 0.0)` |

常用底层函数（可复用）：`at-hatch:make-hatch`（entmake 建填充）、
`at-hatch:hatch-recs`（读填充边界环，保留凸度）、`at-hatch:pattern-defs`（读 `.pat` 定义行）、
`at-hatch:parse-pat`（读 `.pat` 第一个图案）、`at-hatch:parse-pat-all`（读 `.pat` 全部图案段）、
`at-hatch:triangulate`（耳切三角化）、`at-hatch:boundary->loops`（各种边界输入归一）。

## 实现要点

- 填充创建走 `entmake`（多段线路径 + `42` 凸度），不需要临时边界图元，弧段不变形。
- 非标准图案把 `.pat` 定义行（`78`/`53`/`43`/`44`/`45`/`46`/`79`/`49`）随填充写入，
  即使图案未载入支持路径也能正常显示；找不到 `.pat` 定义的图案名会自动退回 `ANSI31`
  （避免生成“看不见”的填充）。
- 实体填充只写 `70 = 1` 与图案类型，不写比例/角度（部分 CAD 会判为非法 DXF）。
- 裁切用面域布尔（`0` 并集 / `1` 交集 / `2` 差集），结果面域直接作填充边界。
- 程序内不使用 `let`、不使用全局 `tan`，局部变量一律走形参/局部变量表。

## 文件

| 文件 | 内容 |
|---|---|
| `at-hatch.lsp` | 菜单注册 + 公共辅助（图案解析、填充 DXF 构建/读取、几何） |
| `create.lsp` | 快捷填充、超级填充、SOLID 变填充、SOLID 满铺多段线 |
| `edit.lsp` | 改填充参数、填充移动、生成边界、批量分离、裁切、修复显示 |
| `pattern.lsp` | 图案提取输出、自定义图案 |
| `preview.lsp` | 图案预览表、任意 `.pat` 实尺预览与清除 |
| `*.pat` | 内置图案资源 |
