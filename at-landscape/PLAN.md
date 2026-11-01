# at-landscape 功能扩展计划

来源：iMini V8.7.1 景观类命令（`/media/data/Public/raw/iMini/`，菜单 `imini.mnu` + `iMini工具说明.txt`）。
状态：⬜ 未开始 / 🔄 进行中 / ✅ 完成

## 已实现 ✅

| iMini 命令 | 本包函数 | 文件 | 状态 |
|---|---|---|---|
| mini_shrub_edge | `at-landscape:shrub-edge` / `ai-shrub-edge` | plant.lsp | ✅ |
| mini_grass | `at-landscape:grass` / `ai-grass` | plant.lsp | ✅ |
| mini_plant_text | `at-landscape:plant-text` / `ai-plant-text` | plant.lsp | ✅ |
| mini_mmtj / mmtj_hatch | `at-landscape:mm-list` / `ai-mm-list` | plant.lsp | ✅ |
| mini_tree_style 沿线布树 | `at-landscape:tree-line` / `ai-tree-line` | plant.lsp | ✅ |
| mini_area_landscape | `at-landscape:area` / `ai-area` | area.lsp | ✅ |
| mini_add_path | `at-landscape:add-path` / `ai-add-path` | path.lsp | ✅ |
| mini_remove_path | `at-landscape:remove-path` / `ai-remove-path` | path.lsp | ✅ |
| mini_lxlc | `at-landscape:mileage` / `ai-mileage` | path.lsp | ✅ |
| mini_jzd | `at-landscape:jzd` / `ai-jzd` | path.lsp | ✅ |
| 明经需求：范围内随机高程点 | `at-landscape:random-elevation` / `ai-random-elevation` | survey.lsp | ✅ |
| 明经需求：断面线及桩号 | `at-landscape:section-lines` / `ai-section-lines` | survey.lsp | ✅ |
| 明经需求：高程点分层设色 | `at-landscape:elev-color` / `ai-elev-color` | survey.lsp | ✅ |

> 2026-10-31 需求来源与取舍见 `../docs/forum-requirements-2026-10-31.md`。

## CAD 实测记录（2026-10-30，AutoCAD 24.1s / MCP）

- `ai-area`：ename 列表与单个 ename 均可，填充开/关正常，文字 `20 m2`
- `ai-add-path 2000 1000`：2 条边界 + 18 条桥板线
- `ai-remove-path 2000 "ANSI31"`：2 条边界 + HATCH；图案不存在时只画 2 条边界，无残留
- `ai-mileage 5000 0`：`K0+000 K5+000 K10+000 K15+000`
- `ai-jzd "JZ"`：4 组「圆 + 点 + JZ1..JZ4」

### survey.lsp 真机回归（2026-11-01，AutoCAD 24.1s / MCP）

| 用例 | 调用 | 实测结果 |
|---|---|---|
| 随机高程点 | `(at-landscape:ai-random-elevation ent 30 5.0 15.0 2)` | 60 个图元 = 30 POINT + 30 TEXT，30 点全部在闭合线内，Z 5.21~14.70，文字两位小数 |
| 断面线 | `(at-landscape:ai-section-lines ent 20000 10000 0)` | 6 条 LINE + 6 个 TEXT；线长 20000、与水平路线垂直；桩号 K0+000~K100+000 |
| 分层设色 | `(at-landscape:ai-elev-color ss nil)` | 30 个点按实际高程均分 5 段改色（颜色 1/2/3/5/6） |
| 自定义区间 | `(at-landscape:ai-elev-color ss '((5 10 1) (10 15 3)))` | 30 个点只出现颜色 1/3，按区间正确分色 |

未测：退化范围（点数 <3 的点表）。当前实现会在 `loop-pts` 返回 nil，理论安全。

## 未实现（backlog）

| 优先级 | 项目 | 说明 |
|---|---|---|
| P2 | 苗木图例表 | iMini 的苗木表带图例缩略图（需块预览），当前只有名称/数量/面积 |
| P2 | 铺装分色规则 | `ai-area` 目前随机浅色，未按铺装材料表取色 |
| P2 | 与 landscape-plants 包合并评估 | 该包是苗木图块库，本包是绘图命令，暂不合并 |

## 已知限制

- 面积换算默认 mm²→m²（1e-6），米制图纸需显式传系数。
- 里程标注按曲线等距取点，圆角处桩号位置按弧长计算。
- 词库仅内置灌木/乔木名各数十条，可在 `shrub-names.txt` / `tree-names.txt` 追加。
- 随机高程点为均匀分布，未做最小间距控制；点位密集时高程文字可能重叠。
- 断面线按弧长等距布置，圆角处不单独加密；断面线半长由调用方给定，不做图幅裁剪。
