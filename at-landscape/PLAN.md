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

## CAD 实测记录（2026-10-30，AutoCAD 24.1s / MCP）

- `ai-area`：ename 列表与单个 ename 均可，填充开/关正常，文字 `20 m2`
- `ai-add-path 2000 1000`：2 条边界 + 18 条桥板线
- `ai-remove-path 2000 "ANSI31"`：2 条边界 + HATCH；图案不存在时只画 2 条边界，无残留
- `ai-mileage 5000 0`：`K0+000 K5+000 K10+000 K15+000`
- `ai-jzd "JZ"`：4 组「圆 + 点 + JZ1..JZ4」

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
