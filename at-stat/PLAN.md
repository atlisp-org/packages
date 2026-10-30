# at-stat 功能扩展计划

来源：iMini V8.7.1「统计表格」类命令（`/media/data/Public/raw/iMini/`，菜单 `imini.mnu` + `iMini工具说明.txt`）。
状态：⬜ 未开始 / 🔄 进行中 / ✅ 完成

## 已实现 ✅

### 原有功能（at-stat.lsp）

| 菜单项 | 本包函数 | 状态 |
|---|---|---|
| 圆心半径 | `at-stat:stat-circle` | ✅ |
| 多段线点坐标 | `at-stat:stat-lwpl`（辅助 `at-stat:get-wxh`） | ✅ |

### iMini「统计表格」映射（stat-tools / count / area / misc）

| iMini 命令 | 本包函数 | AI 入口 | 状态 |
|---|---|---|---|
| mini_text2excel | `at-stat:text2excel` | `at-stat:ai-text2excel` | ✅ |
| mini_block_clist | `at-stat:block-list` | `at-stat:ai-block-list` | ✅ |
| mini_text_list | `at-stat:text-list` | `at-stat:ai-text-list` | ✅ |
| mini_area_text | `at-stat:area-text` | `at-stat:ai-area-text` | ✅ |
| mini_area_analyze | `at-stat:area-analyze` | `at-stat:ai-area-analyze` | ✅ |
| mini_point_area | `at-stat:point-area` | `at-stat:ai-point-area` | ✅ |
| mini_wall_area | `at-stat:wall-area` | `at-stat:ai-wall-area` | ✅ |
| mini_text_Length | `at-stat:text-length` | `at-stat:ai-text-length` | ✅ |
| mini_bylayer_area | `at-stat:by-layer-area` | `at-stat:ai-by-layer-area` | ✅ |
| mini_total_Length | `at-stat:total-length` | `at-stat:ai-total-length` | ✅ |
| mini_shape_list | `at-stat:shape-list` | `at-stat:ai-shape-list` | ✅ |
| mini_cale_txt | `at-stat:cale-txt` | `at-stat:ai-cale-txt` | ✅ |
| mini_mmtj | `at-stat:mmtj` | `at-stat:ai-mmtj` | ✅ |

文件分工：

| 文件 | 内容 |
|---|---|
| `stat-tools.lsp` | 菜单注册（菜单组「统计表格」）+ 公共辅助（`to-str`/`fmt`/`take`/`chunk`/`make-table`/`to-csv`/`write-xlsx`/`write-table-file`/几何判定等） |
| `count.lsp` | 计数统计（块/文字/同形状/长度分层/面积分层）+ AI 入口 |
| `area.lsp` | 面积统计（面积标注/点选标面积/墙面面积/面积分摊/苗木统计）+ AI 入口 |
| `misc.lsp` | 杂项（标注线段长度/文字输出 Excel/文字计算）+ AI 入口 |

## 已知限制

- 本轮仅通过 `atlisp-lint`（0 error）与括号平衡/方言静态检查，**未在 CAD 中实机验证**；首次使用建议在试验图纸上跑一遍。
- HATCH 周长按边界路径直线段求和（忽略凸度 42 组码），为近似值。
- 面积默认换算系数 0.0001（mm²→m²），可在命令中改。
- `at-stat:ai-area-analyze` / `at-stat:ai-point-area` 采用「最内层包含边界」判定，嵌套区域需逐点选取。
- 墙面面积统计的洞口扣除：LINE/曲线取长度，INSERT 取包围盒 X 宽，均按「宽 × 层高」扣减。
- XLSX 输出依赖 Excel COM（`excel:*`），无 Excel 时 `at-stat:write-table-file` 回退为 CSV。

## 待扩展（backlog）

| 优先级 | 项目 | 说明 |
|---|---|---|
| P1 | 统计表模板化 | 表头/列宽/文字高度可配置（现为固定 `at-stat:make-table`） |
| P1 | 结果回写图纸 | 统计结果写回为表格实体（ACAD_TABLE）而非纯文字/线 |
| P2 | 3DSOLID 面积/体积 | 需 `vla-get-Volume` / `vla-get-Area`，各 CAD 兼容性待测 |
| P2 | 面积分摊多方案 | 支持按面积比、按户数比、自定义系数分摊 |
| P2 | 苗木统计规格分组 | 按块属性（规格/胸径）分组统计，现仅按块名计数 |
| P2 | 字段联动 | 统计表用 FIELD 关联源对象，改图后自动更新 |
