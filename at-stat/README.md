# at-stat - 统计表格

统计图元信息并绘制表格输出。含两套命令：
- 原点坐标类统计（圆心半径、多段线点坐标）
- iMini 统计表格类命令（块/文字/面积/长度/形状统计、面积分摊、苗木表、输出 Excel、文字计算）

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 统计表格 → 圆心半径 | `(at-stat:stat-circle)` | 统计圆的圆心坐标和半径，生成表格 |
| 统计表格 → 多段线点坐标 | `(at-stat:stat-lwpl)` | 统计多段线各顶点坐标及间距，生成表格 |
| 统计表格 → 块统计表 | `(at-stat:block-list)` | 按块名汇总数量与外尺寸 |
| 统计表格 → 文字统计表 | `(at-stat:text-list)` | 按内容汇总 TEXT/MTEXT 数量、样式、字高 |
| 统计表格 → 面积标注 | `(at-stat:area-text)` | 每个封闭实体中心标注面积 |
| 统计表格 → 点选标面积 | `(at-stat:point-area)` | 点区域内一点，自动找边界并标面积 |
| 统计表格 → 面积分层统计表 | `(at-stat:by-layer-area)` | 按图层汇总封闭实体面积（×换算系数） |
| 统计表格 → 填充面积统计表 | `(at-stat:hatch-area)` | 按填充图案名称汇总面积与数量 |
| 统计表格 → 房间墙面面积统计 | `(at-stat:wall-area)` | 周长×层高，可扣门窗洞口 |
| 统计表格 → 长度分层统计表 | `(at-stat:total-length)` | 按图层汇总线型实体长度 |
| 统计表格 → 标注线段长度 | `(at-stat:text-length)` | 每条线段中点标注长度文字 |
| 统计表格 → 同形状统计 | `(at-stat:shape-list)` | 按类型+尺寸（长度/面积）分组计数 |
| 统计表格 → 标准层面积分摊表 | `(at-stat:area-analyze)` | 各区域面积 + 按比例分摊总面积 |
| 统计表格 → 文字输出Excel | `(at-stat:text2excel)` | 文字按行列排布导出 CSV/XLSX |
| Excel表格 → 导入CAD表格 | `(at-stat:excel2table)` | 读 Excel/CSV 已用区域绘制表格（线+文字，支持合并单元格） |
| 统计表格 → 文字计算 | `(at-stat:cale-txt)` | 求文字中数值表达式的和/积 |
| 统计表格 → 苗木统计表 | `(at-stat:mmtj)` | 苗木名称/规格/数量统计表 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-stat:stat-circle` | 选择所有圆，按 Y/X 蛇形排序编号，生成含 No./R/X/Y 的表格 |
| `at-stat:stat-lwpl` | 选择一条多段线，提取所有顶点坐标和相邻点间距，生成含 NO./X/Y/L 的表格 |
| `at-stat:get-wxh` | 获取多段线的宽 x 高字符串（最小包围矩形） |

### 统计工具（stat-tools / count / area / misc）

| 函数 | 说明 |
|------|------|
| `at-stat:block-data` / `at-stat:block-list` | 块数据汇总 / 交互绘制块统计表 |
| `at-stat:text-data` / `at-stat:text-list` | 文字数据汇总 / 交互绘制文字统计表 |
| `at-stat:shape-data` / `at-stat:shape-list` | 同形状数据汇总 / 交互绘制同形状统计表 |
| `at-stat:length-by-layer` / `at-stat:total-length` | 按图层长度汇总 / 长度分层统计表 |
| `at-stat:area-by-layer` / `at-stat:by-layer-area` | 按图层面积汇总 / 面积分层统计表 |
| `at-stat:mark-areas` / `at-stat:area-text` | 面积标注 |
| `at-stat:mark-point-areas` / `at-stat:point-area` | 点选标面积 |
| `at-stat:wall-area-data` / `at-stat:wall-area` | 房间墙面面积数据 / 统计表 |
| `at-stat:area-analyze-data` / `at-stat:area-analyze` | 标准层面积分摊数据 / 统计表 |
| `at-stat:mark-lengths` / `at-stat:text-length` | 线段长度标注 |
| `at-stat:text2excel-data` / `at-stat:text2excel` | 文字矩阵提取 / 输出 CSV-XLSX |
| `at-stat:eval-txt-num` / `at-stat:cale-txt` | 文字表达式安全求值 / 批量文字计算 |
| `at-stat:mmtj-data` / `at-stat:mmtj` | 苗木统计数据 / 苗木统计表 |
| `at-stat:excel2table-data` / `at-stat:excel-draw` / `at-stat:excel2table` | Excel/CSV 数据读取 / 表格绘制 / 交互导入 |

## AI 调用

供 agent（MCP `eval_lisp`）直接调用，全部无交互、不弹窗；参数缺省值在函数内补齐。

| AI 入口 | 参数 | 返回 |
|---|---|---|
| `at-stat:ai-block-list` | `(ss top-n out-file)` 选择集或 INSERT 列表 / 前 n / 输出文件 | `((块名 数量 WxH) ...)` |
| `at-stat:ai-text-list` | `(ss out-file)` 选择集或文字列表 / 输出文件 | `((内容 数量 样式 字高) ...)` |
| `at-stat:ai-shape-list` | `(ss tol)` 选择集或图元列表 / 分组容差 | `((类型 尺寸 数量) ...)` |
| `at-stat:ai-total-length` | `(layers out-file)` 图层名列表（nil=全部）/ 输出文件 | `((图层 长度) ...)` |
| `at-stat:ai-by-layer-area` | `(layers factor out-file)` 图层 / 换算系数（nil=0.0001）/ 输出文件 | `((图层 面积) ...)` |
| `at-stat:ai-area-text` | `(ents factor layer)` 封闭实体 / 换算系数 / 文字图层 | `((实体 面积 文字) ...)` |
| `at-stat:ai-point-area` | `(pts factor layer)` 点列表 / 换算系数 / 文字图层 | `((点 面积 边界 文字) ...)` |
| `at-stat:ai-wall-area` | `(ents height)` 房间轮廓列表 / 层高（nil=3000） | 二维矩阵（含洞口扣除/合计行） |
| `at-stat:ai-area-analyze` | `(zones shared-area table)` 区域点或图元 / 分摊面积 / 是否绘表 | 二维矩阵 |
| `at-stat:ai-text-length` | `(ents prefix suffix layer)` 线型实体 / 前后缀 / 文字图层 | `((实体 长度 文字) ...)` |
| `at-stat:ai-text2excel` | `(ents out-file rows cols)` 文字 / 输出文件 / 行列数 | 二维矩阵 |
| `at-stat:ai-cale-txt` | `(ents op result-pt)` 文字 / `"sum"`/`"prod"` / 结果放置点 | 数值 |
| `at-stat:ai-mmtj` | `(blk-names area-ent)` 苗木块名列表（nil=全部）/ 面积边界（nil=不追加） | `((名称 数量) ...)` |
| `at-stat:ai-hatch-area` | `(ents factor)` 填充表 / 换算系数（nil=1e-6） | `((图案 面积 数量) ...)` |
| `at-stat:ai-hatch-area-table` | `(ents pt factor)` 填充表 / 表格插入点 / 换算系数 | 表格 ename |
| `at-stat:ai-excel2table` | `(file pt sheet col-widths row-height text-height layer)` Excel/CSV 文件 / 插入点(左上角) / 工作表名（nil=第一张）/ 列宽表（nil=自动）/ 行高（nil=2×字高）/ 字高（nil=`@::scale 3.5`）/ 图层（nil=当前） | 表格图元 ename 表（线+文字）；失败 nil |

示例：

```lisp
(at-stat:ai-block-list (ssget "x" '((0 . "INSERT"))) 10 "d:/blk.csv")
(at-stat:ai-by-layer-area nil 0.0001 nil)
(at-stat:ai-point-area (list (getvar "viewctr")) 0.0001 "STAT")
(at-stat:ai-excel2table "d:/t.xlsx" '(100 50) nil nil nil nil "TABLE")
```

## 使用说明

- **圆心半径统计**：框选所有圆，自动按蛇形路径排序编号，点击放置表格位置
- **多段线点坐标统计**：选择一条多段线，自动编号顶点并生成坐标和间距表格
- **iMini 统计类命令**：选对象 → 输入换算系数/层高/容差 → 点表格插入点；面积类默认 0.0001（mm²→m²）

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-stat
```

或：

```
(@:package-install "at-stat")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-stat.lsp` | 主程序源码（圆统计、多段线统计） |
| `stat-tools.lsp` | 统计工具主文件：菜单注册（统计表格）+ 公共辅助（表格绘制/格式/文件输出） |
| `count.lsp` | 计数统计（块/文字/同形状/长度分层/面积分层）+ AI 入口 |
| `area.lsp` | 面积统计（面积标注/点选标面积/墙面面积/面积分摊/苗木统计）+ AI 入口 |
| `misc.lsp` | 杂项（标注线段长度/文字输出 Excel/文字计算）+ AI 入口 |
| `hatch-area.lsp` | 填充面积统计（按图案汇总面积/数量 + 表格）+ AI 入口 |
| `excel-import.lsp` | Excel→CAD 表格导入（读已用区域/合并单元格 + 绘制表格 + CSV 回退）+ AI 入口 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.0.7
- 分类: Common
- 网站: http://atlisp.cn
