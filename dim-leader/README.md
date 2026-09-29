# dim-leader - 圆心引线标注

对"共圆心的两个半圆弧"的圆心，用多段线引线标注其旁的单行文本；文本自动分层、
可多列横向错开、并可按需旋转 90°，使标注文字互不重叠，同时尽量缩短引线。

典型场景：测量图里成片的高程点标注（`161-1261.63` 这类"点号-高程"）挤在一起，
需要把文字挪开、加上引线指回原圆心。

## 包信息

| 属性 | 值 |
|------|----|
| 标识名 | `dim-leader` |
| 显示名 | 圆心引线标注 |
| 版本 | 0.0.1 |
| 分类 | 标注 |
| 语言 | CHS |
| 作者 | VitalGG |
| 邮箱 | vitalgg@gmail.com |
| 网站 | http://atlisp.cn |
| 依赖 | `base` |
| 命令 | `DIMLEADER`、`DL` |

## 工作原理

```
① 找圆心   全图 ARC → 按圆心分组 → 恰好 2 条且各跨 180° 的圆弧 → 取其圆心
② 配文本   TEXT 的对齐点(组码 11，缺失取 10) 落在 圆心±offset 处 → 一对一贪心匹配
③ 排布局   按所选布局算法为每个标签选锚点与引线折点
④ 落图     改写 TEXT 的 10/11/50(/72/73) + 逐条 entmake LWPOLYLINE 引线
```

第 ④ 步会把匹配到的文本**移动**到新位置，原始位置不再保留；未参与标注的 TEXT
会作为障碍物参与避让。

## 菜单

在 @lisp 面板的"圆心引线标注"分类下，按布局编号分项（免去手工改 `layout` 配置）：

| 菜单项 | 调用 | 说明 |
|--------|------|------|
| 0 本地错开 | `(dim-leader:run-layout 0)` | 在原位附近逐层/逐列找空位，引线最短 |
| 1 右侧表格 | `(dim-leader:run-layout 1)` | 全图右侧排成列×行网格，引线走行/列间隙 |
| 2 分区表格 | `(dim-leader:run-layout 2)` | 按 y 分带，每带排自己的小表格，引线比 1 短 |
| 3 二维分区 | `(dim-leader:run-layout 3)` | 先栅格化占位，再按二维分区找最近的空白矩形放表格 |
| 4 四向 | `(dim-leader:run-layout 4)` | 标签可放圆心的右/左/上/下，直线引线四向散开 |
| 5 分向轨道 | `(dim-leader:run-layout 5)` | 按重心分四向，每条引线独占一条轨道 |
| 6 芯片式左右分区 | `(dim-leader:run-layout 6)` | 左右分区 + 正交通道搜索，**大图慎用，见已知限制** |
| 7 紧凑双侧 | `(dim-leader:run-layout 7)` | 左右均衡、文字带绕点群居中、3 列交错、引线循环配色 |
| 8 通道布线 | `(dim-leader:run-layout 8)` | 四带 + 每带独占泳道，**引线零共线重叠**（推荐） |
| 参数设置… | `(@::edit-config-dialog)` | 打开配置对话框改下面的配置项 |

命令行等价写法：`(dim-leader:run-layout 8)`；不带参数则用当前配置跑
`(dim-leader:run)` / `DL`。

## 9 种布局怎么选

| 布局 | 引线特征 | 文字重叠 | 适用 |
|------|----------|----------|------|
| 0 本地错开 | 3 段正交，可带绕行走廊 | 0（但见已知限制） | 零散点、要保持"就近" |
| 1 右侧表格 | 4 段正交，独占水平轨道 | 0 | 整齐成表的成果表 |
| 2 分区表格 | 4 段正交 | 0 | 纵向分布广的带状点群 |
| 3 二维分区 | 4 段正交，走无字横通道 | 少量 | 点群密集且形状不规则 |
| 4 四向 | 2 点直线 | 0（但见已知限制） | 希望文字向四向散开 |
| 5 分向轨道 | 4 段正交，独占轨道 | 0 | 大范围点群 |
| 6 芯片式 | 正交通道搜索 + keepout | 0 | **仅小图**（见已知限制） |
| 7 紧凑双侧 | 3 段正交，每条独占 lane | 0 | 兼顾紧凑与可读，配色区分引线 |
| 8 通道布线 | 4~5 段正交，四带 + 泳道 | 0 | **默认推荐**，密集点群 |

## 配置项

通过 `(@:get-config 'dim-leader:xxx)` / `(@:set-config 'dim-leader:xxx 值)` 访问，
或用菜单里的"参数设置…"。

| 配置键 | 默认值 | 说明 |
|--------|--------|------|
| `dim-leader:layout` | `0` | 布局编号，见上表 |
| `dim-leader:layer` | `""` | 引线所在图层；为空时用文字图元原图层（见已知限制） |
| `dim-leader:offset` | `2.0` | 文本相对圆心的水平匹配距离 |
| `dim-leader:match-tol` | `0.5` | 文本匹配容差 |
| `dim-leader:gap` | `1.0` | 文字分层/分列的间隙 |
| `dim-leader:columns` | `4` | 布局 0 允许横向错开的最大列数 |
| `dim-leader:avoid-leader` | `0` | 布局 0 是否让文字避开引线（1 开，明显变慢） |
| `dim-leader:zones` | `8` | 布局 2 的分带数 |
| `dim-leader:rotate` | `1` | 是否允许把部分文本旋转 90° |
| `dim-leader:leader-width` | `nil` | 引线宽度，nil 为细线 |
| `dim-leader:side-distance` | `10.0` | 布局 7：文字带距点群边界的距离 |
| `dim-leader:leader-colors` | `"1 3 5 2 4 6 30 40"` | 引线循环颜色（ACI，空格分隔；空串不配色） |
| `dim-leader:lane-gap` | `1.0` | 布局 8：相邻平行引线的通道间距 |
| `dim-leader:channels-lines` | `0` | 布局 8：每带行/列数，0=按范围自动 |
| `dim-leader:channels-columns` | `1` | 布局 8：列位算法，1=最小位移保序（引线更短），0=等距 |
| `dim-leader:channels-blocks` | `10` | 布局 8：文本带分块数，>=2 时块间留贯穿缝隙 |

## 函数列表

| 函数 | 文件:行 | 说明 |
|------|---------|------|
| `dim-leader:find-centers` | dim-leader.lsp:537 | 全图 ARC → 共圆心双半圆弧的圆心表 |
| `dim-leader:match-texts` | dim-leader.lsp:571 | 圆心与旁侧 TEXT 的一对一贪心匹配 |
| `dim-leader:layout` | dim-leader.lsp:613 | 布局 0 本地错开 |
| `dim-leader:layout-table` | dim-leader.lsp:751 | 布局 1 右侧表格 |
| `dim-leader:layout-table-zone` | dim-leader.lsp:803 | 布局 2 分区表格 |
| `dim-leader:layout-zone2d` | dim-leader.lsp:867 | 布局 3 二维分区 |
| `dim-leader:layout-chip` | dim-leader.lsp:376 | 布局 6 芯片式左右分区 |
| `dim-leader:layout-4dir` | dim-leader.lsp:1005 | 布局 4 四向 |
| `dim-leader:layout-lanes` | dim-leader.lsp:1062 | 布局 5 分向轨道 |
| `dim-leader:layout-compact` | chip.lsp:58 | 布局 7 紧凑双侧 |
| `dim-leader:layout-channels` | channels.lsp:402 | 布局 8 通道布线 |
| `dim-leader:place-text` | dim-leader.lsp:1139 | 移动文本（10/11/50） |
| `dim-leader:place-text-j` | dim-leader.lsp:1152 | 移动文本并设水平/垂直对齐（加 72/73） |
| `dim-leader:make-polyline` | dim-leader.lsp:1168 | 生成 LWPOLYLINE 引线 |
| `dim-leader:run` | dim-leader.lsp:1184 | 主命令：识别 → 匹配 → 布局 → 落图 |
| `dim-leader:run-layout` | dim-leader.lsp:1267 | 按布局编号执行（菜单项调用） |
| `dim-leader:menu-items` | dim-leader.lsp:1275 | 生成菜单项表 |
| `dim-leader:add-menu` | dim-leader.lsp:1284 | 注册菜单项 |
| `dim-leader:text-box` | dim-leader.lsp:515 | 文本世界包围盒 |
| `dim-leader:text-fallback-width` | dim-leader.lsp:500 | `textbox` 不可用时按字符数估算宽度 |
| `c:dimleader` / `c:dl` | dim-leader.lsp:1260 | 命令行别名 |

几何与布线工具（`box-hit-p`、`seg-rect-hit`、`eval-place`、`route-*`、`chip-*`、
`ch-left-edge`、`ch-isotonic` 等）共 40 余个，见源码。

## 依赖

- `base`：kernel 与配置系统（`@:define-config` / `@:get-config` / `@:add-menus` / `@::set-config`）
- `textbox`（AutoLISP 内建）：取文本真实宽度，字体度量必需
- `text:gettextwidth`（可选）：`text-fallback-width` 的首选回退

## 源文件

| 文件 | 行数 | 内容 |
|------|------|------|
| `pkg.lsp` | 13 | 包清单（文件名、分类、依赖） |
| `dim-leader.lsp` | 1295 | 配置、几何工具、识别匹配、布局 0-6、落图、菜单 |
| `chip.lsp` | 118 | 布局 7 紧凑双侧 |
| `channels.lsp` | 462 | 布局 8 通道布线 |
| `PLAN.md` | 141 | 功能扩展计划、已知缺陷与优先级 |

## 使用示例

```lisp
;; 用布局 8 跑（菜单里点"8 通道布线"等价于此）
(dim-leader:run-layout 8)

;; 让文本压引线的情况少一些：关掉 90° 旋转、加大列数
(@:set-config 'dim-leader:rotate 0)
(@:set-config 'dim-leader:columns 6)
(dim-leader:run)

;; 布局 8 换更紧凑的排布：分块 + 等距列位
(@:set-config 'dim-leader:channels-blocks 4)
(@:set-config 'dim-leader:channels-columns 0)
(dim-leader:run)
```

## 已知限制

| 问题 | 表现 | 说明 |
|------|------|------|
| 布局 6 大图不可用 | 507 点需数小时，且常返回"无可用正交通道" | 代价是 `标签数 × 通道候选 × (障碍盒+keepout)`，约 n^1.7 |
| 布局 0 / 4 为 O(n³) | 507 点布局 4 约 3.8 s；布局 0 单次即超 30 s | `box-hit-p` 遍历全部已放置项，缺空间索引（PLAN.md I6/R4） |
| 布局 0 找不到空位仍落位 | 507 点实测 203 处文字盒重叠 | `(> o 400)` 强行置位后仍生成 `best`（PLAN.md I2） |
| 布局 8 深层引线穿文字盒 | 507 点实测约 190 处 | 分块缝隙的设计取舍，不计入验收项，仅视觉 |
| `layer` 为空时落 CLAYER | 引线没跟着文字原图层走 | 实现里省略了组码 8（PLAN.md I3） |
| 只支持 0°/90° 旋转 | 斜排文字的盒算不准 | `text-box` 只按 0/90 分支（PLAN.md I4） |
| MTEXT 被静默丢弃 | 选了 MTEXT 却不做标注 | 匹配阶段只收 TEXT（PLAN.md I9） |
| 角度组码按弧度 | 换 CAD 可能要改 | 本 CAD 的组码 50/51 以弧度计，`place-text` 按此写入 |

`PLAN.md` 里还有回滚、undo 组、试运行、配置对话框等条目，尚未实现。

## 基准测试

包内带一套 AutoLISP / Python 对照工具，可在同一份输入上量各布局的执行效率
（`layouts_py.py` 是 9 种布局的 Python 逐句实现）。

| 脚本 | 作用 |
|------|------|
| `bench_src.py` | 用 `dwg2dxf`+`ezdxf` 从真实图纸抽圆心/文本，产出 `bench_src.json` |
| `bench_scene.py` | 在 CAD 内用 `entmakex` 重建基准场景 |
| `bench_data.py` | 从 CAD 抽取布局输入（文本宽取 CAD 的 `textbox`）到 `data.json`；`--current` 直接用当前打开的图纸 |
| `bench_ab.py` | 在 CAD 内逐布局测 AutoLISP 耗时 |
| `layouts_py.py` | 9 种布局的 Python 实现，可测耗时与规模曲线 |
| `test_run.py` | 在 CAD 上对指定 dwg 跑识别+布局计时+写图 |
| `dump_result.py` | 导出运行结果（文本盒+引线折点） |
| `verify_dwg.py` | 用 ezdxf 独立校验输出图，不依赖包内代码 |

实测（507 点，真实图纸 `test.dwg`，字体 HBtxt，AutoLISP 为 min of 3）：

| 布局 | AutoLISP | Python | 倍数 | 复杂度 |
|------|---------:|-------:|-----:|--------|
| 0 本地错开 | 未测得 ※ | 3 375 ms | — | ~n³ |
| 1 右侧表格 | 15–31 ms | 8.4 ms | 2–4× | ~n |
| 2 分区表格 | 31 ms | 7.4 ms | 4.2× | ~n |
| 3 二维分区 | 407 ms | 36 ms | 11× | ~n |
| 4 四向 | 3 813 ms | 684 ms | 5.6× | ~n³ |
| 5 分向轨道 | 15–31 ms | 6.3 ms | 2–5× | ~n |
| 7 紧凑双侧 | 140 ms | 8.9 ms | 16× | ~n |
| 8 通道布线 | 594 ms | 19.0 ms | 31× | ~n |
| 6 芯片式 | 大图不可用 | 同 | — | ~n^1.7 |

几点说明：

- ※ 布局 0 单次就超过 MCP 的 `cad.stallTimeout` 窗口（≈30 s）而测不到。等价的
  507 点合成场景（文字较窄）下实测 **25 266 ms**。
- 15–31 ms 的双值是真实抖动：这几个布局已接近 `MILLISECS` 计时与 62 ms
  `entget`+`textbox` 度量基线的量级，重测会跳档。
- Python 侧刻意与 LISP 逐句对照（含 `vl-sort` 保序、`fix` 截断、`cons/reverse`
  顺序），**不做任何优化**，因此是"同算法不同解释器"的对比。
- 布局 6 的代价是 `标签数 × 通道候选 × (障碍盒+keepout)`：4 个点就要 2.2 s
  （LISP）/ 6.3 s（Python），16 个点 188 s，507 点外推约 14 小时。

## 许可证

VitalGG / 免费使用（`pkg.lsp` 中 `:OPENSOURCE 0`、`:FREE T`）。
