# dim-leader 功能扩展计划

> @lisp 标注。共圆心两条半圆弧 → 匹配旁侧单行文本 → 多段线引线；9 种布局(0-8)。v0.0.1，自研，依赖 base。

## 1. 概览
- 版本 0.0.1 / 分类 标注 / 依赖 base / 自研
- 规模：3 个 .lsp（`dim-leader.lsp` + `chip.lsp` + `channels.lsp`）共 66 个 defun，16 项 `@:define-config`
- 入口：`dim-leader:run`（dim-leader.lsp:1179）、`c:dimleader` / `c:dl`（:1248/:1249）、菜单 标注 > 圆心引线标注（:1251）
- 流水线：`find-centers`（全图 ARC → 圆心）→ `match-texts`（圆心 ±offset 一对一匹配 TEXT）→ `layout*`（9 种布局选点）→ `place-text*` + `make-polyline`（写图）
- 外部依赖：`@:define-config` / `@:get-config` / `@:add-menu`（kernel）、`textbox`（原生）；`text-width` 已由 `dim-leader:text-fallback-width` 取代（I1 已修）
- 卫生：本目录混入 5 个测试 dwg（未跟踪，.gitignore 不覆盖 dwg）；缺 README.md

## 2. 现状功能清单
| 分组 | 位置 | 说明 |
|---|---|---|
| 纯几何工具 | :28-268 | span-to-half / half-circle-p / center-key / setd / box-hit-p / pt-in-rect / seg-seg-hit / seg-rect-hit / mk-placed / eval-place / setnth / list-nil / dedupe-pts / seg-cross-boxes / route-cross / text-box / text-fallback-width |
| 正交布线（布局6） | :149-494 | orient / seg-seg-hit2 / seg-box-hit2 / route-hit-box(es) / route-hit-route / route-keepouts / route-safe-p / route-length / route-bends / unique-nums / expand-nums / thin-nums / chip-route-cost / chip-consider / chip-find-route / chip-route-attempt / routes-valid-p / chip-side-plans / layout-chip |
| 识别与匹配 | :532 / :566 | find-centers（全图 ARC → 恰好 2 条半圆弧的圆心）、match-texts（圆心 ±offset 一对一匹配，按距离贪心） |
| 9 种布局 | :610 / :746 / :798 / :862 / :1000 / :1057 / :371 / chip.lsp:47 / channels.lsp:150 | 0 本地错开（含走廊绕行）· 1 右侧表格 · 2 分区表格 · 3 二维分区+空白矩形 · 4 四向 · 5 分向轨道（**未用 obstacles**）· 6 芯片式左右分区（正交通道搜索 + keepout，最终硬校验路由）· 7 紧凑双侧（chip.lsp：左右对半、上下中位拆分、3 列交错、颜色循环）· 8 通道布线（channels.lsp：四带 + left-edge 独占泳道，引线零共线重叠） |
| 落图 | :1134 / :1147 / :1163 / :1179 | place-text（10/11/50，不动 72/73）· place-text-j（+72/73）· make-polyline · run |

返回契约不统一（布局 0 返回 15 元素、1/3 返回 6 元素、4/5/6 返回 8 元素），`run` 用 `(if (nth 7 r) …)` 分派（:1235）——见 R2。布局 6 另返回 `(圆心 图元 锚点X 锚点Y 0 水平对齐 垂直对齐 折线点表)`。

### 布局 6（芯片式左右分区，新增）
- 按圆心 X 均值分左右两组；各自按 Y 升序，按纵向可容纳行数 `rowCap` 分列，列距 `maxw+gap`、行距 `maxh+gap`。
- 右侧文本左对齐(hj=0)、锚点 `edge+col*colPitch`；左侧右对齐(hj=2)、锚点 `edge-col*colPitch`；垂直居中(vj=2)。文字盒整体位于全部圆心/障碍之外，天然不压图元。
- 引线：正交通道搜索，候选通道来自 文本盒边缘/圆心/终点 展开的 x、y 值 + 上下轨道；代价 = 路径长度 + 折弯惩罚；已布引线按 `clearance` 膨胀为 keepout 递增避让。
- 硬校验：`routes-valid-p` 复核 引线×文字盒、引线×引线 均不相交；任一失败则整批不落图，错误写入 `dim-leader:*chip-error*`。

### 布局 7（紧凑双侧，chip.lsp，新增）
- 圆心按 X 对半分配左右（数量接近）；每侧按 Y 中位数分上/下两组，文字带绕点群中心 Y 居中。
- 文字独占行、K 列交错；行距=字高+gap、列距=字宽+gap；3 段正交引线 `圆心 →(横) lane →(竖) ty →(横) 文字内缘`。
- 每条引线独占 lane（lane 按序分配），故引线共线重叠极少；`dim-leader:side-distance` 控制走廊宽度（间距）。
- 引线颜色循环：`dim-leader:leader-colors`（ACI 空格分隔字符串），`run` 中对生成的 LWPOLYLINE 逐个 `entmod 62`。
- 配置：`dim-leader:side-distance`（默认 10）、`dim-leader:leader-colors`（默认空）。
- Python 参考实现：`chip_layout.py`（读数据 JSON → 布局 JSON，支持 `--optimize` 做 Y 向折线降交叉）。

### 取向自适应（实验，未入包）
- 针对"L 形"点群：上臂文字竖排 90° 放上/下两侧、竖臂水平文字放左/右两侧，形成 L 形包络；与"区域切分（互不重叠矩形带）+ left-edge 通道"配合。
- 已能保证**文本框零重叠**；但**引线共线重叠未能与 L 形同时消除**——见 F7（已由布局 8 解决）。

### 布局 8（通道布线，channels.lsp，新增，F7 落地）
- 圆心按到点群外接矩形四边（上/下/左/右）的最近距离分到 4 个带。
- 每个带沿带方向按 `字高+gap` 分列成网格（列数与圆心沿带方向顺序一致）；上/下带文字竖排（rot90, hj=1 vj=2 居中），左/右带文字水平（rot0, hj=0/2 vj=2）。
- 每行在靠点群一侧留泳道区，泳道由 `dim-leader:ch-left-edge`（left-edge 算法）分配：同一泳道上的区间互不重叠，不同泳道 y/x 不同。
- 引线 = 圆心 →(深度) 泳道 →(沿带方向) 列 →(深度) 文字边，全正交。竖直/水平段只落在 圆心坐标 或 列坐标 上：圆心坐标基本互异，同行列坐标互异，不同行泳道区不重叠 → 引线零共线重叠。
- 配置：`dim-leader:lane-gap`（默认 1.0，相邻平行引线的通道间距）、`dim-leader:channels-blocks`（**默认 10**：文本带分块数，>=2 时块间留贯穿缝隙，长引线穿缝以少穿文字盒；0/1=不分块）、`dim-leader:channels-lines`（仅不分块时有效；0=自动分行）、`dim-leader:channels-columns`（仅不分块时有效；1=最小位移保序列位，0=等距）。
- 分块模式（`channels-blocks>=2`，默认）：每条带沿带方向分 N 块，块间缝隙宽度按该缝深行引线数自适应（独占通道，保证同缝不共线）；深行引线走最近缝（缝内独占 x、每行水平段 left-edge 泳道）；各带外扩方向背离相邻带，避免角部相碰。实测（507 点）`blocks=10`：**盒重叠 0、共线 0、正交、穿越文字盒 263→约 38、平均引线 163→223、CAD 4.3s**。
- 不分块模式（`channels-blocks=0/1`）：见上文“每带泳道 + 最小位移列”，平均引线更短（163），但穿越文字盒多（约 263）。
- 引线配色：`dim-leader:leader-colors`（ACI 空格分隔，默认 `1 3 5 2 4 6 30 40`），`run` 对每条生成的 LWPOLYLINE 按序 `entmod 62`。相邻引线异色，密集引线可辨。空串则不配色。
- 实测（507 点 `高程点图_局部.dwg`）：文本框重叠 0、引线共线重叠 0、非正交段 0、CAD 用时约 3.8s，配色 8 色循环，`save` 为 `dim-leader-channels.dwg`。标注与既有文字（obstacles）之间的重叠为 0；全图残余 34 对相交全部来自 obstacles 彼此（原图既有，与布局无关）。
- 已知代价：多行/多列带中，深层引线的竖直段会穿过内层文字盒（穿越不计入 F7 验收项，仅视觉）。
- 优化尝试与结论（`/tmp/opencode/layout_opt.py`、`layout_ft*.py`、`layout_iso.py`，Python 对照）：
  - **最小位移保序列位（PAVA）**：**已采用**（`channels-columns=1`）。水平带列位不设边界、竖直带列位约束在点群 y 范围内以避免角部相碰。平均引线 182→**163**、最大 414→**386**，盒/共线仍为 0。
  - **feedthrough 布线**（引线经列间缝隙上行，绕开文字盒）：穿越文字盒 258→50，但平均引线 182→257（需先到带底泳道再上行），综合更乱，且列间缝隙需要更大 pitch、横向带无法纳入点群宽度、角部冲突，暂不采用。
  - **全局最小位移列（不设边界）**：平均引线可到 143，但水平带列位外溢与左/下带在角部产生 61 处盒重叠，弃用。
  - **行数启发式**（每带 `round(sqrt(m/20))` 行）：穿越 258→215，平均长不变（182），收益有限，作为 `channels-lines` 手动项保留。
  - **文本带分块 + 块间贯穿缝隙**（你的建议，已原型 `/tmp/opencode/layout_blocks*.py`）：把每条带沿带方向切成 NB 块，块间留缝隙，深行引线走最近缝隙（缝内竖直段 left-edge 独占 x，行内水平段 left-edge 独占泳道）。实测（507 点）：
    - NB=6/GW=12：穿越文字盒 **263→93**，但平均引线 163→**182**（+12%），且有 9 处共线（缝内轨道数偶超缝宽）。
    - 缝隙轨道数 ≈ 深行引线数，单条缝隙需很宽，总宽 ≈ 深行数×LANE，导致文字带外移、引线变长；块数越多缝隙越多、带越宽。
    - 结论：在本图（深行引线多）分块反而更费空间、更长，视觉更乱，**暂不采用**；若以后以“零穿越文字盒”为第一优先级，可再启用并配自适应缝宽。
  - 结论：当前“四带 + 每带泳道 + 最小位移列（竖直带限界）”为正交/零共线/零盒重叠且引线较短的均衡解。残余代价：多行/多列带中深层引线的竖直段会穿过内层文字盒（穿越不计入 F7 验收项，仅视觉）。
  - **已实施的优化（分块模式）**：`ch-band-blocks` 原先未按带方向排序（与不分块模式不一致），列序随 ssget 序致引线变长。新增 `dim-leader:ch-sort-order` 无损稳定排序（`vl-sort` 会删重复元，故以原始序号打破平局），在 4 带入口处按 run 坐标升序。实测（507 点 `高程点图_局部.dwg`）：`maxL 573→477（-17%）`、`cross 40→38`、`avgL 223→225`、`ov/col/nonorth 恒为 0`。LISP 与 Python 复刻 507 条引线 2342 个坐标点逐点误差 <1e-12。
  - 附带加固：`ch-left-edge` 与 `ch-band-h/v` 的 `vl-sort` 改为带序号打破平局（防同坐标/同区间被静默丢弃）；`ch-isotonic` 均值改用浮点除法；订正“外扩方向”注释为沿带方向错开（角部避让）。

## 3. 新功能（NEW）
- **F1 选择范围 + 试运行（P1）**：当前只能全图跑（3 次 `ssget _X`），大图既慢又难控。入口可选 当前选择集/当前空间/全图；加试运行（只高亮候选，不 entmod/entmake）。
- **F2 引线落点规范化（P1）**：末端直接落在文字插入点（:373/:430/:493/:633），压字且无水平着陆段。改为停在盒外 gap 处 + 水平着陆段。
- **F3 配置对话框（P2）**：10 项配置只能手改 config。复用 `(@::edit-config-dialog)`（at-dim:33 先例），按布局联动显隐（columns/rotate 仅布局 0，zones 仅布局 2）。
- **F4 圆心判定放宽（P2）**：:192 硬要求恰好 2 条半圆弧，同心双圆（4 弧）整组丢弃。增加弧数与半径容差配置。
- **F5 MTEXT 支持（P2）**：当前静默丢弃（:217）。需处理 71/72 组码与多行盒。
- **F6 标注表导出（P2）**：把 (圆心, 文本) 成对结果落成表格/CSV（`table:make`）。
- **F7 L 形通道布线器（引线零重叠，P1）**：**已实现为布局 8**（`channels.lsp`，配置 `dim-leader:layout=8`）。在"区域切分（互不重叠矩形带，文本框零重叠）"基础上，为每条引线分配 **独占泳道**（left-edge，间距 = `dim-leader:lane-gap`），达到**引线零共线重叠**。
  - 现状：`dim-leader-orient7.dwg`（实验）已做到 L 形 + 文本框零重叠；引线仍共享带内通道线段 → 共线重叠（约 1.2k 处）。
  - 难点：带间包络协调（角部让位/走廊）、唯一通道分配（left-edge 开关盒）、单行/单列带过宽会互相碰撞。
  - 方案：区域切分（四带）+ 每带 left-edge 泳道；先在 Python 验证（`orient_channels.py`），再移植 LISP（`channels.lsp`）。
  - 验收：文本框重叠 0、引线共线重叠 0、间距≥配置值、正交、可在 507 点图内可接受时间内完成 —— **全部达成**（CAD 实测 507 点：0/0/正交，3.4s）。

## 4. 改进（IMPROVE）
- **I1 `text-width` 未定义（P0）**：**已修** —— 新增 `dim-leader:text-fallback-width`（:495，优先 `text:gettextwidth`，失败按字符数估算），原 7 处调用全部替换。
- **I2 找不到空位也强行落位（P0）**：:330 `(if (> o 400) (setq found t))` 未找到空位也置真，:347-349 仍生成 best → 静默压字。需独立 placed-ok 标志，失败保留原位并计数上报。
- **I3 图层语义与实现不符（P0）**：配置 :11 写 为空时用图元原图层，实现 :812 省略组码 8 → 落到 CLAYER。应取文本的 `(assoc 8 dat)` 传入 `make-polyline`；加 `tblsearch LAYER` 存在性与锁定位校验（at-dim:41 先例）。
- **I4 障碍盒忽略对齐与非 0/90° 旋转（P0）**：:151-168 假定 72=0/73=0、角度只取 0/90；而 `textbox` 返回的是 相对插入点、未旋转 的局部偏移，本身已含 72/73、倾斜角、宽度因子。且 :781-797 不归一化 72/73，已有居中/右对齐文字会落在非预期位置。按官方 TBOX2 公式 `world = p0 + R(50)·localOffset` 重写。
- **I5 无 `*error*` / 无 undo 组 / 无回滚（P0）**：:820-879 边算边写；图层锁定或不存在时 entmod/entmake 静默失败仍计入 nlead（:877），提示语虚高。应先算后写、保存原始 10/11/50/72/73 供回滚、`vl-catch-all` 包裹并统计成功/失败。
- **I6 布局热路径无空间预筛（P0）**：:51-57 `box-hit-p` 遍历全部 placed，而 placed 初值是全图未匹配文本（:832-848）。最坏 `labels × (2·columns+1) × rot × levels(≤401) × |obstacles|`，5000 障碍时单标签 10⁸ 级比较。最小修法：照 `eval-place`:96 补 minx/maxx 粗筛。
- **I7 兜底落位产生退化折线（P1）**：:685 把锚点设成圆心 → `dedupe-pts` 后只剩 1 顶点，LWPOLYLINE 顶点不足，entmake 可能 nil 而 nlead 仍自增。
- **I8 走廊后处理无条件执行（P1）**：:363-380 每标签试 5 条走廊 × 全量 placed；应挂到 `avoid-leader` 开关下。
- **I9 选了 MTEXT 却丢弃（P1）**：:211 过滤 TEXT,MTEXT，:217 按 0 组码丢掉 MTEXT（且 :781-797 组码对 MTEXT 不适用）→ 白做 entget。过滤串改 TEXT。
- **I10 静默丢标签 / 静默回退（P1）**：:475 `(>= cy b0) (< cy b1)` 使 cy 恰等 yTop 的项无带可归；:591 回退 (0,0)、:631 回退 jc，均可能压图且无提示。需计数上报。
- **I11 走廊评分把自己算成穿越（P1）**：:364 boxes 含本标签盒，末点在盒内/边界（`pt-in-rect` 含边界）→ 每条路线恒定 +1，还污染 :377 的段数 tie-break。应排除自身。
- **I12 中心判定静默丢弃 + 键碰撞（P1）**：:192 要求恰好 2 弧；`center-key`:42 只留 3 位小数，不同圆心可能撞键并被 :238 member 误判同一中心。键改整数对或容差聚类。
- **I13 数值边界（P1）**：gap ≤ 0 → rowPitch/colPitch = 0 → :415/:482/:489 `fix (/ ... 0)` 报错；nth 访问 14 元素元组无越界保护。
- **I14 补 docstring（P1）**：30 个函数全无 docstring（lint 26 条 missing_doc）。须写清几何函数入参与返回格式（placed 14 元素；布局结果 6/8/15 元素并存），父规范要求 说明+返回值。
- **I15 测试 dwg 混入包目录（P1）**：`dim-leader-*.dwg` 等 5 个测试图留在包目录，.gitignore 不覆盖 dwg（本仓库 dwg 是受管资源，已跟踪 27 个），`git add .` 会误提交。移到仓库根 `.tmp/`。
- **I16 未用变量/参数（P2）**：x0（:439）、row/zx0/zy0/cc（:503）、obstacles（:698，布局 5 完全不避让已有文字）、route（:820）；:587 的 d 未声明进私有变量表；Cx/Cy/XR/XL/YU/YD（:642/:699）大写局部名不合惯例。
- **I17 配置说明过期（P2）**：:17 只写布局 0/1/2，代码支持 0-5（:849-860）；zones 仅作用于布局 2，布局 3 硬编码 3（:855）。
- **I18 命令别名（P2）**：c:dimleader / c:dl 缺 princ（lint 已报）；`dl` 与包名/命名空间不一致。
- **I19 文档与版本（P2）**：缺 README.md（本包是唯一没有的普通包）；版本仍 0.0.1，6 种布局与 10 项配置无说明。

## 5. 重构（REFACTOR）
- **R1 抽 `dim-leader:text-metrics`（P0）**：取 11/10 + 40 + 41 + `textbox` 兜底 的同一段逻辑在 :151-168、:260-277、:391-403、:443-454、:509-521、:643-655、:702-718 共 7 处复制。抽 `(dim-leader:text-metrics ent)` → (rx ry w h layer)，6 个布局各减约 15 行，并把 I1 兜底收敛到唯一一处。
- **R2 统一布局结果契约（P1）**：改统一记录（如 `(t x y rot hj vj pts)` alist），在文件头写死契约，消除 `(nth 7 r)` 分派。
- **R3 布局拆分 + 名称注册表（P2）**：5 个布局占 :387-770（近半文件）却靠魔数 0-5 选择。拆为 dim-leader-layout*.lsp（同步改 pkg.lsp `:FILES`），配置改填名字。
- **R4 障碍集空间索引（P0）**：obstacles 按 x 分桶/栅格，查询只看相邻桶，并按引线走廊 y 范围预裁剪。配合 I6 把布局阶段从 10⁸ 降到 10⁵ 量级。
- **R5 纯几何核心可测化（P1）**：16 个工具函数均为纯函数 → 补单测（参照 base/test.lsp 的 push-var/pop-var；语法层用 `atlisp-lint --sbcl`）。先补测试再动布局。
- **R6 算图与写图分离（P1）**：`run` 拆成 计算布局 → 预览/确认 → 提交写图，为 F1 试运行与 I5 回滚提供落点。

## 6. 验证（本包无测试 runner）
- 静态：`./validate-all.sh dim-leader`（仓库根执行）；当前基线 0 错误 / 139 警告（含 channels.lsp 15 条，已消除 channels.lsp 全部 missing_doc）/ 信息若干，目标 missing_doc 与 unused 警告清零。
- CAD 实测（新功能/逻辑改动必做）：`@I dim-leader` 后布局各跑一张图（本目录测试 dwg 可作底图），重点回归 I2/I3/I4/I5/I6/I7 与布局 6 的硬校验、布局 8 的零重叠。
- 布局 8 实测脚本（本次）：`/tmp/opencode/orient_channels.py`（Python 参考）+ 通过 atlisp-mcp 在 `高程点图_局部.dwg` 上 `(dim-leader:run)`，导出后校验 `box_overlap=0 / leader_collinear=0 / non_orth=0 / 3.4s`，另存 `dim-leader-channels.dwg`。

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| ~~I1 text-width 兜底~~ 已修 | P0 | — | 7 处已收敛到 text-fallback-width |
| I2 找不到空位仍落位 | P0 | 0.3h | — |
| I3 图层语义 + 校验 | P0 | 0.8h | 需 CAD 实测 |
| I4 盒计算 + 72/73 | P0 | 1.5h | 官方 TBOX2 公式，需实测 |
| I5 error/回滚/失败计数 | P0 | 1.5h | 依赖 R6 |
| I6 box-hit-p 空间预筛 | P0 | 0.5h | 最小修法 |
| R1 text-metrics | P0 | 1.0h | 减 90 行重复 |
| R4 障碍集空间索引 | P0 | 2.5h | 与 I6 配套 |
| I7-I12 正确性小修 | P1 | 3.5h | 逐项 0.2-1h |
| I13 数值边界 | P1 | 0.5h | — |
| I14 docstring ×30 | P1 | 1.0h | 清 26 警告 |
| I15 测试 dwg 清理 | P1 | 0.2h | 移 .tmp/ |
| F1 选择范围+试运行 | P1 | 2.0h | 依赖 R6 |
| F2 引线着陆段 | P1 | 1.5h | — |
| R2 统一结果契约 | P1 | 1.5h | — |
| R5 纯核心单测 | P1 | 3.0h | 后续改动的前提 |
| R6 算图/写图分离 | P1 | 1.5h | F1/I5 的前提 |
| I16-I19 清理/文档 | P2 | 2.0h | — |
| F3 配置对话框 | P2 | 0.8h | — |
| F4 圆心判定放宽 | P2 | 1.0h | — |
| F5 MTEXT 支持 | P2 | 1.5h | — |
| F6 标注表导出 | P2 | 2.0h | — |
| ~~F7 L 形通道布线器（引线零重叠）~~ 已实现(布局8) | P1 | — | channels.lsp：四带+left-edge 泳道；507 点 CAD 实测 0/0/正交 |
| R3 布局拆分/注册表 | P2 | 2.5h | — |

合计 ≈37h（F7 已完成，余 ≈33h）；P0 小计 8.4h（建议先做），P1 14.7h，P2 9.8h。
