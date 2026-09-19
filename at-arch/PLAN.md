# at-arch 功能扩展计划

> 建筑设计工具（天正 TCH_SPACE 重度依赖）。房间定位/面积汇总/窗地比检查、车位插入与编号、门窗统计、专篇图块插入（防水/绿建/公建/住宅说明）。

## 1. 概览
- 版本 0.1.0 / 分类 建筑工程 / 依赖 base / 自研
- 规模：at-arch.lsp 约 380 行 17 个函数 + lib.lsp 1 个 + 资源 dwg×7（车位/无障碍/机械车位/绿建专篇/防水专篇/河北省登记表）
- 数据源：TCH_SPACE（code/name/useArea/ExtWinArea 经 VLA 读取）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-arch:setup | at-arch.lsp:41 | 配置对话框（tmp-search-str "@arch"） |
| at-arch:insert-parking / -accparking / -machineparking | :45/:58/:67 | 下载+dyndraw 插入车位类 dwg（三函数结构重复） |
| at-arch:insert-block | lib.lsp:2 | 通用图块模板下载+插入（公建/住宅/防水/绿建专篇） |
| at-arch:parking-numbering | :78 | 框选车位 → pickset:sort（顺序+8 倍容差）→ 属性 NUMBER* 递增编号，parking-curr-number 记忆 |
| at-arch:locate-unnamed-space | :108 | 定位未命名房间（TCH_SPACE 组码 1="房间"）zoom w |
| at-arch:locate-space / menu-locate-space | :114/:135 | 按编号（数字）或名称（*通配）定位房间，显示总净面积并选集 |
| at-arch:w/space | :139 | 窗地比检查：TCH_SPACE 层 SPACE → VLA 读 ExtWinArea/useArea → 按 at-arch:w/ua 表（卧室1/7 办公1/5 走廊1/10…）判定，不合规标红(12)/合规无色 |
| at-arch:check-w/ua | :183 | 房间名 wcmatch 匹配窗地比标准表 |
| at-arch:sel-same-space / sel-same-name | :195/:218 | 选同面积（41 容差 0.005/42 容差 10）/同名称房间 |
| at-arch:onoff-spacearea | :238 | 房间面积显隐切换 |
| at-arch:sum-spacearea | :248 | 总建筑面积（useArea 求和） |
| at-arch:door-window-stat | :265 | 门窗统计：框选 → 按型号分组计数 → make-table 出表 |
| at-arch:make-table | :297 | 纯 text 手画表格（序号/型号/数量/总计），未用 table:make |
| at-arch:export-space-csv | :346 | 房间 CSV 导出（编号/名称/面积） |
| at-arch:area-summary | :375 | 按名称分类汇总建筑面积 |

## 3. 新功能（NEW）
- **F1 房间批量命名（P1）**：按规则（前缀+序号/层号-房间号）批量改 TCH_SPACE 组码 1/2，配合 locate-space 形成"命名→定位"闭环。
- **F2 面积汇总表出图（P1）**：area-summary 现仅命令行输出；出 ACAD_TABLE + CSV 双通道（复用 export-space-csv 通道），按层（Z 或命名）分组。
- **F3 窗地比报告出图（P1）**：w/space 的 dyndraw 表格改为可落图 table:make + 不合规房间清单 CSV。
- **F4 车位面积/数量统计（P2）**：按车位类型（普通/无障碍/机械）统计数量与面积，出表（配合规划指标 at-planning）。
- **F5 专篇插入增强（P2）**：insert-block 系列支持参数化（层数/面积等属性回填）、河北省登记表按项目信息自动填表。
- **F6 房间净高/面积联动检查（P3）**：读天正房间属性做合规检查扩展（净高、疏散宽度）。

## 4. 改进（IMPROVE）
- **I0 占位菜单（P0）**：menus.lsp 注册 `at-arch:hello`（未定义）×4，点击即错；删除 menus.lsp（at-arch.lsp:11 已有完整菜单）。
- **I1 车位插入三函数重复（P1）**：insert-parking/acc/machine 仅 dwg 名不同（各 10 行重复）→ 参数化 `at-arch:insert-parking "车位"`，与 lib.lsp 的 insert-block 合并为一个函数。
- **I2 下载等待（P1）**：`(@:alert "正在下载…")(sleep 5)` 固定 5 秒，大文件不够/小文件浪费；改轮询 findfile + 超时。
- **I3 窗地比标准表配置化（P1）**：at-arch:w/ua 全局硬编码（:185）→ `@:define-config` 列表，地方标准（如河北省）可替换。
- **I4 make-table 改用 table:make（P2）**：:297 手画 text 表格（列宽/字高写死 `* draw-scale 8/40`）→ `table:make`（lib 已有），列宽自适应。
- **I5 VLA 读取健壮性（P2）**：TCH_SPACE 属性用 `vlax-get-property` 直读，天正版本差异可能抛错；包 `vlax-3d-object` 前判断 + 回退组码。
- **I6 河北省登记表 dwg 未入菜单（P2）**：资源存在但无插入入口，补菜单项或从 :FILES 移除。

## 5. 重构（REFACTOR）
- **R1 房间数据访问层（P1）**：code/name/useArea/ExtWinArea 在 6 处重复 VLA 读取 → 提取 `(at-arch:space-info ent)` 返回 alist，统一容错。
- **R2 统计输出统一（P2）**：door-window-stat / area-summary / export-space-csv 的"分组计数"逻辑重复 → `(at-arch:group-count lst key-fn)`。

## 6. lib 协同
- 复用：`table:make`（I4/F3）、`pickset:sort`（已用）、`string:*`（CSV 转义）、`excel:*`（F2 可选）
- 跨包：at-planning（规划指标）、at-pm（图纸目录）共享"统计→表格→CSV"管线

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I0 占位菜单 | P0 | 0.1h | — |
| I1 插入参数化 | P1 | 0.5h | — |
| I2 下载轮询 | P1 | 0.5h | — |
| R1 数据访问层 | P1 | 1.5h | — |
| F1 批量命名 | P1 | 2h | — |
| F2/F3 报告出图 | P1 | 各 1.5h | — |
| I3 标准表配置 | P1 | 0.5h | — |
| I4 table:make | P2 | 1h | — |
| F4/F5 | P2 | 各 2h | — |
| I5/I6 | P2 | 1h | — |
| R2 | P2 | 1h | — |
