# at-library 功能扩展计划

> @图库：在线图库（2.4 万符号）检索/插入 + 本地文件夹图库。
> 数据层在 `lib/src/library/`，本包负责交互与出图。

## 1. 已实现

| 能力 | 函数 | 文件 | 状态 |
|---|---|---|---|
| 在线图库搜索插入 | `at-library:insert` / `browse` / `insert-by-name` | at-library.lsp | ✅ |
| 标准图框 | `at-library:frame` | at-library.lsp | ✅ |
| 图例生成 | `at-library:legend` | legend.lsp | ✅ |
| 标准件明细表 | `at-library:bom` | bom.lsp | ✅ |
| 图库文件批量归一化 | `at-library:normalize-file` / `normalize-dir` | normalize.lsp | ✅ |
| 本地文件夹图库扫描 | `at-library:local-scan` / `ai-local-scan` | local.lsp | ✅ |
| 本地图库检索 | `at-library:local-search` / `ai-local-search` | local.lsp | ✅ |
| 本地图库插入 | `at-library:local-insert` / `ai-local-insert` / `local-insert-file` | local.lsp | ✅ |
| 幻灯片库名称读取 | `at-library:slb-names` / `ai-slb-names` / `slb-list` | slb.lsp | ✅ |
| 本地多根目录 | `at-library:local-roots-list` / `local-scan-multi` / `local-search-multi` | local.lsp | ✅ |
| 多库浏览与多选插入 | `at-library:local-browse` / `local-browse-insert` / `local-insert-items` | local.lsp | ✅ |
| 多根配置 | `at-library:local-roots` 配置 + `at-library:local-set-roots` | at-library.lsp / local.lsp | ✅ |

## 2. CAD 实测（2026-11-01，AutoCAD 24.1s / MCP）

测试目录：`.local-test/{trees,shrubs}/{ginkgo,maple}.dwg`

- `ai-local-scan`：返回 2 条，id 为 `shrubs/maple`、`trees/ginkgo`，路径正确。
- `ai-local-search "ginkgo" 5 root`：命中 `trees/ginkgo`，前缀匹配优先。
- `ai-local-insert "ginkgo" '(0 0) root 1.0 0.0`：生成 `INSERT`，块名 `ginkgo`，
  插入点 `(0 0 0)`，比例 1.0；删除块参照后图面恢复。
- `ai-slb-names`：合成 3 条目的 `.slb`（31 字节头 + 3×36 字节目录）→
  `(SLIDE1 SLIDE2 SLIDE3)`；文本来自明经 thread-113336 的 `gxl-slb2sldnames`，
  改为按 `(文件大小 - 31) / 36` 计算条目数，增加 EOF 保护与空格裁剪。

### 多根目录 / 多选插入（2026-11-01 追加）

测试目录：`/tmp/libA/{门/example}.dwg`、`/tmp/libB/{窗/example,窗/window,门/example}.dwg`
（`/tmp/libB/门/example.dwg` 与 libA 的 rel 相同，用于验证去重）。

- `(@:set-config 'at-library:local-roots "/tmp/libA;/tmp/libB")` →
  `(at-library:local-roots-list)` 返回 `("/tmp/libA/" "/tmp/libB/")`。
- `(at-library:local-scan-multi '("/tmp/libA" "/tmp/libB") 6)`：返回 3 条
  （`门/example` 取 libA 的、`窗/example`、`窗/window`），每条带 `root`；
  libB 里同 rel 的 `门/example` 被丢弃。
- `(at-library:ai-local-search "example" 10 '("/tmp/libA" "/tmp/libB"))`：返回 2 条，
  rel 为 `窗/example.dwg`、`门/example.dwg`，无重复。
- `(at-library:local-search-multi "门" 10 roots)`：返回 1 条（`门/example` 前缀命中）。
- `local-insert-items`（`local-browse` 选中后的插入路径）`'(0 0)` 插入 2 条：
  返回 2 个 INSERT ename，第 2 个插入点 x=58013.7（首块宽 52739.7 的 10% 间距），
  包围盒 `x 7075~59815` 与 `65089~117829` 不重叠。
- `(at-library:local-browse)` 在 `FILEDIA=0` 下直接返回 nil、不弹框、不占命令行；
  DCL 多选本身需人工点选（`FILEDIA=1`）。
- 旧接口：`ai-local-scan` 单目录返回 1 条、`local-search` 单目录命中 1 条，行为不变。

## 3. 未实现（backlog）

| 优先级 | 项目 | 说明 |
|---|---|---|
| P1 | 缩略图预览 | 读取 dwg 预览图（`vla-GetThumbnail` / DwgThumbnail）显示在选单里；需 DCL 或面板 |
| P2 | 同名 dwg 处理 | CAD 块表按名称唯一，不同目录同名文件会复用块定义；可加「目录前缀命名」选项 |
| P2 | 扫描缓存 | 大目录每次全量递归；可按 root + 目录 mtime 缓存索引到 `@::*prefix*/library-local.tsv` |
| P2 | 多根同 rel 的手动切换 | 同 rel 目前固定保留第一个根；可在 `local-browse` 里标注「其他根的副本」并允许切换 |
| P3 | 目录树导航 | 现在是一屏平铺（最多 50 条）+ 根目录别名，可补按目录逐级下钻 |

## 4. 已知限制

- 递归深度默认 6 层，跳过非 `.dwg` 文件；不跟随符号链接。
- 插入走 `block:insert`，dwg 直接作为块定义进入当前图，不复制文件、不转换版本。
- 文件名建议唯一；中文名在 GBK 环境下正常，UTF-8 环境需确认 CAD 代码页。
- 多根去重按 `rel`（相对根目录的路径）判断：`libA/门/门.dwg` 与 `libB/门/门.dwg`
  视为同一符号，只保留配置里靠前的根目录。
- `local-browse` 依赖 DCL：`FILEDIA=0` 时不会弹框，直接返回 nil；批量/Agent 场景请用
  `local-scan-multi` + `local-insert-items`。
