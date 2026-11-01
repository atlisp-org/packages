# 应用包功能扩展计划总索引

> 每包一份 `<pkg>/PLAN.md`。状态：⬜ 未开始 / 🔄 进行中 / ✅ 完成
> 优先级标记：P0 必做 / P1 重要 / P2 可选。工时为 PLAN 内汇总的粗估。

| # | 包 | 规模(lsp/defun) | 状态 | 核心扩展点（Top3） | 总工时 |
|---|----|----------------|------|-------------------|--------|
| 1 | acmecad2021 | 2/1 | ✅ | ext:launch 提取；菜单乱码修复；批量转换 CLI | ≈11h |
| 2 | agan-cal | 4/36 | ✅ | 运算符扩展；结果随层；Regexp→lib:re；距离/面积计算 | ≈12h |
| 3 | agan-vport-syn | 2/1 | ✅ | 下载死循环修复；启动/停止命令；LISP 降级同步 | ≈12h |
| 4 | aigc | 3/4 | ✅ | 图纸审查落地；生成超时反馈；删占位菜单(aigc:hello 未定义) | ≈17h |
| 5 | align-all | 2/2 | ✅ | vl-sort 替换 O(n²)；列对齐；自动行距；undo 统一 | ≈7h |
| 6 | align-array | 2/35 | ✅ | 未定义函数 S_PGON/c:ent_DQPL 修复(P0)；硬编码路径；布局空间支持 | ≈25h |
| 7 | al-plate | 3/3 | ✅ | dyndraw 落图+标注(P0)；转角构件死配置实现；展开数据表 | ≈14h |
| 8 | alt-run | 2/1 | ✅ | ext:launch 并入；下载失败提示；带参启动 | ≈2.5h |
| 9 | at-3d | 3/2 | ✅ | (in-package :cl) 包冲突(P0)；实体剖切；切片循环统一 | ≈12h |
| 10 | at-arch | 5/19 | ✅ | at-arch:hello 占位菜单(P0)；面积汇总表出图；房间批量命名 | ≈15h |
| 11 | at-block | 18/95 | ✅ | set-number 重复定义(P0)；动态块替换；备份文件残留(P0) | ≈25h |
| 12 | at-cnc | 4/14 | ✅ | vl-sort 比较函数恒真(P1)；多遍加工；刀具库 | ≈23h |
| 13 | at-color | 2/38 | ✅ | 改色按钮映射错乱(P0)；(exit)终止运行时(P0)；真彩→索引转换 | ≈21h |
| 14 | at-curve | 14/26 | ✅ | 悬空检查崩溃/漏检LINE(P0/P1)；spline2lwpl exit+顶点数(P0/P1)；fillet 三段去重 | ≈32h |
| 15 | at-dim | 11/21 | ✅ | true-value 置空非复原(P1)；cutting-symbol 三处拼写bug(P1)；尺寸链/批量对齐 | ≈32h |
| 16 | at-documents | 2/17 | ✅ | 22个.libdoc全部缺失(P0)；文档覆盖 lib/src(58 ns)；(exit)修复(P0) | ≈22h |
| 17 | at-elec | 2/8 | ✅ | @::*prefix* 未定义双崩(P0)；dyndraw 用法错误；配电箱回路统计 | ≈29h |
| 18 | at-hvac | 5/14 | ⬜ | | |
| 19 | at-lab | 20/39 | ⬜ | | |
| 20 | at-layer | 2/8 | ⬜ | | |
| 21 | at-layout | 9/19 | ⬜ | | |
| 22 | at-linetype | 3/6 | ⬜ | | |
| 23 | at-math | 12/74 | ⬜ | | |
| 24 | at-nlp | 4/9 | ⬜ | | |
| 25 | at-number | 3/3 | ⬜ | | |
| 26 | at-planning | 3/14 | ⬜ | | |
| 27 | at-plot | 6/23 | ⬜ | | |
| 28 | at-pm ⚠子模块 | 19/93 | ⬜ | | |
| 29 | at-purge | 3/61 | ✅ | 不同属性合并重线 mini_line_union2(P2)；空选忽略属性合并(P2)；打断并删除短线 mini_break_erase(P2) | ≈11h |
| 30 | at-select | 7/58 | ✅ | fastsel 沿线触碰/范围选项(P2)；链式支持 DIMENSION/SOLID/3DFACE(P2)；过滤引擎扩展(P2) | ≈10h |
| 31 | at-session | 3/14 | ⬜ | | |
| 32 | at-sidebar | 3/9 | ⬜ | | |
| 33 | at-stat | 3/3 | ⬜ | | |
| 34 | at-structure | 11/51 | ⬜ | | |
| 35 | at-text | 20/141 | ⬜ | | |
| 36 | at-validate | 2/24 | ⬜ | | |
| 37 | at-wsad | 3/5 | ⬜ | | |
| 38 | at-xdata | 3/6 | ⬜ | | |
| 39 | autohotkey | 2/1 | ⬜ | | |
| 40 | base | 29/159 | ⬜ | | |
| 41 | block-update | 2/4 | ⬜ | | |
| 42 | block-view | 4/338 | ⬜ | | |
| 43 | boundary | 3/6 | ⬜ | | |
| 44 | cmd-ext | 3/0 | ⬜ | | |
| 45 | composing | 4/7 | ⬜ | | |
| 46 | dev-tools ⚠子模块 | 23/158 | ⬜ | | |
| 47 | dim-leader | 3/75 | 🔄 | 写图安全(*error*/回滚/图层)；text-box 按 TBOX2 重算 72/73；布局 5/7/8 接 obstacles；选择范围+试运行 | ≈43h |
| 48 | doslib | 2/2 | ⬜ | | |
| 49 | everything | 3/1 | ⬜ | | |
| 50 | flange | 3/2 | ⬜ | | |
| 51 | fonts | 4/27 | ✅ | iMini style_any/hz/khz + font_list 复现 | ≈5h |
| 52 | game-wzq | 2/1 | ⬜ | | |
| 53 | gis-tools (无pkg.lsp) | 0/py | ⬜ | | |
| 54 | helloworld | 2/1 | ⬜ | | |
| 55 | landscape-plants | 2/1 | ⬜ | | |
| 56 | list-rec-wxh | 2/5 | ⬜ | | |
| 57 | lpsidemenu | 3/4 | ⬜ | | |
| 58 | mark-order | 3/4 | ⬜ | | |
| 59 | networkx | 5/3 | ⬜ | | |
| 60 | ocr | 3/1 | ⬜ | | |
| 61 | ole | 2/20 | ✅ | iMini image2ole + oleimage_crop 复现 | ≈4h |
| 62 | opendcl | 3/2 | ⬜ | | |
| 63 | pdftk | 3/16 | ⬜ | | |
| 64 | prefabricated-building | 7/17 | ⬜ | | |
| 65 | psk-tools | 31/561 | ⬜ | | |
| 66 | qrencode | 2/3 | ⬜ | | |
| 67 | quadrilateral | 3/3 | ⬜ | | |
| 68 | road-cross | 3/75 | ⬜ | | |
| 69 | route-of-hole2shape | 2/12 | ⬜ | | |
| 70 | saw-blade | 3/7 | ⬜ | | |
| 71 | sidebar | 2/4 | ⬜ | | |
| 72 | structure-tools | 3/2 | ⬜ | | |
| 73 | svg2dxf | 3/3 | ⬜ | | |
| 74 | xgjmtxcx | 3/1 | ⬜ | | |
| 75 | youyun-plot | 2/1 | ⬜ | | |

## 跨包主题（B10 收尾时统一复核）
- [x] agent 适配：at-xdata AI 入口（0.0.6 stable）、qrencode `ai-make`（0.0.9 release，exe 缺失时返回 nil）
- [x] 论坛计划执行：2026-11-01 `at-stat` Excel/CSV→CAD 表格（0.0.7，已发布 stable，xlsx COM 按用户要求跳过实测）
- [x] CAD 实测与发布：2026-11-01 `docs/cad-test-publish-20261101.md`
      （at-landscape 0.1.1 / at-hatch 0.1.2 / at-library 0.1.4 / at-stat 0.0.6 /
      at-layer 1.2.15 / at-session 0.1.9 / at-plot 0.0.13，stable 已上线）
- [x] 全站扫描：2026-11-01 `docs/forum-scan-20261101.md`
      （33 版块 / 1222 主题；已落地 at-stat 填充面积统计、at-layer TXT/文字建层）
- [x] 社区需求：2026-10-31 明经论坛分析 → `docs/forum-requirements-2026-10-31.md`
      （已落地：at-landscape 随机高程点/断面线/高程分层设色、at-hatch 图案样块表、
      at-library 本地文件夹图库（多根目录/多选插入）、atlispcc project 批量编译；
      共线点集/批量打印仍为 backlog）
- [ ] lib 协同：各包 PLAN 第 6 节候选函数汇总 → `lib/src/<ns>/<fn>.lsp`
- [ ] 跨包重叠：at-stat / list-rec-wxh / at-math(统计) / at-block(numbering) 等
- [ ] 编码治理：at-layout / at-elec / prefabricated-building 的 pkg.lsp 乱码
- [ ] 第三方署名包（需原作者确认）：agan-cal、agan-vport-syn、align-all、align-array、block-view、youyun-plot、lpsidemenu、list-rec-wxh、cmd-ext、ole、pdftk、doslib、everything、autohotkey、alt-run、acmecad2021、landscape-plants、structure-tools、game-wzq

## iMini 功能复现（2026-10）

> 总计划：`docs/imini-reproduce-plan.md`（iMini V8.7.1 的 410 个自定义命令 → 目标包映射）。
> 命名规则：一律 `at-` 前缀；已有同名 `at-*` 包则合并功能，沿用该包已有命名空间前缀。

| 包 | 类型 | 规模 | 状态 | 说明 |
|---|---|---|---|---|
| at-edit | 新建 | 3 lsp / 30 命令 | ✅ | 变换增强 14 + 多段线/转换/布尔/块 16；变换类 CAD 实测 14/14 |
| at-arch | 合并 | +6 lsp / 18 AI | ✅ | 轴网、墙、柱、梁、门窗（`at-arch:` 前缀） |
| at-block | 合并 | +2 lsp / 12 命令 | ✅ | 块重命名/插入点/替换/布块/缩放/统计；翻转与 Xref（`@block:`） |
| at-layer | 合并 | +1 lsp / 9 类命令 | ✅ | 图层状态、隐藏/隔离、实体锁定、清理 `$$` 层、层名大小写、按色分层、绘图顺序、提取器 |
| at-layout | 合并 | +4 lsp / 21 AI | ✅ | 布局改名/合并/分图/取图、视口比例/同步/外框/锁定/图层状态/变灰、布图（`@layout:`） |
| at-stat | 合并 | +4 lsp / 13 AI | ✅ | 块/文字/形状/长度/面积统计、分摊表、文字计算、Excel 输出 |
| at-text | 合并 | +4 lsp / 15 AI | ✅ | 全能编辑、批量改字、排版、连接、字体替换、属性互转、词库（`@text:`） |
| at-linetype | 合并 | +4 lsp / 9 AI | ✅ | 批量改线型、局部虚线、反转、线型树、形文件（mkshape 仅 .shp 文本） |
| at-dim | 合并 | +2 lsp / 14 命令 | ✅ | 标注移动/归位/对齐/合并/总标/检查/恢复、尺寸链、房间标注、多重引线；CAD 实测 14/14 |
| at-landscape | 新建 | 5 lsp / 10 命令 | ✅ | 灌木边缘、立面草、苗木标注与统计、面积、搭桥/挖路、里程、界址点；CAD 实测 5/5 |
| at-interior | 新建 | 6 lsp / 22 AI | ✅ | 柜类平面剖面、天花角线、灯具、板材截面、焊缝管线 |
| at-hatch | 新建 | 4 lsp / 12 命令 + 12 AI | ✅ | 快捷/超级填充、填充改参/移动、边界生成、批量分离、面域布尔裁切、SOLID 互转、图案提取/自定义、修复不显示（`at-hatch:` 前缀）；CAD 实测 12/12 |
| at-select / at-purge / at-color | 合并 | +3 lsp / 10 命令 | ✅ | 快速选择/获得选择集/链式/触碰选择，一键清理/清理短线/词典/合并重线，全能变色/按层变色 |
| at-pm / at-plot / ole / fonts / composing / at-math / at-number | 合并 | 🔄 | 🔄 | 图框合并、截图拆图、图片转 OLE、字形设置、矩形嵌套、文字计算、全能编号（子代理进行中） |

### 方言坑（已写入 AGENTS.md「方言限制」）
`let`/`let*` 不存在、无全局 `tan`、局部表不能绑 `t`/`nil`/`type`、`entity:make-lwpline` 不存在、
`(last '(1 2 3))` 返回 3、`entmakex` 不能含 nil、`and`/`or` 只返回 T/nil、`member` 不适用于字符串、
DIMENSION 的 11/70 组码需 `vla-put-TextPosition`。

### 自查工具（本轮新增，可复制进仓库）
- `check-lisp-pitfalls.py`：扫方言坑
- `fix-let.py`：`let` → `setq` 并自动补局部变量表
- `lisp-balance.py`：定位括号不平衡的具体函数
