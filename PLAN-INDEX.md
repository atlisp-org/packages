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
| 13 | at-color | 2/33 | ✅ | 改色按钮映射错乱(P0)；(exit)终止运行时(P0)；真彩→索引转换 | ≈21h |
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
| 29 | at-purge | 3/57 | ⬜ | | |
| 30 | at-select | 7/39 | ⬜ | | |
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
| 51 | fonts | 3/11 | ⬜ | | |
| 52 | game-wzq | 2/1 | ⬜ | | |
| 53 | gis-tools (无pkg.lsp) | 0/py | ⬜ | | |
| 54 | helloworld | 2/1 | ⬜ | | |
| 55 | landscape-plants | 2/1 | ⬜ | | |
| 56 | list-rec-wxh | 2/5 | ⬜ | | |
| 57 | lpsidemenu | 3/4 | ⬜ | | |
| 58 | mark-order | 3/4 | ⬜ | | |
| 59 | networkx | 5/3 | ⬜ | | |
| 60 | ocr | 3/1 | ⬜ | | |
| 61 | ole | 2/10 | ⬜ | | |
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
- [ ] lib 协同：各包 PLAN 第 6 节候选函数汇总 → `lib/src/<ns>/<fn>.lsp`
- [ ] 跨包重叠：at-stat / list-rec-wxh / at-math(统计) / at-block(numbering) 等
- [ ] 编码治理：at-layout / at-elec / prefabricated-building 的 pkg.lsp 乱码
- [ ] 第三方署名包（需原作者确认）：agan-cal、agan-vport-syn、align-all、align-array、block-view、youyun-plot、lpsidemenu、list-rec-wxh、cmd-ext、ole、pdftk、doslib、everything、autohotkey、alt-run、acmecad2021、landscape-plants、structure-tools、game-wzq
