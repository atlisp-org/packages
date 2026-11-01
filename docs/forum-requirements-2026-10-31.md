# 明经论坛需求分析（2026-10-31）

> 数据源：`bbs.mjtd.com`（明经CAD社区，Discuz! X3.4 / GBK）首页「最新主题」，
> 抓取时间 2026-10-31 20:30（GMT+8）。
> 目的：从社区真实讨论中提取 CAD 内可落地的功能需求，对照本仓库现状找出缺口，
> 按「需求热度 × 实现成本 × 与本项目定位的契合度」排序。

## 1. 筛选口径

- 只统计**能在 CAD 内用 LISP/包实现**的需求；纯软件下载、破解、非 CAD 话题不计。
- 「现有覆盖」以仓库代码为准：`packages/<pkg>/`、`lib/src/<ns>/`、`atlispcc/`。
- 热度取首页显示的「浏览/回复」数，仅作相对排序参考。

## 2. 需求信号与覆盖对照

| # | 论坛主题（2026-10 下旬） | 热度 | 现有覆盖 | 差距判断 | 优先级 |
|---|---|---|---|---|---|
| 1 | 图库浏览器 V2 / 好图库 V1.6 / 0.1M 单文件图库 / 不挑 CAD 的图库 | 4 帖合计 ≈2800 浏览 | `at-library`（在线索引 2.4 万符号 + 图例/明细表/归一化） | **方向不同**：社区要「本地文件夹即图库、免导入、多 CAD」；现有是在线索引。可新增本地目录扫描/预览，不必重做在线库 | P1 |
| 2 | 多段线范围内输入高程区间随机生成高程点 | 101 浏览 | 无 | 测绘/景观基础工具缺失 | **已落地** |
| 3 | 自动生成断面线及桩号 | 305 浏览 | `at-landscape:mileage` 有桩号，无断面线 | 断面线是里程标注的自然延伸 | **已落地** |
| 4 | 更改零散高程点的颜色 | 101 浏览 | 无 | 高程点采样后必然需要分层设色 | **已落地** |
| 5 | AutoCAD 填充图案 (.pat) 预览器 | 537 浏览 | `at-hatch` 有图案提取/自定义/修复，无预览 | 可做「在图中生成图案样块表」的 AI 入口，不需要 DCL 模态框 | P1 |
| 6 | LSP to FAS/VLX 编译器（2 帖） | 2 帖合计 ≈1100 浏览 | `atlispcc` 已覆盖编译/反编译/汇编/链接/VLX/ZELX/往返测试；`dev-tools` 有包编译 | 核心能力已具备；缺 **CI/批量的项目级编译命令** 与相关文档 | P2 |
| 7 | 批量打印（AD 批量打印 / QT 批量打印 / BPlot） | 3 帖 ≈900 浏览 | `at-plot`、`youyun-plot`、MCP `publish_layouts`/`batch_plot_wizard` | 已有多种实现，不宜再加一套；可统一入口/文档 | P3 |
| 8 | 点集中共线最多的点集 | 183 浏览 | `at-math` 有几何工具，无共线子集 | 纯算法，适合 `at-math`，可先做 AI 入口 | P2 |
| 9 | 自动输入法（AutoCAD/中望/浩辰） | 长期置顶 | 无 | 属系统级 IME 切换，不是 LISP 包能解决的范围 | 不做 |
| 10 | 图层管理器 | 近期热帖 | `at-layer` 已有图层状态/隔离/按色分层等 | 无功能缺口 | — |
| 11 | 钣金切 45° 角 | 185 浏览 | `at-cnc` 有钣金/展开能力 | 细分工序，先看需求频次 | P3 |
| 12 | @lisp Agent 用户使用说明及示例 | 236 浏览 / 0 回复 | `docs/@lisp/` 与 Agent 帖子已有完整说明 | 不是代码缺口；社区反馈偏少，建议在产品内做引导 | — |

## 3. 结论

1. **测绘/景观类小工具缺口最大、实现成本最低**：论坛连续出现高程点、断面线、高程配色三个
   相邻需求，而仓库没有对应实现。三者共享「沿曲线取点 + 随机采样」的基础设施，适合一次性补齐。
2. **图库是另一个独立方向**：社区热帖都在解决「本地文件夹直接当图库」，与 `at-library`
   的在线索引互补而非重复。建议单独立项（扫描目录 → 生成索引 → 预览/插入），不要塞进现有语义。
3. **FAS 编译器不必重复投入**：`atlispcc` 的能力已覆盖论坛讨论的编译/反编译/链接/VLX，
   真正的差距在工程化（CI 批量、增量、产物校验），优先级低于业务工具。
4. **批量打印已经饱和**：`at-plot` + MCP 打印工具足够，继续加只会增加维护面。

## 4. 本次落地：at-landscape 测绘三项

新增 `at-landscape/survey.lsp`（3 个交互命令 + 3 个 AI 入口）：

| 功能 | 交互命令 | AI 入口 | 关键参数 |
|---|---|---|---|
| 范围内随机高程点 | `at-landscape:random-elevation` | `at-landscape:ai-random-elevation` | `bounds n lo hi prec` |
| 沿路线断面线 + 桩号 | `at-landscape:section-lines` | `at-landscape:ai-section-lines` | `pline-ent dist half-len start` |
| 高程点分层设色 | `at-landscape:elev-color` | `at-landscape:ai-elev-color` | `ents bands` |

设计取舍：

- **无交互入口**：AI 入口全部走形参，不调用 `getpoint`/`getint`，不弹模态框；成功返回
  ename 列表或数量，失败返回 `nil` 并 `princ` 原因（符合 `AGENTS.md` 的 AI 调用约定）。
- **随机点算法**：包围盒内拒绝采样（射线法判内外），最多 5000 次；退化多边形返回 `nil`，
  不抛错。随机数用包内已有的线性同余发生器，保证同一次会话可复现。
- **断面线**：沿曲线按弧长等距取点，用 `vlax-curve-getpointatdist` 求点、切线旋转 90° 求法向；
  圆角处按弧长自然过渡，不做单独圆弧处理。
- **高程配色**：POINT 取 Z 值，TEXT 取数字内容；`bands` 缺省时按实际高程均分 5 段
  （颜色 1/3/2/5/6），避免用户必须先算区间。
- **图层**：高程点统一放 `ELEV-POINT`（颜色 2），断面线/桩号沿用 `MINI-LANDSCAPE`，
  方便后续单独选择与清理。

验证状态：

- `node ../atlisp-lint/bin/atlisp-lint --file at-landscape/survey.lsp` → **0 error**。
- CAD 实测（2026-11-01，AutoCAD 24.1s / MCP，Drawing4.dwg）：
  - `ai-random-elevation ent 30 5.0 15.0 2` → 30 POINT + 30 TEXT，全部在闭合线内，
    Z 5.21~14.70，文字两位小数；
  - `ai-section-lines ent 20000 10000 0` → 6 条垂直线 + 桩号 K0+000~K100+000，线长 20000；
  - `ai-elev-color ss nil` → 30 点按 5 段改色；自定义区间 `((5 10 1) (10 15 3))` → 只出现颜色 1/3。
  - 明细见 `at-landscape/PLAN.md`。

## 5. 后续 backlog（按优先级）

| 优先级 | 项目 | 落点 | 状态 | 说明 |
|---|---|---|---|---|
| P1 | 本地文件夹图库 | `at-library/local.lsp` | ✅ | 扫描目录 → 检索 → 按名插入；`ai-local-scan/search/insert` |
| P1 | 图案样块表 | `at-hatch/preview.lsp` | ✅ | `ai-pattern-table`：内置/标准图案各画样块 + 名称，免 DCL |
| P2 | CI 批量编译 | `atlispcc project` | ✅ | 按 `pkg.lsp` 的 `:FILES` 批量编译，JSON 报告 + 退出码 |
| P2 | 共线最多点集 | `at-math` | ⬜ | `@math:max-collinear`：输入点表，返回点数最多的共线子集 |
| P3 | 批量打印统一入口 | `at-plot` | ⬜ | 只做参数编排与文档，不新增打印引擎 |
| P3 | 钣金 45° 切角 | `at-cnc` | ⬜ | 先确认是否有稳定需求样本 |

## 6. 历史反馈（2021–2023，搜索补充）

社区对 @lisp 本身的反馈集中在三点，都不是新功能，而是**认知与工程化**问题：

| 反馈 | 出处 | 现状 | 建议 |
|---|---|---|---|
| 「估计没有多少人愿意用云端 lisp，没有本地方便」 | 2021 应用管理器帖 | 实际只在安装/升级时联网，包与源码都落本地 | 在 README/安装页用一句话写明「装完离线可用」，减少误解 |
| 「中望的还有吗，主页怎么没有 zelx 格式了」 | 2022 安装帖回复 | `atlispcc zelx/unzelx` 已支持 ZWCAD 容器 | 发布页同时提供 `.fas` / `.zelx` 说明与转换命令 |
| 「有没有自定义函数查重的功能」 | 2023 开发助手帖 | `atlisp-lint` 已有 `duplicate_defun`、`unused_local_fun` 等规则 | 在 lint 文档里补一节「查重/瘦身」用法 |

这三条适合用文档解决，不需要新增应用包；列入 P2 文档项，避免重复开发。

## 7. 本轮执行结果（2026-11-01）

| 项 | 落地位置 | 验证 |
|---|---|---|
| 本地文件夹图库 | `at-library/local.lsp` | CAD 实测：扫描 2 个 dwg、检索 `ginkgo`、插入为 `INSERT`（块名/基点/比例正确） |
| 图案样块表 | `at-hatch/preview.lsp` | CAD 实测：4 图案 12 图元；36 图案全部生成 HATCH（108 图元，无回退） |
| 包级批量编译 | `atlispcc compiler/project_cmd.py` | 11 个 pytest 全过；`at-landscape` 实际编译 5/5 成功 |
| 测绘三项 | `at-landscape/survey.lsp` | CAD 实测见第 4 节 |

命令速查：

```sh
# 图案预览（CAD 内）
(at-hatch:ai-pattern-table nil '(0 0) 90 60 6 20)

# 本地图库
(at-library:ai-local-scan "D:/lib" 50)
(at-library:ai-local-insert "银杏" '(0 0) "D:/lib" 1.0 0.0)

# CI 批量编译
atlispcc project packages/at-landscape -o dist -O2 --json --report build.json
```
