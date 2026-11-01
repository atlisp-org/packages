# CAD 实测与发布记录（2026-11-01）

> 测试环境：ZWCAD 2026（MCP `192.168.1.55:8110`，Drawing5.dwg）
> 发布环境：AutoCAD 24.1s / @lisp（MCP `192.168.1.53:8110`，edition=stable）
> 发布目标：atlisp.cn 稳定版（stable）

## 1. 测试结果

| 包 | 功能 | 用例 | 结果 |
|---|---|---|---|
| at-landscape 0.1.1 | 随机高程点 | `ai-random-elevation` 10 点 / 5~15m | 20 图元、Z 6.92~14.70 ✅ |
| at-landscape 0.1.1 | 断面线 | `ai-section-lines` 间距 200 / 半长 50 | 6 线 + 6 桩号 ✅ |
| at-landscape 0.1.1 | 高程分层设色 | `ai-elev-color` nil | 10 点改色 ✅ |
| at-hatch 0.1.2 | 图案样块表 | `ai-pattern-table` 3 图案 | 9 图元 = 3 边框 + 3 HATCH + 3 文字 ✅ |
| at-hatch 0.1.2 | 任意 `.pat` 解析 | `parse-pat-all` 2 图案文件 | 返回 2 条 ✅ |
| at-hatch 0.1.2 | 实尺预览 + 清除 | `ai-pat-preview` / `pat-erase` | 图案 P1、图层 PAT-PREVIEW；清除后 0 残留 ✅ |
| at-library 0.1.4 | 本地扫描/检索 | 28 个 dwg 的 archive 目录 | 扫描 20（limit）、检索命中 5 ✅ |
| at-library 0.1.4 | 本地插入 | `ai-local-insert "dim-leader-res2"` | INSERT、块名正确 ✅ |
| at-library 0.1.4 | 多根目录 | `local-scan-multi` 两个根 | 28 条、路径去重 ✅ |
| at-library 0.1.4 | 幻灯片库名称 | 合成 2 条 `.slb` | `(SLIDE1 SLIDE2)` ✅ |
| at-stat 0.0.6 | 填充面积 | `ai-hatch-area` ANSI31 + SOLID | `((SOLID 10000 1) (ANSI31 10000 1))` ✅ |
| at-stat 0.0.6 | 填充面积表 | `ai-hatch-area-table` | 生成 `ACAD_TABLE` ✅ |
| at-layer 1.2.15 | TXT 建层 | `WALL` / `A/B` / `COLUMN` + 前缀 | `X-WALL` / `X-A_B` / `X-COLUMN` ✅ |
| at-layer 1.2.15 | 文字建层 | TEXT `MEETING` | 图层 `MEETING` ✅ |
| at-session 0.1.9 | 图纸备份 | 连续备份 2 次、保留 1 份 | 只剩最新 1 份 ✅ |
| at-plot 0.0.13 | 图幅识别 | A0~A4 横竖 6 组 | 全部正确（含修复后 A2/A4 歧义） ✅ |
| at-plot 0.0.13 | 批打空表 | `ai-plot-all` 无 temp-frames | 返回 nil，无异常 ✅ |

未覆盖：

- `at-plot:ai-plot-all` 的完整 PDF 出图没有执行（需要图框与打印设备，且易触发模态框）；
- `at-library:local-browse` 多选交互依赖键盘/鼠标，只验证了底层的 multi-scan/search；
- `at-hatch:pat-erase` 会清除 `PAT-PREVIEW` 层上的全部预览（含样块表），这是设计行为。

## 2. 测试中发现并修复的问题

`@plot:get-frame-type` 在尺寸歧义时误判：

- `29700×21000` 既可能是 A4@1:100，也可能是 A2@1:200；
- 原实现取迭代中最后一个命中，导致 A2/A4 互换；
- 修复：误差相同时，优先取比例更接近配置首个比例（默认 1:100）的候选；
  验证结果：A3/A2/A4/A1/A0 横竖全部正确。

## 3. 发布结果（stable）

| 包 | 版本 | stable 校验 |
|---|---|---|
| at-landscape | 0.1.1 | ✅ |
| at-hatch | 0.1.2 | ✅ |
| at-library | 0.1.4 | ✅ |
| at-stat | 0.0.6 | ✅ |
| at-layer | 1.2.15 | ✅ |
| at-session | 0.1.9 | ✅ |
| at-plot | 0.0.13 | ✅ |

流程：`@::compile-pkg` → `@::upload-pkg <pkg> release` → `@::upload-pkg <pkg> stable`
→ `@::@get "https://atlisp.cn/flush-data"`。发布后用 `packages-list?edition=stable`
核对，版本号为上表。

## 4. 已知环境问题

- 发布机的 `@::*machineid*` 未初始化，导致 `@::package-update` 的 `strcat` 报
  `stringp nil`；手工设置 `@::*machineid*` 后恢复。若后续发布再次出现该错误，先设：
  `(setq @::*machineid* (getenv "COMPUTERNAME"))`
- 发布过程中 CAD 曾丢失文档（`No open document`），`at-layer` 首次发布失败；
  新建文档后重试成功。批量发布建议每包一次调用，失败单独重试。
