# alt-run 功能扩展计划

> ALTRun 启动器。外部快速启动软件（绿色小工具），LISP 层仅下载与启动，与 acmecad2021 同模式。

## 1. 概览
- 版本 0.0.3 / 分类 APP / 依赖 base / 第三方封装
- 规模：alt-run.lsp 约 25 行 1 个函数；运行时从 `archives/ALTRun.zip` 下载到 `bin/ALTRun/`

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| alt-run:open | alt-run.lsp:14 | 检查 `bin\ALTRun\ALTRun.exe` → 缺失则 `@:down-and-unzip` → `start-bg` 启动（cmdecho 0/1 包裹） |

## 3. 新功能（NEW）
- **F1 带参启动（P2）**：`(alt-run:open "搜索词")` 把参数传给 ALTRun（若其支持命令行参数），实现"CAD 内搜 → ALTRun 定位"。
- **F2 工作目录选项（P3）**：启动前可选绑定目录（ALTRun 的搜索范围）。

## 4. 改进（IMPROVE）
- **I1 下载失败提示（P1）**：`@:down-and-unzip` 失败时静默；补 `(@:prompt "ALTRun 下载失败，请检查网络")`。
- **I2 进程复用（P2）**：已运行时不重复启动（`sys:list-process-name` 含 ALTRun.exe 则提示已启动）。

## 5. 重构（REFACTOR）
- **R1 并入 ext:launch（P1）**：与 acmecad2021/autohotkey/everything/pdftk/youyun-plot 同模式，改用 `lib/src/ext/launch.lsp` 的 `(ext:launch 'alt-run "archives/ALTRun.zip" "bin\\ALTRun\\ALTRun.exe")`，本包缩减到 3 行（见 acmecad2021/PLAN.md R1）。

## 6. lib 协同
- 候选：`ext:launch`（跨 6+ 启动器包复用）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 下载提示 | P1 | 0.2h | — |
| R1 ext:launch | P1 | 0.2h | 依赖 lib 落地 |
| F1 带参启动 | P2 | 1h | 需 ALTRun 支持 CLI |
| I2 进程复用 | P2 | 0.3h | — |
| F2 | P3 | 0.5h | — |
