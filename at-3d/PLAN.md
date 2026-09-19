# at-3d 功能扩展计划

> 3D 曲面剖切工具。选点剖切（过点平面，4 种切面方式）与路径剖切（沿曲线等距分段剖切）。基于 `command "slice"` 驱动。

## 1. 概览
- 版本 0.0.4 / 分类 Common / 依赖 base / 自研
- 规模：at-3d.lsp 约 190 行 2 个公开函数
- 切面方式：1 平行 xy / 2 平行 yz / 3 平行 zx / 4 垂直于导向线（取点所在直线切线的垂面）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-3d:slice-by-pts | at-3d.lsp:18 | 选曲面 → 选点 → 每点按 4 方式切片；切出的 SURFACE 续加入选集迭代 |
| at-3d:slice-by-route | :115 | 选曲面 → 选路径曲线 → getint 分段数 → 等距取点切片（同 4 方式） |

## 3. 新功能（NEW）
- **F1 实体剖切（P1）**：现仅支持 `surface` 图元；扩展 `solid`（`command "slice"` 同样适用）。接口不变，ssget 过滤加 `solid`。
- **F2 自定义面剖切（P1）**：选任意 3 点/圆/多段线定面，替代固定 100 单位偏移的 4 方式。
- **F3 等距批量剖切（P1）**：沿路径按间距（而非段数）批量切，配合 F1 出"系列剖切面"（剖面图组）。
- **F4 剖面填充（P2）**：切面生成后自动加剖面线（hatch pattern 配置化）。
- **F5 剖面标注（P2）**：每个切面自动标序号 + 位置（配合 at-dim）。

## 4. 改进（IMPROVE）
- **I1 `(in-package :cl)`（P0）**：at-3d.lsp:10 直接切入 `:cl` 包——后续所有符号解析到 CL 包，与 defpackage :at-3d 冲突且危险；删除该行。
- **I2 defpackage 导出错误（P1）**：`(:export :slice-by-pts ::slice-by-route)` 双冒号=不可访问导出；菜单靠字符串调用不受影响，但规范应改为 `:slice-by-route`。
- **I3 slice 交互 hack（P1）**：`(while (> (getvar "cmdactive") 0) (command ""))` 用回车应答 slice 的后续提示（输出面选择等），行为依赖提示顺序，CAD 版本/语言不同可能错位；改 `command-s` 显式应答。
- **I4 无错误处理（P1）**：未选点/未选曲面时 `pickset:to-list` 返回 nil 后 `entity:getdxf` 报错；补空检查与 `*error*`（pop-var 恢复）。
- **I5 全局变量（P2）**：`surfaces`、`pts`、`slicesurfaces` 未声明局部。

## 5. 重构（REFACTOR）
- **R1 切片循环统一（P1）**：slice-by-pts 与 slice-by-route 的"4 方式 × command slice"段完全重复（各约 40 行）；提取 `(at-3d:slice-surface surfaces pt mode)` 单一实现，两入口只负责取点序列。
- **R2 面点计算纯化（P2）**：4 种面点生成（100 单位偏移）抽为 `(at-3d:face-pts pt mode / route)` 纯函数，偏移距离配置化。

## 6. lib 协同
- 复用：`curve:length`、`curve:point-at-dist`（vlax-curve 封装）、`pickset:*`
- 跨包：at-cnc（U 轴曲面加工）后续可共用曲面采样

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 in-package | P0 | 0.1h | 现隐患 |
| F1 实体剖切 | P1 | 1h | — |
| I3 command-s | P1 | 1.5h | 需验证各版本提示序 |
| R1 统一切片 | P1 | 1.5h | — |
| F2 自定义面 | P1 | 2h | — |
| F3 等距批量 | P1 | 1.5h | — |
| I2/I4/I5 | P1/P2 | 1h | — |
| F4/F5 | P2 | 各 1.5h | — |
| R2 | P2 | 0.5h | — |
