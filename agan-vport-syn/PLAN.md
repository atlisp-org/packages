# agan-vport-syn 功能扩展计划

> 阿甘对图器（第三方，作者"阿甘"，C# 编写）。同步视口对图：两个模型空间视口同步平移/缩放对比图纸。LISP 层仅做 DLL 下载、netload 与启动。

## 1. 概览
- 版本 0.0.6 / 分类 布局 / 依赖 base / 第三方 C#（aganvportsyn.dll，低版本 -10.dll）
- 规模：agan-vport-syn.lsp 约 40 行 1 个函数；功能主体在 DLL（命令 `agdt`）
- DLL 限制（readme.txt）：仅 AC2013-2022、仅模型空间 2 视口、不支持布局视口、需十字光标水平

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| agan-vport-syn:load | agan-vport-syn.lsp:10 | 版本检查(<AC18 拒绝) → 本地无 DLL 则按远端 filesize 下载（while 轮询）→ netload → `(vla-sendcommand "agdt\n")` 启动 |

## 3. 新功能（NEW）
- **F1 纯 LISP 基础同步（P2）**：无 DLL 时的降级方案——监听两个视口中心/缩放变化，用 `(vla-put-center)/(vla-put-width)` 反向同步另一视口（鼠标在视口 A 平移，视口 B 跟随）。接口 `(agan-vport-syn:sync-lisp vp1 vp2)`，实现要点：`getvar "GETDIR"` 不可用，用鼠标移动事件轮询 `grread` 或 vla-viewport 的 MouseMove 事件（需 VLA 事件，复杂度高）；先做"手动双键同步"（点 A 点 B 两对基点后锁定比例互相同步缩放）。
- **F2 对图辅助命令（P1）**：LISP 层补 `agan-vport-syn:start`（启动对图）、`agan-vport-syn:stop`（结束对比，DLL 目前只能窗口内点按钮）、`agan-vport-syn:reset`（恢复视口）。
- **F3 基点记忆（P1）**：两视口基点存入 config（`@:set-config`），二次启动免重选。
- **F4 三视口/布局视口支持（P3）**：受 DLL 架构限制，需原作者升级；此处仅记录需求。

## 4. 改进（IMPROVE）
- **I1 下载等待无超时（P1）**：agan-vport-syn.lsp 的 `(while (< (vl-file-size ...) filesize) (sleep 5))` 在断网时**死循环**。加最大等待（如 120s）与进度提示 `(@:prompt (strcat "已下载" ...))`。
- **I2 下载校验（P1）**：filesize 相同不代表文件完整（断点续传/损坏）；下载后校验 + 失败重试一次。
- **I3 版本范围提示（P2）**：仅检查 <18，DLL 实际支持 2013-2022；对 >22（AC2024+）提示兼容性。
- **I4 cmdecho/filedia 恢复（P2）**：异常路径（netload 失败）未恢复 `filedia`/`cmdecho`，用 push-var/pop-var 保证恢复。

## 5. 重构（REFACTOR）
- **R1 下载逻辑提取（P1）**：版本探测 + 远端大小 + 下载 + 轮询 + netload 整段（行11-30）抽为 `agan-vport-syn:ensure-dll`，供 F1/F2 复用。
- **R2 与 doslib/opendcl 模式统一（P2）**：本包"按需下载匹配 arx/dll"与 doslib、opendcl 包职责重叠；确认 doslib 是否可统一承接，本包只保留 `agdt` 启动。

## 6. lib 协同
- 复用：`sys:*`（进程/版本）、`file:*`
- 跨包：与 doslib（arx 按需下载）、opendcl（运行时库按需下载）整合评估

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 下载超时 | P1 | 0.5h | 现死循环风险 |
| I2 校验重试 | P1 | 0.5h | — |
| F2 启动/停止命令 | P1 | 1h | 需 DLL 提供 stop 入口，**需原作者确认** |
| R1 ensure-dll | P1 | 0.5h | — |
| F3 基点记忆 | P1 | 1h | — |
| F1 LISP 降级同步 | P2 | 6h | 高复杂度，先做手动双键版 |
| I3/I4 | P2 | 0.5h | — |
| F4 多视口 | P3 | — | 依赖原作者 |

> 功能主体在第三方 C# DLL，LISP 层能做的有限；P1 项均为启动器健壮性改进，不依赖原作者。
