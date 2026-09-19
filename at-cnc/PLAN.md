# at-cnc 功能扩展计划

> @CNC 测试版。CAD 曲线（lwpolyline/circle）生成 G 代码，用于 CNC 数控加工（含 U 轴曲面加工、扩孔/磨孔、SYNTEK 系统适配）。24 项配置，支持 Candle 后处理。

## 1. 概览
- 版本 0.1.9 / 分类 机械 / 依赖 base / 自研
- 规模：at-cnc.lsp 约 540 行 14 个函数 + refer.lsp（旧版 gen-gcodeb，未打包）+ 示例 u.nc
- 流程：选曲线 → 先圆后曲/半径升序排序 → 刀具偏移(r/2) → 逐曲线 lwpl2gcode/circle2gcode（进刀、扩孔 k-times、磨孔 rub-times）→ 归零 M30 → 写 @cnc.nc

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-cnc::setup | at-cnc.lsp:41 | 配置对话框（24 项：units 精度/r 刀径/f 进给/主轴/U 轴/扩孔/磨孔/冲程/SYNTEK…） |
| at-cnc::n2s | :46 | 数字按 units 转字符串（老设备要求带小数点） |
| at-cnc::motor-on/off | :53/:56 | M3 S / M5 |
| at-cnc::umotor-on/off | :59/:62 | M13 S / M15（U 轴） |
| at-cnc::open-candle | :65 | 用 Candle 打开 nc 文件（路径记忆） |
| at-cnc::explorer | :77 | 打开 NC 库文件夹 |
| at-cnc::remove-route | :87 | 删除 CNC_ROUTE 层刀路图元 |
| at-cnc::lwpl2gcode | :96 | 多段线→G 代码（149 行：段遍历、冲程、扩孔、磨孔） |
| at-cnc::circle2gcode | :245 | 圆→G 代码（171 行：整圆/弧、U 轴展开、pre-circle-r 关联） |
| at-cnc::gen-gcode | :416 | 主流程：建层→排序→基准点→初始化→逐曲线→归零→文件落盘 |
| at-cnc:gen-gcodeb | refer.lsp:1 | 旧版交互式生成（换刀 H 交互、双文件 dnmb.cnc 背刀），未入 :FILES |

## 3. 新功能（NEW）
- **F1 多刀路/多遍加工（P1）**：单次走刀 → 粗精多遍（每遍吃刀量配置），生成多段刀路 + 刀具表（T 号/直径/数量）。
- **F2 刀具库（P1）**：刀具参数（号/径/转速/进给/补偿）配置化列表，加工时按曲线自动选刀（现单一 r 全局刀）。
- **F3 后处理器选择（P2）**：SYNTEK 现为 0/1 开关；抽象为 post 配置（SYNTEK/通用/Candle 自带），G 代码头部/进刀方式按 post 分发。
- **F4 加工预览（P2）**：生成 nc 前用 dyndraw 动态显示刀路 + 预计加工时长（路径长/进给估算）。
- **F5 曲面 U 轴完整支持（P2）**：circle2gcode 的 U 轴展开已部分实现（kthickness/u 参数）；补斜面/鼓面加工（配合 at-3d 曲面采样）。
- **F6 多文件输出（P3）**：按曲线分组输出多个 nc（配合 batch 加工），现单文件 @cnc.nc 覆盖。

## 4. 改进（IMPROVE）
- **I1 文件名固定覆盖（P1）**：gen-gcode 每次生成带时间戳文件再复制覆盖 `@cnc.nc`——历史文件保留但 open-candle/explorer 只认 @cnc.nc；补"打开最近生成文件"列表。
- **I2 冲程逻辑说明（P2）**：chopping/chopping-pause 配置存在，lwpl2gcode 内实现；补注释与文档（readme.org 未覆盖全部 24 项配置）。
- **I3 refer.lsp 清理（P1）**：gen-gcodeb 未打包但留在包内，且 :21 `stno` 在 H 换刀路径先引用后赋值（首刀 H 时报错）；决定保留（修复+入 :FILES）或删除。
- **I4 排序比较函数（P1）**：gen-gcode :436 的 vl-sort 比较函数对"LWPOLYLINE vs LWPOLYLINE"返回 t（恒真），同类型排序不稳定；修正为 nil。
- **I5 基准点选择（P2）**：pt-base 用 pickset:getbox 外扩 5+r 取角点，未让用户确认；加 getpoint 复核选项。

## 5. 重构（REFACTOR）
- **R1 进刀/抬刀原语（P1）**：lwpl2gcode 与 circle2gcode 各 150+ 行，进刀(M03 前 Z30/Z5)、退刀、扩孔循环高度重复 → 提取 `(at-cnc::approach pt)`/`(at-cnc::retract)`/`(at-cnc::enlarge r times)` 原语，两函数降到 ~60 行。
- **R2 G 代码构建器（P2）**：散落 write-line → 内部 buffer 列表 + 一次性写盘（便于 F4 预览与 F6 多文件）；`(at-cnc::gc str)` 追加 + `(at-cnc::emit)` 输出。
- **R3 双命名空间（P2）**：`(defpackage :@cnc)` 与 `:at-cnc` 并存，函数名 `at-cnc::`（双冒号）与 @:get-config '@cnc:: 混用；统一为 at-cnc: 前缀 + 单命名空间（配置键名同步迁移，需兼容旧配置）。

## 6. lib 协同
- 复用：`curve:get-points`/`curve:length`、`datetime:current-time`（已用）、`string:*`
- 跨包：al-plate（铝板展开→下料）、route-of-hole2shape（铣削刀路）同属加工类，"轮廓→刀路→G 代码"管线可统一设计

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I4 排序修正 | P1 | 0.3h | 现隐性 bug |
| I3 refer 清理 | P1 | 0.5h | — |
| R1 原语提取 | P1 | 2h | — |
| F1 多遍加工 | P1 | 3h | — |
| F2 刀具库 | P1 | 3h | — |
| I1 历史文件 | P1 | 1h | — |
| R2 构建器 | P2 | 1.5h | F4/F6 前置 |
| F3 后处理器 | P2 | 2h | — |
| F4 预览 | P2 | 2h | — |
| F5 曲面 | P2 | 4h | 依赖 at-3d |
| R3 命名空间 | P2 | 2h | 配置迁移风险 |
| F6 | P3 | 1.5h | — |
