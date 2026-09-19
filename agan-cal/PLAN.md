# agan-cal 功能扩展计划

> 阿甘计算器（第三方，作者"阿甘"）。拾取图面文字/尺寸/属性/天正对象 → 提取数字 → 四则运算 → 动态插入结果文字。C# DLL（AganCal.dll）提供计算器窗口与历史记录。

## 1. 概览
- 版本 0.0.5 / 分类 其它 / 依赖 base / 第三方署名
- 规模：agancal.lsp 364 行 36 个函数 + agan-cal.lsp 初始化 + 资源 AganCal.dll / AganCal18.dll（<AC2019 用 18 版）
- 核心流：`GL:GetNumber`（拾取）→ `GL:Formula`（正则提取算式）→ `cal` 内置计算 → `GL:crjswz`（grread 动态插入 TEXT）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| agan-cal:init | agan-cal.lsp:2 | 初始化历史记录.txt |
| c:ag | agancal.lsp:6 | 启动命令（vl-cmdf "agg"） |
| c:ad / c:a2 / c:a3 / c:a4 | :7-10 | 快捷键：加/减/乘/除（仅计算） |
| c:ys / c:y2 / c:y3 / c:y4 | :15-18 | 计算并插入**结果** |
| c:dd / c:d2 / c:d3 / c:d4 | :21-24 | 计算并插入**算式** |
| c:sq | :11 | 拾取数字到 C# 窗口 |
| c:css | :12 | 插算式（调 C# 命令 GC:agcalcss） |
| c:GL-ad…c:GL-a4 | :40-43 | C# 回调 LISP 的入口 |
| GL:jiafa/jianfa/chengfa/chufa | :45-48 | 四则运算分发 |
| GL:GetNumber | :51 | 循环拾取 `*TEXT,DIMENSION,INSERT,ATTDEF,ACAD_TABLE,TCH_*`，点选亮显、框选高亮，实时回显算式 |
| GL:Formula | :99 | 按类型取组码（MTEXT 1/TCH_SPACE 41/DIMENSION 42/ATTDEF 2…）+ Vbscript.RegExp 提取数字与运算符 |
| GL:Galculate | :141 | `(cal (strcat Num "*1.0"))` 计算、rtos 截断、C# 窗口回显、存历史 |
| GCGalculate | :179 | C# 窗口输入字符串的纯计算入口 |
| c:shiqutxt | :205 | 拾取 → C# 窗口 |
| HighLight / UnHighLight | :213/:223 | redraw 3/4 亮显 |
| ss->lst | :233 | ssnamex → ename 列表 |
| MJ:delsame | :239 | 去重（感谢黄总） |
| mtext2text | :246 | 13 步正则剥离 MTEXT 格式码 |
| GL:crjswz | :301 | grread 动态定位插入 TEXT，支持 A/97 键转 90°，右键/ESC 删除 |

## 3. 新功能（NEW）
- **F1 更多运算符（P1）**：幂 `^`、开方、百分数、`pi`。接口：`GL:GetNumber` 的 ope 参数扩展 + `GL:Galculate` 中 `cal` 表达式预处理（`^` → 嵌套乘或 vlax 计算）。实现要点：`cal` 不支持 `^`，先字符串展开或改用 `(eval)` 安全求值。
- **F2 距离/面积直接计算（P1）**：`c:dist`（两点距离）、`c:area`（闭合区域面积，复用 TCH_SPACE 组码 41 或 `boundary` 包思路），结果同样动态插入。
- **F3 结果转多行文字/表格（P2）**：批量拾取后生成"算式=结果"对照表（ACAD_TABLE），供设计说明引用。接口 `(agan-cal:report lst)`。
- **F4 历史记录面板（P2）**：历史现写 历史记录.txt 无查看入口；加命令读取最近 N 条，支持重新插入/复制。
- **F5 单位换算（P2）**：mm/inch/尺/米 换算，`c:hd`。

## 4. 改进（IMPROVE）
- **I1 配置化硬编码（P1）**：`GL-precision`(行2)、`GL-texthight`(行3) 为全局 setq，改 `@:define-config` + `(@:get-config)`；历史记录路径（行33）改包内相对路径常量。
- **I2 结果文字随层（P1）**：`GL:crjswz` 行305-310 entmake TEXT 未带 8 组码 → 落在 0 层；补 `(cons 8 (getvar "clayer"))` 并支持当前文字样式。
- **I3 除零/溢出提示（P1）**：行169 提示文案 "介于2147483647和-2147483648" 与 `*1.0` 浮点实际不符；区分除零与溢出。
- **I4 天正对象兼容说明（P2）**：TCH_SPACE 取 41 组码，若天正版本不同需适配；加版本探测。

## 5. 重构（REFACTOR）
- **R1 正则引擎替换（P1）**：4 处 `Vbscript.RegExp` COM 对象（行100/142/180/247）→ `lib/re`（lib 已有 re 命名空间），消除 COM 依赖与创建/释放开销。
- **R2 工具函数提取（P1）**：`ss->lst`→`pickset:to-list`、`MJ:delsame`→`list:uniq`、`HighLight/UnHighLight`→`entity:highlight` 批量、`mtext2text`→`text:remove-fmt`（aigc 包已用 `text:remove-fmt`，语义重叠，统一到一个实现）。
- **R3 命名空间（P2）**：`GL:*`/`c:*`/`MJ:*` 全局无包前缀，污染环境；第三方代码，**需原作者确认**后再加 `agan-cal:` 前缀（C# DLL 回调名 c:GL-* 是硬约定，不能动）。

## 6. lib 协同
- 复用：`re:*`、`pickset:to-list`、`list:uniq`、`text:remove-fmt`、`entity:highlight`
- 候选提取：`mtext2text`（与 at-* 中 MTEXT 剥离逻辑比对后统一入 `lib/src/text/`）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 配置化 | P1 | 0.5h | — |
| I2 结果随层 | P1 | 0.5h | — |
| R1 正则替换 | P1 | 1.5h | 需 re 命名空间覆盖全部 13 步 |
| F1 运算符 | P1 | 2h | — |
| F2 距离/面积 | P1 | 1.5h | — |
| R2 函数提取 | P2 | 1h | — |
| F3/F4/F5 | P2 | 各 2h | — |
| R3 命名空间 | P2 | 2h | **需原作者确认** |
