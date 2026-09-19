# align-array 功能扩展计划

> 对齐排列（第三方，作者 start4444/ST，2020-03）。DCL 对话框驱动：单体对齐/等距排列/阵列（重排、复制）/插图框/自动识别图框，用于图框排版。1405 行单文件。

## 1. 概览
- 版本 0.0.3 / 分类 排版 / 依赖 base / 第三方
- 规模：align-array.lsp 1405 行 35 个函数，DCL 运行时生成（make-dcl-pl :1079）
- 选取方式 5 类（LB_TMP）：单体 / 图框块 / 矩形方框 / 手工框选 / 自动识别图框
- 阵列方式（RB_zhenlieFS）：重排阵列 / 复制阵列；间距（中中/边边）；排序（自动/点选顺序）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| align-array:align-array | :36 | 主入口：UCS 保存 → 生成 DCL → 20 个 action_tile 绑定 → start_dialog → cond 分发 20 种结果 → UCS 恢复 |
| S_RECT | :206 | 读 DCL 各 tile 值到全局（间距/类别/阵列方式/排序方式/基点） |
| S_RECT2 / S_RECT3 | :236/:240 | 排列/阵列"点选定距"，随后调 c:ent_DQPL（**未定义**） |
| ss9pt | :246 | 选择集 9 点外框（院长外包围） |
| SStoLST | :272 | ss → ename 列表 |
| setdate | :280 | 初始化 DCL 各控件显示值 |
| fuzhizhenlie / chongpaizhenlie | :318/:327 | 阵列方式切换时的控件变灰/变亮 |
| S_DQcx | :339 | 单点定位：选基点后按 9 点框对齐/排列核心 |
| S_PLcx | :416 | 两点定距排列核心 |
| enbox | :539 | 单体外框对角点 |
| DQ_BK / PL_BK | :543/:559 | 按类别（图框/方框…）构建待移选集 |
| mid_pt / ckb_rec | :577/:581 | 中点；按长宽比勾选手工矩形 |
| layer_suo_str | :595 | 组合"上锁图层"过滤字符串 |
| chatukuang | :608 | 插图框：按 dwg 文件块插入、自动缩放居中（默认 D:\A2图框MR.dwg） |
| chatukuang_tuzhong | :626 | 插图框-取图中已有图框块 |
| tukuang_BK | :653 | 图框块类别的对齐/排列调度 |
| juxing_WL | :685 | 矩形框类别调度 |
| select_SG | :718 | 手工框选类别调度（每选一次画一矩形） |
| zidongshibie_WL | :757 | 自动识别图框（全对象对比，大图纸慢） |
| zhenlie_PX / zuo>>you_pl | :788/:798 | 从左到右从上到下排序 / 横排子函数 |
| zhenlie_DT | :817 | 单体阵列主程序 |
| zhenlie_SS | :906 | 选集阵列主程序 |
| zhenlie_FUZHI | :993 | 复制阵列（类似 CAD 自带阵列） |
| maketukuangxian | :1004 | 自动识别图框画矩形框 |
| ebox / area / pt4 / PtInPoly | :1031-1051 | 包围盒/面积/点在多边形内 |
| quchongfu | :1052 | 选集去重 |
| make-dcl-pl | :1079 | 生成 DCL 临时文件 |

## 3. 新功能（NEW）
- **F1 布局空间支持（P1）**：现仅模型空间；布局中图框排版是高频场景。实现要点：检测 `getvar "CVPORT"`，布局下对 paperspace 对象操作（ssget 加 `_P` 前缀），DCL 增加"空间"单选。
- **F2 排列时旋转（P2）**：纵向排列自动旋转 90°（横图框变竖图框），选项开关。
- **F3 属性联动插图框（P2）**：chatukuang 插图框后自动填标题栏属性（图号/图名/比例），配合 at-pm 图纸目录。
- **F4 排序基准扩展（P2）**：按块名/图层/插入点文本排序（现在仅坐标排序），适合"先排 A 类再排 B 类图框"。
- **F5 跨文件排版（P3）**：从多个 dwg 收集图框块统一排列（配合 at-pm/at-session 多文档）。

## 4. 改进（IMPROVE）
- **I0 未定义函数（P0）**：`S_PGON`、`S_toggle`（:84 起 18 处 action_tile 内联）、`c:ent_DQPL`（:182/:238/:243 直接调用）在包内无定义，疑为原作者环境残留——**点任一按钮即运行时错误**。需作者提供源码或本地补桩（S_PGON/S_toggle 推断为坐标转换/状态切换；c:ent_DQPL 推断为"单体对齐排列"后处理）。
- **I1 硬编码路径（P0）**：:52 `t_lujing "D:\\A2图框MR.dwg"`、:182 getfiled 初始目录 `"C:/Users/Administrator/Desktop/"`。改 `@:define-config` + 当前目录。
- **I2 全局变量污染（P1）**：约 20 个无包前缀全局（cp_xx/LB_TMP/tukuang_TMP/RB_*…）且跨会话记忆；统一加 `align-array:` 前缀或迁入 config（记忆数值可保留）。
- **I3 UCS hack（P1）**：:37-38/:201 保存/恢复 UCS 用 `command "ucs" "na" ...`，命名 UCS 不存在时恢复失败（ucs_old 已存在但被删的场景）；改 `(getvar "UCSNAME")` 记录 + `setvar` 恢复，或 `vla-put-UCSOrigin`。
- **I4 群组错位已知 bug（P1）**：头部注释自述"有群组的时候框选类型会错位，未解决"（:20）；实现要点：ssget 后 `group:explode` 临时解组 → 操作 → 重组（lib 有 group 命名空间）。
- **I5 错误处理（P1）**：主函数无 `*error*`，DCL 异常退出时 UCS/临时 dcl 文件不恢复。
- **I6 自动识别性能（P2）**：zidongshibie_WL 全对象对比，大图纸卡；加 `getconf` 限制 + 进度提示。

## 5. 重构（REFACTOR）
- **R1 单文件拆分（P1）**：1405 行 → 按功能拆 `align.lsp`（S_DQcx/S_PLcx/DQ_BK/PL_BK）、`array.lsp`（zhenlie_*）、`tukuang.lsp`（chatukuang*）、`detect.lsp`（zidongshibie_*）、`dcl.lsp`（make-dcl-pl/setdate），pkg.lsp :FILES 同步。
- **R2 调度表驱动（P2）**：:126-200 的 20 项 cond × 5 类嵌套重复严重（dd 1-6 结构完全相同仅参数不同）；改参数表 `((方向 基点模式 9pt-idx) …)` + 单一分发函数。
- **R3 工具函数提取（P2）**：ss9pt/enbox/ebox/PtInPoly/quchongfu 与 lib（geometry/pickset/list）重复，统一复用。

## 6. lib 协同
- 复用：`geometry:*`（包围盒/点在多边形内）、`pickset:to-list`、`group:*`（解组）、`list:uniq`
- 跨包：与 align-all（行距）、at-pm（图框管理/图纸目录）、at-layout（视口排版）联动；插图框填属性可与 at-block:attrib 共用

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I0 未定义函数 | P0 | 3h | **需原作者确认或逆向补桩**，现不可用风险 |
| I1 硬编码路径 | P0 | 0.5h | — |
| I2 全局前缀 | P1 | 1.5h | 第三方代码，建议先与作者确认 |
| I3 UCS | P1 | 1h | — |
| I4 群组 | P1 | 2h | — |
| R1 拆文件 | P1 | 2h | — |
| I5 error | P1 | 0.5h | — |
| F1 布局空间 | P1 | 3h | 价值最高 |
| I6 性能 | P2 | 1h | — |
| R2/R3 | P2 | 各 1.5h | — |
| F2-F5 | P2/P3 | 各 2-4h | — |

> 第三方包，结构性改动（R1/R2/I2）建议先与原作者 start4444 确认授权；P0 两项不依赖原作者即可修。
