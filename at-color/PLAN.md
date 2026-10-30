# at-color 功能扩展计划

> @lisp Color。修改图元/图层颜色、去除真彩色、全图变单色。v0.1.2，自研，依赖 base。

## 1. 概览
- 版本 0.1.2 / 分类 Color / 依赖 base / 自研
- 规模：at-color.lsp 227 行 33 个函数 + at-color-chs/cht.lang（GBK 翻译文件，未打包）
- 三条命令：`at-color:change-color`（快速改色 DCL 对话框）、`at-color:del-rgb`（去真彩色）、`at-color:one-color`（全图/所选变单色，含块内图元）
- 风格：无 defpackage，全局函数（`at-color:` 与遗留 `GL:`/`C:` 前缀并存）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-color:one-color | at-color.lsp:5 | 全图或所选图元变单色（252 色），块内图元同步变色；TCH_* 天正文字走 truecolor |
| at-color:del-rgb | :34 | 框选图元删除 420 真彩色，回退索引色 |
| at-color:change-color | :38 | 运行时生成 DCL 临时文件，弹"快速改色"对话框（对象色 9+随层/随块/其它 × 图层色 9+其它） |
| chcolor（局部） | :8 | one-color 内部：TCH_* 设 truecolor，其余设 62 |
| GL:gs1..gs9 | :158 | 对象改色 1–9（change 命令循环选择） |
| GL:gs10 / gs11 | :167/:168 | 对象改色 190（紫）/ 15（咖） |
| GL:gsqt | :169 | 调色板选色改对象色 |
| GL:gssc | :174 | 对象改随层（change P C bylayer） |
| GL:gdxys | :182 | 改对象色子程序（`change` 命令 + 循环 ssget） |
| GL:gcs1..gcs9 | :190 | 图层改色 1–9 |
| GL:gcs10 / gcs11 | :199/:200 | 图层改色 190 / 15 |
| GL:gcsqt | :201 | 调色板选色改图层色 |
| GL:gtcys | :205 | 改图层色子程序（VLA 遍历所选图元图层） |
| C:shf | :224 | cecolor 恢复 bylayer |

## 3. 新功能（NEW）
- **F1 真彩色→索引色转换（P1）**：del-rgb 只是删 420；补"真彩色转最近索引色"（AcCmColor 匹配，写 62+67=0），出转换对照表。
- **F2 颜色统计（P1）**：按颜色（62/420 分组）统计图元数量 + 色块预览条，落 at-stat 表格式；为"改色前评估"服务。
- **F3 图层色同步（P2）**：把图层颜色批量设为该层图元的主色（众数），一键"图层色跟随内容"。
- **F4 自定义色库（P2）**：change-color 对话框增加色库行（配置存 `@:get-config`，可存 3 组×9 色），替代固定 1–9。
- **F5 变单色预览（P2）**：one-color 落色前 dyndraw 预览 + 确认，避免全图误变（配合 I5 撤销）。

## 4. 改进（IMPROVE）
- **I1 改色对话框按钮映射错乱（P0）**：DCL 对象色区 key 11=随层、12=随块、10=其它，但 cond 映射为 re11→gs11(色15)、re12→gsqt(调色板)、re10→gssc(随层)、re13→gssc（无 13 号 tile，死代码）——即点"随层"变咖色、点"随块"开调色板、点"其它"才变随层，**byblock 完全未实现**；图层色区同理 key 60=其它却映射 gcs10(色190)，gcsqt 死代码（at-color.lsp:59-61 vs 123-149）。
- **I2 `(exit)` 终止 LISP 运行时（P0）**：one-color 取消选色时 `(if (null color) (exit))`（at-color.lsp:14）会直接退出 AutoLISP 会话，应改 `(princ)`。
- **I3 跨平台 ProgID（P1）**：`vlax-create-object "AutoCAD.AcCmColor.<ACADVER>"`（:16）在 ZWCAD/GStarCAD/BricsCAD 下创建失败；改 `"AcCmColor"` 或 `entity:get-truecolor` 同族取色对象。
- **I4 颜色源标志 67 未处理（P1）**：one-color 设 62、del-rgb 删 420 后，67=1（真彩色源）的图元显示不变；两处均需同步写 67=0。
- **I5 无撤销事务（P1）**：全部经 `change` 命令逐次执行，无法一次 UNDO 回滚；改 `entity:putdxf`/`vla-put-color` + undo mark/back（base 暂无事务函数，见第 6 节）。
- **I6 嵌套块未递归（P2）**：one-color 用 `block:ent-list` 单层遍历，块中块图元漏改。
- **I7 提示重复（P2）**：del-rgb `@::prompt` 与 `prompt` 双重提示（:35-36）。
- **I8 翻译文件缺失（P2）**：at-color-chs/cht.lang 为 GBK 编码且不在 `:FILES`，`:LOCALE "CHS"` 声明失效；转 UTF-8 并入包或删声明。

## 5. 重构（REFACTOR）
- **R1 24 个一行包装函数收敛（P1）**：`GL:gs1..11`/`GL:gcs1..11` 各 12 个一行函数 → 色表 `((1 . 1)(2 . 2)...(10 . 190)(11 . 15))` + 统一入口，defun 数 33→约 13。
- **R2 DCL 运行时解析（P2）**：change-color 写临时 dcl 再逐行 `vl-string-left-trim` 解析（:80-94）极脆弱；改数据表 + 程序化生成对话框，或 DCL 模板资源文件入 `:FILES`。
- **R3 VLA 对象比较（P2）**：GL:gtcys 用 `(= (equal vlay vlay1) nil)` 比较两个 VLA 图层对象（:218）；改图层名列表 `vl-position` 去重。
- **R4 前缀统一（P2）**：`GL:`/`C:` 遗留全局前缀 → `at-color::`，`C:shf` 提为菜单命令"恢复当前颜色"。

## 6. lib 协同
- 复用：`entity:putdxf`/`entity:getdxf`、`entity:get-color`/`entity:get-truecolor`、`block:ent-list`、`pickset:to-list`、`ui:dyndraw`（F5）
- 建议提取（≥2 包复用）：`entity:set-color`（62+420+67 三码一致写入，at-color/one-color 与 at-purge 清色需求共用）；undo mark/back 事务助手（at-color/at-purge/at-linetype 均有批量改属性需求）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 按钮映射 | P0 | 0.5h | 含补 byblock 实现 |
| I2 exit 修复 | P0 | 0.1h | — |
| I3 ProgID | P1 | 0.5h | — |
| I4 67 标志 | P1 | 1h | — |
| I5 事务化 | P1 | 1.5h | 依赖事务助手 |
| R1 包装收敛 | P1 | 0.5h | — |
| F1 真彩转换 | P1 | 3h | — |
| F2 颜色统计 | P1 | 3h | 配合 at-stat |
| F3 图层色同步 | P2 | 2h | — |
| F4 色库 | P2 | 2h | — |
| F5 预览 | P2 | 1.5h | 依赖 I5 |
| R2 DCL 重构 | P2 | 2h | — |
| R3/R4/I6-I8 | P2 | 2h | — |

合计 ≈21h

## 8. 已完成（2026-10，iMini 复现）
- **color-change.lsp 新增**（对应 iMini 一键变色 mini_yjbh / 全能改色）：
  - `c:color-change` 全能变色：所选/全图实体 变指定颜色（acad_colordlg）/ 颜色映射（1=2,3=4）/ 变随层
  - `c:color-by-layer` 按层变色：指定图层（通配符）全部实体批量变色（nil=随层）
  - `@color:set-ent-color`：62/420/67 三码一致写入（0~255 ACI 写 62 删 420 置 67=0；-1 随层 -2 随块；>255 真彩色整数写 420+62=256+67=1）
  - AI 入口：`at-color:ai-change`（ents mode param）、`at-color:ai-by-layer-color`（layer color），无交互，startundo/endundo 包裹
  - 菜单：Color → 全能变色 / 按层变色
- 与计划条目的关系：
  - 覆盖了 I4 的"67 标志同步写入"意图（新入口 `@color:set-ent-color` 均同步写 67），但既有 one-color/del-rgb 未改动，I4 本身仍待处理
  - F2（颜色统计）未做；"改色前预览确认"（F5）在 ai-change 中未加（AI 入口按约定不做交互确认）
- 遗留 P0（I1 按钮映射错乱、I2 (exit)）未动，优先处理
