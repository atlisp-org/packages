# 迷你装修（at-interior）

装修设计工具包：柜类平面/立面/剖面、窗帘盒、玻璃隔断、天花轮廓框/平面线/线立面、
石膏板剖面、电线/灯具平面、木板/玻璃/石材/型钢/钢框/石材墙截面、焊缝/软管/管线/背板开孔。
复现 iMini 装修类命令（22 个，每个均有交互入口与无交互 `ai-*` 入口）。

- 包名：`at-interior`（菜单组「装修工具」，分类：建筑工程，依赖 `base`）
- 版本：0.1.0
- 单位：mm；侧向约定 `side=0` 左侧（p1→p2 逆时针 90°），`side=1` 右侧

## 安装

CAD 命令行：`@I at-interior`，或

```lisp
(@:package-install "at-interior")
```

菜单：`装修工具`（22 项，分柜类/天花灯具/截面/细部四组，与下表一一对应）。

## 命令

| 命令（菜单） | 交互入口 | 说明 |
|---|---|---|
| 衣柜平面 | `at-interior:closet` | 背边两点 + 深度 + 门数 + 侧：背边矩形、门分格线、0.55 深挂衣杆线 |
| 柜体平面 | `at-interior:cabinet` | 底边 + 深度 + 抽屉数 + 门数：外轮廓、抽屉区（p1 端 0.4W）、门区开放格 + 拉手 |
| 柜体立面 | `at-interior:cabinet-elev` | 底边 + 柜高 + 门数：轮廓、门分格线、顶部拉手短线 |
| 柜体剖面 | `at-interior:cabinet-sec` | 底边 + 柜高 + 搁板数 + 背板厚：轮廓、18 厚搁板均布、背板（靠 p2 端） |
| 窗帘盒 | `at-interior:curtain` | 窗线 + 盒深 + 间隙 + 拉绳：盒体矩形、窗帘线、可选拉绳（0.8W 处） |
| 玻璃隔断 | `at-interior:glass-wall` | 隔墙线 + 厚 + 门宽 + 门洞位置：双线、门洞断开斜纹玻璃带、门扇 + 开启弧 |
| 天花轮廓框 | `at-interior:elev-frame` | 选闭合折线 + 偏出高度 + 方向 + 方柱边长：各边立面矩形 + 角部方柱 + H= 文本 |
| 天花平面线 | `at-interior:jianxian-plan` | 对角点 + 内缩 + 标高：轴对齐外框、内缩线、灯槽环带（宽 50）、中心标高符号 |
| 天花线立面 | `at-interior:jianxian-elev` | 地线 + 主顶标高 + 槽距 + 下吊 + 板厚：地/墙/主顶/灯槽/副顶/板厚线 + 下吊灯 + 标高 |
| 石膏板剖面 | `at-interior:gypsum-sec` | 板底线 + 下吊 + 吊杆间距：梁底线、主龙骨线（-drop+50）、板顶/板底线、吊杆（200 起） |
| 电线平面 | `at-interior:elec-line` | 依次拾取点 + 开关：折线（端点重合自动闭合）、开关符号（圆 r15 + 刀杆 + S1） |
| 灯具平面 | `at-interior:light-plan` | 选闭合曲线 + 间距 + 块名：沿曲线等距布灯具符号（圆 r50 + 斜十字）或 INSERT 块 |
| 木板截面 | `at-interior:sec-wood` | 底边 + 板厚 + 材料：填充矩形（实木 STITCH / 多层板细 STITCH / 实心 SOLID） |
| 玻璃截面 | `at-interior:sec-glass` | 底边 + 类型：单片 6mm 或双片 6+6（片间空腔 14），SLANT 斜纹 |
| 石材截面 | `at-interior:sec-stone` | 底边 + 石板厚 + 挂件间距：SAND 石板 + 背面挂件（30 短线 + r5 圆，自 x=50 起） |
| 型钢截面 | `at-interior:sec-steel` | 底边 + 类型 + 尺寸：plate 板 / i 工字 / h 型钢（12 点轮廓），SOLID 填充 |
| 钢框截面 | `at-interior:sec-steel-frame` | 底边 + 框高 + 框腿厚：外框 + 内矩形 + 环带 SOLID 填充 |
| 石材墙剖面 | `at-interior:sec-stone-wall` | 底边 + 石板厚 + 间隙 + C 型钢间距：地线、120 墙身（CONCRETE）、C 型钢、挂件、海棠角石板 |
| 焊缝 | `at-interior:weld` | 焊缝线 + 类型 + 角焊缝高：角焊缝（三角，间距 100）/ 对接焊缝（宽 3 线 + V 符号）+ L= 标注 |
| 软管 | `at-interior:flex` | 中心线 + 半径：双 DASHED 偏移线 + 两端管件（长 40） |
| 管线 | `at-interior:tube` | 折线点 + 半径 + 管径 + 支管长：双线墙（斜接）、最长段中点 DN 标注、内顶点支管 |
| 背板开孔 | `at-interior:back-holes` | 底边 + 孔间距：底边线 + 均布圆孔（r5 同心 r1.5，自 x=50 起） |

## AI 调用

以下入口**无交互**（不弹窗、不取点），可直接由 MCP `eval_lisp` 调用；
成功返回图元名表，失败返回 `nil` 并用 `princ` 说明原因。缺省值在函数内补全。

| 函数 | 参数 | 说明 / 示例 |
|---|---|---|
| `at-interior:ai-closet` | `p1 p2 depth doors side` | 衣柜平面。depth 缺省 600，doors 缺省 2，side 0/1 缺省 0。`(at-interior:ai-closet '(0 0) '(2400 0) 600 3 0)` |
| `at-interior:ai-cabinet` | `p1 p2 depth drawers doors side` | 柜体平面。depth 缺省 550，drawers/doors 缺省 0（doors=0 时门区均分 3 开放格）。`(at-interior:ai-cabinet '(0 0) '(3000 0) 550 2 2 0)` |
| `at-interior:ai-cabinet-elev` | `p1 p2 height doors` | 柜体立面。height 缺省 800，doors 缺省 2（1=不分格）。`(at-interior:ai-cabinet-elev '(0 0) '(1200 0) 800 2)` |
| `at-interior:ai-cabinet-sec` | `p1 p2 height shelves back-thk` | 柜体剖面。height 缺省 800，shelves 缺省 3（搁板厚 18），back-thk 缺省 9（0=不画）。`(at-interior:ai-cabinet-sec '(0 0) '(500 0) 800 3 9)` |
| `at-interior:ai-curtain` | `p1 p2 box-depth gap cord` | 窗帘盒。box-depth 缺省 200，gap 缺省 100，cord 1=画拉绳（缺省 0）。`(at-interior:ai-curtain '(0 0) '(1800 0) 200 100 1)` |
| `at-interior:ai-glass-wall` | `p1 p2 thickness door-w door-pos` | 玻璃隔断。thickness 缺省 100，door-w 缺省 900（0=无门），door-pos 门洞中心比例 0~1 缺省 0.5。`(at-interior:ai-glass-wall '(0 0) '(4000 0) 100 900 0.5)` |
| `at-interior:ai-elev-frame` | `pline-ent height side thk` | 天花轮廓框。`pline-ent` 闭合折线 ename；height 缺省 3600，side 0=外扩（按质心判定）/1=内收，thk 方柱边长缺省 200。`(at-interior:ai-elev-frame (car (entsel "选折线: ")) 3600 0 200)` |
| `at-interior:ai-jianxian-plan` | `p1 p2 offset elev` | 天花平面线。对角点；offset 内缩缺省 500（灯槽宽固定 50），elev 缺省 3000。`(at-interior:ai-jianxian-plan '(0 0) '(6000 4000) 500 3000)` |
| `at-interior:ai-jianxian-elev` | `p1 p2 h1 dist drop thk` | 天花线立面。h1 主顶标高缺省 3600，dist 灯槽距墙缺省 300，drop 下吊缺省 400，thk 板厚缺省 12。`(at-interior:ai-jianxian-elev '(0 0) '(5000 0) 3600 300 400 12)` |
| `at-interior:ai-gypsum-sec` | `p1 p2 drop rod-gap` | 石膏板剖面。drop 下吊缺省 150，rod-gap 吊杆间距缺省 1000（自 p1 起 200 首吊）。`(at-interior:ai-gypsum-sec '(0 0) '(6000 0) 150 1000)` |
| `at-interior:ai-elec-line` | `lamp-pts switch` | 电线平面。`lamp-pts` 点表（≥2，端点重合自动闭合）；`switch` 开关点（nil=不画）。`(at-interior:ai-elec-line '((100 100) (500 100) (500 400)) '(600 500))` |
| `at-interior:ai-light-plan` | `pline-ent dist blk` | 灯具平面。`pline-ent` 闭合曲线 ename；dist 间距缺省 1000；blk 灯具块名（可空，空=默认符号）。`(at-interior:ai-light-plan (car (entsel "选曲线: ")) 1000 nil)` |
| `at-interior:ai-sec-wood` | `p1 p2 thk kind` | 木板截面。thk 缺省 18；kind：`wood` 实木 / `ply` 多层板（缺省）/ `solid` 实心。`(at-interior:ai-sec-wood '(0 0) '(1200 0) 18 "ply")` |
| `at-interior:ai-sec-glass` | `p1 p2 typ` | 玻璃截面。typ 1=单片 6mm（缺省）/ 2=双片 6+6。`(at-interior:ai-sec-glass '(0 0) '(1500 0) 2)` |
| `at-interior:ai-sec-stone` | `p1 p2 thk hanger-gap` | 石材截面。thk 缺省 20，hanger-gap 挂件间距缺省 600。`(at-interior:ai-sec-stone '(0 0) '(800 0) 20 600)` |
| `at-interior:ai-sec-steel` | `p1 p2 typ a b c d` | 型钢截面。typ `plate`/`i`/`h`（缺省 h）；plate: a=板厚缺省 10；i/h: a=总高缺省 200、b=翼缘宽缺省 100、c=翼缘厚缺省 10、d=腹板厚缺省 6。`(at-interior:ai-sec-steel '(0 0) '(200 0) "h" 200 100 10 6)` |
| `at-interior:ai-sec-steel-frame` | `p1 p2 height leg` | 钢框截面。height 缺省 150，leg 框腿厚缺省 8。`(at-interior:ai-sec-steel-frame '(0 0) '(1500 0) 150 8)` |
| `at-interior:ai-sec-stone-wall` | `p1 p2 thk gap c-int` | 石材墙剖面。thk 石板厚缺省 20，gap C 型钢与石板间隙缺省 20，c-int C 型钢间距缺省 1000。`(at-interior:ai-sec-stone-wall '(0 0) '(3000 0) 20 20 1000)` |
| `at-interior:ai-weld` | `p1 p2 typ leg` | 焊缝。typ `fillet` 角焊缝（缺省，自 x=50 起间距 100，base 8）/ `butt` 对接焊缝；leg 角焊缝高缺省 6。`(at-interior:ai-weld '(0 0) '(2000 0) "fillet" 6)` |
| `at-interior:ai-flex` | `p1 p2 radius` | 软管。radius 缺省 25；双 DASHED 线 + 两端管件。`(at-interior:ai-flex '(0 0) '(1000 0) 25)` |
| `at-interior:ai-tube` | `pts radius dn branch` | 管线。pts 点表（≥2）；radius 缺省 50；dn 标注缺省 `DN<radius*2>`；branch 支管长缺省 0。`(at-interior:ai-tube '((0 0) (600 0) (600 600)) 50 "DN100" 0)` |
| `at-interior:ai-back-holes` | `p1 p2 gap` | 背板开孔。gap 孔间距缺省 300。`(at-interior:ai-back-holes '(0 0) '(1200 0) 300)` |

## 图层

| 图层 | 颜色 | 用途 |
|---|---|---|
| `mi-gj` | 8 | 柜类/窗帘盒（衣柜、柜体、窗帘盒） |
| `mi-glass` | 7 | 玻璃隔断 |
| `mi-ceil` | 8 | 天花（轮廓框、平面线、线立面、石膏板剖面） |
| `mi-dengju` | 1 | 灯具/电线 |
| `mi-sec` | 8 | 板材/玻璃/石材/型钢/钢框/石材墙截面 |
| `mi-weld` | 8 | 焊缝 |
| `mi-tube` | 8 | 软管/管线（软管线用 DASHED） |
| `mi-hole` | 8 | 背板开孔 |

图层由 `mi-ensure-layer` 按需创建（缺失的 DASHED 线型会自动加载，失败回退 CONTINUOUS）。

## 实现要点

- 公共几何/绘制工具（`mi-*`，见 `at-interior.lsp`）：向量运算、直线/多段线/圆/圆弧/
  文本/矩形图元、单边界与环带填充（`mi-hatch`/`mi-hatch-ring`）、材料填充映射
  （`mi-fill-spec`：glass→SLANT、wood→STITCH、ply→细 STITCH、stone→SAND、
  conc→CONCRETE、其他→SOLID）、标高符号（`mi-elev-symbol`）、灯具符号
  （`mi-light-symbol`）、H/I 型钢 12 点轮廓（`mi-steel-profile`）、折线偏移
  （`mi-pline-offset`，顶点斜接）、曲线等距采样（`mi-curve-pts`）。
- 交互入口统一用 `(command "_.UNDO" "Begin")` / `(command "_.UNDO" "End")` 包裹
  （部分 CAD 如 ZWCAD 24.1s 没有 `startundo`/`endundo`），结尾 `(princ)`。
- 依赖加载用 `(if (null <fn>) (require 'ns:*))`（部分 CAD 没有 `fboundp`）。
- 交互命令的中文选项（材料/类型等）在交互函数内映射为内部代号，`ai-*` 入口直接收代号。
