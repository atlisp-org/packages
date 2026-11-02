# 领域包 Agent 统计入口 — 2026-11-01

对应 `PLAN-INDEX.md` 的「Agent 适配」第 4 项：为 at-elec / at-hvac / at-structure /
at-planning 补批量计算与统计的无交互入口。所有新入口均为 `ai-*` 命名、参数化、
不调用 `ssget` 交互选择、不弹窗、不取点。

## at-elec 0.1.1

新增 `ai.lsp`：

| 函数 | 说明 |
|---|---|
| `@elec:ai-load-stat sel` | 统计设备块负荷，返回 count/total/demand/factor；`sel` 为 nil 时全图 INSERT |
| `@elec:ai-equip-stat sel` | 按块名统计设备数量 |
| `@elec:ai-circut-numbering sel prefix start` | 给曲线回路编号，返回编号数量 |
| `@elec:ai-cable-label ent cable-type` | 给曲线标注电缆型号，返回文字 ename |

同时修复：

- **I1**：`@::*prefix*` → `@:*prefix*`（draw-readme / draw-plan-example）
- **I6**：需要系数配置化 `@elec:demand-factor`（默认 0.7）
- **R2**：交互命令与 AI 入口共用 `@elec:stat-load` / `@elec:stat-equip`
- 普通块属性也参与功率统计（动态属性“功率” → 属性“功率” / `POWER`）

CAD 实测（192.168.1.53，AutoCAD 24.1s）：

```lisp
(@elec:ai-load-stat (list blk))  ;; 块属性 POWER=100
=> (("count" . 1) ("total" . 100.0) ("demand" . 70.0) ("factor" . 0.7))
(@elec:ai-equip-stat (list blk)) => (("TEST-EQ-53D" . 1))
(@elec:ai-circut-numbering (list l1 l2) "WL" 1) => 2
(@elec:ai-cable-label l1 "YJV-3x2.5") => 文字 ename，图层 ELEC-CABLE
```

## at-hvac 0.0.32

新增 `ai.lsp`：

| 函数 | 说明 |
|---|---|
| `@hvac:ai-fengji-stat sel` | 风机型号数量统计，未知型号保留为“未知型号” |
| `@hvac:ai-fengji-list` | 内置风机盘管型号参数列表 |
| `@hvac:ai-fengji-info model` | 按型号返回冷量/热量/功率/风量 |
| `@hvac:ai-equip-balance sel` | 分集水器回路数/总长/最长/最短/差值/超限 |
| `@hvac:ai-equip-bom sel` | 分集水器 BOM（FPQ-回路数） |

同时统一风机统计走 `@hvac:ai-fengji-stat`，并修正设备参数表风量取错列
（原来用热量列，改为风量列）。

CAD 实测：

```lisp
(@hvac:ai-fengji-stat (list i1 i2))  ;; 两块 MODEL=FP85
=> (("FP85" . 2))
(@hvac:ai-fengji-info "FP85")
=> 冷量 4520 / 热量 7670 / 功率 84 / 风量 850
(@hvac:ai-equip-balance nil) => nil   ;; 当前图无 TCH_DBHEQUIPMENT
(@hvac:ai-equip-bom nil)     => nil
```

分集水器平衡/BOM 依赖 `TCH_DBHEQUIPMENT` 自定义对象，测试图无该对象，仅验证空集
不报错；实际图纸需带天正暖通分集水器。

## at-structure 0.0.21

新增 `ai.lsp`：

| 函数 | 说明 |
|---|---|
| `at-structure:ai-steel-area str` | 解析钢筋文字返回面积（mm²） |
| `at-structure:ai-steel-area-text ent` | 解析单行文字中 `;` 分隔的多组钢筋 |
| `at-structure:ai-stat-beam-size sel` | 梁截面尺寸去重排序；nil 时扫描梁图层 |

`at-structure:stat-beam-size` 改为调用 AI 核心，减少重复逻辑。

CAD 实测：

```lisp
(at-structure:ai-steel-area "%%1328@100")      => 502.655
(at-structure:ai-steel-area "2%%13220+3%%13222") => 1768.72
(at-structure:ai-steel-area-text text)          => (("2%%13220" . 628.319) ...)
(at-structure:ai-stat-beam-size (list t2 t3))   => ("300X700" "300x600")
```

## at-planning 0.1.2

新增 `ai.lsp`：

| 函数 | 说明 |
|---|---|
| `@planning:ai-calc pt1 pt2` | 统计用地/绿地/建筑基底面积、绿地率、车位 |
| `@planning:ai-index pt1 pt2 overground underground plot parking-underground` | 技术经济指标与限值对比 |

关键点：范围筛选改为 `ssget "_X"` + 包围盒判断，不再依赖当前视图；即使目标范围
不在屏幕内也能统计。

CAD 实测（测试地块 10000㎡、绿地 400㎡、建筑基底 100㎡）：

```lisp
(@planning:ai-calc '(0 0) '(200 200))
=> land-total 10000.0 / green-total 400.0 / greening-rate 0.04 /
   building-total 100.0 / parking 0

(@planning:ai-index '(0 0) '(200 200) 1000 200 800 5)
=> density 1.0% / far 0.08 / greening-rate 4.0% /
   parking-total 5 / density-pass T / far-pass T / greening-pass nil
```

反向范围 `(200 200) → (0 0)` 也能正确统计。

## 发布

四个包均已上传 release + stable，主站 / s1 / s3 核验版本：

| 包 | 版本 |
|---|---|
| at-elec | 0.1.1 |
| at-hvac | 0.0.32 |
| at-structure | 0.0.21 |
| at-planning | 0.1.2 |

## 后续

- at-hvac 的分集水器平衡/BOM 需要一张带 `TCH_DBHEQUIPMENT` 的实测图补测。
- at-elec 的 `@elec:ai-circut-numbering` 目前按曲线中点上方固定 200 偏移；后续可加
  偏移/图层/方向参数。
- 领域包还有 at-cnc / at-3d / at-lab / at-documents 等未补 `ai-*` 入口。
