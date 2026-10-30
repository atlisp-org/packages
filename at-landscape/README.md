# @lisp景观（at-landscape）

景观工具：灌木边缘线、立面草、苗木面积统计、铺装、路线里程、界址点等，复现 iMini 景观类命令。

## 安装

CAD 命令行：`@I at-landscape` 或 `(@:package-install "at-landscape")`。

依赖 `base`（自动安装）。

## 菜单

加载后屏幕菜单「迷你景观」中包含全部命令。

## 命令

| 命令 | 说明 |
|---|---|
| `at-landscape:shrub-edge` | 多段线变灌木边缘线（外侧不规则波浪轮廓） |
| `at-landscape:grass` | 画立面草（随机弯曲草叶） |
| `at-landscape:plant-text` | 苗木标注（数量「N 株」/ 灌木面积「X m²」） |
| `at-landscape:mm-list` | 苗木统计表（按块名/图层统计数量与面积） |
| `at-landscape:tree-line` | 沿线布置树符号 |
| `at-landscape:area` | 内轮廓面积标注 + 随机浅色填充 |
| `at-landscape:add-path` | 封闭多段线搭桥（双线边界 + 桥板线） |
| `at-landscape:remove-path` | 封闭多段线挖路（双线边界 + 铺装填充） |
| `at-landscape:mileage` | 路线里程标注（K0+000 格式） |
| `at-landscape:jzd` | 界址点编号（JZ1、JZ2…） |

## AI 调用

所有 `ai-` 入口无交互、不弹窗，成功返回 ename 或列表，失败返回 `nil`。

| 函数 | 参数 | 说明 |
|---|---|---|
| `at-landscape:ai-shrub-edge` | pline-ent [dist] [amp] | 灌木边缘线，dist 偏距缺省 300，amp 波幅缺省 30 |
| `at-landscape:ai-grass` | p1 p2 [n] [h] | 立面草，n 草叶数缺省 15，h 高度缺省 300 |
| `at-landscape:ai-plant-text` | ents mode [type] | mode = `count` 标数量 / `area` 标面积 |
| `at-landscape:ai-mm-list` | [blk-names] [out-file] | 苗木统计表，返回数据列表 |
| `at-landscape:ai-tree-line` | pline-ent [dist] [r] | 沿线布树，dist 间距、r 冠径 |
| `at-landscape:ai-area` | bounds [factor] [colors] | 面积标注 + 随机色填充；factor 缺省 1e-6（mm²→m²） |
| `at-landscape:ai-add-path` | pline-ent [width] [plank-gap] | 搭桥，width 缺省 2000，plank-gap 缺省 500 |
| `at-landscape:ai-remove-path` | pline-ent [width] [pattern] | 挖路，pattern 缺省 `ANSI31`（找不到图案只画边界） |
| `at-landscape:ai-mileage` | pline-ent [dist] [start] | 里程标注，dist 缺省 20000，start 起始里程 |
| `at-landscape:ai-jzd` | pline-ent [prefix] | 界址点编号，prefix 缺省 `JZ` |

示例：

```lisp
;; 选一条闭合多段线，标注面积并随机填充
(at-landscape:ai-area (list (car (entsel))) 1e-6 T)

;; 沿路线每 20 米标里程
(at-landscape:ai-mileage (car (entsel)) 20000 0)

;; 沿多段线布树，间距 5000，冠径 1200
(at-landscape:ai-tree-line (car (entsel)) 5000 1200)
```

## 资源

- `shrub-names.txt` / `tree-names.txt`：灌木、乔木名称词库（每行一词），供苗木标注与统计时取用。

## 说明

- 面积换算默认按图纸单位为毫米（系数 1e-6），按米绘制的图纸请传 `1e0`。
- 图案填充依赖 `acad.pat` 中存在对应图案名；缺失时只画边界并 `princ` 提示。
- 本包复现 iMini 景观类命令，未实现项见 `PLAN.md`。
