# at-planning - 规划设计

规划设计相关功能，规划指标的计算汇总。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 规划 > 规划设置 | `(at-planning:setup)` | 打开配置对话框 |
| 规划 > 指定范围 | `(at-planning:set-range)` | 指定计算范围 |
| 规划 > 生成绿地 | `(at-planning:gen-greenland)` | 生成闭合绿地曲线 |
| 规划 > 用地面积 | `(at-planning:land-area)` | 生成各地块面积 |
| 规划 > 绿地面积 | `(at-planning:area-of-green)` | 生成绿地面积 |
| 规划 > 填充物件 | `(at-planning:hatch-zone)` | 重新填充各地块 |
| 规划 > 车位折减 | `(at-planning:reduction-green)` | 计算林荫车位折减 |
| 规划 > 算绿地率 | `(at-planning:greening-rate)` | 计算绿地率 |
| 规划 > 数据输入 | `(at-planning:input)` | 输入建筑面积数据 |
| 规划 > 经济指标 | `(at-planning:make-index)` | 生成经济指标表 |

## AI 调用（无交互）

| 函数 | 说明 |
|------|------|
| `@planning:ai-calc pt1 pt2` | 统计范围内用地/绿地/建筑基底面积、绿地率、车位数量 |
| `@planning:ai-index pt1 pt2 overground underground plot parking-underground` | 计算主要技术经济指标（密度、容积率、绿地率、车位、限值对比） |

```lisp
;; 只算面积
(@planning:ai-calc '(0 0) '(100000 80000))
;; 算经济指标；后四个参数可为 nil
(@planning:ai-index '(0 0) '(100000 80000) 30000 5000 nil 120)
```

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `@planning:land-layer` | 用地红线,用地界线 | 用地红线图层 |
| `@planning:building-layer` | 建筑轮廓 | 建筑轮廓图层 |
| `@planning:green-layer` | 绿地线 | 绿地图层 |
| `@planning:floor-area-ratio-limit` | 2.0 | 容积率限值 |
| `@planning:building-density-limit` | 35 | 建筑密度限值(%) |
| `@planning:greening-rate-limit` | 20 | 绿地率限值(%) |
| `@planning:floor-num` | 3 | 建筑层数 |

## 源文件

| 文件 | 说明 |
|------|------|
| `at-planning.lsp` | 主程序 |

## 依赖

- `base`
