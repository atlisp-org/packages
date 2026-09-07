# saw-blade - 锯片

绘制圆锯片和直锯齿。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 锯片 > 锯片设置 | `(saw-blade:setup)` | 打开锯片参数配置对话框 |
| 锯片 > 绘圆锯片 | `(saw-blade:draw-circle)` | 绘制完整圆锯片 |
| 锯片 > 绘直锯齿 | `(saw-blade:draw-sawtooth)` | 绘制直排锯齿 |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `saw-blade:outer-diameter` | 700.0 | 圆盘外径 |
| `saw-blade:inner-diameter` | 100.25 | 圆盘内径 |
| `saw-blade:hole-diameter` | 250.0 | 孔分布圆直径 |
| `saw-blade:radius` | 1.5 | 自动圆角半径 |
| `saw-blade:maxlength-corner` | 3.0 | 自动圆角原始倒角最大线长 |
| `saw-blade:max-angle` | 80.0 | 自动圆角转角最大角度 |
| `saw-blade:min-angle` | 20.0 | 自动圆角转角最小角度 |
| `saw-blade:hole-number` | 6 | 圆盘孔洞数 |
| `saw-blade:hole-d` | 19.0 | 孔径 |
| `saw-blade:tooth-number` | 280 | 锯齿数 |
| `saw-blade:tooth-height` | 10 | 锯齿深度 |
| `saw-blade:tooth-step` | 10 | 直锯齿距 |

## 源文件

| 文件 | 说明 |
|------|------|
| `saw-blade.lsp` | 主程序，包含锯片和锯齿绘制功能 |

## 依赖

- `base`

## 使用说明

1. 运行 "锯片设置" 配置参数
2. 运行 "绘圆锯片" 选择圆心位置绘制
3. 或运行 "绘直锯齿" 选择起点和终点绘制

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.0.5
- **分类**: 机械
- **开源**: 否
