# at-cnc - @CNC 测试版

CAD 曲线生成 G 代码，用于 CNC 数控机床加工 CAM。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| @CNC → 生成G代码 | `(at-cnc::gen-gcode)` | 选择曲线生成 G 代码文件 |
| @CNC → 配置@CNC | `(at-cnc::setup)` | 打开配置对话框 |
| @CNC → 删除刀路 | `(at-cnc::remove-route)` | 删除生成的刀路曲线 |
| @CNC → 打开Candle | `(at-cnc::open-candle)` | 用 Candle 打开生成的 NC 文件 |
| @CNC → 打开NC库 | `(at-cnc::explorer)` | 打开 NC 文件所在文件夹 |

## 配置项

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `@cnc::init` | 0 | 首行是否加载初始化指令 |
| `@cnc::units` | 0.001 | 加工精细度（最小精度，单位 mm） |
| `@cnc::r` | 6.0 | 刀具直径 |
| `@cnc::motor-speed` | 4000 | 主轴马达转速 |
| `@cnc::f` | 50 | 进给速率 |
| `@cnc::f-u` | 1.0 | U 轴进给速率 |
| `@cnc::cutter-compensation-left` | 0 | 刀具左补偿值 |
| `@cnc::cutter-compensation-right` | 0 | 刀具右补偿值 |
| `@cnc::chopping` | 0 | 工作时是否加冲程 |
| `@cnc::chopping-pause` | 10.0 | 冲程前暂停时间 |
| `@cnc::k-thickness` | 0.3 | 扩孔厚度 |
| `@cnc::k-times` | 30 | 扩孔次数 |
| `@cnc::rub-times` | 3 | 磨孔次数 |
| `@cnc::rub-f` | 300 | 磨孔进给速率 |
| `@cnc::U-axis` | 1 | 是否有 U 轴 |
| `@cnc::umotor-speed` | 800 | U 轴马达转速 |
| `@cnc::to-origin` | 1 | 完成后是否回库 |
| `@cnc::thickness` | 10.0 | 工件厚度（若曲线有厚度则采用曲线值） |
| `@cnc::layer-route` | CNC_ROUTE | 刀路曲线所在图层 |
| `@cnc::candle` | "" | Candle exe 文件路径 |
| `@cnc::nc-files` | CNC | 生成的 NC 文件路径 |
| `@cnc::syntek` | 0 | SYNTEK CE 系统（U 轴最大行程 2.0） |

## 核心函数

| 函数 | 说明 |
|------|------|
| `at-cnc::gen-gcode` | 主入口：选择 LWPOLYLINE/CIRCLE，生成 G 代码并保存 |
| `at-cnc::lwpl2gcode` | 多段线转 G 代码（直线/圆弧插补） |
| `at-cnc::circle2gcode` | 圆转 G 代码（U 轴铣孔或刀补铣孔） |
| `at-cnc::setup` | 配置对话框 |
| `at-cnc::remove-route` | 删除刀路图层上的对象 |
| `at-cnc::open-candle` | 调用 Candle 打开 NC 文件 |
| `at-cnc::explorer` | 资源管理器打开 NC 目录 |
| `at-cnc::n2s` | 数字转字符串（确保有小数点） |

## 使用流程

1. 在图中绘制要加工的闭合轮廓（LWPOLYLINE 或 CIRCLE）
2. 执行 `@CNC → 生成G代码`，选择曲线
3. 在生成的 NC 文件中查看 G 代码
4. 用 Candle 打开 NC 文件进行 CAM 仿真

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-cnc
```

或：

```
(@:package-install "at-cnc")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-cnc.lsp` | 主程序源码（G 代码生成、配置、UI） |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.1.9
- 分类: 机械
- 网站: http://atlisp.cn
