# at-structure - @lisp 结构工具

结构绘图工具包，提供钢筋面积查询、钢筋绘制、箍筋绘制、柱截面绘制等功能，以及混凝土力学参数计算。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 结构工具 → 动态查面积 | `(at-structure:query-steelbar)` | 动态查询鼠标悬停文字的钢筋面积 |
| 结构工具 → 查钢筋面积 | `(at-structure:menu-get-area)` | 选中钢筋文字，返回钢筋面积 |
| 结构工具 → 画点钢筋 | `(at-structure:menu-draw-one-rebar)` | 在指定位置绘制单根点钢筋 |
| 结构工具 → 画钢筋排 | `(at-structure:menu-draw-edge-rebar)` | 绘制一排钢筋 |
| 结构工具 → 画箍筋 | `(at-structure:menu-draw-stirrup)` | 绘制箍筋（两点定义，水平/垂直时绘制单肢箍） |
| 结构工具 → 绘柱截面 | `(at-structure:menu-draw-columns)` | 从钢筋表中取信息绘制柱截面 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-structure:get-steel-area` | 解析钢筋文字字符串（如 `%%1328@100`），计算钢筋面积 |
| `at-structure:query-steelbar` | 动态查钢筋面积，鼠标悬停时实时显示 |
| `at-structure:menu-get-area` | 交互式选择钢筋文字并显示面积 |
| `at-structure:menu-draw-one-rebar` | 交互式绘制单根点钢筋 |
| `at-structure:menu-draw-edge-rebar` | 交互式绘制钢筋排 |
| `at-structure:menu-draw-stirrup` | 交互式绘制箍筋 |
| `at-structure:menu-draw-columns` | 从文字矩阵读取钢筋表并绘制柱截面 |

## 混凝土参数函数 (concrete.lsp)

| 函数 | 说明 |
|------|------|
| `concrete:fck` | 混凝土轴心抗压强度标准值 |
| `concrete:ftk` | 混凝土轴心抗拉强度标准值（非标准强度标号按直线插值） |
| `concrete:fc` | 混凝土轴心抗压强度设计值 |
| `concrete:ft` | 混凝土轴心抗拉强度设计值 |
| `concrete:ec` | 混凝土弹性模量 |
| `concrete:Gc` | 混凝土剪变模量 |

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-structure
```

或：

```
(@:package-install "at-structure")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-structure.lsp` | 主程序源码（菜单、钢筋查询、绘图） |
| `concrete.lsp` | 混凝土力学参数计算 |
| `seismic.lsp` | 抗震相关 |
| `stat.lsp` | 统计相关 |
| `beam.lsp` | 梁相关 |
| `column.lsp` | 柱相关 |
| `hotkey.lsp` | 快捷键 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.0.20
- 分类: 结构
- 网站: http://atlisp.cn
