# at-math - @lisp 数学工具

可操作单行文本的基本数学运算工具包，支持求和求积、批量四则运算、插序号、统计图元、行列运算等功能。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 数学2 → 范围选数 | `(@math:select-number)` | 范围选择数字 |
| 数学2 → 线域标数 | `(@math:mark-in-lwpl)` | 在多段线区域内标注数字 |
| 数学2 → 框选标数 | `(@math:mark-in-w)` | 框选区域标注数字 |
| 数学2 → 提物理量 | `(@math:unit-number)` | 提取带单位的物理量数值 |
| 数学2 → 代数运算 | `(@m:cal-text-post)` | 对文本中的数字进行代数运算 |

## 主要功能模块

### 文本求和 (sumtxt.lsp)

| 函数 | 说明 |
|------|------|
| `at-math:sumtxt` | 统计文本中的项与数据，对不同项汇总求和 |

### 几何工具 (tools-geometry.lsp)

几何计算相关函数。

### 数学工具 (tools-math.lsp)

基本数学运算函数。

### 公式工具 (formula.lsp)

公式解析与计算。

### 矩阵运算 (matrix.lsp)

行列运算相关功能。

### 标数工具 (mark-in-zone.lsp)

在指定区域内标注数字。

### 单位换算 (unit-number.lsp)

带单位物理量的提取与换算。

### 计算器 (cal.lsp)

交互式计算器功能。

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-math
```

或：

```
(@:package-install "at-math")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-math.lsp` | 主入口（help 函数） |
| `tools-geometry.lsp` | 几何工具 |
| `tools-math.lsp` | 数学工具 |
| `stat.lsp` | 统计功能 |
| `sumtxt.lsp` | 文本求和 |
| `formula.lsp` | 公式计算 |
| `matrix.lsp` | 矩阵运算 |
| `mark-in-zone.lsp` | 区域标数 |
| `unit-number.lsp` | 单位换算 |
| `cal.lsp` | 计算器 |
| `menu.lsp` | 菜单定义 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 1.0.58
- 分类: 通用
- 网站: http://atlisp.cn
