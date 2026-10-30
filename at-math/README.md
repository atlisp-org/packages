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
| 数学 → 全能计算 | `(at-math:cale-txt)` | 选中数字/算式文字，求和、求积或逐项求值 |

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

### 全能文字计算 (cale-txt.lsp)

| 函数 | 说明 |
|------|------|
| `at-math:cale-txt` | 交互命令：选中数字/算式文字，求和、求积或逐项求值 |
| `at-math:ai-cale` | AI 入口：无交互计算，返回结果值与文字 ename |
| `@math:cale-eval` | 内置四则表达式解析器（+ - * / 括号、小数、一元正负号，不使用 eval） |

## AI 调用

| 函数 | 说明 | 示例 |
|------|------|------|
| `at-math:ai-cale` | 对选中文字求和/求积/逐项求值，无交互、不弹窗；sum/prod 返回 `(结果值 文字ename)`，eval 返回 `((值 . 文字ename) ...)` | `(at-math:ai-cale (ssget '((0 . "*text"))) "sum" (getpoint))`<br>`(at-math:ai-cale ents "eval" nil)` |

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
| `cale-txt.lsp` | 全能文字计算 |
| `menu.lsp` | 菜单定义 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 1.0.58
- 分类: 通用
- 网站: http://atlisp.cn
