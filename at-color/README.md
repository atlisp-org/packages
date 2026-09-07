# at-color - @lisp Color

修改图元或图层颜色，移除真彩色（TrueColor）。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| Color → Modify color | `(at-color:change-color)` | 快速改色对话框（改对象颜色/改图层颜色） |
| Color → Remove TrueColor | `(at-color:del-rgb)` | 框选去除真彩色，用索引色替换 |
| Color → 图变单色 | `(at-color:one-color)` | 将图纸变为单一颜色（含块定义） |

## 函数

| 函数 | 说明 |
|------|------|
| `at-color:change-color` | 弹出 DCL 对话框，支持 9 种索引色 + 随层/随块/其他，可改对象或图层颜色 |
| `at-color:del-rgb` | 框选含有真彩色（group code 420 > 0）的图元，删除 420 码用索引色替代 |
| `at-color:one-color` | 选择颜色后，将所有图元（含块定义内）变为该色；无选择时全图变色 |

## DCL 对话框按钮说明

**改对象颜色区域：**
- 颜色 1-9 按钮：直接设为对应索引色
- 随层：设为 ByLayer
- 随块：设为 ByBlock
- 其它：弹出 AutoCAD 颜色选择对话框

**改图层颜色区域：**
- 颜色 1-9 按钮：将所选图元所在图层改为对应颜色
- 其它：弹出颜色选择对话框

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-color
```

或：

```
(@:package-install "at-color")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-color.lsp` | 主程序源码（DCL 对话框、颜色操作） |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.1.2
- 分类: Color
- 网站: http://atlisp.cn
