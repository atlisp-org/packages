# at-number - 编号工具

给图形编号，支持矩形、圆、图块等相同图形的自动编号。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 编号工具 → 编号设置 | `(at-number:setup)` | 打开编号配置对话框 |
| 编号工具 → 图形编号 | `(at-number:number-entity)` | 对选中的图形进行自动编号 |
| 编号工具 → 删除编号 | `(at-number:delete-number)` | 删除编号文字 |

## 配置项

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `@number:layer` | number | 编号文字所在图层 |
| `@number:order` | yx | 编号的位置排序方式（yx 表示先 Y 后 X） |
| `@number:fontsize` | 2.5 | 编号字体大小（实际值 = 该值 x @lisp 绘图比例） |

## 函数

| 函数 | 说明 |
|------|------|
| `at-number:setup` | 弹出配置对话框修改编号设置 |
| `at-number:number-entity` | 选择图形后按排序规则自动编号，文字放在图形质心位置 |
| `at-number:delete-number` | 删除编号图层上所有数字文字 |

## 使用说明

1. 先选择相似图形（可选），再执行 `图形编号`
2. 编号按指定排序规则（yx/xy）自动排列
3. 编号文字自动放置在图形质心位置
4. 需要清除编号时执行 `删除编号`

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-number
```

或：

```
(@:package-install "at-number")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-number.lsp` | 主程序源码 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.0.6
- 分类: Common
- 网站: http://atlisp.cn
