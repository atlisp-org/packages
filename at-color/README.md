# at-color - @lisp Color

修改图元或图层颜色，移除真彩色（TrueColor）。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| Color → Modify color | `(at-color:change-color)` | 快速改色对话框（改对象颜色/改图层颜色） |
| Color → Remove TrueColor | `(at-color:del-rgb)` | 框选去除真彩色，用索引色替换 |
| Color → 图变单色 | `(at-color:one-color)` | 将图纸变为单一颜色（含块定义） |
| Color → 全能变色 | `(c:color-change)` | 所选实体 变指定颜色 / 颜色映射 / 变随层 |
| Color → 按层变色 | `(c:color-by-layer)` | 指定图层（可通配）的全部实体批量变色 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-color:change-color` | 弹出 DCL 对话框，支持 9 种索引色 + 随层/随块/其他，可改对象或图层颜色 |
| `at-color:del-rgb` | 框选含有真彩色（group code 420 > 0）的图元，删除 420 码用索引色替代 |
| `at-color:one-color` | 选择颜色后，将所有图元（含块定义内）变为该色；无选择时全图变色 |
| `c:color-change` | 全能变色：所选/全图实体 变指定颜色（acad_colordlg）/ 颜色映射（1=2,3=4）/ 变随层 |
| `c:color-by-layer` | 按层变色：指定图层（通配符）全部实体批量变色（颜色回车=随层） |
| `@color:set-ent-color` | 单图元改色：62/420/67 三码一致写入（0~255 ACI；-1 随层；-2 随块；>255 真彩色整数） |
| `at-color:ai-change` | AI 入口：全能变色（见「AI 调用」） |
| `at-color:ai-by-layer-color` | AI 入口：按层变色（见「AI 调用」） |

## DCL 对话框按钮说明

**改对象颜色区域：**
- 颜色 1-9 按钮：直接设为对应索引色
- 随层：设为 ByLayer
- 随块：设为 ByBlock
- 其它：弹出 AutoCAD 颜色选择对话框

**改图层颜色区域：**
- 颜色 1-9 按钮：将所选图元所在图层改为对应颜色
- 其它：弹出颜色选择对话框

## AI 调用

> 无交互入口，全部参数走形参；成功返回改动的图元数，失败返回 nil（提示用 princ 输出）；改动图形用 startundo/endundo 包裹。
> 颜色数约定：0~255=ACI 索引色；>255=真彩色整数（如 16711680=红）；-1=随层；-2=随块。

| 函数 | 参数 | 返回值 | 示例 |
|------|------|--------|------|
| `at-color:ai-change` | ents：选择集或 ename 列表，nil=全图；mode："color"=变指定颜色 / "map"=按映射 / "bylayer"=变随层；param：mode=color 时颜色数，mode=map 时映射表 `((原色 . 新色) ...)`，mode=bylayer 传 nil | 改动的图元数，失败 nil | `(at-color:ai-change nil "color" 1)` |
| `at-color:ai-by-layer-color` | layer：图层名（支持通配符，如 "WALL*"）；color：颜色数，nil=变随层 | 改动的图元数，图层不存在返回 nil | `(at-color:ai-by-layer-color "WALL" 1)` |

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
| `color-change.lsp` | 全能变色/按层变色（含 AI 入口） |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.1.2
- 分类: Color
- 网站: http://atlisp.cn
