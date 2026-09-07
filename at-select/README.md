# @lisp选择工具

@lisp选择工具，如选择填充区中的某类图块等。记录选择集，选择集过滤等。

## 功能特性

### 选择菜单
| 命令 | 说明 |
|------|------|
| 选择设置 | 打开选择设置对话框 |
| 填充选块 | 选择一个填充，返回填充内的块 |
| 线域选块 | 选择一个单环闭合多段线，选中曲线内的块 |
| 线域选择 | 选择一个单环闭合多段线，选中曲线内的图元 |
| 填充选择 | 选择一个或多填充图形，再选择在填充图形内需要选中的图形 |
| 绘线指引 | 绘制指向选择集的线 |
| ss1 | 记录当前已选择的图形为ss1。如果没有选择且高亮的图形，则高亮ss1 |
| ss2 | 记录当前已选择的图形为ss2 |
| ss3 | 记录当前已选择的图形为ss3 |
| ss4 | 记录当前已选择的图形为ss4 |
| ss5 | 记录当前已选择的图形为ss5 |
| 分堆加框 | 将所选图形进行分堆，并为每个堆群加包围矩形框 |

### 选择1菜单
| 命令 | 说明 |
|------|------|
| 选择同类型 | 选择同类型的图形 |
| 选择同层 | 选择同层的图形 |
| 选择同色 | 选择同色的图形 |
| 选择同线型 | 选择同线型的图形 |
| 选择闭合线 | 选择闭合的多段线 |
| 选择未闭合线 | 选择未闭合的多段线 |
| 选择短线 | 选择短线，即小于给定长度的线 |
| 选择定长线 | 选择定长线，即给定的固定长度的线 |
| 选择相似线 | 选择相似曲线 |

## 函数列表
| 函数 | 说明 |
|------|------|
| `@select:setup` | 打开选择设置对话框 |
| `c:ss1` | 记录当前已选择的图形为ss1 |
| `c:ss2` | 记录当前已选择的图形为ss2 |
| `c:ss3` | 记录当前已选择的图形为ss3 |
| `c:ss4` | 记录当前已选择的图形为ss4 |
| `c:ss5` | 记录当前已选择的图形为ss5 |
| `boundarypath2pts` | 边界路径转栏选点集 |
| `interself-p` | 判断点集是否自交 |
| `at-select:select-blk-by-hatch` | 选择一个填充，返回填充内的块 |
| `at-select:select-blk-by-lwpl` | 选择一个单环闭合多段线，选中曲线内的块 |
| `at-select:select-by-lwpl` | 选择一个单环闭合多段线，选中曲线内的图元 |
| `at-select:select-by-hatch` | 选择一个或多填充图形，再选择在填充图形内需要选中的图形 |
| `@select:line-to-ss` | 绘制指向选择集的线 |
| `at-select:select-closed-lwpl` | 选择闭合的多段线 |
| `at-select:select-unclosed-lwpl` | 选择未闭合的多段线 |
| `at-select:select-sametype` | 选择同类型的图形 |
| `at-select:select-shortlines` | 选择短线，即小于给定长度的线 |
| `at-select:select-samelayer` | 选择同层的图形 |
| `at-select:select-samecolor` | 选择同色的图形 |
| `at-select:select-samelinetype` | 选择同线型的图形 |
| `at-select:select-similar` | 选择相似曲线 |
| `at-select:select-samelens-lines` | 选择定长线，即给定的固定长度的线 |
| `at-select:cluster-box` | 将所选图形进行分堆，并为每个堆群加包围矩形框 |

## 配置
| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `@select:blksname` | `""` | 选择时要匹配的块名 |
| `@select:onboundary` | `1` | 1 选择在边界上图元; 0 不选边界上的图元 |
| `curve:similarity` | `0.95` | 曲线相似度，0到1之间的值 |
| `@select:cluster-gap` | `1000` | 分堆时不同堆的图形之间的最小间距 |
| `@select:clusterbox-layer` | `"cluster"` | 分堆结果包围盒所在的图层 |

## 依赖
- `base` 包

## 安装
在 CAD 命令行执行：
```
@I at-select
```
或
```
(@:package-install "at-select")
```

## 源文件
| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单文件 |
| `at-select.lsp` | 核心选择功能：填充选块、线域选块、线域选择、填充选择、选择集记录(ss1-ss5)、绘线指引 |
| `select.lsp` | 属性选择功能：同类型、同层、同色、同线型、闭合线、未闭合线、短线、定长线、相似线 |
| `cluster-box.lsp` | 分堆加框功能：将所选图形进行分堆并加包围矩形框 |
| `menus.lsp` | 菜单注册：选择菜单和选择1菜单 |

## 作者
- **VitalGG** - vitalgg@gmail.com

## 版本
- 0.1.9

## 网站
- http://atlisp.cn
