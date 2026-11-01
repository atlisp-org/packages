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
| 链式选择 | 从一条线出发，沿首尾相接递归选出整条链 |
| 快速选择 | 按过滤表达式（图层=墙,块=门*,颜色=1）快速选择 |
| 获得选择集 | 按过滤表达式获得选择集，另支持 类型=LINE |
| 触碰选择 | 选中实体，自动选中与其相交或端点相接的所有实体（递归） |
| 曲线对齐 | 曲线对齐 |
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
| `c:quick-sel` | 快速选择：按过滤表达式（图层=墙,块=门*,颜色=1，AND，通配符）选择匹配图元 |
| `c:getsel` | 获得选择集：同快速选择，另支持 类型=LINE,ARC 等 |
| `c:touch-sel` | 触碰选择：选中实体，自动选中与其相交或端点相接的所有实体（递归扩展） |
| `at-select:ai-quick` | AI 入口：按过滤表达式快速选择，返回 ename 列表 |
| `at-select:ai-getsel` | AI 入口：按表达式获得选择集（支持 类型=），返回 ename 列表 |
| `at-select:ai-chain` | AI 入口：从给定曲线出发沿首尾相接的链选出整条链 |
| `at-select:ai-touch` | AI 入口：选中与给定实体相交或端点相接的所有实体（递归） |

## AI 调用

> 无交互入口，全部参数走形参；成功返回 ename 列表，失败返回 nil（提示用 princ 输出）。

| 函数 | 参数 | 返回值 | 示例 |
|------|------|--------|------|
| `at-select:ai-quick` | filter：过滤表达式（图层=/块=/颜色=，多条件 AND，通配符；颜色支持 bylayer/随层），nil=全图 | ename 列表，失败 nil | `(at-select:ai-quick "图层=墙,颜色=1")` |
| `at-select:ai-getsel` | filter：同 quick 语法，另支持 类型=LINE,ARC（值可含多个逗号分隔类型） | ename 列表，失败 nil | `(at-select:ai-getsel "类型=LINE,图层=墙")` |
| `at-select:ai-chain` | start-ent：起始曲线实体；tol：端点容差（nil 用 1e-6） | 链上 ename 列表（含起始实体），失败 nil | `(at-select:ai-chain (car (entsel)) 0.001)` |
| `at-select:ai-touch` | ent：曲线实体；tol：容差（nil 用 0.001） | ename 列表（含 ent），失败 nil | `(at-select:ai-touch (car (entsel)) 0.001)` |

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
| `chain-sel.lsp` | 链式选择 |
| `object-align.lsp` | 曲线对齐 |
| `quick-sel.lsp` | 快速选择/获得选择集/触碰选择：过滤引擎（图层=/块=/颜色=/类型=）、链式选择、触碰选择 |
| `menus.lsp` | 菜单注册：选择菜单和选择1菜单 |

## 作者
- **VitalGG** - vitalgg@gmail.com

## 版本
- 0.1.9

## 网站
- http://atlisp.cn
