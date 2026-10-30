# at-block

> @lisp Block - 图块操作工具包

| 属性 | 值 |
|------|-----|
| 包名 | `at-block` |
| 全名 | @lisp Block |
| 版本 | 1.1.11 |
| 作者 | VitalGG |
| 邮箱 | vitalgg@gmail.com |
| 分类 | 图块相关 |
| 依赖 | `base` |
| 开源 | 否 |
| 免费 | 是 |
| 语言 | CHS |
| 网址 | http://atlisp.cn |

## 简介

块替换（不支持动态块）、块编号、块视图切换、外部参照管理等图块相关操作工具集。定义好块名和用于编号的属性名，即可从上到下、从左到右自动编号。

## 安装

在 CAD 命令行执行：

```
@I at-block
```

或：

```lisp
(@:package-install "at-block")
```

## 配置项

| 配置键 | 默认值 | 说明 |
|--------|--------|------|
| `@block:block-name` | 块名 | 用于排号的块名称；可填多个，用逗号/分号/空格分隔（如 `车位,车位-无障碍`），多个块名共用一条连续编号 |
| `@block:attribute-name` | 属性名 | 用于排号的块内属性的名称 |
| `@block:attribute-prefix` | `""` | 用于排号的块内属性值前缀 |
| `@block:attribute-suffix` | `""` | 用于排号的块内属性值后缀 |
| `@block:overlay-index` | `0` | 覆盖用于排号的块内属性原值；0=全覆盖；n=替换原内容以-分隔的第n部分 |
| `@block:xref-layer` | `xref-lock` | 用于放置外部参照的图层名 |
| `@block:sort-order` | `xY` | 排序规则；xyXY任意两两组合，y在前表示y坐标优先，大X表示从右到左排序；填 `snake`（或 `S`/`蛇形`）按行蛇形排序 |
| `@block:sort-fuzz` | `10,10` | 按位置排序时坐标的容差，逗号分隔不同轴容差 |
| `@block:snake-row-tol` | `1200` | 蛇形排序：同一排的 y 容差（取车位深度的一半左右） |
| `@block:snake-min-row` | `5` | 蛇形排序：一排数量达到该值才参与换向；不足的零星车位（墙边单列等）排在最后 |
| `@block:size` | `1000` | 用于检测块重叠时块的最大尺寸（对角线长度） |
| `@block:overlap-tol` | `0.1` | 重叠检测容差；两块包围盒重叠量小于等于该值视为不重叠（排除并排块边线相接的误判） |
| `@block:lib` | `D:/Design/standard/lib/` | 以块方式管理的图库路径 |

## 菜单

### Block 菜单

| 菜单项 | 函数 | 说明 |
|--------|------|------|
| Block Config | `@block:config` | 打开块配置对话框 |
| Block Replace | `@block:subst` | 将目标块替换成源块 |
| block auto numbering | `@block:set-number` | 块自动编号 |
| Numbering by route | `@block:numbering-by-route` | 按曲线路线顺序对块递增编号（支持直线/多段线/弧段/圆/样条） |
| Anyblock numbering | `@block:set-any-block-number` | 对任意含指定属性的块编号 |
| Block numbering Config | `@block:setup` | 设置要进行编号的图块和属性 |
| Set as decomposable | `@block:explodable` | 将块设置为可分解 |
| Set as non decomposable | `@block:explode-disable` | 将块设置为不可分解 |
| Change block base point | `@block:menu-change-base` | 修改块基点 |
| Align block base point | `@block:menu-align-base` | 批量对齐块基点 |
| Insert all block | `@block:insert-all` | 将当前 dwg 中所有块按指定点和距离排列插入 |
| Select same block | `@block:select-same` | 选择一个块后选中所有同名块 |
| 统计嵌套块个数 | `@block:count-nested` | 统计指定块名的总参照个数（含嵌套） |
| 块间复制 | `@block:copy-by-blk` | 在块与块之间复制图形 |

### Block2 菜单

| 菜单项 | 函数 | 说明 |
|--------|------|------|
| 块视图切换 | `@block:outline-dialog` | 属性块大纲，快速切换块视图 |
| Positioning overlapping blocks | `@block:overblocks` | 定位相互重叠的同名块 |
| 定位任意重叠块 | `@block:overblocks2` | 定位任意重叠块（尺寸不超过设定值） |
| 一炸到底 | `@block:explode-all` | 将框选区域内所有块全部分解 |
| 写块到库 | `@block:write-file` | 将当前 dwg 中的块导出到库文件夹 |
| 炸剪裁块 | `@block:menu-explode-cliped` | 炸开带剪裁的块 |
| 属性样式刷 | `@block:match-att-style` | 将修改的属性样式刷到其它同名块引用的同名属性上 |
| 多重转普通 | `@block:minsert2insert` | 将多重插入块改为普通块 |
| 保护转块 | `@block:tchpr2insert` | 解除天正保护图块 |
| 分堆建块 | `@block:block-cluster` | 按间隙将选中对象分堆并各自建块 |
| 设置分堆间隙 | `@block:set-clustergap` | 设置分堆建块的间隙值 |
| 按线转块 | `@block:rotate-blk-by-line` | 选择块内一条直线，按该直线水平旋转块 |

### 块参照操作 菜单

| 菜单项 | 函数 | 命令别名 | 说明 |
|--------|------|----------|------|
| 参照归层 | `@block:xref-layer` | — | 将外部参照移至同一图层 |
| 重载选定 | `@block:XFR` | `XFR` | 重载选定外部参照文件 |
| 重载所有 | `@block:CZCZ` | `CZCZ` | 重新加载所有外部参照 |
| 卸载选定 | `@block:XFX` | `XFX` | 卸载选定外部参照文件 |
| 卸载所有 | `@block:XFXA` | `XFXA` | 卸载所有外部参照 |
| 拆离选定 | `@block:XFD` | `XFD` | 拆离选定外部参照文件 |
| 拆离所有 | `@block:XFDA` | `XFDA` | 拆离所有外部参照 |
| 绑定选定 | `@block:XFB` | `XFB` | 绑定选定外部参照文件（支持绑定/插入模式） |
| 绑定所有 | `@block:BDCZ` | `BDCZ` | 绑定所有外部参照（支持绑定/插入模式） |
| 命令提示 | `@block:help-xref-hk` | — | 显示外部参照操作命令列表 |

## 主要功能说明

### 块替换
选择源块后框选目标块，将所有目标块替换为源块（不支持动态块）。

### 块自动编号
1. 先通过 `Block numbering Config` 设置目标块名和属性名
2. 执行编号，程序自动按坐标排序（排序规则和容差可配置）
3. 输入起始编号，自动为所有匹配块填写连续编号
4. 支持前缀、后缀、覆盖模式等多种编号格式

> 块名可配置多个（逗号/分号/空格分隔，如 `车位,车位-无障碍`），多个块名在同一次编号中连续排号。

**蛇形排序（`@block:sort-order` = `snake`）**：适合车位、桩位等成排布置的对象。

- 按包围盒中心分行（`snake-row-tol`），第 1 排从左到右，整排（数量 ≥ `snake-min-row`）后换向，形成 S 形；
- 不足 `snake-min-row` 的零星车位（墙边单列等）排在最后：按列从左到右、列内自上而下；
- 比 `xY` 更符合"沿车道一路编下去"的直觉，找位不用来回折返。

**没有属性的块**：编号时若发现同名的块缺少目标属性（如 `ADFHADGHSA` 没有 `CAR-NUMBER`），
会自动跳过并提示（命令行 `princ`，同时写入 `@block:last-hint` 供 AI/脚本读取），
跳过的块不占用号，编号保持连续无空号。这类块只能改用文字编号。

### 按曲线路线编号
选择一条曲线（LINE/LWPOLYLINE/POLYLINE/ARC/CIRCLE/SPLINE），程序沿曲线方向对相交的块进行递增编号。

- 围栏按曲线等距采样（每 50 图形单位一点），带弧段的多段线、圆弧、圆、样条也按真实路径选块，不会退化成弦漏选。
- 编号顺序按块插入点沿曲线的投影距离排序，距离相同保持原选择顺序。
- `@block:block-name` 可配置多个块名（逗号/分号/空格分隔，如 `车位,车位-无障碍`），多个块名**共用一条连续编号序列**，一次跑完。
- 编号格式与“块自动编号”一致：支持前缀/后缀，`@block:overlay-index` 非 0 时按原属性值的 `-` 分段替换。
- 沿路线未找到块时给出提示，不再弹出模态框、也不会误触发“块编号配置”。

### 嵌套块统计
递归统计指定块名的总参照个数，支持多级嵌套。例如块B内嵌2个A、块C内嵌3个A，可正确计算总数。

### 外部参照管理
支持对选定或所有外部参照进行重载、卸载、拆离、绑定操作，快捷键 `XFR`/`CZCZ`/`XFX`/`XFXA`/`XFD`/`XFDA`/`XFB`/`BDCZ`。

### 写块到库
将当前图纸中的选定块导出为独立 dwg 文件到配置的图库路径。

### 剪裁块操作
- **炸剪裁块**：分解带 XCLIP 的块引用，保留剪裁边界内的图形
- **剪裁图形生成块**：框选范围裁切图形

### 分堆建块
按设定的间隙值将选中对象自动分组，每组创建一个块。

### 多重插入块转普通块
将 MINSERT（多重插入块）转换为普通 INSERT 块引用。

### 天正保护转块
解除天正软件的 TCH_PR 保护块，使其可正常分解（需要以修复形式打开被加密文件）。

### 重叠块定位
按包围盒正面积重叠判定（重叠量需超过 `@block:overlap-tol` 容差），仅边线/角相接的并排块（如车位块边线重合）不算重叠。

## AI 调用

| 函数 | 说明 | 示例 |
|------|------|------|
| `@block:box-overlap?` | 判断两个包围盒是否有超过容差的正面积重叠（并排相接返回 nil） | `(@block:box-overlap? '((0 0 0) (3000 6000 0)) '((3000 0 0) (6000 6000 0)))` => nil |
| `@block:overlap-pairs` | 从 块-包围盒 列表找出所有重叠块对，返回 `((blk1 blk2) ...)` | `(@block:overlap-pairs '((e1 ((0 0 0) (3000 6000 0))) (e2 ((2900 0 0) (5900 6000 0)))) 0.1)` => `((e1 e2))` |
| `@block:ai-number-by-route` | 沿曲线按路径顺序给属性块编号，无交互、返回按顺序的块列表；块名可用 `车位,车位-无障碍` 传多个，统一连续编号 | `(@block:ai-number-by-route (car (entsel)) "车位,车位-无障碍" "NUMBER" 1)` |
| `@block:find-on-curve` | 沿曲线按路径顺序查找指定块名的块（不写属性） | `(@block:find-on-curve (car (entsel)) "车位")` |
| `@block:sort-by-snake` | 按行列蛇形排序块（成排的走 S 形，零星车位按列排最后），返回排好的 ename 列表 | `(@block:sort-by-snake blks 1200 5)` |
| `@block:ai-number-by-snake` | 按蛇形顺序给属性块连续编号（可全图或限定选择集），无交互、返回按编号顺序的块列表；全图时第 4 参数传 `nil`；没有属性的块会被跳过并提示（`princ` + 写入 `@block:last-hint`） | `(@block:ai-number-by-snake "车位" "NUMBER" 1 nil)`<br>`(@block:ai-number-by-snake "车位,车位-无障碍" "NUMBER" 101 ss)` |
| `@block:has-attribute-p` | 判断块是否含指定属性（标签不区分大小写） | `(@block:has-attribute-p blkref "NUMBER")` |
| `@block:missing-attribute-hint` | 对缺少属性的块打印提示并写入 `@block:last-hint`，返回跳过数量 | `(@block:missing-attribute-hint blks "NUMBER")` |

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单文件 |
| `at-block.lsp` | 核心功能：配置、块替换、编号、基点操作、分解、嵌套统计等 |
| `xref.lsp` | 外部参照管理（重载/卸载/拆离/绑定） |
| `insert-all.lsp` | 插入所有块 |
| `numbering-by-route.lsp` | 按曲线路线编号 |
| `copy-to-blk.lsp` | 块间图形复制 |
| `overkill.lsp` | 重叠块检测与定位 |
| `attrib.lsp` | 属性样式刷 |
| `minsert.lsp` | 多重插入块转普通块 |
| `tch-pr.lsp` | 天正保护块转换 |
| `write-file.lsp` | 写块到库 |
| `block-cluster.lsp` | 分堆建块 |
| `explode.lsp` | 剪裁块分解 |
| `clip-to-blk.lsp` | 剪裁图形生成块 |
| `rotate-blk-by-line.lsp` | 按块内直线旋转块 |

| `blk-manage.lsp` | 块管理：重命名/改基点/替换增强/沿路径布块/多块缩放/比例调整/图库统计 |
| `xref-tools.lsp` | 翻转与外部参照：批量翻转块/自由翻转实体/参照改相对路径/Xref 转块/批量条件图 |

## iMini 复现命令（新增）

### 块管理（blk-manage.lsp）

| 交互 | AI 入口 | 说明 |
|---|---|---|
| `@block:blk-rename` | `@block:ai-blk-rename old new` | 块定义重命名（所有引用同步更新） |
| `@block:blk-rename-batch` | `@block:ai-blk-rename-pattern pattern new-pattern` | 通配符批量重命名 |
| `@block:blk-insertion` | `@block:ai-blk-insertion blk-name new-basept ref` | 修改块插入基点 |
| `@block:blk-replace` | `@block:ai-blk-replace src dst map-attrs ss` | 块替换增强（属性按名映射） |
| `@block:blk-along-line` | `@block:ai-blk-along-line pline-ent blk-name dist rotate endp` | 沿路径等距布块 |
| `@block:multi-scale` | `@block:ai-multi-scale blks factor` | 多块以各自插入点缩放 |
| `@block:dtbl` | `@block:ai-dtbl blks op factor` | 批量调整线型/块/填充比例（op=ltype/blk/hatch） |
| `@block:block-see` | `@block:ai-block-see out-file top-n` | 图块统计：`(块名 实体数 使用次数 宽 高)` |

### 翻转与外部参照（xref-tools.lsp）

| 交互 | AI 入口 | 说明 |
|---|---|---|
| `@block:block-flipm` | `@block:ai-block-flipm blks axis strategy` | 批量翻转块；strategy=auto 时含文字块改用旋转 180° |
| `@block:block-flips` | `@block:ai-block-flips ents p1 p2 keep-src` | 实体自由翻转，文字保持可读 |
| `@block:xref-repath` | `@block:ai-xref-repath xrefs base-dir` | 外部参照改相对路径 |
| `@block:xref2blk` | `@block:ai-xref2blk xref blk-name` | Xref 复制为块 |
| `@block:batch-xref` | `@block:ai-batch-xref filter out-dir prefix` | 按条件批量做 Xref 条件图 |

示例：

```lisp
;; 统计图中所有块（前 5 名）
(@block:ai-block-see nil 5)

;; 把门块沿路径布置，间距 600
(@block:ai-blk-along-line (car (entsel)) "门" 600 T T)

;; 批量翻转（上下方向）
(@block:ai-block-flipm (pickset:to-list (ssget '((0 . "INSERT")))) "x" "auto")
```
