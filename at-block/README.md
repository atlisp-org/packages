# at-block

> @lisp Block - 图块操作工具包

| 属性 | 值 |
|------|-----|
| 包名 | `at-block` |
| 全名 | @lisp Block |
| 版本 | 1.1.8 |
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
| `@block:block-name` | 块名 | 用于排号的块名称 |
| `@block:attribute-name` | 属性名 | 用于排号的块内属性的名称 |
| `@block:attribute-prefix` | `""` | 用于排号的块内属性值前缀 |
| `@block:attribute-suffix` | `""` | 用于排号的块内属性值后缀 |
| `@block:overlay-index` | `0` | 覆盖用于排号的块内属性原值；0=全覆盖；n=替换原内容以-分隔的第n部分 |
| `@block:xref-layer` | `xref-lock` | 用于放置外部参照的图层名 |
| `@block:sort-order` | `xY` | 排序规则；xyXY任意两两组合，y在前表示y坐标优先，大X表示从右到左排序 |
| `@block:sort-fuzz` | `10,10` | 按位置排序时坐标的容差，逗号分隔不同轴容差 |
| `@block:size` | `1000` | 用于检测块重叠时块的最大尺寸（对角线长度） |
| `@block:lib` | `D:/Design/standard/lib/` | 以块方式管理的图库路径 |

## 菜单

### Block 菜单

| 菜单项 | 函数 | 说明 |
|--------|------|------|
| Block Config | `@block:config` | 打开块配置对话框 |
| Block Replace | `@block:subst` | 将目标块替换成源块 |
| block auto numbering | `@block:set-number` | 块自动编号 |
| Numbering by route | `@block:numbering-by-route` | 按曲线路线顺序对块递增编号 |
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

### 按曲线路线编号
选择一条曲线（LINE/LWPOLYLINE/MLINE），程序沿曲线方向对相交的块进行递增编号。

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
