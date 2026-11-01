# at-purge - 图面清理

图面清理工具，清理 DGN 线型问题、分解多重插入块。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 实体 → 清理DGN | `(at-purge:remove-dgn)` | 修复 DGN 线型问题并运行 purge 清理 |
| 实体 → 分解重块 | `(@:explode-minsert)` | 分解多重插入块（MINSERT） |
| 实体 → 一键清理 | `(c:purge-quick)` | purge 未引用的图层/块/文字样式/线型 + 清理空块 + 合并重复块 |
| 实体 → 清理短线点空字 | `(c:clean-lines)` | 删除短线、孤立点、空文字 |
| 实体 → 删除常规词典 | `(c:del-dicts)` | 删除图中常规词典（ACAD_GROUP 默认不删） |
| 实体 → 合并重线 | `(c:union-lines)` | 重叠共线线段合并为一条 + 多段线删除冗余共线顶点 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-purge:remove-dgn` | 删除 ACAD_DGNLINESTYLECOMP 字典项，然后执行 `purge a * N` 清理 |
| `@:explode-minsert` | 选择多重插入块，创建新的 INSERT 实体后执行 explode 分解 |
| `c:overkill` / `c:-overkill` | OVERKILL 清理（DCL 对话框：模糊、忽略图层/块、多段线/部分/端点模式） |
| `at-purge:ai-quick` | AI 入口：一键清理（见「AI 调用」） |
| `at-purge:clean-empty-blks` | 删除引用为 0 的空块定义，返回删除数 |
| `at-purge:find-dup-blks` | 按块指纹（dxf 排序归一化）找出几何相同的重复块组 |
| `at-purge:merge-dup-blks` | 合并重复块（引用替换 + 删多余定义），返回合并数 |
| `at-purge:ai-clean-lines` | AI 入口：删除短线/孤立点/空文字（见「AI 调用」） |
| `at-purge:ai-del-dicts` | AI 入口：删除指定常规词典（见「AI 调用」） |
| `at-purge:ai-union-lines` | AI 入口：合并重线（见「AI 调用」） |
| `at-purge:rm-collinear-verts` | 删除多段线冗余共线顶点（连续三点共线删中间点） |

## 使用说明

- **清理 DGN**：修复因导入 DGN 文件导致的线型问题，自动执行全量 purge
- **分解重块**：选择多重插入块（MINSERT），将其转换为普通 INSERT 后分解
- **一键清理**：全部=全量 purge + 清空块 + 合并重复块；按类（图层/块/样式/线型）用纯表操作清理，自动保护 0/当前层、`*_`/`ACAD_*`、Standard、BYLAYER/BYBLOCK/Continuous
- **清理短线点空字**：设定长度阈值，短于它的线段/多段线边、孤立点、空文字被删除
- **删除常规词典**：默认删 ACAD_MTRANS/ACAD_LAYER_STATES/ACAD_MLEADERSTYLE；删 ACAD_GROUP 会丢失组，需显式指定
- **合并重线**：同层同色同线型、共线且重叠/相接的线段合并为一条；多段线顺带删除冗余共线顶点

## AI 调用

> 无交互入口，全部参数走形参；成功返回计数/列表，失败返回 nil（提示用 princ 输出）；改动图形均用 startundo/endundo 包裹。

| 函数 | 参数 | 返回值 | 示例 |
|------|------|--------|------|
| `at-purge:ai-quick` | what："layer"/"blk"/"style"/"ltype"，nil=全部类型 | `((:layers n) (:blks n) (:styles n) (:ltypes n) (:empties n) (:dups n))`，失败 nil | `(at-purge:ai-quick nil)` |
| `at-purge:ai-clean-lines` | min-len：短线阈值（<它的线/多段线边被删）；ss：选择集或图元列表，nil=全图 | `((:lines n) (:polylines n) (:points n) (:texts n))`，失败 nil | `(at-purge:ai-clean-lines 10)` |
| `at-purge:ai-del-dicts` | names：词典名列表（字符串）或逗号分隔字符串；nil=默认清单（ACAD_GROUP 需显式指定） | 已删除的词典名列表，失败 nil | `(at-purge:ai-del-dicts nil)` |
| `at-purge:ai-union-lines` | ss：选择集或图元列表，nil=全图；tol：容差（nil 用 0.001） | `((:merged n) (:verts n) (:deleted n))`，失败 nil | `(at-purge:ai-union-lines nil 0.001)` |

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-purge
```

或：

```
(@:package-install "at-purge")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-purge.lsp` | 主程序源码（清理 DGN、分解重块） |
| `overkill.lsp` | OVERKILL 清理（DCL 对话框交互） |
| `purge-extra.lsp` | 一键清理/清理短线点空字/删除常规词典/合并重线（含 AI 入口） |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.0.7
- 分类: 文件
- 网站: http://atlisp.cn
