# at-linetype 功能扩展计划

> @lisp 线型管理包：加载/编辑/编译 .lin 与 .shp，并（自 0.1.9 之后）并入原 `mini-linetype` 包，
> 复现 iMini 线型类命令。命名空间统一 `at-linetype:`，依赖 base。

## 1. 概览

- 原有能力：`at-linetype:reload` / `compile-shp` / `load-shx` / `edit` / `edit-shp` / `require`
- 并入能力（原 mini-linetype）：批量改线型、局部变虚线、反转线型方向、输出线型定义、
  炸线型为块、变线为树、成片布树、制作形文件、线型快捷命令
- 源文件：`at-linetype.lsp`（原有）、`ltype-tools.lsp`、`ltype-ext.lsp`、
  `shape-ext.lsp`、`tree-ext.lsp`
- 菜单组：「linetype」（原有）+「线型工具」（并入部分，互不重名）

## 2. iMini 命令映射

| iMini 命令 | at-linetype 入口 | AI 入口 | 源文件 |
|---|---|---|---|
| `ltype`（批量改线型） | `at-linetype:ltype` | `at-linetype:ai-ltype` | ltype-ext.lsp |
| `pline_jbbx`（局部变虚线） | `at-linetype:jb-vd` | `at-linetype:ai-jb-vd` | ltype-ext.lsp |
| `reverse_line`（反转线型方向） | `at-linetype:reverse` | `at-linetype:ai-reverse` | ltype-ext.lsp |
| `lt_out`（输出线型定义） | `at-linetype:lt-out` | `at-linetype:ai-lt-out` | ltype-ext.lsp |
| `lt_shp2blk` / `shp2blk`（炸线型为块） | `at-linetype:shp2blk` | `at-linetype:ai-shp2blk` | tree-ext.lsp |
| `tree_style`（变线为树） | `at-linetype:tree-style` | `at-linetype:ai-tree-style` | tree-ext.lsp |
| `tree_group`（成片布树） | `at-linetype:tree-group` | `at-linetype:ai-tree-group` | tree-ext.lsp |
| `mkshape`（制作形文件） | `at-linetype:mkshape` / `mkshape-core` / `compile-shp-file` | `at-linetype:ai-mkshape` | shape-ext.lsp |
| `ltype_key`（线型快捷命令） | `at-linetype:lt-key` / `lt-key-get` / `lt-key-save` | `at-linetype:ai-lt-key` | shape-ext.lsp |

注：并入时把原 `mini-linetype:compile-shp` 改名为 `at-linetype:compile-shp-file`，
避免与本包原有 `at-linetype:compile-shp`（无参、编译 @lisp.shp）冲突。

## 3. 已知限制

- **mkshape 的 SHX 二进制格式未完整实现（P1）**：`at-linetype:mkshape-core` 只生成
  `.shp` 文本源（`*1,字节数,形名` + 顶点笔画 token）与一段 `.lin` 片段，
  **不自行生成 `.shx` 二进制索引**。`.shx` 仍依赖 CAD 的 `COMPILE` 命令
  （`at-linetype:compile-shp-file` 内部调用 `_.compile`），在部分 CAD 平台
  （ZWCAD/GStarCAD 某些版本）可能不存在该命令，此时只能手工编译。
- mkshape 的形状精度有限（P1）：弧段按 8 段弦近似；全部顶点归一化到 64 单位整数网格；
  只写一条连续笔画（无抬笔/多笔画），不支持形内宽度与嵌套形；形名自动转大写。
- `.lin` 片段中 `s=1`（比例）与旋转 0 为固定值，缩放需用户手工改。
- `at-linetype:compile-shp`（原有）与本包 `load-shx` 仍只面向 `@lisp.shp`，不知道新生成的 .shp 文件。

## 4. 新功能（NEW）

- **F1 形文件多笔画/抬笔（P1）**：`mkshape-core` 支持选择集中多条多段线输出为同一 .shp
  的多个形（`*2,…`），并支持 `0` 之外的抬笔码。
- **F2 mkshape 直接写 .shx（P1）**：实现 SHX 头部（版本/形名表/偏移）与字节流的
  二进制写出，脱离 CAD `COMPILE` 依赖。
- **F3 线型预览（P2）**：在临时图层画出一段示例线，直观预览 .lin 片段效果。
- **F4 iMini 其余命令补齐（P2）**：对照 iMini 线型类菜单补缺口（如按图层批量改线型比例）。

## 5. 改进（IMPROVE）

- **I1 菜单分组（P2）**：现有两组菜单（linetype / 线型工具）可合并为一个父菜单。
- **I2 lt-key 存储（P2）**：快捷键映射文件写 `@:*prefix*` 路径，多用户共享时可能互相覆盖，
  建议按 `getvar "LOGINNAME"` 分目录。
- **I3 错误处理（P2）**：`ltype-tools.lsp` / `shape-ext.lsp` 中多处 `(command ...)`
  缺 `*error*` 保护，`cmdecho` 恢复依赖正常路径，建议统一用 `(command-s ...)` 或
  `@:catch` 包装。

## 6. lib 协同

- 候选提取：`bulge2pts`（凸度拆弧）、`pt-in-poly`、`poly-area`、`pline-verts`/`pline-closed-p`
  与 at-linetype 之外的包重复度高，≥2 包复用时迁到 `lib/src/curve/`。
- 候选提取：`tan`（本方言无全局 tan），可进 `lib/src/math/tan.lsp`。
