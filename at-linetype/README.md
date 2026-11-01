# at-linetype - @lisp Linetype

CAD 线型管理工具，加载、编辑、编译自定义线型和形文件。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| linetype → 加载@lisp线型 | `(at-linetype:reload "@lisp")` | 加载 @lisp.lin 线型文件 |
| linetype → 加载设备线型 | `(at-linetype:reload "equip")` | 加载 equip.lin 设备线型 |
| linetype → 加载我的线型 | `(at-linetype:reload "user")` | 加载 user.lin 自定义线型 |
| linetype → 编辑我的线型 | `(at-linetype:edit)` | 用编辑器打开 user.lin 编辑 |
| linetype → 编辑形文件 | `(at-linetype:edit-shp)` | 编辑 @lisp.shp 形文件 |
| linetype → 编译形文件 | `(at-linetype:compile-shp)` | 编译 @lisp.shp → @lisp.shx 并加载 |
| linetype → 我要定制线型 | `(at-linetype:require)` | 打开浏览器联系开发者定制线型 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-linetype:reload` | 重新加载指定的 .lin 线型文件 |
| `at-linetype:compile-shp` | 编译形文件并加载 |
| `at-linetype:load-shx` | 自动检测并加载 @lisp.shx（不存在则先编译） |
| `at-linetype:edit` | 编辑 user.lin 自定义线型文件 |
| `at-linetype:edit-shp` | 编辑 @lisp.shp 形文件 |
| `at-linetype:require` | 打开浏览器联系开发者 |

## 资源文件

| 文件 | 说明 |
|------|------|
| `@lisp.lin` | @lisp 标准线型定义 |
| `equip.lin` | 设备线型定义 |
| `@lisp.shp` | 线型使用的形文件源码 |

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-linetype
```

或：

```
(@:package-install "at-linetype")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-linetype.lsp` | 主程序源码 |
| `@lisp.lin` | 线型定义文件 |
| `equip.lin` | 设备线型定义文件 |
| `@lisp.shp` | 形文件源码 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.1.9
- 分类: Common
- 网站: http://atlisp.cn

---

## 线型工具（原 mini-linetype 包）

原独立包 `mini-linetype` 已并入本包（命名空间统一为 `at-linetype:`），
复现 iMini 线型类命令。菜单组为「线型工具」，与上面的 `linetype` 菜单并存。

### 菜单命令

| 菜单路径 | 函数 | 说明 |
|---------|------|------|
| 线型工具 → 批量改线型 | `(at-linetype:ltype)` | 把所选实体/图层的线型批量改为目标线型 |
| 线型工具 → 局部变虚线 | `(at-linetype:jb-vd)` | 多段线指定基段变为虚线，其余保持原线型 |
| 线型工具 → 反转线型方向 | `(at-linetype:reverse)` | 反转直线/多段线的线型方向 |
| 线型工具 → 输出线型定义 | `(at-linetype:lt-out)` | 把图内线型定义导出为 .lin 文件 |
| 线型工具 → 炸线型为块 | `(at-linetype:shp2blk)` | 线型中的形元素替换为块/树，线型改 CONTINUOUS |
| 线型工具 → 变线为树 | `(at-linetype:tree-style)` | 沿多段线按间距布树并删除原线 |
| 线型工具 → 成片布树 | `(at-linetype:tree-group)` | 闭合边界内随机布树（互不重叠） |
| 线型工具 → 制作形文件 | `(at-linetype:mkshape)` | 多段线 → .shp 形文件 + .lin 线型片段 |
| 线型工具 → 线型快捷命令 | `(at-linetype:lt-key)` | 定义并保存 键=线型 快捷映射 |

### 新增函数

| 函数 | 说明 |
|------|------|
| `at-linetype:ltype` | 批量改线型（交互） |
| `at-linetype:jb-vd` / `jb-vd-core` | 局部变虚线 |
| `at-linetype:reverse` / `reverse-entity` | 反转线型方向 |
| `at-linetype:lt-out` / `lt-out-core` | 输出线型定义到 .lin |
| `at-linetype:shp2blk` / `shp2blk-core` | 炸线型为块 |
| `at-linetype:tree-style` / `tree-style-core` | 变线为树 |
| `at-linetype:tree-group` / `tree-group-core` | 成片布树 |
| `at-linetype:mkshape` / `mkshape-core` | 制作形文件（.shp + .lin 片段） |
| `at-linetype:compile-shp-file` | 编译指定 .shp 为 .shx（区别于本包原有的 `at-linetype:compile-shp`） |
| `at-linetype:lt-key` / `lt-key-get` / `lt-key-save` / `lt-key-load` | 线型快捷映射 |
| `at-linetype:pline-verts` / `pline-closed-p` / `bulge2pts` / `make-lwpl` / `tan` 等 | 公共几何/图元辅助函数 |

### AI 调用

无交互入口，参数全部走形参，失败返回 nil，不弹模态框：

| 函数 | 参数 | 返回 |
|------|------|------|
| `at-linetype:ai-ltype` | `ltype-name ents layers lin-file` | 处理数量 |
| `at-linetype:ai-jb-vd` | `pline-ent edges ltype` | 生成实体列表 |
| `at-linetype:ai-reverse` | `ents` | 反转后的实体列表 |
| `at-linetype:ai-lt-out` | `out-file names` | 输出文件路径 |
| `at-linetype:ai-shp2blk` | `ents blk` | 创建的实体列表 |
| `at-linetype:ai-tree-style` | `pline-ent dist r` | 创建的树实体列表 |
| `at-linetype:ai-tree-group` | `boundary-ent density r` | 创建的树实体列表 |
| `at-linetype:ai-mkshape` | `pline-ent name out-file` | `(shp路径 .lin片段)` |
| `at-linetype:ai-lt-key` | `map`（nil 时只读返回当前映射） | 完整映射列表 |

示例：

```lisp
(at-linetype:ai-ltype "DASHED" (ssget) nil nil)
(at-linetype:ai-tree-style (car (entsel)) 500 100)
(at-linetype:ai-mkshape (car (entsel)) "TREE" "c:/a/tree.shp")
```

### 新增源文件

| 文件 | 说明 |
|------|------|
| `ltype-tools.lsp` | 菜单注册 + 公共函数（原 mini-linetype.lsp） |
| `ltype-ext.lsp` | 批量改线型 / 局部变虚线 / 反转 / 输出线型（原 ltype.lsp） |
| `shape-ext.lsp` | 制作形文件 / 线型快捷命令（原 shape.lsp） |
| `tree-ext.lsp` | 炸线型为块 / 变线为树 / 成片布树（原 tree.lsp） |
