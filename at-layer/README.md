# at-layer — @lisp 图层工具

@lisp 图层管理工具，提供图层关闭/冻结/锁定其它、解锁/解冻全部、选图进层，
以及图层状态管理、隐藏实体、独立显示、锁定实体、双$层清理、层名大小写、
按颜色分层、按图层调整绘图顺序、图层提取器等扩展功能。

## 基本信息

| 字段 | 值 |
|---|---|
| 包名 | `at-layer` |
| 全名 | @lisp图层 |
| 版本 | 1.2.14 |
| 作者 | VitalGG |
| 邮箱 | vitalgg@gmail.com |
| 分类 | 通用 |
| 许可 | 免费 / 非开源 |
| 网站 | http://atlisp.cn |

## 依赖

- **base**（必需）

## 源文件

| 文件 | 说明 |
|---|---|
| `at-layer.lsp` | 主源码，菜单注册与原有函数实现 |
| `layers-ext.lsp` | 图层扩展函数（iMini 图层增强命令复现） |

## 安装

在 CAD 命令行执行：

```
@I at-layer
```

或：

```lisp
(@:package-install "at-layer")
```

## 菜单

注册在「图层」菜单下，共 27 个菜单项：

| 菜单文字 | 命令/函数 | 说明 |
|---|---|---|
| 关闭其它 | `(@layer:off-other)` | 关闭未被选中的图层 |
| 冻结其它 | `(@layer:frozen-other)` | 冻结未被选中的图层 |
| 锁定其它 | `(@layer:lock-other)` | 锁定未被选中的图层 |
| 解锁全部 | `(@layer:unlock-all)` | 解锁所有图层 |
| 解冻全部 | `(@layer:thaw-all)` | 解冻所有图层 |
| 图层全开 | `layon` | 打开所有图层（AutoCAD 内置命令） |
| 图层恢复 | `layerp` | 恢复上一次图层状态（AutoCAD 内置命令） |
| 特性随层 | `laycur` | 将对象特性设为 ByLayer（AutoCAD 内置命令） |
| 合并图层 | `laymrg` | 合并图层（AutoCAD 内置命令） |
| 删除图层 | `laydel` | 删除图层（AutoCAD 内置命令） |
| 图层漫游 | `laywalk` | 图层漫游（AutoCAD 内置命令） |
| 选图进层 | `(@layer:ent-to-clayer)` | 将选中对象移至当前图层 |
| 保存图层状态 | `(@layer:save-state)` | 保存当前所有图层状态 |
| 恢复图层状态 | `(@layer:restore-state)` | 按名称恢复图层状态 |
| 状态列表 | `(@layer:state-list)` | 列出已保存的图层状态 |
| 隐藏实体 | `(@layer:hide-ents)` | 关闭选中实体所在图层 |
| 显示已隐藏 | `(@layer:show-hidden)` | 恢复被隐藏的图层 |
| 独立显示 | `(@layer:isolate)` | 仅显示选中实体所在图层 |
| 取消独立显示 | `(@layer:un-isolate)` | 恢复独立显示 |
| 锁定实体 | `(@layer:lock-ents)` | 给选中实体打 LAYERLOCK 标记 |
| 解锁实体 | `(@layer:unlock-ents)` | 移除选中实体的 LAYERLOCK 标记 |
| 解锁全部实体 | `(@layer:unlock-ents-all)` | 移除全部 LAYERLOCK 标记 |
| 清理双$图层 | `(@layer:kill-dollar)` | 删除名称含 $$ 的图层及其实体 |
| 层名大小写 | `(@layer:strcase)` | 层名统一大写/小写 |
| 按颜色分层 | `(@layer:by-color)` | 选中实体按颜色拆层 |
| 图层调整顺序 | `(@layer:ldorder)` | 按图层调整绘图顺序 |
| 图层提取器 | `(@layer:extract)` | 导出图层信息为 CSV |

## 函数（原有）

### `@layer:off-other`

关闭其它图层。选择对象后，保留对象所在图层，关闭其余所有图层。若当前图层不在选择集中，则自动切换为选择集中第一个对象所在的图层。

### `@layer:frozen-other`

冻结其它图层。选择对象后，保留对象所在图层，冻结其余所有图层。若当前图层不在选择集中，则自动切换为选择集中第一个对象所在的图层。

### `@layer:lock-other`

锁定其它图层。选择对象后，保留对象所在图层，锁定其余所有图层。若当前图层不在选择集中，则自动切换为选择集中第一个对象所在的图层。

### `@layer:unlock-all`

解锁全部图层。遍历所有图层并执行解锁。

### `@layer:thaw-all`

解冻全部图层。遍历所有图层并执行解冻。

### `@layer:ent-to-clayer`

将选中对象移至当前图层，并清除对象的线型（group code 6）、线型比例（48）和颜色（62）覆盖，使其随层。支持两种选择方式：先选择对象再执行命令，或执行命令后逐个选择对象。

### `c:ent2clayer`

`@layer:ent-to-clayer` 的命令行别名，可在 CAD 命令行直接输入 `ent2clayer` 调用。

## 函数（扩展，layers-ext.lsp）

### 1. 图层状态管理

状态保存到全局变量 `*layer-states*`，结构为 `((状态名 ((层名 . (开 冻结 锁定)) ...)) ...)`。

- `@layer:save-state (name)` — 保存当前所有图层的开/冻结/锁定状态，按名称区分多份。
- `@layer:restore-state (name)` — 按名称恢复图层状态（先解锁→设冻结→设开关→再锁定，避免锁定层无法解冻）。
- `@layer:state-list ()` — 列出已保存的状态名称。
- `@layer:current-states ()` — 读取当前所有图层的状态表（辅助函数）。

### 2. 隐藏实体

被关图层记录到 `*layer-hidden*`，重复调用不重复记录；当前层被关时自动切到 0 层。

- `@layer:hide-ents (ss)` — 关闭选中实体所在图层并记录。
- `@layer:show-hidden ()` — 恢复 `*layer-hidden*` 中的图层并清空列表。

### 3. 独立显示

被关图层记录到 `*layer-iso*`。

- `@layer:isolate (ss)` — 关闭除选中实体所在图层外的所有图层并记录。
- `@layer:un-isolate ()` — 恢复被独立显示关闭的图层并清空列表。

### 4. 锁定实体（xdata 标记）

核心机制：给实体打 `LAYERLOCK` 应用名的 xdata（1000 码），供其他包/脚本识别锁定实体；
不提供模态拦截，实体删除/修改后标记随实体消失。

- `@layer:lock-ents (ss)` — 给选中实体打 LAYERLOCK 标记。
- `@layer:unlock-ents (ss)` — 移除选中实体的 LAYERLOCK 标记。
- `@layer:unlock-ents-all ()` — 移除图中全部 LAYERLOCK 标记。
- `@layer:is-locked? (ent)` — 判断实体是否带 LAYERLOCK 标记。
- `@layer:locked-list ()` — 获取图中全部带标记的实体列表。
- `@layer:ensure-app (name)` — 注册 xdata 应用名（辅助函数）。

### 5. 清理双$图层

- `@layer:kill-dollar (keep)` — 删除名称含 `$$` 的图层及其上的实体；
  `keep=1` 时实体保留并移入 0 层。

### 6. 层名大小写

- `@layer:strcase (mode layers)` — 层名统一大写/小写：新建目标层（复制原层颜色/线型/标志位）→
  移动实体 → 删除旧层；若目标层已存在（忽略大小写）则直接并入。
  `mode` 为 `"uc"`/`"lc"`，`layers` 为 nil 时处理全部图层。

### 7. 按颜色分层

- `@layer:by-color (ss prefix)` — 选中实体按颜色（62/63 码）拆到 `前缀+颜色号` 图层
  （默认 `颜色N`），新层颜色设为对应颜色，实体改随层；BYLAYER(256)/BYBLOCK(0/255) 跳过。

### 8. 按图层调整绘图顺序

- `@layer:ldorder (layers order)` — 按图层把实体移到绘图顺序最前/最后
  （逐实体 `vla-MoveToFront`/`vla-MoveToBack`）；`layers` 为 nil 时处理全部图层，
  `order` 为 `"front"`/`"back"`。

### 9. 图层提取器

- `@layer:extract (out-file)` — 导出全部图层信息（层名/颜色/线型/线宽/开/冻结/锁定/实体数）
  为 CSV 文件；`out-file` 为 nil 时默认 `<图名>_layers.csv`。

## AI 调用

无交互入口，参数全走形参；成功返回实体/列表/数量，失败返回 nil，提示仅 `princ`。
实体参数（`ents`）接受 ename、ename 列表或选择集。

| 函数 | 参数 | 返回值 | 示例 |
|---|---|---|---|
| `@layer:ai-save-state` | name（nil=default） | t/nil | `(@layer:ai-save-state "work")` |
| `@layer:ai-restore-state` | name（nil=default） | t/nil | `(@layer:ai-restore-state "work")` |
| `@layer:ai-state-list` | 无 | 状态名列表 | `(@layer:ai-state-list)` |
| `@layer:ai-hide` | ents | 关闭的图层列表 | `(@layer:ai-hide (pickset:to-list (ssget)))` |
| `@layer:ai-show-hidden` | 无 | 恢复的图层列表 | `(@layer:ai-show-hidden)` |
| `@layer:ai-isolate` | ents | 关闭的图层列表 | `(@layer:ai-isolate (pickset:to-list (ssget)))` |
| `@layer:ai-un-isolate` | 无 | 恢复的图层列表 | `(@layer:ai-un-isolate)` |
| `@layer:ai-lock` | ents | 被标记实体列表 | `(@layer:ai-lock (list (car (entsel))))` |
| `@layer:ai-unlock` | ents | 被移除标记实体列表 | `(@layer:ai-unlock (list (car (entsel))))` |
| `@layer:ai-unlock-all` | 无 | 被移除标记数量 | `(@layer:ai-unlock-all)` |
| `@layer:is-locked?` | ent | t/nil | `(@layer:is-locked? (car (entsel)))` |
| `@layer:locked-list` | 无 | 带标记实体列表 | `(@layer:locked-list)` |
| `@layer:ai-kill-dollar` | keep（1=保留实体） | 删除的图层数 | `(@layer:ai-kill-dollar 1)` |
| `@layer:ai-strcase` | mode("uc"/"lc"), layers(nil=全部) | (旧层 . 新层) 列表 | `(@layer:ai-strcase "uc" nil)` |
| `@layer:ai-by-color` | ents, prefix（nil=颜色） | 生成的图层列表 | `(@layer:ai-by-color (list (car (entsel))) "颜色")` |
| `@layer:ai-ldorder` | layers(nil=全部), order("front"/"back") | 处理的图层列表 | `(@layer:ai-ldorder nil "front")` |
| `@layer:ai-extract` | out-file（nil=默认） | 输出文件路径 | `(@layer:ai-extract "C:/layers.csv")` |

## 配置

无需额外配置。加载后自动注册「图层」菜单。
