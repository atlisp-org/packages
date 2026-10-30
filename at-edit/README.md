# @lisp全能编辑（at-edit）

全能编辑工具：缩放/移动/复制/镜像增强、动态偏移、全能修剪、打断、多段线编辑、布尔运算、轮廓提取、遮罩等，复现 iMini 编辑类命令。

## 安装

CAD 命令行：`@I at-edit` 或 `(@:package-install "at-edit")`。

依赖 `base`（自动安装）。

## 菜单

加载后屏幕菜单「迷你编辑」中包含全部命令。

## 命令

### 变换与修剪

| 命令 | 说明 |
|---|---|
| `at-edit:exscale` | 全能缩放（文字高度同步缩放） |
| `at-edit:exmove` | 移动增强（支持块内实体） |
| `at-edit:excopy` | 复制增强（支持块内实体、多次复制） |
| `at-edit:exmirror` | 镜像增强（保留原件可选） |
| `at-edit:exerase` | 删除增强（支持块内实体） |
| `at-edit:mstretch` | 多窗口拉伸 |
| `at-edit:rotate-center` | 批量中心旋转（绕各自形心） |
| `at-edit:dy-offset` | 单选动态偏移 |
| `at-edit:multi-offset` | 多选多重偏移（多个距离批量） |
| `at-edit:extrim` | 全能修剪（多边界） |
| `at-edit:break-inter` | 交点断开 |
| `at-edit:break-at` | 连续点打断 |
| `at-edit:extend2point` | 延长或修剪到两点连线 |
| `at-edit:fix-line` | 框选修补单线（删短线、连断线） |

### 多段线、转换与块

| 命令 | 说明 |
|---|---|
| `at-edit:pljoin` | 连接多段线（按容差串接线/弧） |
| `at-edit:pwidth` | 多段线宽度批量修改 |
| `at-edit:add-vertex` | 边上增加顶点 |
| `at-edit:delete-vertex` | 删除顶点 |
| `at-edit:lw-arc` | 线弧转换（弧变直 / 直变弧） |
| `at-edit:pline-batch` | 多段线批处理（连接/闭合/反转/清理/圆角） |
| `at-edit:curve2pline` | 曲线转多段线（样条/椭圆/弧/圆） |
| `at-edit:region2pline` | 面域转多段线 |
| `at-edit:circle2donut` | 圆变圆环 |
| `at-edit:donut2circle` | 圆环转圆 |
| `at-edit:pline-bool` | 多段线/填充布尔运算（并/差/交） |
| `at-edit:boundary` | 智能外轮廓 |
| `at-edit:quick-block` | 快速作块 |
| `at-edit:explode-block` | 全能炸块（递归嵌套） |
| `at-edit:wipeout` | 由边界生成遮罩 |
| `at-edit:erase-wipeout` | 删除遮罩 |

## AI 调用

所有 `ai-` 入口无交互、不弹窗，成功返回 ename 或列表，失败返回 `nil`。
`ents` 为 ename 列表或选择集；`pt` / `p1` / `p2` 为三维点表。

### 变换与修剪

| 函数 | 参数 | 说明 |
|---|---|---|
| `at-edit:ai-exscale` | ents basept factor [scale-text] | 缩放，scale-text 缺省 T 同步缩放文字高度 |
| `at-edit:ai-exmove` | ents p1 p2 | 按向量 p1→p2 移动 |
| `at-edit:ai-excopy` | ents p1 p2 [n] | 复制，n 缺省 1 |
| `at-edit:ai-exmirror` | ents mp1 mp2 [copy] | 镜像，copy 缺省 T 保留原件 |
| `at-edit:ai-exerase` | ents | 删除（含块内实体） |
| `at-edit:ai-mstretch` | ents windows disp | windows 为 `((p1 p2) ...)` 窗口对角点 |
| `at-edit:ai-rotate-center` | ents angle | 绕各实体形心旋转，角度制 |
| `at-edit:ai-dy-offset` | ent dirpt dist [side] | 向 dirpt 方向偏移 dist |
| `at-edit:ai-multi-offset` | ents dists [side] | dists 为距离列表 |
| `at-edit:ai-extrim` | targets boundaries [extend] | 修剪目标到边界 |
| `at-edit:ai-break-inter` | ents | 实体两两在交点断开 |
| `at-edit:ai-break-at` | ent pts | 沿实体在点列表处打断 |
| `at-edit:ai-extend2point` | p1 p2 targets | 以两点连线为界延长/修剪 |
| `at-edit:ai-fix-line` | corner1 corner2 [minlen] [tol] | 框内删短线并串接 |

### 多段线、转换与块

| 函数 | 参数 | 说明 |
|---|---|---|
| `at-edit:ai-pljoin` | ents [tol] [del] | 连接首尾相接的线弧为多段线 |
| `at-edit:ai-pwidth` | ents width [head-tail] | head-tail 为 `(头宽 尾宽)` |
| `at-edit:ai-add-vertex` | ent pt | 在最接近 pt 的边上插入顶点 |
| `at-edit:ai-delete-vertex` | ent pt [idx] | 删除顶点（pt 或 0 基索引） |
| `at-edit:ai-lw-arc` | ent mode | mode = `a2l` 弧变直 / `l2a` 直变弧 |
| `at-edit:ai-pline-batch` | ents op [arg] | op = join/close/reverse/clean/fillet |
| `at-edit:ai-curve2pline` | ents [tol] [del] | 曲线转多段线，tol 为弦高 |
| `at-edit:ai-region2pline` | ents [tol] | 面域转多段线 |
| `at-edit:ai-circle2donut` | ents width [del] | 圆变圆环 |
| `at-edit:ai-donut2circle` | ents [del] | 圆环转两个同心圆 |
| `at-edit:ai-pline-bool` | ents mode [del] | mode = union/subtract/intersect |
| `at-edit:ai-boundary` | ents [pt] [layer] | 由实体围合生成外轮廓 |
| `at-edit:ai-quick-block` | ents name basept [del-src] | 快速作块 |
| `at-edit:ai-explode-block` | ents [depth] | 递归炸块，depth 缺省 10 |
| `at-edit:ai-wipeout` | ent [del] | 封闭边界生成遮罩 |
| `at-edit:ai-erase-wipeout` | [ents] | 省略则删除全部遮罩 |

示例：

```lisp
;; 选中直线连成多段线
(at-edit:ai-pljoin (ssnamex->ents (ssget)) 1e-6 T)

;; 两个点画外轮廓
(at-edit:ai-boundary (list (car (entsel))) '(0 0 0) "轮廓")

;; 把选中曲线转成多段线
(at-edit:ai-curve2pline (list (car (entsel))) 1.0 T)
```

## 说明

- 修剪/布尔等操作依赖 `vla-IntersectWith` 与 `BOUNDARY`，仅支持二维平面实体（直线、弧、圆、多段线、面域、填充）。
- 遮罩（WIPEOUT）在不支持该命令的 CAD 上返回 `nil` 并提示。
- 本包复现 iMini「全能编辑」类命令，未实现项见 `PLAN.md`。
