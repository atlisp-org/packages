# at-edit 功能扩展计划

来源：iMini V8.7.1「全能编辑」类命令（`/media/data/Public/raw/iMini/`，菜单 `imini.mnu` + `iMini工具说明.txt`）。
状态：⬜ 未开始 / 🔄 进行中 / ✅ 完成

## 已实现 ✅

### 变换与修剪（transform.lsp，14 命令）

| iMini 命令 | 本包函数 | 状态 |
|---|---|---|
| mini_exscale | `at-edit:exscale` / `at-edit:ai-exscale` | ✅ |
| mini_exmove | `at-edit:exmove` / `at-edit:ai-exmove` | ✅ |
| mini_excopy | `at-edit:excopy` / `at-edit:ai-excopy` | ✅ |
| mini_exmirror | `at-edit:exmirror` / `at-edit:ai-exmirror` | ✅ |
| mini_exerase | `at-edit:exerase` / `at-edit:ai-exerase` | ✅ |
| mini_mstretch | `at-edit:mstretch` / `at-edit:ai-mstretch` | ✅ |
| mini_rotate_center | `at-edit:rotate-center` / `at-edit:ai-rotate-center` | ✅ |
| mini_dy_offset | `at-edit:dy-offset` / `at-edit:ai-dy-offset` | ✅ |
| mini_MOffset / mini_moffset | `at-edit:multi-offset` / `at-edit:ai-multi-offset` | ✅ |
| mini_extrim | `at-edit:extrim` / `at-edit:ai-extrim` | ✅ |
| mini_break_inter | `at-edit:break-inter` / `at-edit:ai-break-inter` | ✅ |
| mini_break_at | `at-edit:break-at` / `at-edit:ai-break-at` | ✅ |
| mini_extendtopoint | `at-edit:extend2point` / `at-edit:ai-extend2point` | ✅ |
| mini_fix_line | `at-edit:fix-line` / `at-edit:ai-fix-line` | ✅ |

### 多段线、转换与块（pline.lsp，16 命令）

| iMini 命令 | 本包函数 | 状态 |
|---|---|---|
| mini_pljoin | `at-edit:pljoin` / `at-edit:ai-pljoin` | ✅ |
| mini_pwidth | `at-edit:pwidth` / `at-edit:ai-pwidth` | ✅ |
| mini_add_vertex | `at-edit:add-vertex` / `at-edit:ai-add-vertex` | ✅ |
| mini_delete_vertex | `at-edit:delete-vertex` / `at-edit:ai-delete-vertex` | ✅ |
| mini_lw_arc | `at-edit:lw-arc` / `at-edit:ai-lw-arc` | ✅ |
| mini_pline_batch | `at-edit:pline-batch` / `at-edit:ai-pline-batch` | ✅ |
| mini_curve_2_pline | `at-edit:curve2pline` / `at-edit:ai-curve2pline` | ✅ |
| mini_region_2_pline | `at-edit:region2pline` / `at-edit:ai-region2pline` | ✅ |
| mini_circle_2_donut | `at-edit:circle2donut` / `at-edit:ai-circle2donut` | ✅ |
| mini_donut_2_circle | `at-edit:donut2circle` / `at-edit:ai-donut2circle` | ✅ |
| mini_pline_booler | `at-edit:pline-bool` / `at-edit:ai-pline-bool` | ✅ |
| mini_boundary | `at-edit:boundary` / `at-edit:ai-boundary` | ✅ |
| mini_quick_block | `at-edit:quick-block` / `at-edit:ai-quick-block` | ✅ |
| mini_exp_mblk | `at-edit:explode-block` / `at-edit:ai-explode-block` | ✅ |
| mini_wipeout | `at-edit:wipeout` / `at-edit:ai-wipeout` | ✅ |
| mini_erase_wipeout | `at-edit:erase-wipeout` / `at-edit:ai-erase-wipeout` | ✅ |

### 局部放大（zoom-detail.lsp，2 命令）

来源：bbs.mjtd.com 论坛「局部放大」需求（thread 106891，clean-room 实现，
不复用其 gxl-* 私有函数）。

| 功能 | 本包函数 | 状态 |
|---|---|---|
| 局部放大（详图） | `at-edit:zoom-detail` / `at-edit:ai-zoom-detail` | 🔄 静态验证通过，CAD 实机验证中 |

实现要点：框选源区域 → 纯 DXF 几何变换（`pt' = ip + (pt-base)*factor`）+
`entmakex` 复制放大（不依赖 VLA 缩放/`command`，MCP 可直调）→ 绘制边界
（圆/矩形，lib `entity:make-circle`/`make-rectangle`）、引线（源窗口最近角 →
边界上对应点）、比例标注（lib `entity:make-text`）。支持
LINE/POINT/SOLID/3DFACE/CIRCLE/ARC/TEXT/MTEXT/LWPOLYLINE/POLYLINE/
INSERT/ELLIPSE/SPLINE，其余类型跳过并计入 `skipped`。

## 未实现（backlog）

| 优先级 | iMini 命令 | 说明 | 建议实现 |
|---|---|---|---|
| P1 | mini_patrial_stretch | 局部动态拉伸 | 用 grread 动态拖拽，AI 入口可用 mstretch 替代 |
| P1 | mini_patrial_offset | 局部动态偏移 | 同上，可用 dy-offset 替代 |
| P1 | mini_patrial_ortho | 局部正交 | 选中多段线局部段强制水平/垂直 |
| P1 | mini_bo_inter | 内轮廓内偏移 | 内轮廓 + 指定偏移距离生成环 |
| P1 | mini_path | 最短路径描边 | 图论最短路（iMini 为 C++），LISP 可用 Dijkstra 近似 |
| P1 | mini_pline_divide | 按面积分封闭多段线 | 二分法求分割线位置 |
| P1 | mini_curve_div / mini_divide_sp | 加线等分 / 等弦长均分曲线 | 与 curve:* 库结合 |
| P1 | mini_trans_reg | 四边形变形 | 四点透视变换矩阵 |
| P1 | mini_CopyAlongCurve | 沿路径无缝复制 | 按弧长等距复制 |
| P2 | mini_freehand | 手绘抖动效果 | 线段加随机扰动 |
| P2 | mini_mpolyout | 带宽度多段线变线框 | 宽度轮廓展开 |
| P2 | mini_chk_open | 检查闭合画圆 | 开口位置检测 |
| P2 | mini_BlockWipeouts | 给块添加遮罩 | 块内边界生成遮罩 |
| P2 | mini_blk2lay0 | 块内实体到 0 层 | 块定义内实体改层 |
| P2 | mini_exptoblay | 炸开到块层 | 炸开后继承块所在层 |
| P2 | mini_group_creat / explode | 快速建组 / 炸组 | `(command "_.GROUP")` 封装 |
| P2 | mini_wipeout_Frames | 遮罩外框开关 | WIPEOUTFRAME 系统变量 |
| P2 | mini_3p_rectang | 全能矩形（三点/倒角/圆角） | 参数化矩形 |
| P2 | mini_mfillet | 两组平行线倒角 | 平行线间生成半圆/倒角 |
| P2 | mini_sxcj | 批量双线裁剪 | 双线交叉处按墙厚裁剪 |
| P2 | mini_line_union / mini_multimerge | 合并重线 / 合并图层字样线型 | 与 at-purge 的 `ai-union-lines` 功能重叠，择一实现 |
| P2 | mini_line_clean | 清理短线点空字 | 已由 at-purge 的 `ai-clean-lines` 覆盖 |

## 已知限制

- 三维实体（3DSOLID/BODY）不支持修剪、布尔、轮廓提取。
- 面域布尔依赖 `vla-Boolean`，BricsCAD 部分版本返回值不同，已做兼容判断。
- WIPEOUT 生成依赖 `_.WIPEOUT` 命令，个别 CAD 版本需先 `_P`（多段线模式）。
- 未在 CAD 中实机验证（本轮为静态 lint 通过）；首次使用建议先在试验图纸上跑一遍。
