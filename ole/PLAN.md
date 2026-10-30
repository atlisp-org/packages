# ole 功能扩展计划

> OLE 图像包：将图片文件以 OLE/光栅图像方式插入 DWG，支持批量插入与图像裁剪。
> v0.2.0，通用类，依赖 `base`，另依赖外部 `atlisp-ole` 工具（shell 批量插入）。

## 1. 概览

- 版本 0.2.0 / 分类 Common / 依赖 base
- 规模：2 个 .lsp（`ole.lsp` 主程序 + `image2ole.lsp` 新增）；1 个菜单组「ole图像」
- 本轮 iMini 复现新增：`image2ole`（图片转 OLE 插入）、`oleimage_crop`（裁剪 OLE/图像）

## 2. 已实现

| # | 命令 | 交互入口 | AI 入口 | 说明 |
|---|---|---|---|---|
| 1 | 图片转 OLE | `ole:image2ole` | `ole:ai-image2ole` | 优先 `vla-CreateOLEObject`，不可用降级 `vla-AddRaster`（princ 说明） |
| 2 | 裁剪 OLE/图像 | `ole:ole-crop` | `ole:ai-ole-crop` | IMAGE 走 `vla-put-ClipBoundary` 矩形裁剪；OLE 不支持裁剪，按矩形调整位置大小 |

新增底层函数：`ole:create-ole`、`ole:add-raster`、`ole:clip-raster`、`ole:fit-ole`。

## 3. 未实现（backlog）

- **P1 真 OLE 嵌入**：`vla-CreateOLEObject` 在 AutoCAD 中不可用，当前 OLE 路径实际走
  光栅降级；真正的 OLE 嵌入依赖外部 `atlisp-ole` 工具（shell 异步，无法给 AI 同步返回）。
  计划：探索 `-INSERTOBJ` 命令行 / ActiveX `AcadOle` 接口，实现同步、可返回 ename 的 OLE 嵌入。
- **P1 裁剪边界形状扩展**：现 `clip-raster` 仅支持矩形四点；计划支持任意多边形 ClipBoundary。
- **P2 图像参数（宽度/高度/旋转）**：`ai-image2ole` 只暴露 `scale`；计划加 width/height/rotation
  形参，对齐 iMini `image2ole` 的参数面板。
- **P2 多图批量 AI 入口**：现有 `ole:multi-insert` 走 shell 异步，无 AI 无交互入口。

## 4. 已知限制

- OLE 对象（AcDbOle2Frame）无法通过 ClipBoundary 裁剪，`ole:ai-ole-crop` 对 OLE 退化为
  按矩形区域重设位置/大小（`ole:fit-ole`），并 `princ` 说明。
- `vla-CreateOLEObject` 在 AutoCAD 2006+ 不提供该方法，`ole:create-ole` 通过 `vl-catch-all-apply`
  兜底，任何异常（含「无函数定义」）都视为不可用并降级。
- 本方言 `and`/`or` 只返回 `T`/`nil`，代码中取值均用 `if`/`cond`；`(last x)` 未使用。
- `e2o` 为内核别名（`vlax-ename->vla-object`），对 nil ename 会报错，入口均先判空。
