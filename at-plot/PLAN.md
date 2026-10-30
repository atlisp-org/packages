# at-plot 功能扩展计划

## iMini 复现（2026-10）

| iMini 命令 | 本包函数 | 状态 |
|---|---|---|
| mini_create_image / mini_create_image_x | `@plot:screenshot` / `@plot:ai-screenshot` | ✅ |
| mini_tzml（批量打印目录拆图） | `@plot:split-layout` / `@plot:ai-split-by-layout` | ✅（拆图；打印部分尽力而为） |

## backlog

| 优先级 | 项目 | 说明 |
|---|---|---|
| P1 | 截图带图框裁切 | 按图框边界裁切截图区域 |
| P2 | 拆图后批量打印队列 | 拆完统一打印，支持每布局不同页面设置 |
| P2 | 截图分辨率/DPI 参数 | 当前用视口像素尺寸 |

## 已知限制

- `vla-GetImage` 在部分 CAD 版本不可用，此时截图返回 nil。
- 布局拆图依赖 `vla-SaveAs` 新文档，图纸较大时耗时。
- 未做 CAD 实机验证（仅静态 lint 通过）。
