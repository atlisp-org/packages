# @打印输出-测试版

@lisp 打印输出辅助工具，含批量打印、多图打印、智能识别图框。

## 功能特性

| 菜单 | 命令 | 说明 |
|------|------|------|
| 配置 | `@plot:setup` |  |
| AI识别图框 | `@plot:frame-recognition` |  |
| 识别PL图框 | `@plot:frame-recognition-by-polyline` |  |
| 识别直线框 | `@plot:frame-recognition-by-line` |  |
| 识别块图框 | `@plot:frame-recognition-by-block` |  |
| 标记图框 | `@plot:mark-frames` |  |
| 删图框标记 | `@plot:delete-mark` |  |
| 批打输出 | `@plot:plot-all` | 按图框实际尺寸自动识别图幅/横竖 |
| 批打并合并 | `(@plot:ai-plot-all nil T)` | 无交互批打，可选合并为一个 PDF |
| 训练学习 | `@plot:train` |  |

## 配置

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `@plot:scale-of-frame` | `100 50 30 25 20 150 120 200 500 1 5.996` | 图幅比例，逗号或空格分隔 |
| `@plot:layers` | `*` | 图框所在图层，多个用 `,` 分隔，支持 `*` |
| `@plot:export-path` | `D:\output\` | 打印输出路径 |
| `@plot:default-frame` | `A2` | 图幅识别失败时的默认图幅（A0~A4） |
| `@plot:default-orient` | `L` | 图幅识别失败时的默认方向（L=横，P=竖） |

## 图幅识别

`(@plot:get-frame-type w h)` 用图框实际宽高（图纸单位）匹配 A0~A4 × `@plot:scale-of-frame`
中的比例，相对容差 0.5%，多个候选取尺寸最接近的一个。返回点对 `("A3" . "L")`
（L=横，P=竖），识别失败返回 `nil`。

```lisp
(@plot:get-frame-type 42000.0 29700.0) => ("A3" . "L")   ; A3 1:100 横
(@plot:get-frame-type 29700.0 42000.0) => ("A3" . "P")   ; A3 1:100 竖
(@plot:get-frame-type 59400.0 42000.0) => ("A2" . "L")   ; A2 1:100 横
```

## 依赖

- `base` 包

## 安装

在CAD命令行中执行：
```lisp
@I at-plot
```

或：
```lisp
(@:package-install "at-plot")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `at-plot.lsp` | 包入口 |
| `frame-recognition.lsp` | AI 识别图框（PL 图框/直线框/块图框） |
| `mark-frame.lsp` | 图框标记与删除 |
| `export.lsp` | 批量打印输出 |
| `train.lsp` | 识别训练学习 |
| `screenshot.lsp` | 视图截图（`@plot:ai-screenshot`，GetImage 方法） |
| `split-layout.lsp` | 按布局拆分为独立 dwg，可选打印 PDF（`@plot:ai-split-by-layout`） |

## 作者

- VitalGG
  - vitalgg@gmail.com

## 版本

- 0.0.13：图幅识别歧义修复（A4@1:100 与 A2@1:200 等尺寸相同时按 1:100 优先）
- 0.0.12

## 许可证

请查看源码获取许可信息

## 快速截图 / 布局拆图（iMini 复现）

- `@plot:screenshot`：把当前视图保存为 PNG/JPG 图片。
- `@plot:split-layout`：为每个布局生成独立 dwg，可选同时打印 PDF。

## AI 调用

| 命令 | AI 入口 | 说明 |
|---|---|---|
| `@plot:plot-all` | `@plot:ai-plot-all out-dir merge-p` | 无交互批打 `temp-frames` 图层上的图框，返回 PDF 路径表；merge-p=T 时按图框左上角排序后用 pdftk 合并，返回合并文件路径 |
| `@plot:screenshot` | `@plot:ai-screenshot out-file [region]` | 当前视图或指定窗口截图存图；region 为 `(左下点 右上点)` 模型坐标 |
| `@plot:split-layout` | `@plot:ai-split-by-layout out-dir [to-pdf]` | 按布局把当前图拆成多个 dwg；to-pdf=T 时同时整幅打印 PDF |

示例：

```lisp
;; 批打并合并为一个 PDF（图幅/横竖按每个图框自动识别）
(@plot:ai-plot-all "D:/out" T)

;; 只批打，返回 PDF 路径表
(@plot:ai-plot-all "D:/out" nil)

;; 截当前视图
(@plot:ai-screenshot "D:/shot.png" nil)

;; 按布局拆图并打印
(@plot:ai-split-by-layout "D:/layouts" T)
```

说明：
- 批打文件名固定为「序号-图名.pdf」，图名由 `@pm:pickout-maptitle-first` 识别，无图名时只用序号。
- 合并依赖 pdftk（`@lisp bin` 目录或系统 PATH）；找不到 pdftk 时保留单独 PDF 并返回路径表。
- 截图优先 `vla-GetImage`，不可用时返回 nil 并 princ 原因（不同 CAD 版本能力不同）。
- 视图窗口取自 `(getvar "view")`；部分 CAD（如 ZWCAD LMS Tech 24.x）该变量为 nil
  且无 VLA ViewCenter，此时退回整幅范围 `extmin/extmax`。
- 布局拆图会新建临时文档逐个布局另存，源文档不变；文件名自动避开非法字符。
