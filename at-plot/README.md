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
| 批打输出 | `@plot:plot-all` |  |
| 训练学习 | `@plot:train` |  |

## 配置

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `at-plot:first` | `I'm th default value for at...` | This Item 's Explain. |

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

- 0.0.11

## 许可证

请查看源码获取许可信息

## 快速截图 / 布局拆图（iMini 复现）

- `@plot:screenshot`：把当前视图保存为 PNG/JPG 图片。
- `@plot:split-layout`：为每个布局生成独立 dwg，可选同时打印 PDF。

## AI 调用

| 命令 | AI 入口 | 说明 |
|---|---|---|
| `@plot:screenshot` | `@plot:ai-screenshot out-file [region]` | 当前视图或指定窗口截图存图；region 为 `(左下点 右上点)` 模型坐标 |
| `@plot:split-layout` | `@plot:ai-split-by-layout out-dir [print]` | 按布局把当前图拆成多个 dwg；print=T 时同时整幅打印 PDF |

示例：

```lisp
;; 截当前视图
(@plot:ai-screenshot "D:/shot.png" nil)

;; 按布局拆图并打印
(@plot:ai-split-by-layout "D:/layouts" T)
```

说明：
- 截图优先 `vla-GetImage`，不可用时返回 nil 并 princ 原因（不同 CAD 版本能力不同）。
- 视图窗口取自 `(getvar "view")`；部分 CAD（如 ZWCAD LMS Tech 24.x）该变量为 nil
  且无 VLA ViewCenter，此时退回整幅范围 `extmin/extmax`。
- 布局拆图会新建临时文档逐个布局另存，源文档不变；文件名自动避开非法字符。
