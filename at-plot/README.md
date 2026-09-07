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
| `at-plot.lsp` | |
| `frame-recognition.lsp` | |
| `mark-frame.lsp` | |
| `export.lsp` | |
| `train.lsp` | |

## 作者

- VitalGG
  - vitalgg@gmail.com

## 版本

- 0.0.8

## 许可证

请查看源码获取许可信息
