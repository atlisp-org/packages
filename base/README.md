# @lisp support env

Base package, support @lisp package to run.

## 功能特性

| 函数 | 说明 |
|------|------|
| `push-var` | 当前变量状态入栈, 参数支持单个字符串，符号，nil(默认变量表)  |
| `plot:to-pdf` | 打印到PDF文件。参数： 图幅 纵横 第一点 第二点 输出文件名。 |
| `plot:to-dev` | 打印到设备。参数： 图幅 纵横 第一点 第二点 输出文件名。 |

## 配置

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `base:plotter` | `VitalPDF.pc3` | Please change it to your ploter,and setup page sizes. |
| `base:plotter-pdf` | `VitalPDFzw.pc5` | PDF Printer |
| `base:plotter-pdf` | `VitalPDFg.pc3` | PDF Printer |
| `base:plotter-pdf` | `VitalPDF.pc3` | PDF Printer |
| `base:projects-output` | `D:\\Output` | 本地输出目录 |

## 依赖

无外部依赖

## 安装

在CAD命令行中执行：
```lisp
@I base
```

或：
```lisp
(@:package-install "base")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `base.lsp` | |
| `format.lsp` | |
| `condition.lsp` | |
| `foracad.lsp` | |
| `plot.lsp` | |

## 作者

- VitalGG
  - vitalgg@gmail.com

## 版本

- 1.5.57

## 许可证

请查看源码获取许可信息
