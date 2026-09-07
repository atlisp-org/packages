# DosLib

DosLib 扩展工具。根据 CAD 运行环境自动下载匹配的 ARX 文件，无需下载全部文件。

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `doslib:load` | 无 | 自动检测并加载 DosLib ARX 库文件 |

## 依赖

- **base** - @lisp 基础包
- **DosLib ARX** - 根据 CAD 版本和平台（x86/x64）自动下载

## 工作原理

1. 检测当前 CAD 版本号（acadver 前两位）
2. 检测系统架构（AMD64 或 x86）
3. 自动下载匹配的 `DOSLib{版本}{架构}.arx` 文件
4. 加载 ARX 到 CAD

## 安装

```lisp
(@:package-install "doslib")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `doslib.lsp` | 主程序源码 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.0.7
- **分类**: Lib
- **网站**: http://atlisp.cn
