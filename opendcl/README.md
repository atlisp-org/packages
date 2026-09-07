# OpenDCL 运行时库

根据 CAD 运行环境自动下载匹配的 OpenDCL 运行时支持库和语言包，无需下载全部文件。

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `@:load-opendcl` | 无 | 加载 OpenDCL 运行时库 |

## 依赖

- **OpenDCL ARX** - 根据 CAD 版本和平台自动下载
- **Runtime.Res.dll** - 语言资源文件（根据 CAD 语言环境下载）

## 工作原理

1. 检测当前 CAD 版本号（acadver 前两位）
2. 检测系统架构（AMD64 或 x86）
3. 自动下载匹配的 `OpenDCL.{架构}.{版本}.arx` 文件
4. 下载对应语言包（CHS/DEU/ENU/ESM/FRA/RUS/ZH）
5. 加载 ARX 到 CAD

## 支持语言

| 语言代码 | 语言 |
|----------|------|
| CHS | 中文简体 |
| ZH | 中文 |
| ENU | 英文 |
| DEU | 德语 |
| ESM | 西班牙语 |
| FRA | 法语 |
| RUS | 俄语 |

## 安装

```lisp
(@:package-install "opendcl")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `opendcl.lsp` | 主程序源码 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 1.0.18
- **分类**: Lib
- **许可**: GPL
- **网站**: http://atlisp.cn
