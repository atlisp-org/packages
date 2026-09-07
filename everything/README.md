# Everything

基于名称快速定位文件和文件夹。调用 Everything 搜索引擎，实现文件即时搜索。

## 菜单命令

| 菜单组 | 命令名称 | 调用函数 | 说明 |
|--------|----------|----------|------|
| 外部程序 | Everything | `(everything:start)` | 启动 Everything 搜索工具 |

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `everything:start` | 无 | 下载并启动 Everything.exe 搜索工具 |

## 依赖

- **base** - @lisp 基础包
- **外部程序** - Everything.exe（首次运行自动下载）

## 安装

```lisp
(@:package-install "everything")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `everything.lsp` | 主程序源码 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.0.8
- **分类**: APP
- **网站**: http://atlisp.cn
