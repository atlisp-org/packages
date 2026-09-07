# 侧边栏

CAD 侧边栏屏幕菜单。自动生成 XML 菜单配置，支持 UTF-8 和 ANSI 编码。

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `sidebar:make-menu` | 无 | 生成 @lisp 屏幕菜单 XML 文件 |
| `sidebar:load` | 无 | 加载侧边栏 DLL 组件 |

## 依赖

- **base** - @lisp 基础包
- **CAD_ScreenMenu.dll** - 侧边栏 .NET 组件
- **unzip.exe** - 解压工具
- **iconv.exe** - 编码转换工具（ANSI 环境需要）

## 工作原理

1. 读取 @lisp 菜单注册信息
2. 生成 `Cmd_atlisp.xml` 屏幕菜单配置
3. 在 ANSI 环境下自动转换编码
4. 加载 `CAD_ScreenMenu.dll` 显示侧边栏

## 安装

```lisp
(@:package-install "sidebar")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `sidebar.lsp` | 主程序源码 |
| `CAD_ScreenMenu.dll` | 侧边栏 .NET 组件 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.0.15
- **分类**: Common
- **网站**: http://atlisp.cn
