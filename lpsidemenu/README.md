# lpsidemenu - LP 侧边栏

海角天涯的 .NET 侧边栏，支持自定义和 20 个菜单标签。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| LP侧边栏 > 设置侧栏 | `(lpsidemenu:setup)` | 打开侧栏配置对话框 |
| LP侧边栏 > 加载侧栏 | `(lpsidemenu:load)` | 加载 .NET 侧边栏 |
| LP侧边栏 > 生成菜单 | `(lpsidemenu:make-menu)` | 生成 @lisp.ini 菜单配置 |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `lpsidemenu:width` | 120 | 面板宽度 |
| `lpsidemenu:isaddlp` | 1 | 是否加载LP菜单 (1加载, 0不加载) |
| `lpsidemenu:menugroup-color` | White | 菜单组文字颜色 |
| `lpsidemenu:menuitem-color` | White | 菜单项文字颜色 |
| `lpsidemenu:menuitem-bgcolor` | LightSlateGray | 菜单项背景颜色 |
| `lpsidemenu:menuitem-hovercolor` | Black | 鼠标悬停时文字颜色 |
| `lpsidemenu:menuitem-hoverbgcolor` | LightGreen | 鼠标悬停时背景颜色 |
| `lpsidemenu:title` | @lisp侧边栏 | 选项板标题 |

## 源文件

| 文件 | 说明 |
|------|------|
| `lpsidemenu.lsp` | 主程序 |
| `lpsidemenu2013.dll` | AutoCAD 版侧边栏 DLL |
| `lpZWsidemenu2013.dll` | ZWCAD 版侧边栏 DLL |
| `lpGSsidemenu2013.dll` | GStarCAD 版侧边栏 DLL |
| `Setting.ini` | 侧边栏配置文件 |

## 依赖

- `base`

## 安装

在 CAD 命令行执行：
```
@I lpsidemenu
```

## 信息

- **作者**: 海角天涯 (vitalgg@gmail.com)
- **版本**: 0.0.13
- **分类**: Common
- **开源**: 否
