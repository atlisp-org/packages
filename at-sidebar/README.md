# @侧边栏

@lisp侧边栏,C#版本

## 功能特性

| 菜单 | 命令 | 说明 |
|------|------|------|
| 加载侧边栏 | `at-sidebar:load` |  |
| 编译侧边栏 | `at-sidebar:compile` |  |

## 函数列表

| 函数 | 说明 |
|------|------|
| `at-sidebar:make-pattern-img` | 开发版本 |
| `at-sidebar:make-library-img` | 开发版本 |
| `at-sidebar:solve-self-reference` | 解决自参照问题 |

## 配置

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `at-sidebar:first` | `我是配置项 at-sidebar:first 的值` | 这个配置项的用途说明。 |

## 依赖

- `base` 包

## 安装

在CAD命令行中执行：
```lisp
@I at-sidebar
```

或：
```lisp
(@:package-install "at-sidebar")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `at-sidebar.lsp` | |
| `at-sidebar-ZWCAD.csproj` | |
| `at-sidebar-AutoCAD.csproj` | |
| `at-sidebar-GstarCAD.csproj` | |
| `at-sidebar.slnx` | |
| `global-using.cs` | |
| `global-using-AutoCAD.cs` | |
| `global-using-ZWCAD.cs` | |
| `global-using-GstarCAD.cs` | |
| `palette.cs` | |
| `atlispwv.cs` | |
| `compile.bat` | |
| `install-dotnet.bat` | |

## 作者

- VitalGG
  - vitalgg@gmail.com

## 版本

- 0.1.24

## 许可证

请查看源码获取许可信息
