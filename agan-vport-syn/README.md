# agan-vport-syn - 阿甘对图器

同步视口对图工具，用于在布局空间中同步多个视口的显示内容。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 布局工具 > 阿甘对图 | `(agan-vport-syn:load)` | 加载并启动对图器 |

## 源文件

| 文件 | 说明 |
|------|------|
| `agan-vport-syn.lsp` | 主程序，加载 DLL 并启动对图功能 |
| `aganvportsyn.dll` | 对图器核心 .NET DLL |
| `aganvportsyn-10.dll` | 对图器兼容版本 DLL |

## 配置

无配置项。

## 依赖

- `base`
- CAD 版本 >= 2008 (AutoCAD 2008+)

## 安装

在 CAD 命令行执行：
```
@I agan-vport-syn
```

## 说明

- 程序会自动检查 CAD 版本，低版本 CAD 不支持
- 首次运行会从网络下载 DLL 文件
- 支持 AutoCAD / ZWCAD / GStarCAD

## 信息

- **作者**: 阿甘 (vitalgg@gmail.com)
- **版本**: 0.0.6
- **分类**: 布局
- **开源**: 否
