# acmecad2021 - AcmeCAD2021

CAD 文件版本转换工具，支持 DWG/DXF/DWF 转 PDF/BMP/GIF/JPEG/WMF/TIFF，以及 DWG/DXF 版本互转（最高支持 AutoCAD 2022）。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 外部程序 > acmecad2021 | `(acmecad2021:open)` | 启动 AcmeCAD2021 |

## 源文件

| 文件 | 说明 |
|------|------|
| `acmecad2021.lsp` | 主程序，自动下载并启动 AcmeCAD2021 |

## 依赖

- `base`

## 配置

无配置项。首次运行会自动下载 `AcmeCAD2021.zip` 到 `bin` 目录。

## 安装

在 CAD 命令行执行：
```
@I acmecad2021
```

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.0.1
- **分类**: APP
- **开源**: 否
