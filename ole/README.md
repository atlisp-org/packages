# OLE 图像

将图像文件以 OLE 方式插入到 DWG 中。支持批量插入和光栅图像插入。

## 菜单命令

| 菜单组 | 命令名称 | 调用函数 | 说明 |
|--------|----------|----------|------|
| ole图像 | ole设置 | `(ole:setup)` | 配置图像插入参数 |
| ole图像 | 批量插入 | `(ole:multi-insert)` | 批量插入 OLE 图像 |
| ole图像 | 插入图像 | `(ole:insert-img)` | 插入单张图像 |
| ole图像 | 配置环境 | `(ole:install)` | 运行安装脚本 |
| ole图像 | 批量光栅 | `(ole:multi-rasteriamge)` | 批量插入光栅图像 |

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `ole:setup` | 无 | 打开配置对话框 |
| `ole:install` | 无 | 运行 PowerShell 安装脚本 |
| `ole:multi-insert` | 无 | 批量插入 OLE 图像 |
| `ole:insert-img` | 无 | 插入单张 OLE 图像 |
| `ole:calc-ptins` | 无 | 计算下一个插入点位置 |
| `ole:scale-img` | 无 | 调整图像比例 |
| `ole:make-title` | `str` | 创建图像标题文字 |
| `ole:osmode-off` | 无 | 关闭对象捕捉 |
| `ole:osmode-on` | 无 | 恢复对象捕捉 |
| `ole:multi-rasteriamge` | 无 | 批量插入光栅图像 |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `ole:gap` | 300 | 插入图像的水平间隙 |
| `ole:scale` | 1.0 | 插入图像与原图的比例 |
| `ole:width` | 3000.0 | 图像宽度 |
| `ole:img-types` | jpg,png | 图像文件类型（逗号分隔） |
| `ole:title-size` | 50 | 图像名称字高 |
| `ole:title-style` | 黑体 | 图像名称字体样式 |

## 依赖

- **base** - @lisp 基础包
- **atlisp-ole** - OLE 处理工具（自动下载）

## 安装

```lisp
(@:package-install "ole")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `ole.lsp` | 主程序源码 |
| `install.ps1` | PowerShell 安装脚本 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.1.7
- **分类**: Common
- **网站**: http://atlisp.cn
