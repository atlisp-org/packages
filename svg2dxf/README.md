# svg2dxf

将 SVG 矢量格式转换为 DXF 格式。开发中功能。

## 菜单命令

| 菜单组 | 命令名称 | 调用函数 | 说明 |
|--------|----------|----------|------|
| svg2dxf | svg转dxf | `(svg2dxf:svg2dxf)` | SVG 转 DXF |
| svg2dxf | 图片转svg | `(svg2dxf:img2svg)` | 图片转 SVG |

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `svg2dxf:hello` | 无 | 显示功能提示信息 |
| `svg2dxf:svg2dxf` | 无 | SVG 转 DXF（开发中） |
| `svg2dxf:img2svg` | 无 | 图片转 SVG（开发中） |

## 依赖

- **base** - @lisp 基础包
- **Python** - 需要 Python 环境
- **vtracer** - Rust 图像矢量化工具

## 安装

```lisp
(@:package-install "svg2dxf")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `svg2dxf.lsp` | 主程序源码 |
| `svg2dxf.py` | Python 转换脚本 |
| `readme.org` | 原始说明文档 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.0.1
- **分类**: 通用
- **网站**: http://atlisp.cn
