# QR 二维码生成

生成二维码，并用多段线绘制到 DWG 中。支持自定义绘制比例。

## 菜单命令

| 菜单组 | 命令名称 | 调用函数 | 说明 |
|--------|----------|----------|------|
| 文本2 | 生成二维码 | `(qrencode:draw)` | 选择文本生成二维码 |

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `qrencode:draw` | 无 | 选择文本对象生成二维码 |
| `qrencode:mkpline` | `pt col scale` | 创建单个二维码方块 |
| `qrencode:qr-data` | `str` | 调用 exe 获取矩阵 |
| `qrencode:render` | `str pt` | 在 pt 绘制二维码，返回块参照 |
| `qrencode:make` | `str` | 交互：取点后生成 |
| `qrencode:ai-make` | `str pt` | **AI 入口**：无交互、不下载、不取点；返回块参照或 nil |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `qrencode:scale` | 100 | 二维码绘制比例 |

## AI 调用

```lisp
;; 在 (0 0) 绘制二维码，返回块参照 ename；exe 缺失时返回 nil 并 princ
(qrencode:ai-make "https://atlisp.cn" '(0 0))
```

`qrencode:ai-make` 不弹窗、不下载、不调用 `getpoint`，适合 MCP/Agent 调用。

## 依赖

- **base** - @lisp 基础包
- **QRencodeForLisp.exe** - 二维码生成工具（自动下载）

## 安装

```lisp
(@:package-install "qrencode")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `qrencode.lsp` | 主程序源码 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.0.9
- **分类**: 图档管理
- **网站**: http://atlisp.cn
