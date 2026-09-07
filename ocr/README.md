# ocr - 视口文字识别

识别 CAD 视口中的文字，基于 OCR 服务。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| ocr > 识别文字 | `(ocr:viewport)` | 识别当前视口中的文字 |

## 源文件

| 文件 | 说明 |
|------|------|
| `ocr.lsp` | 主程序，导出视口为 BMP 并调用 OCR 服务 |
| `routes.py` | Python OCR 路由服务 |

## 依赖

- `base`
- 需要本地运行 OCR Python 服务 (localhost:5000)

## 配置

无配置项。OCR 服务地址固定为 `http://localhost:5000/ocr/ocr`。

## 使用说明

1. 确保 OCR Python 服务已启动
2. 在 CAD 中选择要识别的对象
3. 程序将选区导出为 BMP 并发送到 OCR 服务

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.0.1
- **分类**: Common
- **开源**: 否
