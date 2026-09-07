# PDF 处理工具

基于 pdftk free 的 PDF 处理工具。用于 PDF 合并、拆分、加密、解密、加水印、戳记等操作。

## 菜单命令

| 菜单组 | 命令名称 | 调用函数 | 说明 |
|--------|----------|----------|------|
| PDF相关 | PDF设置 | `(pdftk:setup)` | 配置戳记和水印文件路径 |
| PDF相关 | 合并PDF | `(pdftk:menu-merge)` | 合并文件夹下所有 PDF |
| PDF相关 | 拆分PDF | `(pdftk:menu-burst)` | 拆分 PDF 为单页文件 |
| PDF相关 | PDF加戳记 | `(pdftk:menu-stamp)` | 给 PDF 加戳记 |
| PDF相关 | 批量加戳记 | `(pdftk:menu-batch-stamp)` | 批量加戳记 |
| PDF相关 | PDF加水印 | `(pdftk:menu-background)` | 给 PDF 加水印 |
| PDF相关 | 批量加水印 | `(pdftk:menu-batch-background)` | 批量加水印 |
| PDF相关 | 解密PDF | `(pdftk:menu-decrypt)` | 去除 PDF 所有者密码 |
| PDF相关 | 加密PDF | `(pdftk:menu-encrypt)` | 设置 PDF 密码和权限 |

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `pdftk:setup` | 无 | 打开配置对话框 |
| `pdftk:download` | 无 | 下载 pdftk.exe 和相关文件 |
| `pdftk:menu-merge` | 无 | 合并 PDF 菜单入口 |
| `pdftk:merge` | `folder` | 合并指定文件夹下所有 PDF |
| `pdftk:menu-burst` | 无 | 拆分 PDF 菜单入口 |
| `pdftk:burst` | `pdf-filename` | 拆分 PDF 为单页 |
| `pdftk:menu-encrypt` | 无 | 加密 PDF 菜单入口 |
| `pdftk:encrypt` | `filename owner-pw user-pw permissions` | 加密 PDF |
| `pdftk:menu-decrypt` | 无 | 解密 PDF 菜单入口 |
| `pdftk:decrypt` | `filename owner-pw` | 解密 PDF |
| `pdftk:menu-stamp` | 无 | 加戳记菜单入口 |
| `pdftk:stamp` | `filename` | 给 PDF 加戳记 |
| `pdftk:menu-batch-stamp` | 无 | 批量加戳记入口 |
| `pdftk:menu-background` | 无 | 加水印菜单入口 |
| `pdftk:background` | `filename` | 给 PDF 加水印 |
| `pdftk:menu-batch-background` | 无 | 批量加水印入口 |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `pdftk:stamp` | D:\Design\standard\stamp.pdf | 戳记 PDF 文件路径 |
| `pdftk:background` | D:\Design\standard\background.pdf | 水印 PDF 文件路径 |
| `pdftk:pre-folder` | D:\ | 最后一次操作的文件夹位置 |

## 加密权限选项

| 权限名称 | 说明 |
|----------|------|
| Printing | 高质量打印 |
| DegradedPrinting | 低质量打印 |
| ModifyContents | 编辑内容 |
| Assembly | 组装 |
| CopyContents | 复制内容 |
| ScreenReaders | 屏幕阅读器 |
| ModifyAnnotations | 编辑注释 |
| FillIn | 填充 |
| AllFeatures | 以上所有 |

## 依赖

- **base** - @lisp 基础包
- **pdftk.exe** - PDF 处理工具（自动下载）
- **iconv.exe** - 编码转换工具

## 安装

```lisp
(@:package-install "pdftk")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pdftk.lsp` | 主程序源码 |
| `background.pdf` | 默认水印模板 |
| `stamp.pdf` | 默认戳记模板 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.2.10
- **分类**: 图档管理
- **网站**: http://atlisp.cn
