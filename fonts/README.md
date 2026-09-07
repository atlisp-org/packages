# @lisp 字体库

CAD 常用字体集合。包含天正、PKPM、探索者等字体。自动配置 ACAD 环境变量，修复缺失字体问题。

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `fonts:merge` | 无 | 归并字体样式，将中文字体替换为系统 TTF 字体 |
| `fonts:merge1` | 无 | 归并字体样式（旧版实现） |
| `fonts:nulltoFonts` | `shxx shxb ttf` | 替换空字体为指定字体文件 |
| `fonts:check` | 无 | 检查字体文件是否存在，返回缺失列表 |
| `fonts:check_fontfile` | `sty ttf wid` | 检查并修复单个字体样式 |
| `fonts:fix-fonts` | `tips` | 自动修复缺失字体 |
| `fonts:use_myfonts` | 无 | 批量替换字体为用户默认字体 |

## 包含字体文件

| 字体文件 | 说明 |
|----------|------|
| `gbhzfs.shx` | 国标汉字仿宋字体 |
| `China.shx` | 中文字体 |
| `Gbcbig.shx` | 国标大字体 |
| `hztxt.shx` | 汉字字体 |
| `Pkpmeng.shx` | PKPM 英文字体 |
| `romans.shx` | 罗马字体 |
| `Tssdchn.shx` | 天正汉字字体 |
| `Tssdeng.shx` | 天正等线字体 |
| `Tssdeng2.shx` | 天正等线字体 2 |
| `yjkeng.shx` | 盈建科英文字体 |
| `yjkchn.shx` | 盈建科中文字体 |
| `txt.shx` | 标准文本字体 |

## 依赖

- 无（独立字体包）

## 安装

```lisp
(@:package-install "fonts")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `fonts.lsp` | 主程序源码 |
| `fix-font.lsp` | 字体修复工具 |
| `*.shx` | 字体文件 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 1.0.29
- **分类**: 文本
- **网站**: http://atlisp.cn
