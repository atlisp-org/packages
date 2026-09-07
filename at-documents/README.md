# at-documents - @lisp 开发文档

通用函数库文档及搜索工具。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| Support > @通用函数库 | `(at-documents:lib-manager-dialog)` | 打开函数库文档管理器 |

## 源文件

| 文件 | 说明 |
|------|------|
| `at-documents.lsp` | 主程序，包含文档加载、搜索和管理功能 |
| `*.libdoc` | 各模块函数文档文件 (22个) |

## 文档模块

block, layer, list, point, curve, layout, m, matrix, group, vla, string, entity, line, pickset, text, p, table, ui, std, excel, base64

## 依赖

- `dev-tools`

## 配置

无配置项。

## 使用说明

1. 运行命令打开函数库管理器
2. 可按分类筛选函数
3. 可搜索函数名或描述
4. 查看函数详情和源码

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.6.41
- **分类**: DevelopTools
- **开源**: 否
