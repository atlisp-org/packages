# composing - 排版

各种排版工具，支持分堆等间隙排版、总长等距排版等多种排版方式。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 排版 > 分堆等间隙排版 | `(composing:cluster-gap)` | 分堆后等间隙排列 |
| 排版 > 分堆总长等距排版 | `(composing:cluster)` | 分堆后总长等距排列 |
| 排版 > 图形总长等距排版 | `(composing:entity)` | 图形总长等距排列 |
| 排版 > 图形等间隙排版 | `(composing:entity-gap)` | 图形等间隙排列 |

## 源文件

| 文件 | 说明 |
|------|------|
| `composing.lsp` | 主程序入口，菜单注册 |
| `cluster-composing` | 分堆排版核心模块 |

## 依赖

- `base`

## 配置

无配置项。

## 安装

在 CAD 命令行执行：
```
@I composing
```

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.0.7
- **分类**: 排版
- **开源**: 否
