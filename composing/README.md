# composing - 排版

各种排版工具，支持分堆等间隙排版、总长等距排版等多种排版方式。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 排版 > 分堆等间隙排版 | `(composing:cluster-gap)` | 分堆后等间隙排列 |
| 排版 > 分堆总长等距排版 | `(composing:cluster)` | 分堆后总长等距排列 |
| 排版 > 图形总长等距排版 | `(composing:entity)` | 图形总长等距排列 |
| 排版 > 图形等间隙排版 | `(composing:entity-gap)` | 图形等间隙排列 |
| 排版 > 矩形嵌套排版 | `(composing:unest)` | FFD 矩形嵌套排版（多板） |

## AI 调用

| 函数 | 说明 |
|------|------|
| `composing:ai-nest` | FFD 矩形嵌套排版，返回每件摆放坐标（无交互） |
| `composing:ai-nest-draw` | 把 `ai-nest` 结果画到 CAD（板框 + 件矩形 + 件号） |

示例：

```lisp
;; 5 件 400x300、3 件 600x200，排入 1220x2440 板，间距 10，允许旋转
(composing:ai-nest '((400 300 5) (600 200 3)) 1220 2440 10 T)
;; => ((1 (1 x y w h 0) (2 x y w h 0) ...) (2 (...)) ...)
```

## 源文件

| 文件 | 说明 |
|------|------|
| `composing.lsp` | 主程序入口，菜单注册 |
| `cluster-composing` | 分堆排版核心模块 |
| `nest-composing` | 矩形嵌套排版（FFD 算法）核心模块 |

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
