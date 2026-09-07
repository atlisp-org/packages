# 边界分析

生成图形所在闭合区域的边界，填充边界等。基于特定块生成区域边界。

## 菜单命令

| 菜单组 | 命令名称 | 调用函数 | 说明 |
|--------|----------|----------|------|
| 边界 | 生成边界 | `(boundary:make-by-blk)` | 由特定块生成区域边界 |
| 边界 | 填充边界 | `(boundary:hatch)` | 填充边界区域 |
| 边界 | 删除边界 | `(boundary:remove-boundary)` | 删除边界线 |
| 边界 | 删除边界填充 | `(boundary:remove-hatch)` | 删除边界填充 |
| 边界 | 显示块 | `(boundary:show-ent)` | 显示隐藏的块 |

## 函数列表

| 函数名 | 参数 | 说明 |
|--------|------|------|
| `boundary:make-by-blk` | 无 | 由特定块生成区域边界 |
| `boundary:hatch` | 无 | 填充边界区域 |
| `boundary:remove-hatch` | 无 | 删除边界填充 |
| `boundary:remove-boundary` | 无 | 删除边界线 |
| `boundary:show-ent` | 无 | 显示隐藏的块 |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `boundary:blkname` | gc124 | 要生成边界的块名 |
| `boundary:layer` | tmp-boundary | 生成的边界所在图层 |
| `boundary:color` | 2 | 边界图层颜色（红色） |

## 依赖

- **base** - @lisp 基础包

## 工作流程

1. 选择要生成边界的块（按 `boundary:blkname` 配置过滤）
2. 自动缩放到选中区域
3. 创建临时图层 `tmp-boundary`
4. 为每个块生成闭合边界
5. 隐藏已生成边界的块
6. 可对边界进行填充或删除

## 安装

```lisp
(@:package-install "boundary")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `boundary.lsp` | 主程序源码 |
| `pkg.lsp` | 包清单 |

## 作者信息

- **作者**: VitalGG
- **邮箱**: vitalgg@gmail.com
- **版本**: 0.0.5
- **分类**: Common
- **网站**: http://atlisp.cn
