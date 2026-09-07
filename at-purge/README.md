# at-purge - 图面清理

图面清理工具，清理 DGN 线型问题、分解多重插入块。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 实体 → 清理DGN | `(at-purge:remove-dgn)` | 修复 DGN 线型问题并运行 purge 清理 |
| 实体 → 分解重块 | `(@:explode-minsert)` | 分解多重插入块（MINSERT） |

## 函数

| 函数 | 说明 |
|------|------|
| `at-purge:remove-dgn` | 删除 ACAD_DGNLINESTYLECOMP 字典项，然后执行 `purge a * N` 清理 |
| `@:explode-minsert` | 选择多重插入块，创建新的 INSERT 实体后执行 explode 分解 |

## 使用说明

- **清理 DGN**：修复因导入 DGN 文件导致的线型问题，自动执行全量 purge
- **分解重块**：选择多重插入块（MINSERT），将其转换为普通 INSERT 后分解

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-purge
```

或：

```
(@:package-install "at-purge")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-purge.lsp` | 主程序源码 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.0.7
- 分类: 文件
- 网站: http://atlisp.cn
