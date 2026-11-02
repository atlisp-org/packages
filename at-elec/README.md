# @lisp建筑电气

建筑电气设计工具：回路编号、电缆标注、负荷统计、设备统计与插入等。

## 依赖

- `at-pm` 包

## 安装

在CAD命令行中执行：
```lisp
@I at-elec
```

或：
```lisp
(@:package-install "at-elec")
```

## AI 调用（无交互）

| 函数 | 说明 |
|------|------|
| `@elec:ai-load-stat sel` | 统计设备负荷，返回 `(("count" . n) ("total" . w) ("demand" . w) ("factor" . k))`；`sel` 为 nil 时统计全图 INSERT |
| `@elec:ai-equip-stat sel` | 按块名统计设备数量，返回 `((名称 . 数量) ...)` |
| `@elec:ai-circut-numbering sel prefix start` | 给曲线回路编号，返回编号数量 |
| `@elec:ai-cable-label ent cable-type` | 给曲线标注电缆型号，返回文字 ename |

```lisp
;; 全图设备负荷
(@elec:ai-load-stat nil)
;; 指定选择集设备统计
(@elec:ai-equip-stat (ssget '((0 . "INSERT"))))
;; 编号 5 条回路，前缀 WL，从 1 开始
(@elec:ai-circut-numbering (ssget '((0 . "LINE"))) "WL" 1)
```

## 源文件

| 文件 | 说明 |
|------|------|
| `at-elec.lsp` | 菜单、交互命令 |
| `ai.lsp` | AI 无交互入口 |
| `readme-elec.dwg` | |
| `example-elec.dwg` | |

## 作者

- VitalGG
  - vitalgg@gmail.com

## 版本

- 0.1.1

## 许可证

请查看源码获取许可信息
