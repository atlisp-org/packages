# @lisp curve

曲线相关工具集，提供曲线测量、优化、连接、标注等功能。

## 功能特性

### 曲线工具菜单

| 命令 | 说明 |
|------|------|
| 曲线配置 | 打开 @curve 配置对话框 |
| 双线互连 | 选择两条线，从最近端点连接成一条 |
| 优化多段线 | 优化多段线顶点，当连续多点共线或共圆时减少顶点 |
| 平滑路口 | 根据设定的转弯半径和道路转角范围，平滑道路的转角 |
| 曲线面积 | 标注闭合曲线的面积 |
| 曲线长度 | 在曲线中点标注曲线的长度 |
| 每段长度 | 标注曲线的每段长度 |
| 垂线缺口 | 在丁字线交点处生成缺口 |
| 连线端点 | 连接线端点 |
| 统计线长 | 分类汇总曲线的长度 |
| 单线变双 | 将单线双向偏移成双线 |
| 交点编号 | 一条曲线上与其他多段线的交点进行递增编号 |
| 排线相连 | 用水平及垂直路径线连接原物体到目标物体 |
| 样条转多段 | 将样条曲线转换为多段线 |
| lw转3D | 将二维多段线转化为三维多段线 |

### 曲线工具2菜单

| 命令 | 说明 |
|------|------|
| 检查闭合 | 检查曲线端点闭合情况 |
| 清闭合标志 | 清除闭合检查标记 |
| 按序号连点 | 按编号顺序连接点 |

## 函数列表

| 函数 | 说明 |
|------|------|
| `@curve:setup` | 打开 @curve 配置对话框 |
| `at-curve:join` | 选择两条线，从最近端点连接成一条 |
| `at-curve:area` | 标注闭合曲线的面积 |
| `at-curve:length` | 在曲线中点标注曲线的长度 |
| `at-curve:per-length` | 标注曲线的每段长度 |
| `at-curve:notch` | 在丁字线交点处生成缺口 |
| `at-curve:link-end` | 连接线端点 |
| `at-curve:stat` | 分类汇总曲线的长度 |
| `at-curve:dualline` | 将单线双向偏移成双线 |
| `@curve:inters-number` | 一条曲线上与其他多段线的交点进行递增编号 |
| `@curve:link-obj` | 用水平及垂直路径线连接原物体到目标物体 |
| `@curve:spline2lwpl` | 将样条曲线转换为多段线 |
| `@curve:menu-lw2pl` | 将二维多段线转化为三维多段线 |
| `at-curve:optimize-lwpl` | 优化多段线顶点 |
| `at-curve:fillet` | 路口圆角（内部函数） |
| `at-curve:fillet-road` | 平滑道路的转角 |
| `at-curve:noclosed-endpt` | 检查曲线端点闭合情况 |
| `at-curve:rm-flagpts` | 清除闭合检查标记 |
| `at-curve:join-by-number` | 按编号顺序连接点 |

## 配置

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `@curve:types` | `*POLYLINE,circle,arc,ellipse,spline,region` | 可操作的曲线的图元类型 |
| `@curve:dualline-width` | `120.0` | 单线变双线的默认宽度 |
| `@curve:dualline-closed` | `0` | 单线变双线后封口形式，0:不封口，1:直线，2:半圆 |
| `@curve:notch-type` | `0` | 生成缺口的默认类型，0 楔形，1 圆弧 |
| `@curve:notch-width` | `80` | 生成缺口的默认宽度 |
| `@curve:notch-height` | `50` | 生成缺口的默认深度 |
| `@curve:optimize-angle` | `0.001` | 可优化多段线的夹角差 |
| `@curve:radius` | `30.0` | 自动圆角的半径 |
| `@curve:maxlength-corner` | `80.0` | 自动圆角的原始倒角最大线长 |
| `@curve:max-angle` | `110.0` | 自动圆角的道路转角最大角度 |
| `@curve:min-angle` | `70.0` | 自动圆角的道路转角最小角度 |
| `@curve:gap` | `100` | 排线间距 |
| `@curve:pin-length` | `200` | 引脚长度，物体外伸的直线长度 |
| `@curve:pin-width` | `50` | 引线宽度，多段线的宽度 |

## 依赖

- `base` 包

## 安装

在 CAD 命令行输入：

```
@I at-curve
```

或执行：

```lisp
(@:package-install "at-curve")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `at-curve.lsp` | 主文件，菜单注册、双线互连、面积/长度标注、单线变双 |
| `notch.lsp` | 垂线缺口工具 |
| `optimize-lwpl.lsp` | 多段线优化 |
| `fillet-road.lsp` | 道路转角圆角 |
| `chain-line.lsp` | 连接线端点 |
| `inters-number.lsp` | 交点编号 |
| `link-obj.lsp` | 排线相连 |
| `unclosed.lsp` | 闭合检查 |
| `sp2pl.lsp` | 样条转多段线 |
| `join-by-number.lsp` | 按序号连点 |
| `lw-to-3d.lsp` | 二维多段线转三维多段线 |
| `stat.lsp` | 线长统计 |
| `pkg.lsp` | 包清单 |

## 作者

- **VitalGG** — vitalgg@gmail.com

## 版本

- **当前版本**：0.1.35
- **许可证**：免费
- **开源**：否
