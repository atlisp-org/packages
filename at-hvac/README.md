# at-hvac — @lisp 暖通

暖通系统化绘图工具，提供暖通空调（HVAC）平面图绘制、设备布置、管道标注、地暖盘管生成、分集水器平衡分析及设备表生成等功能。

## 基本信息

| 项目 | 内容 |
|------|------|
| 包名 | `at-hvac` |
| 全称 | @lisp 暖通 |
| 版本 | 0.0.31 |
| 作者 | VitalGG |
| 邮箱 | vitalgg@gmail.com |
| 分类 | 暖通 |
| 语言 |chs |
| 许可 | 非开源 |
| 主页 | http://atlisp.cn |
| 依赖 | `at-pm` |

## 菜单结构

安装后通过菜单 **@暖通** 访问以下功能：

| 菜单项 | 函数 | 说明 |
|--------|------|------|
| 插入说明 | `@hvac:draw-readme` | 插入暖通说明图块（自动下载 `readme-hvac.dwg`） |
| 平面图样例 | `@hvac:draw-plan-example` | 插入暖通平面图样例（自动下载 `example-hvac.dwg`） |
| 布置风机风口 | `@hvac:insert-block "方壁式轴流风机"` | 插入方壁式轴流风机组块 |
| 插入新风机组 | `@hvac:insert-block "新风机组"` | 插入新风机组块 |
| 风机型号说明 | `@hvac:fengji-info` | 弹窗显示卧式暗装风机盘管型号参数 |
| 风机统计 | `@hvac:stat-fengji` | 统计所选风机并生成材料表 |
| 标地暖管 | `@hvac:dim-pipe` | 标注单根地暖管的间距与长度 |
| 批量标地暖管 | `@hvac:batch-dim-pipe` | 批量标注地暖管，未成功标注的管道变红色 |
| 分集水器平衡分析 | `@hvac:equip-balance` | 分析采暖分集水器各回路长度平衡 |
| 绘制设备表 | `@hvac:make-equip-bom` | 生成分集水器主要设备表 |

## 公开函数一览

### 主模块 `at-hvac.lsp`

| 函数 | 说明 |
|------|------|
| `@hvac:draw-readme` | 插入暖通说明图块并炸开 |
| `@hvac:draw-plan-example` | 插入暖通平面图样例 |
| `@hvac:insert-block <dwgname>` | 插入 DWG 图块（自动下载缺失资源） |
| `@hvac:fengji-info` | 显示风机盘管型号参数表 |
| `@hvac:stat-fengji` | 选择风机 → 统计 → 生成设备参数表 |
| `@hvac:dim-pipe` | 点选单根地暖管标注长度（D=300mm, L=…m） |
| `@hvac:batch-dim-pipe` | 框选批量标注地暖管间距与长度，支持鼠标交互定位 |
| `@hvac:move-pt-base` | 更改多线段标注控制点位置（内部辅助） |

### 地暖标注模块 `dim-pipe.lsp`

| 函数 | 说明 |
|------|------|
| `@hvac:dim-pipe` | 标注单根地暖管（层 `*地暖*` 的 LWPOLYLINE） |
| `@hvac:batch-dim-pipe` | 批量标注，自动计算管间距（取众数），标注位置可鼠标拖动 |

### 设备模块 `equip.lsp`

| 函数 | 说明 |
|------|------|
| `@hvac:equip-balance` | 分集水器平衡分析：回路数、总长、最长/最短、最大差值；差值 >15m 高亮 |
| `@hvac:make-equip-bom` | 生成分集水器设备统计表（编号、名称、规格型号、单位、数量、备注） |

### 地暖盘管模块 `floor-heating.lsp`

| 函数 | 说明 |
|------|------|
| `hvac:floor-heating <boundary> <spacing>` | 在闭合多段线内绘制蛇形地暖盘管 |
| `hvac:floor-heating-spiral <boundary> <spacing>` | 在闭合多段线内绘制螺旋形地暖盘管 |
| `hvac:floor-heating-auto <spacing>` | 自动搜索图中所有闭合 LWPOLYLINE，批量绘制螺旋形盘管 |
| `hvac:floor-heating-auto-snake <spacing>` | 自动搜索图中所有闭合多段线，批量绘制蛇形盘管 |

## 风机盘管型号参数

| 型号 | 全称 | 冷量(W) | 热量(W) | 功率(W) | 风量(m³/h) |
|------|------|---------|---------|---------|------------|
| FP51 | 卧式暗装风机盘管 FP-51 | 2890 | 4820 | 59 | 510 |
| FP85 | 卧式暗装风机盘管 FP-85 | 4520 | 7670 | 84 | 850 |
| FP102 | 卧式暗装风机盘管 FP-102 | 5420 | 8850 | 118 | 1020 |
| FP136 | 卧式暗装风机盘管 FP-136 | 7200 | 10800 | 156 | 1360 |

## 源文件列表

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单文件 |
| `at-hvac.lsp` | 主模块：菜单注册、风机统计、图块插入、说明/样例绘制 |
| `dim-pipe.lsp` | 地暖管标注（单根 & 批量） |
| `equip.lsp` | 分集水器平衡分析 & 设备表生成 |
| `floor-heating.lsp` | 地暖盘管自动绘制（蛇形 & 螺旋形） |

## 资源文件（DWG）

| 文件 | 用途 |
|------|------|
| `方壁式轴流风机.dwg` | 轴流风机组块 |
| `新风机组.dwg` | 新风机组块 |
| `风机盘管.dwg` | 风机盘管组块 |
| `盘管风机.dwg` | 盘管风机组块 |
| `通风器.dwg` | 通风器组块 |
| `readme-hvac.dwg` | 暖通说明图块 |
| `readme-hvac-A1.dwg` | A1 幅面说明 |
| `example-plan-hvac.dwg` | 暖通平面图样例 |
| `example-空调平面图.dwg` | 空调平面图样例 |
| `example-采暖干管平面图.dwg` | 采暖干管平面图样例 |
| `example-地板辐射采暖平面图.dwg` | 地板辐射采暖平面图样例 |

## 配置项

- **`@::draw-scale`** — 出图比例，用于控制标注文字缩放（`vla-put-ScaleFactor` 取 `10 × draw-scale`）

## 安装方式

在 CAD 命令行执行：

```
@I at-hvac
```

或：

```lisp
(@:package-install "at-hvac")
```

依赖 `at-pm` 包，首次使用会自动安装。图块资源（DWG）在首次调用相关功能时自动下载到 `packages/at-hvac/` 目录。

## 依赖关系

- **at-pm** — 包管理器，负责资源下载与路径管理
- **base**（隐含）— @lisp 基础函数库（`entity:*`, `curve:*`, `block:*`, `stat:*`, `table:*`, `ui:*` 等）

## 使用示例

```lisp
;; 插入说明
(@hvac:draw-readme)

;; 插入方壁式轴流风机
(@hvac:insert-block "方壁式轴流风机")

;; 统计所选风机生成材料表
(@hvac:stat-fengji)

;; 标注单根地暖管
(@hvac:dim-pipe)

;; 批量标注地暖管（框选后鼠标交互定位）
(@hvac:batch-dim-pipe)

;; 分集水器平衡分析
(@hvac:equip-balance)

;; 生成设备表
(@hvac:make-equip-bom)

;; 在选定区域内绘制蛇形地暖盘管（间距 150mm）
(hvac:floor-heating (car (entsel)) 150)

;; 自动为图中所有闭合区域绘制螺旋形盘管
(hvac:floor-heating-auto 150)
```
