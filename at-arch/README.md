# at-arch @建筑

建筑设计工具包：房间定位、窗地比检查、车位编号、门窗统计、面积汇总等。

## 基本信息

| 字段 | 值 |
|------|-----|
| 包名 | `at-arch` |
| 全名 | @建筑 |
| 版本 | 0.1.1 |
| 作者 | VitalGG |
| 邮箱 | vitalgg@gmail.com |
| 分类 | 建筑工程 |
| 语言 | chs（简体中文） |
| 开源 | 否 |
| 依赖 | `base` |
| 主页 | http://atlisp.cn |

## 功能菜单

### @建筑
| 菜单项 | 命令 | 说明 |
|--------|------|------|
| 建筑设置 | `(at-arch:setup)` | 打开配置对话框 |
| 未命名房间 | `(at-arch:locate-unnamed-space)` | 定位未命名房间并缩放视图 |
| 定位房间 | `(at-arch:menu-locate-space)` | 按编号或名称定位房间（支持通配符） |
| 检查窗地比 | `(at-arch:w/space)` | 核查窗地比，不合规房间标红并生成核查表 |
| 选择同形房间 | `(at-arch:sel-same-space)` | 选择面积相同的房间 |
| 选择同名房间 | `(at-arch:sel-same-name)` | 选择名称相同的房间 |
| 显隐房间面积 | `(at-arch:onoff-spacearea)` | 切换房间面积标注的显示/隐藏 |
| 总建筑面积 | `(at-arch:sum-spacearea)` | 求各单块建筑面积之和 |
| 面积汇总 | `(at-arch:area-summary)` | 按房间名称分类统计建筑面积 |
| 导出面积CSV | `(at-arch:export-space-csv)` | 将房间面积导出为CSV文件 |

### @建筑说明
| 菜单项 | 命令 | 说明 |
|--------|------|------|
| 公建说明 | `(at-arch:insert-block "公建说明")` | 插入公建说明图块模板 |
| 住宅说明 | `(at-arch:insert-block "住宅说明")` | 插入住宅说明图块模板 |
| 防水专篇 | `(at-arch:insert-block "防水设计-专篇")` | 插入防水设计专篇图块 |
| 绿建专篇 | `(at-arch:insert-block "绿建专篇")` | 插入绿色建筑专篇图块 |

### @建筑车库
| 菜单项 | 命令 | 说明 |
|--------|------|------|
| 插入车位 | `(at-arch:insert-parking)` | 插入标准车位图块 |
| 插入无障碍车位 | `(at-arch:insert-accparking)` | 插入无障碍车位图块 |
| 插入机械车位 | `(at-arch:insert-machineparking)` | 插入机械车位图块 |
| 选择车位 | `(at-arch:parking-select)` | 按规则选中全部车位，并打印普通/无障碍/机械车位数量 |
| 车位编号 | `(at-arch:parking-numbering)` | 框选车位后按蛇形顺序连续编号（不选=全图） |

### 车位的识别与编号

**识别（不依赖块名）**：`at-arch:parking-p` 判定一个块是不是车位——块名命中 `@arch:parking-blk`，
**或者** 图层命中 `@arch:parking-layer` 且外形尺寸落在 `@arch:parking-size` 范围内。
这样命名不规范的车位块（如 `ded`）也能识别，而同图层上的挡车器、图框等不会被误判。

| 函数 | 说明 |
|------|------|
| `at-arch:parking-p` | 是车位返回 T（名称规则 或 图层+尺寸规则） |
| `at-arch:parking-name-p` | 仅按块名规则判断 |
| `at-arch:parking-layer-p` | 仅按图层规则判断 |
| `at-arch:parking-geom-p` | 仅按外形尺寸判断（短边=宽，长边=深） |
| `at-arch:parking-type` | 返回「机械车位 / 无障碍车位 / 普通车位」 |
| `at-arch:parking-number-att` | 取车位块的编号属性名（先精确 `NUMBER`，再按 `@arch:parking-number-att` 通配） |
| `at-arch:parking-list` | 从选择集/列表/全图中筛出车位块，返回 `(车位列表 . 非车位数)` |

**编号**：`at-arch:parking-number (sel start)` 无交互核心，按 `@arch:parking-number-order`
（`snake` 蛇形 / 或 `xyXY` 组合）排序后写入编号属性；没有编号属性的车位会跳过并提示，且不占号。

AI 调用入口：

```lisp
(at-arch:ai-parking-number 1 nil)      ; 全图，从 1 开始
(at-arch:ai-parking-number 101 (ssget)) ; 限定选择集
```

### @门窗统计
| 菜单项 | 命令 | 说明 |
|--------|------|------|
| 门窗统计表 | `(at-arch:door-window-stat)` | 按型号分类统计门窗数量，可生成统计表 |

## 配置项

| 配置键 | 默认值 | 说明 |
|--------|--------|------|
| `@arch:parking-blk` | `车位*,*车位*,机械车位*,*机械停车*,停车位*` | 车位块名规则（支持通配符，逗号分隔多规则） |
| `@arch:parking-layer` | `*停车位*,*车位*` | 车位所在图层规则（配合尺寸识别命名不规范的块） |
| `@arch:parking-size` | `2200,3700,4600,6200` | 车位识别尺寸范围：最小宽,最大宽,最小深,最大深 |
| `@arch:parking-number-order` | `snake` | 编号排序：`snake` 蛇形，或 `xyXY` 组合（如 `Yx`） |
| `@arch:parking-number-att` | `*NUMBER*,*编号*,*车位号*` | 编号属性名规则（支持通配符） |
| `@arch:parking-row-tol` | `1200` | 蛇形排序：同一排的 y 容差 |
| `@arch:parking-min-row` | `5` | 蛇形排序：一行数量达到该值才换向 |

## 窗地比限值规则

| 房间类型（通配符） | 最小窗地比 |
|-------------------|-----------|
| 卧室, 起居*, 厨房 | 1/7 |
| 设计*, 绘图* | 1/4 |
| 办公*, 会议* | 1/5 |
| 复印*, 档案*, *包间* | 1/6 |
| 走廊, 走道, 楼梯*, 卫* | 1/10 |

## 依赖文件

以下 dwg 资源文件随包分发，首次使用时自动从远程下载：

| 文件 | 用途 |
|------|------|
| `车位.dwg` | 标准车位图块 |
| `车位-无障碍.dwg` | 无障碍车位图块 |
| `机械车位.dwg` | 机械车位图块 |
| `公建说明.dwg` | 公建说明模板 |
| `住宅说明.dwg` | 住宅说明模板 |
| `绿建专篇.dwg` | 绿色建筑专篇模板 |
| `防水设计-专篇.dwg` | 防水设计专篇模板 |

## 源码文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-arch.lsp` | 主程序（菜单注册、全部功能实现） |
| `lib.lsp` | 辅助函数（图块插入通用逻辑） |
| `窗地比.lsp` | 窗地比最小限值数据定义 |

## 安装

在 CAD 命令行执行：

```
@I at-arch
```

或：

```
(@:package-install "at-arch")
```

## 建筑绘图补充（iMini 复现，2026-10）

菜单「建筑绘图」，共 18 个 AI 入口。交互入口同名去掉 `ai-` 前缀。

| 命令 | AI 入口 | 说明 |
|---|---|---|
| 直线轴网 | `at-arch:ai-axis-line origin x-spacings y-spacings [layer]` | 生成正交轴线 + 轴号圈 + 编号 |
| 轴线编号 | `at-arch:ai-axis-text axis-ents` | 对已有轴线重编轴号 |
| 修改轴线编号 | `at-arch:ai-axis-update axis-texts [start] [prefix]` | 轴号重排 |
| 画墙 | `at-arch:ai-wall pts thickness [layer]` | 多段线路径生成双线墙 |
| 单线变墙 | `at-arch:ai-line2wall ents thickness [both] [del]` | 折线两侧偏移成墙 |
| 批量改墙宽 | `at-arch:ai-wall-width ents tnew` | 按新墙厚重画墙双线 |
| 墙体完成面 | `at-arch:ai-wall-cover ents d` | 向外偏移完成面线 |
| 删除墙柱门窗 | `at-arch:ai-erase-wall ents / layers` | 按图层关键字批量删除 |
| 画柱 | `at-arch:ai-column pt shape w h [label]` | r/o/c/l/t 五种柱形 |
| 多段线变柱 | `at-arch:ai-pline2column ents` | 矩形/圆→柱 |
| 画梁 | `at-arch:ai-beam p1 p2 width [layer]` | 中心线 + 梁宽双线 |
| 单线变梁 | `at-arch:ai-line2beam ents width` | 折线生成梁 |
| 批量改梁宽 | `at-arch:ai-beam-width ents w` |  |
| 两点画门 | `at-arch:ai-door p1 p2 thickness side leaves` | 门框 + 门扇 + 开启弧 |
| 两点画窗 | `at-arch:ai-win p1 p2 thickness` | 三线窗符号 |
| 墙上开门 | `at-arch:ai-hole-door ents pt width side leaves` | 墙体断开 + 门 |
| 门窗编号 | `at-arch:ai-win-text ents label` | 中心编号文字 |
| 门窗表 | `at-arch:ai-win-list ents` | 返回门窗统计表数据 |

示例：

```lisp
;; 3.6m+3.6m 开间、4.2m+4.5m 进深的轴网
(at-arch:ai-axis-line '(0 0 0) '(3600 3600) '(4200 4500) "AXIS")

;; 300x600 方柱
(at-arch:ai-column '(0 0 0) "r" 300 600 "KZ1")

;; 沿墙线生成 240 厚墙
(at-arch:ai-line2wall (list (car (entsel))) 240 T nil)
```
