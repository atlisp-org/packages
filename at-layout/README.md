# @lisp甯冨眬

甯冨眬宸ュ叿

## 依赖

- `base` 包

## 安装

在CAD命令行中执行：
```lisp
@I at-layout
```

或：
```lisp
(@:package-install "at-layout")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `at-layout.lsp` | |
| `pvp.lsp` | |
| `merge.lsp` | |
| `divide-map.lsp` | |
| `stat-vp.lsp` | |
| `add-mapsheet.lsp` | |
| `lock-vp.lsp` | |
| `purge.lsp` | |

## 作者

- VitalGG
  - vitalgg@gmail.com

## 版本

- 1.0.10

## 许可证

请查看源码获取许可信息

---

# 布局视口工具（原 mini-layout 并入本包）

> 复现 iMini 工具集的 16 个布局视口命令：布局改名/增减/排序、布局合并/分图、
> 布局选字重命名、局部取图、视口比例/同步对齐/模型空间外框、视口锁定、
> 视口图层状态编辑、视口选层变灰/还原、图纸空间 1:1 布图、模型空间做块布图/炸开、
> 快速局部重生成。

## 菜单

包加载后新增菜单组「布局视口工具」（与原有的「布局」「布局2」菜单并列）：

| 菜单项 | 函数 | 说明 |
|--------|------|------|
| 布局改名增减排序 | `@layout:tab-sort` | 批量重命名(图纸%s/A-{} 模式)、增/删布局、按名或倒序重排标签 |
| 布局合并 | `@layout:layout-merge` | 多个布局合并为一个，每个源布局的主视口按网格排布进目标布局，源布局删除 |
| 布局分图 | `@layout:layout-isolate` | 一个布局里的多个视口拆成多个布局（每视口一个布局），原布局删除 |
| 布局选字重命名 | `@layout:layout-rename` | 用图纸空间内一段文字的内容重命名当前布局 |
| 局部取图 | `@layout:jbqt` | 模型空间框选区域生成新布局，视口对准该区域并按 1:N 出图 |
| 编辑视口比例... | `@layout:vp-scale` | 给一组视口设置统一比例 1:N |
| 同步对齐视口 | `@layout:vp-sync` | 把一组视口按网格对齐排布（行列、间距可设，自动列数） |
| 在模型空间绘制视口外框 | `@layout:vp-outline` | 在模型空间为每个视口画带编号的外框（线+文字，可指定图层） |
| 视口锁定或解锁 | `@layout:vport-lock` | 锁定/解锁视口（DXF 70 bit 16） |
| 视口图层状态编辑 | `@layout:vplayer` | 逐个视口设置 `图层=状态|...`（on/off/freeze/thaw），保存到视口 xdata 并应用 |
| 视口选层变灰 | `@layout:vport-grey` | 把视口内（或指定）图层置灰，原颜色记在 xdata 供还原 |
| 还原视口颜色 | `@layout:vport-ungrey` | 还原被置灰的图层颜色 |
| 图纸空间1：1布图 | `@layout:ps-scale` | 模型空间框选区域，图纸空间建 1:1 视口 |
| 模型空间做块布图 | `@layout:ms-scale` | 图纸空间实体打块插入模型空间（比例自动取主视口 1:N，放大后打印尺寸不变） |
| 还原炸开布图块 | `@layout:ms-exp` | 炸开做块布图生成的块 |
| 快速局部重生成 | `@layout:regen` | 框选区域，只重生成与之相交的视口 |

## AI 调用

| 函数 | 说明 | 示例 |
|------|------|------|
| `@layout:ai-tab-rename` | 按模式批量重命名全部布局（`%s`/`{}` 替换序号），返回新布局名列表 | `(@layout:ai-tab-rename "图纸%s" 1)` |
| `@layout:ai-tab-add` | 新建 n 个布局（自动去重命名），返回新布局名列表 | `(@layout:ai-tab-add 3)` |
| `@layout:ai-tab-del` | 从末尾删 n 个布局（当前布局保留），返回已删除布局名列表 | `(@layout:ai-tab-del 2)` |
| `@layout:ai-tab-sort` | 重排布局标签（`"name"` 按名/`"order"` 倒序），返回新顺序列表 | `(@layout:ai-tab-sort "name")` |
| `@layout:ai-layout-merge` | 合并多个布局到目标布局（网格排布主视口，删源布局），返回目标布局名 | `(@layout:ai-layout-merge '("布局1" "布局2") "合并布局")` |
| `@layout:ai-layout-isolate` | 一个布局的多视口拆成多个布局（删原布局），返回新布局名列表 | `(@layout:ai-layout-isolate "布局1")` |
| `@layout:ai-layout-rename` | 用图纸空间内文字（可按子串过滤，取最左一条）重命名布局，返回新布局名 | `(@layout:ai-layout-rename "布局1" "平面图")` |
| `@layout:ai-jbqt` | 模型空间两角点+比例 N 生成新布局视口，返回视口 ename | `(@layout:ai-jbqt '(0 0 0) '(10000 8000 0) 50 "一层平面图")` |
| `@layout:ai-vp-scale` | 一组视口设置比例 N（1:N；0<N<1 按 customscale），返回已改视口列表 | `(@layout:ai-vp-scale (@layout:vports) 100)` |
| `@layout:ai-vp-sync` | 一组视口按网格对齐（rows/cols=0 自动，gap 缺省 500），返回已移动视口列表 | `(@layout:ai-vp-sync (@layout:vports) 2 3 500)` |
| `@layout:ai-vp-outline` | 在模型空间为布局内视口画带编号外框（layer nil=ML_OUTLINE），返回实体列表 | `(@layout:ai-vp-outline nil nil)` |
| `@layout:ai-vport-lock` | 锁定/解锁一组视口（DXF 70 bit 16），返回已改视口列表 | `(@layout:ai-vport-lock (@layout:vports) t)` |
| `@layout:ai-vplayer` | 设置视口图层状态并保存 xdata + 立即应用，返回已应用对列表 | `(@layout:ai-vplayer vp '(("A" . "off") ("B" . "freeze")))` |
| `@layout:ai-vplayer-query` | 读取视口上已保存的图层状态，返回对列表 | `(@layout:ai-vplayer-query vp)` |
| `@layout:ai-vplayer-apply` | 把视口上已保存的图层状态应用到图层表，返回已应用对列表 | `(@layout:ai-vplayer-apply vp)` |
| `@layout:ai-vport-grey` | 一组图层置灰（layers nil=视口视图内实体所在层；原色记 xdata），返回已置灰层名列表 | `(@layout:ai-vport-grey vp nil 8)` |
| `@layout:ai-vport-ungrey` | 还原置灰图层（layers nil=全部带标记层），返回已还原层名列表 | `(@layout:ai-vport-ungrey nil)` |
| `@layout:ai-ps-scale` | 模型空间两角点→图纸空间 1:1 视口，返回视口 ename | `(@layout:ai-ps-scale '(0 0 0) '(1000 800 0) '(10 10 0))` |
| `@layout:ai-ms-scale` | 布局图纸空间实体打块插入模型空间（scale nil=按主视口 1:N 自动），返回块 ename | `(@layout:ai-ms-scale "布局1" '(0 0 0) nil)` |
| `@layout:ai-ms-exp` | 炸开一组块插入，返回已炸开 ename 列表 | `(@layout:ai-ms-exp (@layout:ss-to-list ss))` |
| `@layout:ai-regen` | 模型空间两角点区域，重生成相交视口，返回已重生成视口列表 | `(@layout:ai-regen '(0 0 0) '(5000 5000 0))` |

约定：AI 入口均无交互、不弹模态框，提示走 `princ`；成功返回实体/列表，失败返回 nil。
`vports`/`ents` 类参数既接受 ename 列表，也接受选择集。

## 新增源文件

| 文件 | 说明 |
|------|------|
| `layout-draw.lsp` | 菜单注册 + 公共辅助（布局/视口 VLA 封装、选择集工具） |
| `layout.lsp` | 布局管理：改名增减排序/合并/分图/选字重命名/局部取图 |
| `vport.lsp` | 视口工具：比例/同步对齐/外框/锁定/图层状态/变灰还原 |
| `plot.lsp` | 布图工具：图纸空间1:1/做块布图/炸开/局部重生成 |
