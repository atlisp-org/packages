# @图库（at-library）

把 atlisp.cn 在线图库（**24,699 个专业符号，15 个专业**）变成 CAD 里的日常能力：
搜索、插入、图框、图例、标准件明细表。

数据层在函数库 `lib/src/library/`（`library:` 命名空间），本包只做交互与出图。

## 命令（菜单「@图库」）

| 菜单 | 函数 | 说明 |
|---|---|---|
| 搜索并插入 | `(at-library:insert)` | 关键字 → 序号 → 插入点 |
| 按目录浏览 | `(at-library:browse "电气/元件库")` | 目录内挑选 |
| 插入标准图框 | `(at-library:frame "A2" "横" pt)` | 图库「图框库」，左下角对齐 |
| 生成图例 | `(at-library:legend pt)` | 扫当前布局用到的块 → 符号 + 名称 |
| 标准件明细表 | `(at-library:bom pt)` | 名称/规格/标准号/数量 |
| 更新图库索引 | `(at-library:update)` | 强制刷新本地索引 |
| 图库体检/统计 | `(at-library:stats)` | 符号数 / 索引时间 |

## 依赖

```lisp
(require 'library:*)   ; library:search / info / insert / update
```

* 索引首次使用时从 `https://atlisp.cn/dw/library/index/library.tsv` 下载到
  `@::*prefix*` 下的 `library/library.tsv`，7 天过期自动更新。
* 符号 dwg 按需下载到 `@::*prefix*` 下的 `library/dw/<专业>/`，默认每次插入都取最新版
  （`*library:always-update*` 设 nil 可改成只下缺失的）。

## 实测注意

* `library:insert` 的 5 个参数**都要给**（AutoLISP 不支持省略参数），对齐方式用 nil 表示 auto。
* 图库里 13.7% 的符号插入基点离几何很远（实测最远 1.1 km）。auto 只在偏移超过符号尺寸时
  按几何中心对齐——门的铰点、图框的角点这类**有意**的基点不会被改掉。
* 48.8% 的符号 `$INSUNITS=0`，无法换算真实尺寸；此时不会自动缩放。
* 全部为 AC1032（2018 格式），AutoCAD 2017 及更早打不开。
