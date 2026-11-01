# @图库（at-library）

把 atlisp.cn 在线图库（**24,699 个专业符号，15 个专业**）变成 CAD 里的日常能力：
搜索、插入、图框、图例、标准件明细表。

数据层在函数库 `lib/src/library/`（`library:` 命名空间），本包只做交互与出图。

## 命令（菜单「@图库」）

| 菜单 | 函数 | 说明 |
|---|---|---|
| 搜索并插入 | `(at-library:insert)` | 关键字 → 序号 → 插入点 |
| 按目录浏览 | `(at-library:browse)` | 输目录 → 序号 → 插入点 |
| 按目录浏览（目录已知） | `(at-library:browse-dir "电气/元件库")` | 目录内挑选 |
| 插入标准图框 | `(at-library:frame "A2" "横" pt)` | 图库「图框库」，左下角对齐 |
| 生成图例 | `(at-library:legend pt)` | 扫当前布局用到的块 → 符号 + 名称 |
| 标准件明细表 | `(at-library:bom pt)` | 名称/规格/标准号/数量 |
| 更新图库索引 | `(at-library:update)` | 强制刷新本地索引 |
| 图库体检/统计 | `(at-library:stats)` | 符号数 / 索引时间 |
| 本地图库：列出目录 | `(at-library:local-list)` | 扫描一个目录（含子目录）的 dwg，显示相对路径 |
| 本地图库：按名称插入 | `(at-library:local-insert)` | 关键字 → 序号 → 插入点，dwg 直接作块插入 |
| 幻灯片库名称 | `(at-library:slb-list)` | 选 `.slb`，列出库内全部幻灯片名称 |
| 本地图库：多库浏览插入 | `(at-library:local-browse)` | 跨多个根目录浏览 → 多选 → 给一个基点批量插入 |
| 本地图库：设置根目录 | `(at-library:local-set-roots)` | 配置多个根目录（`;` 分隔），写回配置 |

## AI 调用（无交互）

| 函数 | 说明 |
|---|---|
| `(at-library:insert-by-name "银杏" '(0 0 0))` | 关键字取第一条命中的符号插入到指定点，返回块参照 ename，失败 nil |
| `(library:search "门" 5 nil)` | 检索图库，返回 alist（id/name/size/units/…） |
| `(at-library:ai-local-scan "D:/lib" 50)` | 扫描本地图库目录，返回条目表（id/name/file/rel） |
| `(at-library:ai-local-scan '("D:/libA" "D:/libB") 50)` | 多根扫描，条目额外带 `root` 字段 |
| `(at-library:ai-local-search "银杏" 5 "D:/lib")` | 本地图库检索，不打印、不交互 |
| `(at-library:ai-local-search "银杏" 10 "D:/libA;D:/libB")` | 跨库检索（也可传字符串表），按 rel 去重 |
| `(at-library:ai-local-insert "银杏" '(0 0) "D:/lib" 1.0 0.0)` | 按名称或 dwg 全路径插入，返回块参照 ename |
| `(at-library:ai-slb-names "D:/lib/old.slb")` | 读取幻灯片库名称列表，不打开图形 |

## 本地文件夹图库

不依赖在线索引：把一个或多个目录（含子目录）当作图库，目录结构即分类，
`.dwg` 文件名即符号名——**库就是文件夹**，不需要导入或制作 SLD。

### 多根目录

配置项 `at-library:local-roots` 存多个根目录，用 `;` 分隔（也可以用
`(at-library:local-set-roots)` 交互设置）：

```lisp
;; 直接改配置
(@:set-config 'at-library:local-roots "D:/图库/门窗;D:/图库/家具;E:/标准件")

;; 或交互设置（会写回配置文件）
(at-library:local-set-roots)
```

只配置一个目录时行为与旧版一致；`*at-library:local-root*` 仍作为兼容的
「上次使用的单目录」保留。

### 浏览与多选插入

```lisp
;; 跨所有根目录浏览，多选后给一个基点，自动从左到右排开
(at-library:local-browse)

;; 旧的单目录命令保持可用
(at-library:local-list)
(at-library:local-insert)

;; 程序化调用
(at-library:local-scan-multi '("D:/libA" "D:/libB") 6)
(at-library:local-search-multi "门" 10 '("D:/libA" "D:/libB"))
(at-library:ai-local-search "乔木" 10 '("D:/libA" "D:/libB"))
(at-library:ai-local-insert "银杏" '(0 0) "D:/libA;D:/libB" 1.0 0.0)

;; 多选后的批量插入（不经过对话框，基点和间距自己给）
(at-library:local-insert-items
  (at-library:local-search-multi "门" 10 '("D:/libA" "D:/libB")) '(0 0) 5.0)
```

说明：

- 递归深度默认 6 层，跳过非 `.dwg` 文件。
- 多根扫描时条目多一个 `"root"` 字段（以 `/` 结尾的根目录），`rel` 仍是相对该根
  的路径；相同 `rel` 只保留第一个根目录中的文件，避免重复列出。
- 多根检索跨库去重，排序仍是「名称前缀命中优先，再按包含匹配」。
- `local-browse` 的列表显示为「根目录别名 | 相对路径」，别名取根目录最后一段
  （重名时加 `(2)`、`(3)`），最多列 50 条；需要 `FILEDIA=1` 才能弹出多选对话框
  （批量/脚本环境请直接用 `local-insert-items`）。
- 插入走 `block:insert`，dwg 直接作为块定义引入当前图，不复制文件。
- 不同子目录下的同名 dwg 会复用同名块定义（CAD 块表按名称唯一），
  需要区分时请让文件名唯一。

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
