# AGENTS.md — @lisp packages（应用包仓库）

## 这是什么
- `atlisp/` 根 monorepo 的 git 子模块（本目录 `.git` 是 gitlink，实际 gitdir 在 `../.git/modules/packages`）。70+ 个 AutoLISP 应用包，运行在 @lisp 内核之上（AutoCAD 2006+ / ZWCAD 2015+ / GStarCAD 2015+ / BricsCAD 的 VLISP 环境）。
- 无 JS 构建、无 CI、无统一测试框架。验证 = 静态 lint + 在 CAD 中实测。
- 主远程为 `gitee`（`git@gitee.com:atlisp/packages.git`），`github` 是镜像，分支 `main`。提交信息习惯用 `sync sub` / `feat:` / `fix:` / `update` 前缀。

## 包结构
- 每包一个目录。`pkg.lsp` 是必需清单：`(@:def-pkg '((:NAME . "包名") ...))`。`:FILES` 中**无扩展名条目 = 同名 `.lsp` 源码**，带扩展名为资源文件（dwg/dcl/fas 等；资源文件是被 git 跟踪的，不在 .gitignore 内）。用 `:REQUIRED "base"` 声明依赖，绝大多数包依赖 `base`。
- 标识名（`:NAME`）必须与包目录名完全一致，创建后不得改名。
- `@:def-pkg` 与 `@::def-pkg` 两种风格并存（多数为 `@::def-pkg`，dev-tools/at-pm 也是）。改包时保持该文件已有风格，不要统一改造。
- `menus.lsp` / `menu.lsp`（可选）：加载时用 `@:add-menu` / `@:add-menus` 注册嵌套列表菜单。
- 源码必须 **UTF-8**。GB2312/ANSI 会让 CAD 括号匹配错乱。注意 `at-elec`、`at-layout`、`prefabricated-building` 的显示名是 GBK 二次转码乱码，不要照抄其 pkg.lsp 文案。
- `gis-tools/` 不是标准包（无 pkg.lsp，是 Python 工具集），不要按包流程处理。

## 验证（没有测试 runner）
- 单文件静态检查（bin 无扩展名）：
  ```
  node ../atlisp-lint/bin/atlisp-lint --file <pkg>/<file>.lsp
  ```
  常用旗标：`--src <目录>`、`--project`（跨文件分析）、`--staged`（仅暂存区）、`--sbcl`（SBCL 语法校验）、`--cache`、`--fix`（自动修尾空格/BOM/括号）、`--format-check`。错误级规则命中时退出码为 1。配置在 `atlisp-lint.json` / `.atlisp-lint.json`，内联忽略用 `; atlisp-lint: disable=规则名`。
- 整仓 / 单包批量校验：
  - 根仓库：`make lint`（检查 lib/src + packages）、`make packages-check`（检查每包 pkg.lsp）、`make lint-fix`
  - 本目录：`./validate-all.sh [包名]`（lint 包内全部 .lsp，有错误则退出码 1）
- CAD 实测（新功能/逻辑改动必做）：CAD 命令行 `@I <包名>` 安装，或执行 `(@:package-install "包名")` 后再调用包函数（也可走 atlisp-mcp 的 init_atlisp → at_command 流程）。`base/test.lsp` 里有 `push-var`/`pop-var` 保存恢复系统变量的测试写法可参考。

## 命名约定（2026-10 起）

iMini 复现工作产生的 `mini-*` 包已全部并入 `at-*` 包（`docs/imini-reproduce-plan.md`），
仓库中**不应再出现 `mini-*` 目录**。需要新增同类功能时，直接加到对应 `at-*` 包：
并入现有包沿用该包已有命名空间前缀（如 `@text:`、`@block:`、`@layout:`），
新建包才用 `<包名>:`。

## 方言限制（实测，写代码前必看）

本仓库的 AutoLISP 方言**不是** Common Lisp，以下写法会直接在 CAD 里报错：

- **没有 `let` / `let*`** —— 报 `no function definition`。局部绑定一律用 `setq` +
  函数形参/局部变量表（`(defun f (a / b c) ...)`）。
- **没有全局 `tan`** —— 需自备 `(defun ns:tan (x) (/ (sin x) (cos x)))`。
- 局部变量表**不能**绑定保护符号 `t` / `nil`（`(defun f (/ t) ...)` 直接报
  “要绑定的对象不正确: T”），也不要绑定内置函数名 `type`（动态作用域会让
  `entity:getdxf`、`(type x)` 等全部失效）。
- `entity:make-lwpline` 不存在，用 `entity:make-lwpolyline`。
- `(last '(1 2 3))` 在本运行时返回 `3` 而非 `(3)`，取末元素请用
  `(car (reverse lst))`。
- `entmakex` 的 dxf 列表里不能有 `nil` 元素（报 `DXF 组不正确: nil`），
  用 `(entmakex (vl-remove nil dxf))`。
- **`and` / `or` 只返回 `T` / `nil`**（AutoLISP 语义，不是 Common Lisp 的「返回最后一个值」）。
  需要取值的写法必须用 `if` 或 `(cond (测试 值) (t 默认))`，例如
  `(setq x (and a b))` 拿到的是 `T` 而不是 `b` 的值。
- `member` 只用于表；判断字符是否在字符串里用 `vl-string-search`。
- DIMENSION 的 11（文字位置）/ 70 组码用 `entmod` 常静默失效，改文字位置用
  `vla-put-TextPosition`；旋转标注（70=32）的 50 组码必须写在
  `AcDbAlignedDimension` 段内。

静态自查脚本（本机 `/tmp/opencode/check-lisp-pitfalls.py`，可复制进仓库复用）能扫出
上述问题。

## AI 调用约定（agent 通过 MCP eval_lisp 调用）

包代码不只服务交互用户，也会被 AI agent 直接调用。凡是有 agent 可能用到的能力，
请提供一个**无交互入口**：

- 命名：`<ns>:ai-<动作>`（首选，表述清楚）或沿用已有的 `<ns>:xxx-backend`；
  不要指望 agent 调 `C:` 命令或 `ui:dyndraw` / `grread` / `dcl` 那类入口——它们
  会等鼠标/键盘输入，MCP 调用会一直挂到超时。
- 参数：要画的点、图幅、比例、数量等全部走形参；缺省值在函数内补，不要 `getpoint`。
- 返回：成功返回实体/结果（ename、列表），失败返回 nil；**不要弹 `@::alert`
  模态框**，提示信息用 `princ` 输出即可。
- 依赖资源（DWG/DCL 等）先 `findfile` 检查，缺了就返回 nil 并 `princ` 原因，
  不要在后台偷偷下载 + `sleep`。
- 文档：函数 docstring 里写清「参数含义 + 示例调用」，并补一行到 README 的
  `## AI 调用` 表；`atlisp-lint` 0 error 是底线。
- 已经装好的包可以由 agent 自动安装（`install_package` → `@::package-install`），
  所以入口函数不要假设用户手工配置过什么；缺配置时给可用的默认值。

示例（at-pm）：

```lisp
(defun @pm:ai-insert-tukuang (mapsheet pt scale / ...)
  "AI 调用入口：插入标准图框，返回 ename，失败 nil。
示例: (@pm:ai-insert-tukuang \"A3\" '(0 0 0) 1.0)"
  ...)
```

## 提交与发布
- 包目录**不是**独立 git 仓库（仅 `dev-tools/`、`at-pm/` 是子模块）：直接在 packages 仓库内 commit/push。根 `AGENTS.md` 中"每个包是独立仓库 / 用 push-pkg.sh 提交"的说法已过时（push-pkg.sh 会对任意包报"不是git仓库"）。
- `.gitignore` 已忽略编译产物 `.fas`、`.zelx`、`*-whole.lsp`、`*.shx` 及 `lib/`，不要手动提交它们。
- `dev-tools/`、`at-pm/` 是独立 git 子模块：内部改动在子模块仓库完成，然后在本仓库升级 submodule 指针（提交信息 `sync sub`），不要直接改子模块里的代码。
- 包编译/发布工具在 CAD 内的 `dev-tools` 包里（`@@@` 开发面板、`@::compile-package` 编译、`@::publish-package` 发布）。

## 功能扩展计划
- 每包一份 `<pkg>/PLAN.md`（已知问题清单与扩展点，标 P0/P1/P2），总索引 `PLAN-INDEX.md`。改包前先看该包 PLAN.md，P0 项优先处理。

## 归属判定
- 新需求是否应作为应用包，判定规则见 `../docs/@lisp/库与包归属规则.md`。
- 满足以下任一条件归本包体系：完整交互流（选择→处理→确认→回滚）、带资源/外部依赖、注册命令菜单等全局状态、单一场景的领域逻辑。
- 拿不准默认归包；通用原子函数被 ≥2 个包复用时，提取到 `lib/src/<ns>/<fn>.lsp`。

## 参考
- 父仓库 `../AGENTS.md` 含全局规范（函数库命名 `lib/src/<ns>/<fn>.lsp` → `ns:fn`、各子项目构建命令、VSCode 扩展构建等）；其中与 packages 仓库实际 git 结构冲突的部分（包级独立仓库、push-pkg.sh）以上文"提交与发布"为准。
- `dev-tools/AGENTS.md` 是 dev-tools 包自身规范（编译 LISPSYS 编码坑等）。
