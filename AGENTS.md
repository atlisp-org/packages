# AGENTS.md — @lisp packages（应用包仓库）

## 这是什么
- `atlisp/` 根 monorepo 的 git 子模块（本目录 `.git` 是 gitlink，实际 gitdir 在 `../.git/modules/packages`）。73 个 AutoLISP 应用包，运行在 @lisp 内核之上（AutoCAD 2006+ / ZWCAD 2015+ / GStarCAD 2015+ / BricsCAD 的 VLISP 环境）。
- 无 JS 构建、无 CI、无统一测试框架。验证 = 静态 lint + 在 CAD 中实测。
- 主远程为 `gitee`（`git@gitee.com:atlisp/packages.git`），`github` 是镜像，分支 `main`。提交信息习惯用 `sync sub` / `feat:` / `fix:` / `update` 前缀。

## 包结构
- `pkg.lsp` 是必需清单文件：`(@:def-pkg '((:NAME . "包名") ...))`。`:FILES` 中**无扩展名条目 = 同名 `.lsp` 源码**，带扩展名为资源文件（dwg/dcl/fas）。用 `:REQUIRED "base"` 声明依赖，绝大多数包依赖 `base`。
- 标识名（`:NAME`）必须与包目录名完全一致（dev-tools 文档明确），创建后不得改名。
- 两种风格并存：`@:def-pkg`（29 个包）与 `@::def-pkg`（43 个包）。改包时保持该文件已有风格。
- `menus.lsp` / `menu.lsp`（可选）：加载时用 `@:add-menu` / `@:add-menus` 注册嵌套列表菜单。
- 源码必须 **UTF-8**。GB2312/ANSI 会让 CAD 括号匹配错乱。注意 `at-elec`、`at-layout`、`prefabricated-building` 的显示名是 GBK 二次转码乱码，不要照抄其 pkg.lsp 文案。

## 验证（没有测试 runner）
- 静态检查（bin 无扩展名）：
  ```
  node ../atlisp-lint/bin/atlisp-lint --file <pkg>/<file>.lsp
  ```
  有用旗标：`--src <目录>`、`--project`（跨文件分析）、`--cache`、`--fix`（自动修尾空格/BOM/括号）、`--format-check`。错误级规则命中时退出码为 1。配置在 `atlisp-lint.json` / `.atlisp-lint.json`，内联忽略用 `; atlisp-lint: disable=规则名`。
- CAD 实测：在 CAD 命令行 `@I <包名>` 安装，或执行 `(@:package-install "包名")` 后再调用包函数（也可走 atlisp-mcp 的 init_atlisp → at_command 流程）。`base/test.lsp` 里有 `push-var`/`pop-var` 保存恢复系统变量的测试写法可参考。

## 提交与发布
- `.gitignore` 已忽略编译产物 `.fas`、`.zelx`、`*-whole.lsp`、`*.shx` 及 `lib/`，不要手动提交它们。
- `dev-tools/`、`at-pm/` 是独立 git 子模块：内部改动在子模块仓库完成并升级 submodule 指针，不要直接改子模块里的代码。
- 包编译/发布工具在 CAD 内的 `dev-tools` 包里（`@@@` 开发面板、`@::compile-package` 编译、`@::publish-package` 发布）。

## 归属判定

新需求是否应作为应用包，判定规则见 `../docs/@lisp/库与包归属规则.md`。
满足以下任一条件归本包体系：完整交互流（选择→处理→确认→回滚）、带资源/外部依赖、注册命令菜单等全局状态、单一场景的领域逻辑。
拿不准默认归包；通用原子函数被 ≥2 个包复用时，提取到 `lib/src/<ns>/<fn>.lsp`。

## 参考
- 父仓库 `../AGENTS.md` 含全局规范（函数库命名 `lib/src/<ns>/<fn>.lsp` → `ns:fn`、各子项目构建命令、VSCode 扩展构建等）。
- `dev-tools/AGENTS.md` 是 dev-tools 包自身规范（编译 LISPSYS 编码坑等）。