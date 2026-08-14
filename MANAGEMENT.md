# @lisp 应用包仓库 — 项目管理计划

## 1. 项目概述

本仓库（`packages/`）是 @lisp CAD 应用云系统的应用包集，与父仓库 `atlisp/atlisp` 为 git 子模块关系。当前包含 **73 个 AutoLISP 应用包**，运行于 @lisp 内核之上，兼容 AutoCAD 2006+、ZWCAD 2015+、GStarCAD 2015+、BricsCAD 的 VLISP 环境。

- **主远程**：`git@gitee.com:atlisp/packages.git`（gitee，唯一权威源）
- **镜像**：`git@github.com:atlisp-org/packages.git`（github，只作同步备份）
- **默认分支**：`main`
- **子模块**：`dev-tools`（@lisp 开发工具）、`at-pm`（工程管理）— 均为独立 git 仓库，本仓库只升级 submodule 指针

## 2. 目标与范围

### 2.1 目标
维护全部应用包的可安装、可运行、可发布状态，向 @lisp 应用云提供稳定包源。

### 2.2 范围
- 应用包源码（`.lsp`/`.dcl`/`.dwg` 等资源）的新增、修改、删除
- `pkg.lsp` 清单文件维护（名称/版本/依赖/文件清单）
- 菜单注册（`menus.lsp`/`menu.lsp`）
- 版本管理与发布同步

### 2.3 不在范围内
- 内核功能（`kernel/`）与函数库（`lib/`）
- 子模块（dev-tools / at-pm）内部代码 — 不在本仓库直接修改
- 服务器 / MCP / VSCode 等其余子项目

## 3. 组织与职责

| 角色 | 职责 |
|------|------|
| 仓库管理员（VitalGG） | 合并 PR、发布版本、核准包名与目录结构 |
| 包维护者 | 维护单个/多个应用包，保证 lint 通过、CAD 实测可用 |
| 贡献者 | 通过 PR 提交新包、Bug 修复、功能增强 |

## 4. 包结构与规范

每个包一个目录，目录名 = 标识名（`:NAME`）。**创建后不得改名。**

```
<pkg>/
  pkg.lsp        # 必需清单：(:NAME . "包名")、:VERSION、:REQUIRED、:FILES
  <pkg>.lsp      # 同名主源码（:FILES 中的无扩展名条目自动对应 .lsp）
  menus.lsp      # 可选，@:add-menu 注册菜单
  其他资源       # 资源文件：dwg/dcl 等（在 :FILES 中带扩展名显式声明）
```

**硬性约束：**
- `:NAME` 必须与目录名完全一致
- 源码必须 **UTF-8** 编码（GB2312/ANSI 会导致 CAD 括号匹配错乱）
- 依赖用 `:REQUIRED "base"` 声明，绝大多数包依赖 `base`
- 保留包文件已有的 `@:def-pkg` / `@::def-pkg` 风格，不混用改造
- 不抄袭 `at-elec`、`at-layout`、`prefabricated-building` 的显示名文案（其存在 GBK 二次转码乱码，是历史错误）

## 5. 开发流程

### 5.1 新增/修改一个包

1. **创建/编辑源码**，遵循 UTF-8，保持包内既有命名风格
2. **编写/更新 `pkg.lsp`**，确认 `:NAME` 与目录一致、`@:def-pkg` 风格与现有相符
3. **静态检查**（必须）：
   ```bash
   node ../atlisp-lint/bin/atlisp-lint --file <pkg>/<file>.lsp
   # 跨文件分析：--project；自动修复：--fix；生产规范校验：--format-check
   ```
   错误级规则命中时退出码为 1；已知问题用 `; atlisp-lint: disable=规则名` 内联忽略
4. **CAD 实测**（新功能/逻辑改动必做）：在 CAD 中 `@I <包名>` 安装或执行 `(@:package-install "包名")`，再调用包函数验证。参考 `base/test.lsp` 的 `push-var`/`pop-var` 测试写法

### 5.2 引入新资源/新功能
- 新增资源文件（dwg/dcl 等）必须在 `:FILES` 中显式列出
- 新增菜单在 `menus.lsp` 中用 `@:add-menu` / `@:add-menus` 注册嵌套列表菜单

## 6. 版本控制与提交规范

### 6.1 分支与提交
- 所有提交直接走 `main`（共享集中式仓库，PR 由管理员合并）
- 提交信息前缀习惯：`sync sub`（子模块同步）/ `feat:`（新功能）/ `fix:`（修复）/ `update`（更新）

### 6.2 子模块维护
- `dev-tools/`、`at-pm/` 内部改动在**各自仓库**完成并发布，再在本仓库**升级 submodule 指针**（提交信息建议 `sync sub`）
- **禁止**直接修改子模块内的代码
- 本仓库 `.gitignore` 已忽略编译产物（`.fas`/`.zelx`/`*-whole.lsp`/`*.shx`/`lib/`），不得手动提交

## 7. 版本管理与发布

| 阶段 | 动作 |
|------|------|
| 开发 | 修改源码，更新 `:VERSION`（语义化递增） |
| 校验 | 静态 lint 全绿 + CAD 实测通过 |
| 发布 | 提交并推送 gitee `main` 分支；`@::publish-package`（在 CAD 内 dev-tools 中执行） |
| 同步 | github 镜像由 gitee 自动/定期同步 |

**发布入口**：包编译/发布工具在 CAD 内的 `dev-tools` 包里（`@@@` 开发面板、`@::compile-package` 编译、`@::publish-package` 发布）。

## 8. 质量保障

- **无 CI、无统一测试框架**，验证 = 静态 lint + CAD 实测
- 每次改动至少保证：受影响包 lint 无错误级规则、依赖关系正确、可被 `@:package-install` 正常安装
- 修改公共依赖（`base` 等）需回归所有依赖方

## 9. 沟通与协作

- 贡献流程参照 `git-使用说明.org`（clone → 修改 → commit → push gitee → 管理员 PR/合并）
- 新包提交后由管理员核准包名与目录结构后再并入 `main`
- 文档与规范以仓库内 `AGENTS.md` 为权威（本文件是其管理维度的补充）

## 10. 里程碑（当前）

| 事项 | 状态 |
|------|------|
| 73 包存量维护 | 进行中 |
| dev-tools/at-pm 子模块编码修复 | 已完成（历史提交 `2d4bf6a` 等） |
| AGENTS.md 开发规范落地 | 已完成（提交 `e7ef5a0`） |
| 持续包新增与发布 | 进行中 |