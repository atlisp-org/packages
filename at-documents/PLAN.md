# at-documents 功能扩展计划

> @lisp开发文档。通用函数库文档查看器（DCL 分页列表 + 分类筛选 + 搜索 + 详情/源码）与文档生成器（libdoc → openai 问答机器人 CSV）。v0.6.41，自研，依赖 dev-tools。

## 1. 概览
- 版本 0.6.41 / 分类 DevelopTools / 依赖 dev-tools / 自研
- 规模：at-documents.lsp 325 行 17 个 defun（6 顶层 + 11 局部）；`:FILES` 声明 22 个 `.libdoc` 资源
- **P0 现状问题：`:FILES` 列出的 22 个 `.libdoc` 文件仓库中全部缺失**（目录仅有 at-documents.lsp/fun.desc/pkg.lsp/README.md），打包后文档库为空，管理器无数据可显示
- 运行依赖 `packages/base/base-whole-dev.lsp`（找不到则 `@:down-pkg-file` 下载）+ dev-tools 的 `dev:gen-doc`/`fun:src-code`
- 路径全部硬编码 `packages\base\` / `packages\at-documents\`（反斜杠，Windows）

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| at-documents:load-libdoc | at-documents.lsp:14 | 读 `packages\at-documents\*.libdoc` 到 `@:*libdoc*`（每行一个 S 表达式）+ 分类表 |
| at-documents:lib-manager-dialog | :29 | DCL 文档管理器：20 条/页分页、分类 popup、搜索框、Detail/Src 按钮（alert 弹窗）、运行时生成 dcl |
| at-documents:gen-libdoc | :190 | 对 `packages\base\lib-*.lsp` 调 `dev:gen-doc` 生成 `.libdoc`，清空文件 |
| gen-base-doc | :208 | 全局命令包装（无命名空间） |
| @doc:gen-openai | :210 | 由 libdoc 生成 `openai.csv`（函数库问答机器人技能表，按分类 + 逐函数两档） |
| @doc:gen-openai-pkg | :267 | 由包注册表（`@:*pkgs*`）生成 `openai-pkg.csv`（应用包问答表，先 `@:package-update`） |
| indent / page-up / page-down / show-detail / show-src / list-libdoc-by-category / list-libdoc-by-search / show-libdocs-list / init-dialog | :34-128 | 局部辅助（DCL 字符串缩进、分页、筛选、列表刷新） |

## 3. 新功能（NEW）
- **F1 文档覆盖 lib/src（P1）**：gen-libdoc 现只扫 `packages\base\lib-*.lsp`（旧布局）；补扫 `lib/src/<ns>/<fn>.lsp`（58 个命名空间），分类从目录名取，`lib-*.libdoc` 与新 `<ns>.libdoc` 并存过渡。
- **F2 文档缺失校验命令（P1）**：新增 `(at-documents:check-docs)`——对照 `:FILES` 的 `.libdoc` 与实际文件、对照 lib/src 命名空间，列出缺失项（本次 P0 问题即由此类工具兜底）。
- **F3 在线文档站（P2）**：openai.csv 之外，libdoc → Markdown 站点（每分类一页 + 搜索），与 atlisp.cn 的 package-info 联动。
- **F4 文档内跳转/复制（P2）**：Detail 弹窗补"复制用法"按钮；Src 弹窗改为 DCL text 区可滚动（alert 不可滚动，长源码被截断）。
- **F5 作者/标签筛选（P2）**：DCL 中 author 分支已有注释占位（:175-178）；补全（libdoc 格式需加作者字段）。
- **F6 使用频率统计（P2）**：记录函数被 Detail/Src 查看次数，输出高频函数表，指导文档质量排序。

## 4. 改进（IMPROVE）
- **I1 .libdoc 缺失（P0）**：22 个 `.libdoc` 未入库（:FILES 已声明）→ 在 CAD 内跑 gen-libdoc 生成并提交，或临时从 :FILES 移除（否则 `@I` 安装/打包报错或空库）。
- **I2 `(exit)` 终止运行时（P0）**：`new_dialog` 失败时 `(exit)`（:170）直接退出 AutoLISP 会话，改 `(princ)`。
- **I3 路径硬编码（P1）**：`packages\base\`/`packages\at-documents\` 反斜杠 + 相对 `@:*prefix*`（:10/:17/:192-206）——要求 CAD 搜索路径直指仓库根；改配置项 + `/` 反斜杠兼容（`vl-filename-as-typed` 或统一 `\\`）。
- **I4 依赖未声明（P1）**：`fun:src-code`/`dev:gen-doc` 来自 dev-tools，`:REQUIRED "dev-tools"` 已声明 ✓；但 `@doc:gen-openai-pkg` 调 `@:package-update`（at-pm 包）未声明依赖，缺 at-pm 时报错；加守卫或声明。
- **I5 基础库加载方式（P2）**：`(if (findfile "packages/base/base-whole-dev.lsp") (load ...) (@:down-pkg-file ...))`（:10-13）——`base-whole-dev.lsp` 是编译产物（.gitignore 忽略 `*-whole.lsp`），仓库内不存在，每次首启走下载；且 load 全局污染。建议改 `@:require-pkg` 式声明。
- **I6 全局命令（P2）**：`gen-base-doc` 无命名空间前缀（:208）→ `at-documents:gen-base-doc`。
- **I7 分页边界（P2）**：`(/ (1- (length libdocs-list)) 20)`（:122/:124）在 0 条时得 0 正常，但 20 整除边界（如 40 条）页码显示 "1/2" 正确、末页 next 禁用判断对 40 条误禁用（(39/20)=1 = 末页索引 1，正确）——需补 0/1/20/21/40/41 六组用例实测。
- **I8 死代码（P2）**：show-src 内 12 行注释掉的 defun-q-list-ref 实现（:70-83）；author 相关注释块（:86/:175-180）。
- **I9 fun.desc 定位（P2）**：fun.desc（68KB 生成样例）不在 :FILES 且与 .libdoc 格式重复；确认留档价值后移入 docs/ 或删。

## 5. 重构（REFACTOR）
- **R1 DCL 生成与数据分离（P2）**：lib-manager-dialog 内 60 行 DCL 字符串拼接 → 独立 dcl 模板资源文件入 :FILES（或 opendcl 包模板化），代码只剩 set_tile 数据填充。
- **R2 libdoc 读写层（P2）**：load-libdoc 的 `read-line`+`read` 逐行解析 → 统一 libdoc 格式版本头（`;; libdoc v2`）+ 结构化字段（name/desc/usage/params/return/example/author），为 F3/F5 铺路。
- **R3 CSV 转义逻辑收敛（P2）**：gen-openai 与 gen-openai-pkg 两段几乎相同的转义/引号处理（:247-264 vs :305-323）→ 提取 `(at-documents::csv-escape str)`。
- **R4 前缀统一（P2）**：`@doc:` / `at-documents:` 双前缀 → `at-documents:` + 内部 `at-documents::`。

## 6. lib 协同
- 复用：`string:from-lst`/`from-list`/`subst-all`/`to-lst`、`ui:select`（F6 报表可用 `table:make`/`ui:input`）
- 建议提取：无新增候选（本包为工具包）；但 **gen-libdoc 的 `dev:gen-doc` 流程应同步覆盖 lib/src**，否则 at-documents 文档与 lib 实际函数持续脱节（与 B5 base、dev-tools 子模块联动）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| I1 libdoc 补齐 | P0 | 2h | 需 CAD 内生成 |
| I2 exit 修复 | P0 | 0.1h | — |
| F1 lib/src 覆盖 | P1 | 3h | 依赖 dev:gen-doc 适配 |
| F2 缺失校验 | P1 | 1h | — |
| I3 路径硬编码 | P1 | 1.5h | — |
| I4 at-pm 守卫 | P1 | 0.5h | — |
| F3 文档站 | P2 | 4h | — |
| F4 弹窗增强 | P2 | 2h | — |
| F5 作者筛选 | P2 | 1.5h | 依赖 R2 |
| F6 频率统计 | P2 | 1.5h | — |
| R1-R4 + I5-I9 | P2 | 5h | — |

合计 ≈22h
