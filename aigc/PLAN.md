# aigc 功能扩展计划

> AIGC 生成式 AI 测试版。基于 Ollama 本地大模型（aibot 模块 `@:aigc`）：选文本生成内容、图纸审查（未实现）、本地部署 Ollama、拉取模型。

## 1. 概览
- 版本 0.1.6 / 分类 Common / 依赖 base / 自研
- 规模：aigc.lsp 约 115 行 4 个公开函数 + 菜单（aigc.lsp:2）+ 占位 menus.lsp
- 模型清单：gemma3 / deepseek-r1 / qwen2.5 / qwen2.5-coder / opencoder / starcoder2 / codegemma 共 16 个

## 2. 现状功能清单
| 函数 | 位置 | 功能 |
|---|---|---|
| aigc:gen-content | aigc.lsp:29 | 点选 TEXT → `text:get-mtext`+`text:remove-fmt` 取需求 → 文字下方建 MTEXT 进度条 → `@:aigc` 生成 → `text:from-markdown` 渲染回填 |
| aigc:Local-deployment | :60 | 检测 ollama 进程 → 检测 OllamaSetup.exe → 无则 PowerShell 下载到 userprofile → startapp 安装 |
| aigc:pull-models | :83 | `ui:select-multi` 多选模型 → 逐个 `ollama pull`；Ollama 未运行时引导部署 |
| aigc:check-drawing | :101 | **桩函数**：仅提示"正在开发中" |
| aigc:*models* | :10 | 全局模型列表（与 pull-models 内局部 list 重复） |

## 3. 新功能（NEW）
- **F1 图纸审查落地（P0，核心卖点）**：`aigc:check-drawing` 实现——收集图纸文本（图名/说明/表头）+ 图框标题栏属性 + 块/图元统计摘要，拼结构化 prompt（"引用标准是否过期、图纸深度、漏项检查"），LLM 返回 markdown → 生成审查报告 MTEXT/表格。接口 `(aigc:check-drawing / scope)`，实现要点：`ssget "x" '((0 . "*TEXT,ATTDEF,ACAD_TABLE"))` 采样 + `stat:*` 汇总 + `@:aigc`。
- **F2 图纸摘要/说明生成（P1）**：`aigc:describe` —— 对选中图块/区域生成设计说明文字（工程量、图元构成）。
- **F3 批量生成（P1）**：`aigc:gen-batch` —— 选多个文本逐个生成，结果统一排布（复用 at-text 排版思路）。
- **F4 模型管理面板（P2）**：`aigc:models` —— `ollama list/ps` 状态查询、当前模型切换（config 存储）、显存/速度提示。
- **F5 提示词模板（P2）**：按任务（审查/摘要/翻译）存系统提示词模板到 config，`@:aigc` 调用时拼接。
- **F6 结果历史（P2）**：生成结果按时间存包内 JSON（`json:*`），支持查看/重新插入。

## 4. 改进（IMPROVE）
- **I1 生成过程反馈（P1）**：`@:aigc` 阻塞无进度；加超时（`timer:*`）与"可中断"说明；失败（模型未拉取/服务未起）给出可操作提示（"请先 aigc:pull-models 下载 qwen2.5:1.5b"）。
- **I2 结果位置冲突（P1）**：gen-content 固定放在文字正下方 1.5 倍字高，密集区域重叠；改为偏移查找空位或放到命令行+可选落图。
- **I3 Ollama 版本探测（P2）**：Local-deployment 只查安装文件 >1e9 字节，不查服务端口（11434）；加 `@::cmd "shell" "ollama --version"`。
- **I4 模型清单去重（P2）**：`aigc:*models*` 与 pull-models 内局部 list 两份维护，统一为 config 列表。

## 5. 重构（REFACTOR）
- **R1 删除占位菜单（P0）**：menus.lsp 注册 `aigc:hello`（未定义函数）×4，菜单点击即报错；删除整个 menus.lsp（aigc.lsp:2 已有真实菜单）。
- **R2 模型列表配置化（P1）**：`aigc:*models*` → `@:define-config 'aigc:models`，pull/gen 统一读取。
- **R3 错误处理（P1）**：gen-content 无 `*error*`，`@:aigc` 失败后进度 MTEXT 残留（仅 response 为 nil 时 entdel）；补 push-var/pop-var + 清理。

## 6. lib 协同
- 复用：`text:remove-fmt`/`text:from-markdown`/`text:get-mtext`、`json:*`（历史）、`timer:*`（超时）、`ui:select-multi`
- 候选提取：`aigc:Local-deployment` 的"下载 exe + 启动安装"模式与 acmecad2021/alt-run 的 `ext:launch` 合并设计（见跨包主题）

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 备注 |
|---|---|---|---|
| R1 删占位菜单 | P0 | 0.1h | 现会报错 |
| F1 图纸审查 | P0 | 4h | 测试版转正关键 |
| I1 生成反馈/超时 | P1 | 2h | — |
| I2 位置冲突 | P1 | 1h | — |
| R2/R3 | P1 | 1h | — |
| F2 摘要生成 | P1 | 2h | — |
| F3 批量 | P1 | 2h | — |
| F4/F5/F6 | P2 | 各 1.5-2h | — |
| I3/I4 | P2 | 0.5h | — |
