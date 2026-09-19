# acmecad2021 功能扩展计划

> AcmeCAD2021 外部程序启动器。第三方工具封装（绿色破解版 Acme CAD Converter），LISP 层仅负责下载与启动。

## 1. 概览
- 版本 0.0.1 / 分类 APP / 依赖 base / 第三方署名（VitalGG 封装）
- 规模：1 个功能函数（acmecad2021.lsp 622B），无资源文件（运行时从 `archives/AcmeCAD2021.zip` 下载到 bin/）
- 能力：DWG/DXF/DWF 版本转换（R2.5–R2022）、转 PDF/BMP/JPEG/GIF/WMF/TIFF、水印、字体替换、批量转换、分层导出

## 2. 现状功能清单
| 函数 | 位置 | 功能 | 入口 |
|---|---|---|---|
| acmecad2021:open | acmecad2021.lsp:7 | 检查/下载 AcmeCAD2021.exe 后 `start-bg` 启动 | 菜单"外部程序>acmecad2021"（:5，菜单名 GBK 乱码） |

## 3. 新功能（NEW）
- **F1 参数化批量转换（P2）**：在 CAD 内选择文件夹/文件列表，拼 AcmeCAD2021 命令行参数（`-i in.dwg -o out.pdf` 类）实现"框选 dwg → 批量转 PDF"，不离开 CAD。接口：`(acmecad2021:convert src-list dst-fmt /dst-dir)`，实现要点：`file:glob` 收集 + `startapp` 带参启动 + 完成后提示。
- **F2 版本批量降级（P2）**：针对 xref 场景，扫描当前 dwg 的所有 xref 版本，提示/执行批量降级到当前 CAD 可打开的版本。
- **F3 转换结果落图（P3）**：转换出的图片/PDF 自动插入图纸指定位置（复用 ole 包思路）。

## 4. 改进（IMPROVE）
- **I1 菜单乱码（P0）**：acmecad2021.lsp:5 `(@:add-menu "澶栭儴绋嬪簭" ...)` 是"外部程序"的 GBK 二次转码乱码，改为正确的 `"外部程序"`（与 alt-run 一致）。
- **I2 下载失败反馈（P1）**：`@:down-and-unzip` 失败/断网时无提示，`findfile` 为 nil 时静默退出。补 `(@:prompt "下载失败...")`。
- **I3 启动前进程检测（P2）**：已启动时避免重复拉起（`sys:list-process-name` 判断 AcmeCAD2021.exe）。

## 5. 重构（REFACTOR）
- **R1 抽取外部程序启动器到 lib（P1）**：`findfile → @:down-and-unzip archives/X.zip bin → command "start-bg"` 模式在 acmecad2021 / alt-run / autohotkey / everything / pdftk / youyun-plot / doslib / opendcl ≥2 包复用。提取 `lib/src/ext/launch.lsp`：`(ext:launch 'app-name archive-relpath exe-relpath /args)`，各包 3 行调用。

## 6. lib 协同
- 候选：`ext:launch`（见 R1，跨 ≥8 个启动器包复用）。
- `file:glob` 已在 lib（F1 使用）。

## 7. 优先级与工作量
| 条目 | 优先级 | 预估 | 依赖 |
|---|---|---|---|
| I1 菜单乱码 | P0 | 0.1h | — |
| R1 ext:launch 提取 | P1 | 2h（含 8 包改造） | lib 提交 |
| I2 下载反馈 | P1 | 0.5h | — |
| F1 批量转换 | P2 | 3h | 需确认 AcmeCAD2021 支持 CLI 参数 |
| I3 进程检测 | P2 | 0.3h | — |
| F2/F3 | P3 | 各 2h | — |

> 第三方软件封装，功能扩展受 AcmeCAD2021 自身能力限制；P0/P1 均为 LISP 层小改。
