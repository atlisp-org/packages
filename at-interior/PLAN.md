# at-interior 功能扩展计划

来源：iMini V8.7.1 装修类命令（`docs/imini-reproduce-plan.md`「at-interior（装修设计）」节）。
状态：⬜ 未开始 / 🔄 进行中 / ✅ 完成

## 已实现 ✅

| iMini 命令 | 本包函数 | 文件 | 状态 |
|---|---|---|---|
| mini_closet | `at-interior:closet` / `ai-closet` | furniture.lsp | ✅ |
| mini_ggpm / gzlm / gzpm | `at-interior:cabinet` / `ai-cabinet`（平面） | furniture.lsp | ✅ |
| （柜体立面） | `at-interior:cabinet-elev` / `ai-cabinet-elev` | furniture.lsp | ✅ |
| （柜体剖面） | `at-interior:cabinet-sec` / `ai-cabinet-sec` | furniture.lsp | ✅ |
| mini_curtain | `at-interior:curtain` / `ai-curtain` | furniture.lsp | ✅ |
| mini_blgd_wall | `at-interior:glass-wall` / `ai-glass-wall` | furniture.lsp | ✅ |
| mini_elev_interior | `at-interior:elev-frame` / `ai-elev-frame` | ceiling.lsp | ✅ |
| mini_jianxian_plan | `at-interior:jianxian-plan` / `ai-jianxian-plan` | ceiling.lsp | ✅ |
| mini_jianxian_elev | `at-interior:jianxian-elev` / `ai-jianxian-elev` | ceiling.lsp | ✅ |
| mini_gypsumsec_cell | `at-interior:gypsum-sec` / `ai-gypsum-sec` | ceiling.lsp | ✅ |
| mini_elec_line | `at-interior:elec-line` / `ai-elec-line` | ceiling.lsp | ✅ |
| mini_light_plan | `at-interior:light-plan` / `ai-light-plan` | ceiling.lsp | ✅ |
| mini_woodsec / _floor、mini_mugongban、mini_jiaban | `at-interior:sec-wood` / `ai-sec-wood`（wood/ply/solid） | sec.lsp | ✅ |
| mini_glasssec | `at-interior:sec-glass` / `ai-sec-glass` | sec.lsp | ✅ |
| mini_stonesec | `at-interior:sec-stone` / `ai-sec-stone` | sec.lsp | ✅ |
| mini_steel（钢板）、mini_xg（型钢部分） | `at-interior:sec-steel` / `ai-sec-steel`（plate/i/h） | sec.lsp | ✅ |
| mini_jggk | `at-interior:sec-steel-frame` / `ai-sec-steel-frame` | sec.lsp | ✅ |
| mini_stonesec_wall | `at-interior:sec-stone-wall` / `ai-sec-stone-wall` | sec.lsp | ✅ |
| mini_weld_line | `at-interior:weld` / `ai-weld` | detail.lsp | ✅ |
| mini_flex | `at-interior:flex` / `ai-flex` | detail.lsp | ✅ |
| mini_tube | `at-interior:tube` / `ai-tube` | detail.lsp | ✅ |
| mini_hbsk | `at-interior:back-holes` / `ai-back-holes` | detail.lsp | ✅ |

共 22 个命令（22 交互入口 + 22 个 `ai-*` 无交互入口），5 个源文件 + 公共工具文件。

## 修复记录（2026-10-31 收尾时）

| 问题 | 修复 | 级别 |
|---|---|---|
| 依赖加载用 `(fboundp ...)`，ZWCAD 24.1s 无 `fboundp`，包加载即报错 | 改 `(if (null <fn>) (require 'ns:*))`（仓库通用写法） | P0 |
| 22 个交互入口用 `startundo`/`endundo`，ZWCAD 24.1s 无此二函数 | 统一改 `(command "_.UNDO" "Begin")` / `(command "_.UNDO" "End")` | P0 |
| `weld` 交互入口把 getkword 中文（角焊缝/对接焊缝）直接传给 `ai-weld`（期望 `fillet`/`butt`），且回车缺省误画对接焊缝 | 交互函数内映射中文→内部代号（同 sec-wood/sec-steel 模式） | P0 |
| `ai-elev-frame` 对闭合折线：末顶点重复首顶点 → 零长边生成退化多段线；`side=1`（内收）实际未生效 | 去除重复末顶点；side=1 按质心外侧取反侧 | P1 |
| `mi-light-symbol` 对空串块名（交互回车）直接 `tblobj ""` | 加 `(not (= blk ""))` 守卫 | P2 |

## CAD 实测记录

- 2026-10-31，ZWCAD 24.1s (LMS Tech) / MCP（192.168.1.53）：
  - `(startundo)` → `no function definition: STARTUNDO`（确认缺失）
  - `(fboundp ...)` → `no function definition: FBOUNDP`（确认缺失）
  - `vl-load-com` → T；`getvar "ACADVER"` → `24.1s (LMS Tech)`
  - ⚠️ 实测探针（一次无限 `entnext` 循环）导致该 CAD 主机离线（192.168.1.53
    100% 丢包），主机恢复后需补验：`UNDO Begin/End` 闭环、22 个 `ai-*` 冒烟。
- 静态验证：6 文件 `atlisp-lint` 0 错误；`--format-check` 通过；
  `check-lisp-pitfalls.py` 无可疑写法。

## 未实现（backlog）

| 优先级 | iMini 命令 | 项目 | 说明 |
|---|---|---|---|
| P1 | mini_zxmb | 装修面板 `ai-panel` | 参数化装修面板（柜体+背板+收口组合） |
| P1 | cdpm | 层板抽屉剖面 | 抽屉导轨/层板剖切大样 |
| P1 | mini_gypsumsec_wall | 轻钢龙骨板墙剖面 | 竖龙骨+双面石膏板+隔音棉剖面（mini_gypsumsec_cell 已覆盖吊顶侧） |
| P2 | mini_xg | 型钢截面面板 `ai-sec-steel-panel` | 型钢库选择面板（常用 H/I/板 一键出图） |
| P2 | mini_gdgpm | 高低柜 | 高低柜组合平面/剖面 |
| P2 | mini_mfpm / mfdb | 门窗 | 门/窗平面与剖面符号 |
| P2 | mini_znmf | 门窗（组合） | 与 mfpm 合并实现 |
| P2 | mqblpm | 幕墙玻璃剖面 | 立柱+横梁+玻璃+胶缝 |
| P2 | cbj | 车边镜 | 柜侧车边镜/拉手剖切符号 |
| P2 | wire_cross | 线槽截面 | 线槽/桥架剖面符号 |
| P2 | ygpx | 圆管剖断线 | 圆管剖切后的断线表达 |

## 已知限制

- `ai-elec-line` 开关符号固定朝 +X 方向，非水平布线时朝向不自动适配（P2）。
- `ai-glass-wall` 的 `door-pos` 未做 0~1 范围校验，越界时门洞位置异常但不报错（P2）。
- `ai-elev-frame` 依赖 `entget` 的 10 组码取顶点，弧形段多的曲线按弦处理（P2）。
- 灯具/标高符号为固定尺寸（圆 r50、r15、字高 10/20/25），未按比例缩放（P2）。
- `mi-*` 工具函数为本包私有；若被 ≥2 个包复用，按归属规则提取到 `lib/src/`（P2）。
