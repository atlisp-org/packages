# at-wsad - @lisp 给排水

建筑给排水专业通用管理系统，提供给排水说明、图例、平面图、系统图、详图等样例插入。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| @给排水 → 给排水说明 | `(@wsad:draw-readme)` | 插入给排水说明块 |
| @给排水 → 给排水图例 | `(@wsad:draw-legend-example)` | 插入给排水图例样例 |
| @给排水 → 给排水平面样例 | `(@wsad:draw-plan-example)` | 插入给排水平面图样例 |
| @给排水 → 给排水系统样例 | `(@wsad:draw-system-example)` | 插入给排水系统图样例 |
| @给排水 → 给排水详图样例 | `(@wsad:draw-detail-example)` | 插入给排水详图样例 |

## 函数

| 函数 | 说明 |
|------|------|
| `@wsad:draw-readme` | 插入 readme-wsad.dwg 块并炸开 |
| `@wsad:draw-plan-example` | 插入 example-wsad-plan.dwg 样例块 |
| `@wsad:draw-system-example` | 插入 example-wsad-system.dwg 样例块 |
| `@wsad:draw-legend-example` | 插入 example-wsad-legend.dwg 样例块 |
| `@wsad:draw-detail-example` | 插入 example-wsad-detail.dwg 样例块 |

## 资源文件

| 文件 | 说明 |
|------|------|
| `readme-wsad.dwg` | 给排水说明图块 |
| `example-wsad-plan.dwg` | 给排水平面图样例 |
| `example-wsad-system.dwg` | 给排水系统图样例 |
| `example-wsad-legend.dwg` | 给排水图例样例 |
| `example-wsad-detail.dwg` | 给排水详图样例 |

## 依赖

- `at-pm`

## 安装

在 CAD 命令行执行 `@I at-wsad` 或 `(@:package-install "at-wsad")`。

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-wsad.lsp` | 主程序源码 |
| `readme-wsad.dwg` | 给排水说明图块 |
| `example-wsad-plan.dwg` | 平面图样例 |
| `example-wsad-system.dwg` | 系统图样例 |
| `example-wsad-legend.dwg` | 图例样例 |
| `example-wsad-detail.dwg` | 详图样例 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.0.3
- 分类: 给排水
- 网站: http://atlisp.cn
