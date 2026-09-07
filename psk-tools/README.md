# psk-tools - 管道求解工具

管道求解工具，用以取代鸿业 ACS10.0 的暖通空调绘图插件。

## 菜单命令

通过 `menu` 文件注册菜单（具体菜单项由 PSK 软件定义）。

## 源文件

| 文件 | 说明 |
|------|------|
| `psk-tools.lsp` | 主程序入口 |
| `menu` | 菜单定义文件 |
| `psk.fas` | PSK 核心编译模块 |
| `functions.fas` | 函数库编译模块 |
| `psk-tools.zip` | 完整安装包 |
| `command.lsp` | 命令定义 |
| `parts.lsp` / `partset.lsp` / `pipe-parts.lsp` | 管道零件定义 |
| `dialogs.dcl` / `catelogExplor.dcl` / `propertyedit.dcl` | DCL 对话框 |
| `util.lsp` / `var.lsp` | 工具函数和变量 |
| `psk.cui` / `psk.cuix` / `psk.mns` / `psk.mnr` | AutoCAD 菜单文件 |
| `setup.vbs` | 安装脚本 |
| `prop.csv` | 属性数据 |

## 依赖

- `base`（隐含）

## 配置

首次运行会自动解压 `psk-tools.zip` 并写入注册表。

## 安装

在 CAD 命令行执行：
```
@I psk-tools
```

## 信息

- **作者**: vectra (vitalgg@foxmail.com)
- **版本**: 0.3.5
- **分类**: 暖通
- **开源**: 否
