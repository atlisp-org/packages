# structure-tools - 结构计算工具集

5 个结构常用计算工具：小虎工具箱、STR、老董结构工具箱、结构计算工具箱、钢结构细部。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 结构计算工具 > 小虎工具箱 | `(stru:jiegou-tools "XiaoHu")` | 启动小虎工具箱 |
| 结构计算工具 > 结构计算STR | `(stru:jiegou-tools "STR")` | 启动 STR 结构计算 |
| 结构计算工具 > 老董结构 | `(stru:jiegou-tools "LaoDong")` | 启动老董结构工具箱 |
| 结构计算工具 > 结构计算 | `(stru:jiegou-tools "JieGou")` | 启动结构计算工具箱 |
| 结构计算工具 > 钢结构细部 | `(stru:jiegou-tools "steel")` | 启动钢结构细部设计 |

## 源文件

| 文件 | 说明 |
|------|------|
| `structure-tools.lsp` | 主程序，启动外部 EXE 工具 |

## 外部工具

| 文件 | 说明 |
|------|------|
| `XiaoHu-tools.exe` | 小虎工具箱第四版 |
| `STR-tools.exe` | 结构计算 STR |
| `LaoDong-tools.exe` | 老董结构工具箱 2.0 |
| `JieGou-tools.exe` | 结构计算工具箱 |
| `steel-tools.exe` | 钢结构细部设计 |

## 依赖

- `base`（隐含）

## 配置

无配置项。外部工具首次运行会自动下载。

## 安装

在 CAD 命令行执行：
```
@I structure-tools
```

## 信息

- **作者**: 网络收集
- **版本**: 1.0.2
- **分类**: 结构
- **开源**: 否
