# at-session - @lisp 会话管理

管理当前或历史打开的多个 DWG 文档，支持保存/恢复会话、批量打开关闭。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| 会话管理 → 上班 | `(at-session:goto-work)` | 打开最近保存的"下班"会话 |
| 会话管理 → 恢复会话 | `(at-session:open)` | 打开最近保存的会话 |
| 会话管理 → 历史会话 | `(at-session:history)` | 从历史会话列表中选择并恢复 |
| 会话管理 → 保存会话 | `(at-session:save-current)` | 保存当前打开的所有 DWG 为一个会话 |
| 会话管理 → 关闭会话 | `(at-session:close)` | 关闭最近一次会话记录的所有 DWG |
| 会话管理 → 下班 | `(at-session:knock-off)` | 保存当前会话为"下班"并关闭所有 DWG，退出 CAD |
| 会话管理 → 关所有dwg | `(at-session:save-and-close-all)` | 保存并关闭所有已打开的 DWG |
| 会话管理 → 打开目录 | `(at-session:open-directory-dwgs)` | 打开选定文件夹下的所有 DWG 文件 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-session:read` | 从配置文件读取所有会话记录 |
| `at-session:write` | 将会话记录写入配置文件 |
| `at-session:goto-work` | 恢复"下班"时保存的会话 |
| `at-session:open` | 恢复最近一次会话 |
| `at-session:history` | 弹出列表选择历史会话恢复 |
| `at-session:save-current` | 保存当前打开的 DWG 列表为新会话 |
| `at-session:close` | 关闭最近会话记录的 DWG |
| `at-session:save-and-close-all` | 保存并关闭所有 DWG |
| `at-session:knock-off` | 保存"下班"会话并退出 CAD |
| `at-session:open-directory-dwgs` | 批量打开文件夹内所有 DWG |

## 典型工作流

1. **上班**：执行 `上班`，自动打开上次"下班"时的 DWG 文件
2. **工作中**：随时 `保存会话` 记录当前工作状态
3. **下班**：执行 `下班`，自动保存会话并关闭所有 DWG，退出 CAD

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-session
```

或：

```
(@:package-install "at-session")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-session.lsp` | 主程序源码（会话读写、打开关闭逻辑） |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.1.8
- 分类: Efficiency tools
- 网站: http://atlisp.cn
