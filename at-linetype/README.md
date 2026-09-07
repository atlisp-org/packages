# at-linetype - @lisp Linetype

CAD 线型管理工具，加载、编辑、编译自定义线型和形文件。

## 菜单命令

| 菜单路径 | 命令 | 说明 |
|---------|------|------|
| linetype → 加载@lisp线型 | `(at-linetype:reload "@lisp")` | 加载 @lisp.lin 线型文件 |
| linetype → 加载设备线型 | `(at-linetype:reload "equip")` | 加载 equip.lin 设备线型 |
| linetype → 加载我的线型 | `(at-linetype:reload "user")` | 加载 user.lin 自定义线型 |
| linetype → 编辑我的线型 | `(at-linetype:edit)` | 用编辑器打开 user.lin 编辑 |
| linetype → 编辑形文件 | `(at-linetype:edit-shp)` | 编辑 @lisp.shp 形文件 |
| linetype → 编译形文件 | `(at-linetype:compile-shp)` | 编译 @lisp.shp → @lisp.shx 并加载 |
| linetype → 我要定制线型 | `(at-linetype:require)` | 打开浏览器联系开发者定制线型 |

## 函数

| 函数 | 说明 |
|------|------|
| `at-linetype:reload` | 重新加载指定的 .lin 线型文件 |
| `at-linetype:compile-shp` | 编译形文件并加载 |
| `at-linetype:load-shx` | 自动检测并加载 @lisp.shx（不存在则先编译） |
| `at-linetype:edit` | 编辑 user.lin 自定义线型文件 |
| `at-linetype:edit-shp` | 编辑 @lisp.shp 形文件 |
| `at-linetype:require` | 打开浏览器联系开发者 |

## 资源文件

| 文件 | 说明 |
|------|------|
| `@lisp.lin` | @lisp 标准线型定义 |
| `equip.lin` | 设备线型定义 |
| `@lisp.shp` | 线型使用的形文件源码 |

## 依赖

- `base`

## 安装

在 CAD 命令行执行：

```
@I at-linetype
```

或：

```
(@:package-install "at-linetype")
```

## 源文件

| 文件 | 说明 |
|------|------|
| `pkg.lsp` | 包清单定义 |
| `at-linetype.lsp` | 主程序源码 |
| `@lisp.lin` | 线型定义文件 |
| `equip.lin` | 设备线型定义文件 |
| `@lisp.shp` | 形文件源码 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- 版本: 0.1.9
- 分类: Common
- 网站: http://atlisp.cn
