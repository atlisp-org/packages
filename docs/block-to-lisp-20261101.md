# 块定义转 Lisp 源码（clean-room）— 2026-11-01

需求来源：明经论坛 `thread-85342`（Block 自动转为 Lisp 源代码）。该帖核心是 VLX，
作者未公开源码，因此本实现**没有反编译、没有复制其代码**，只按“把块定义重建为 Lisp”
这一行为重新实现。

## 新增

`at-block/embed.lsp`：

| 函数 | 说明 |
|---|---|
| `@block:ai-block->lisp blkname outfile` | AI 入口：块名/块参照 → 可重建的 Lisp 源码；outfile 为 nil 时返回源码字符串 |
| `@block:block->lisp` | 交互入口：选择块参照，保存 Lisp 源码 |

实现方式：

1. 读取块表记录和块内图元 DXF；
2. 去掉 `-1`/`5`/`330`/`360`/`-2`/`102`/XData 等不应进入 entmake 的组码；
3. 递归收集嵌套块，按依赖顺序生成 `at-block:embed-N`；
4. 生成 `(entmake '(...))`，最后 `at-block:embed-block` 按名称分发；
5. 不支持的图元跳过，并在返回值 `skipped` 中列出。

## CAD 实测（192.168.1.53 / AutoCAD 24.1s）

测试块：LINE + CIRCLE + TEXT + LWPOLYLINE + ATTDEF，另测嵌套 INSERT。

```lisp
(@block:ai-block->lisp "EMB-T8" "C:/Users/vitalgg/@lisp/emb8.lsp")
;; => (("file" . ...) ("blocks" . 1) ("skipped"))

;; 删除原块定义后加载生成文件并重建
(at-block:embed-block "EMB-T8")
;; => "EMB-T8"
```

结果：

- 重建后的块图元：`LINE CIRCLE TEXT ATTDEF`
- 属性：`(("TAG" . "VAL"))`，属性定义保留
- 嵌套测试：根块和嵌套块都重建成功，根块 INSERT 正常

## atlispcc 验证

生成源码转 UTF-8 后：

```bash
atlispcc validate embr8-utf8.lsp   # PASS
atlispcc compile  embr8-utf8.lsp   # 生成 FAS
atlispcc decompile embr8.fas       # 能反编译
```

但发现一个限制：**atlispcc 编译含 quoted DXF 点对表的 `entmake` 时，FAS 执行失败**，
反编译结果里部分 DXF 列表被还原成字符串（如 `(entmake "0.0")`）。因此本包发布仍使用
CAD 自带 `vlisp-compile`；atlispcc 只用于源码语法校验和反编译观察。

## 限制

- 目标图需已有块内使用的图层、线型、文字样式。
- 动态块、剪裁块、复杂标注（DIMENSION/MLEADER 等）可能丢失特性。
- 图元类型不在白名单时会被跳过并返回 `skipped`。
- 生成文件按 CAD 系统 ANSI（中文 Windows 为 GBK）写出；跨 UTF-8 环境需转换。
