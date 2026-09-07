# @lisp文本

@lisp常用文本操作工具。功能有文字对齐、加前后缀、多个单选文本的排版等。

## 功能特性

### 文本菜单

| 命令 | 说明 |
|------|------|
| 文本设置 | 打开文本相关配置对话框 |
| 文字排版 | 对多个乱序排列的单行文本左对齐，并按行距整齐排列 |
| 插入时间戳 | 在指定位置插入当前时间戳记 |
| 左对齐 | 调整单行文本左对齐（文本的位置不变） |
| 右对齐 | 调整单行文本右对齐（文本的位置不变） |
| 居中对齐 | 调整单行文本中对齐（文本的位置不变） |
| 属性变文本 | 将属性（ATTDEF）转化为单行文本 |
| 单行转多 | 将多个单行文字合并为多行文字（MTEXT） |
| 加前后缀 | 为选中的文本批量添加前缀和后缀 |
| 批量加序号 | 为选中文本按指定格式批量添加序号 |
| 点哪加哪 | 点选文本中的字符进行字母数字动态递增复制 |
| 绘线查找 | 选择一个文本，查找范围内相同内容的文本并连线 |
| 生成表格 | 将选中的文本按行列关系生成表格 |
| 格式数字 | 批量格式化文本中的数字（整数位、小数位、填充字符） |
| 文本转表格 | 批量将单行文本按规律分隔并绘制表格 |

### 文本2菜单

| 命令 | 说明 |
|------|------|
| 按行合并 | 合并同一行的多个单行文本为一个 |
| 文本加框 | 选择或输入文本，查找匹配文本并添加矩形或圆形框 |
| 删文本框 | 删除文本框（指定图层上的矩形或圆） |
| 重排序号 | 重新编排文字前面的序号，支持 `n.` 或 `n.n` 形式 |
| 定位重叠字 | 高亮显示有重叠的文字 |
| 文字避让 | 处理重叠的单行文本，自动避让（成功设绿色，不成功设黄色） |
| 删除重字 | 删除内容相同且重叠的文字 |
| 定位压线字 | 高亮显示压线的单行文本 |
| 翻译文本 | 翻译选中的单行或多行文本 |
| 英译中 | 翻译选中纯英文的文本至中文 |
| 移除样式 | 去除多行文本中的格式，仅保留换行 |
| 统一样式 | 统一设置文本和多行文本中的字体格式 |
| 朗读文本 | 依次朗读选中的文本内容 |

## 函数列表

| 函数 | 说明 |
|------|------|
| `@text:setup` | 打开文本配置对话框 |
| `@text:multi-text-align` | 多个乱序单行文本左对齐并按行距排列 |
| `@text:insert-time` | 插入时间戳记 |
| `@text:justify` | 调整单行文本对齐方式（位置不变） |
| `@text:justifytext-left` | 单行文本左对齐 |
| `@text:justifytext-right` | 单行文本右对齐 |
| `@text:justifytext-middle` | 单行文本居中对齐 |
| `@text:a2t` | 属性（ATTDEF）转化为单行文本 |
| `@text:to-mtext` | 单行文字合并为多行文字 |
| `@text:menu-add-prefix-or-suffix` | 菜单入口：批量添加前后缀 |
| `@text:add-prefix-suffix` | 批量添加前后缀（程序调用） |
| `@text:find-from-line` | 选择文本查找相同内容并连线 |
| `@text:menu-format-number` | 菜单入口：格式化文本中的数字 |
| `@text:text2table` | 将文本按行列关系生成表格 |
| `@text:menu-add-order` | 菜单入口：批量加序号对话框 |
| `@text:inc-word` | 点选文本中字符进行动态递增复制 |
| `@text:menu-speak` | 朗读选中的文本 |
| `@text:draw-box` | 为文本添加矩形或圆形框 |
| `@text:menu-draw-box` | 菜单入口：文本加框 |
| `@text:menu-remove-box` | 菜单入口：删除文本框 |
| `@text:translate` | 翻译选中的文本 |
| `@text:translate-from-en` | 英文翻译至中文 |
| `@text:locate-overlay-text` | 定位并高亮显示重叠文字 |
| `@text:delete-overlay-sametext` | 删除内容相同且重叠的文字 |
| `@text:handle-overlay-text` | 文字避让处理 |
| `@text:locate-overline-text` | 定位并高亮显示压线文字 |
| `@text:join-in-line` | 合并同一行的多个单行文本 |
| `@text:sort-serial-number` | 重新编排文本前的序号 |
| `@text:string-to-table` | 将文本按分隔符转换为表格 |
| `@text:remove-mtext-style` | 去除多行文本格式 |
| `@text:set-style` | 统一设置文本字体格式 |
| `@text:MText` | 创建多行文字对象（内部函数） |
| `@:set-fonts` | 设置文字样式字体 |

## 配置

| 配置项 | 默认值 | 说明 |
|--------|--------|------|
| `@text:fonts` | `tssdeng.shx,tssdchn.shx` | 文字样式 Standard 的字体文件 |
| `@text:color` | `10` | 临时绘线的颜色号，每次变化加以区别 |
| `@text:temp-layer` | `@temp@` | 临时绘线的图层 |
| `@text:order-prefix` | `""` | 批量加序号的文字前缀 |
| `@text:order-suffix` | `""` | 批量加序号的文字后缀 |
| `@text:order-startnum` | `1` | 批量加序号的起始序号 |
| `@text:target-lang` | `en` | 翻译目标语言（支持 en/zh/zht/jp/kor 等） |
| `@text:box-type` | `1` | 文本框类型：1 矩形，2 圆 |
| `@text:box-linewidth` | `0.5` | 框线宽度 |
| `@text:box-offset` | `1.0` | 框线偏移值 |
| `@text:box-color` | `1` | 框线颜色号 |
| `@text:box-layer` | `textbox` | 框线图层 |
| `@text:en-style` | `Standard` | 纯英文单行文本字体样式 |
| `@text:zh-style` | `HZ` | 中文单行文本字体样式 |
| `@text:en-font` | `Arial` | 英文字体 |
| `@text:zh-font` | `YouYuan` | 中文字体 |
| `@text:mtext-width` | `1.0` | 多行文本高宽比 |

## 依赖

- `base` 包

## 安装

在 CAD 命令行中执行：

```
@I at-text
```

或通过包管理器：

```
(@:package-install "at-text")
```

安装后会自动注册「文本」和「文本2」菜单，以及 `tts` 快捷键（朗读文本）。

## 源文件

| 文件 | 说明 |
|------|------|
| `at-text.lsp` | 核心功能：文本对齐、前后缀、属性转文本、单行转多行、绘线查找、格式化数字 |
| `inc-word.lsp` | 字母数字动态递增功能 |
| `table.lsp` | 文本生成表格功能 |
| `speak.lsp` | 文本朗读功能 |
| `add-order.lsp` | 批量添加序号功能 |
| `mtext.lsp` | 多行文本样式处理 |
| `menu.lsp` | 菜单注册和快捷键定义 |
| `string-to-table.lsp` | 文本按分隔符转表格功能 |
| `join-in-line.lsp` | 按行合并文本功能 |
| `sort-serial-number.lsp` | 序号重排序功能 |
| `handle-overlap-text.lsp` | 重叠文字检测、避让和删除 |
| `box.lsp` | 文本加框和删框功能 |
| `translate.lsp` | 文本翻译功能 |

## 作者

- **VitalGG** - vitalgg@gmail.com
- http://atlisp.cn

## 版本

1.1.6
