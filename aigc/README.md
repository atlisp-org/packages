# aigc - AIGC 生成式人工智能

根据描述的条件生成内容，集成 Ollama 本地大语言模型的 AIGC 工具（测试版）。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| AIGC > 生成内容 | `(aigc:gen-content)` | 选择文本，根据文本条件生成内容 |
| AIGC > 图纸审查 | `(aigc:check-drawing)` | 审查图纸是否合规（开发中） |
| AIGC > 本地部署 | `(aigc:Local-deployment)` | 部署 Ollama 到本地电脑 |
| AIGC > 下载模型 | `(aigc:pull-models)` | 下载 LLM 大语言模型 |

## 支持的模型

| 模型 | 说明 |
|------|------|
| gemma3:1b / gemma3:4b | Google Gemma 模型 |
| deepseek-r1:1.5b / deepseek-r1:7b | DeepSeek R1 模型 |
| qwen2.5:0.5b ~ 7b | 通义千问 2.5 模型 |
| qwen2.5-coder:0.5b ~ 7b | 通义千问代码模型 |
| opencoder:1.5b | OpenCode 模型 |
| starcoder2:3b / 7b | StarCoder2 模型 |
| codegemma:2b / 7b | Google 代码 Gemma 模型 |

## 源文件

| 文件 | 说明 |
|------|------|
| `aigc.lsp` | 主程序，包含 AIGC 生成、Ollama 部署、模型下载等功能 |

## 依赖

- `base`
- `aibot` 模块（生成内容时自动加载）

## 配置

无配置项。需要本地安装 Ollama 并运行。

## 安装

在 CAD 命令行执行：
```
@I aigc
```

## 使用流程

1. 运行 "本地部署" 下载安装 Ollama
2. 运行 "下载模型" 选择需要的模型
3. 在图纸中输入文本描述，运行 "生成内容"

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.1.6
- **分类**: Common
- **开源**: 否
