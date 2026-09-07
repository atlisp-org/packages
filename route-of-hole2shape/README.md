# route-of-hole2shape - 铣削路径

绘制圆孔到边线的刀路，以及圆自身的刀路。

## 菜单命令

| 菜单 | 命令 | 说明 |
|------|------|------|
| 孔到边刀路 > 设置 | `(route-of-hole2shape:config)` | 打开配置对话框 |
| 孔到边刀路 > 手动刀路 | `(route-of-hole2shape:menu-route)` | 手动选择圆和多段线生成刀路 |
| 孔到边刀路 > 自动刀路 | `(route-of-hole2shape:auto)` | 自动识别形状和孔生成刀路 |
| 孔到边刀路 > 圆变PL线 | `(route-of-hole2shape:c2pl)` | 将圆转换为多段线 |
| 孔到边刀路 > 删除刀路 | `(route-of-hole2shape:remove-route)` | 删除刀路线 |
| 孔到边刀路 > PL圆分色 | `(route-of-hole2shape:bianbie)` | 按类型给多段线和圆分色 |

## 配置项

| 配置名 | 默认值 | 说明 |
|--------|--------|------|
| `route-of-hole2shape:offset` | 3.0 | 刀路偏移量 (正为外偏, 负为内偏) |
| `route-of-hole2shape:layer` | BBB | 刀路线所在图层 |
| `route-of-hole2shape:color` | 200 | 新建刀路线颜色 |
| `route-of-hole2shape:c2pl-vertex` | 4 | 圆转多段线的顶点数 (>=2) |

## 源文件

| 文件 | 说明 |
|------|------|
| `route-of-hole2shape.lsp` | 主程序，包含刀路计算和生成功能 |

## 依赖

- `base`

## 使用说明

1. 绘制闭合多段线 (形状) 和内部圆 (孔)
2. 运行 "自动刀路" 或 "手动刀路"
3. 选择顺时针/逆时针方向
4. 设置偏移量
5. 程序自动计算并生成刀路线

## 信息

- **作者**: VitalGG (vitalgg@gmail.com)
- **版本**: 0.0.19
- **分类**: 定向开发
- **开源**: 否
