# 明经论坛 Playwright 详情补抓（2026-11-01）

- 详情：30 帖成功（原 HTTP 轮次 30 帖计划，2 帖被挑战/权限拦截）
- 方式：Playwright Chromium，真实浏览器执行 JS 挑战；串行 8 秒间隔
- 补抓：178244、35156 已拿到；113010 需阅读权限，使用用户提供的登录 cookie 后拿到
- 输出：reports/forum-browser-20261101.json / .md

## 统计

- 总正文：135,598 字符
- 含 defun 帖子：8 帖
- defun 总数：59
- 代码块：16

## 代码量最高的帖子

| tid | 版块 | 标题 | 字符 | defun | 代码块 | 链接 |
|---|---|---|---:|---:|---:|---|
| 101879 | AutoLISP/Visual LISP 编程技术 | ExCel表格->AutoCAD表格(源码公布) | 80939 | 23 | 2 | https://bbs.mjtd.com/thread-101879-1-1.html |
| 100616 | AutoLISP/Visual LISP 编程技术 | 【Gu_xl】Lisp程序设计错误处理的技巧 | 15041 | 16 | 0 | https://bbs.mjtd.com/thread-100616-1-1.html |
| 82692 | AutoLISP/Visual LISP 编程技术 | 【Gu_xl】基于方位角计算的拓扑多边形自动构建快速算法 | 3857 | 9 | 2 | https://bbs.mjtd.com/thread-82692-1-1.html |
| 15283 | 源码共享 | [原创]对象均布与对齐工具 | 1555 | 4 | 2 | https://bbs.mjtd.com/thread-15283-1-1.html |
| 106891 | AutoLISP/Visual LISP 编程技术 | 【局部放大】源码公布 | 4935 | 2 | 2 | https://bbs.mjtd.com/thread-106891-1-1.html |
| 22137 | OpenDCL与LISP扩展库 | ObjectDCL学习笔记(作者：李松) | 3078 | 2 | 0 | https://bbs.mjtd.com/thread-22137-1-1.html |
| 85342 | AutoLISP/Visual LISP 编程技术 | 【Gu_xl】原创实用工具：Block 自动转为Lisp源代码 | 1409 | 2 | 2 | https://bbs.mjtd.com/thread-85342-1-1.html |
| 171203 | AutoLISP/Visual LISP 编程技术 | 【源码】将单行/多行文字合并 | 1902 | 1 | 0 | https://bbs.mjtd.com/thread-171203-1-1.html |
| 6098 | 数据库/EXCEL/objectDBX/RealDWG 编程技术 | [VBA]利用ObjectDbx技术在当前图形中插入其它未打开的图形中的图块 | 3822 | 0 | 6 | https://bbs.mjtd.com/thread-6098-1-1.html |
| 113010 | AutoLISP/Visual LISP 编程技术 | XLRX_API for CAD2004~2024函数库 2022.11.20更新高版本支持！！！ | 3005 | 0 | 0 | https://bbs.mjtd.com/thread-113010-1-1.html |
| 35156 | AutoLISP/Visual LISP 编程技术 | AutoCAD二次开发安装程序制作(2019-11-11) | 2789 | 0 | 0 | https://bbs.mjtd.com/thread-35156-1-1.html |
| 178244 | AutoCAD工具插件 | 在秋枫发布的源码基础上修改的批量打印程序v3.6.1（2026-07-25更新） | 2359 | 0 | 0 | https://bbs.mjtd.com/thread-178244-1-1.html |
| 61313 | AutoCAD工具插件 | 【经典】4.51极速版特性工具，新增公差、动态填充及线型、铁路功能，太多了无法列举 | 2335 | 0 | 0 | https://bbs.mjtd.com/thread-61313-1-1.html |
| 193128 | AutoCAD工具插件 | [永久免费]自动输入法【AutoCAD/中望CAD/浩辰CAD】 【2026/9/14更新，版本 2.1.8】 | 1763 | 0 | 0 | https://bbs.mjtd.com/thread-193128-1-1.html |
| 180587 | AutoLISP/Visual LISP 编程技术 | Pycad来袭，你还在等什么 | 1573 | 0 | 0 | https://bbs.mjtd.com/thread-180587-1-1.html |

## 代码可复用性判断

- **101879 ExCel表格->AutoCAD表格(源码公布)**：完整 Excel2CAD 源码（2013），23 个 defun；含按页面设置分页、合并单元格、按层/块取色。当前 at-stat 已覆盖导入+合并，分页/取色可作后续增强。
- **100616 【Gu_xl】Lisp程序设计错误处理的技巧**：Gu_xl 错误处理长文，16 个 defun；重点是可复用 *error* 保存/恢复图层、系统变量、临时实体的模板。
- **82692 【Gu_xl】基于方位角计算的拓扑多边形自动构建快速算法**：拓扑多边形算法，9 个 defun，但核心源码被“发帖数>20”隐藏，只能看到用时统计/进度条等辅助函数。
- **15283 [原创]对象均布与对齐工具**：对象均布与对齐，4 个 defun；作者明确不公开完整源码，正文只是 AutoVBALoad 适配片段。
- **106891 【局部放大】源码公布**：局部放大完整主程序，2 个 defun；依赖大量 gxl-* 私有函数，不能直接移植，但交互流程可参考。
- **22137 ObjectDCL学习笔记(作者：李松)**：ObjectDCL 学习笔记，2 个 defun；偏教程，不是可直接复用的业务代码。
- **85342 【Gu_xl】原创实用工具：Block 自动转为Lisp源代码**：Block→Lisp 生成器，2 个 defun；核心是 VLX，正文只有 gxl-Blk-Check 片段。
- **171203 【源码】将单行/多行文字合并**：文字合并，1 个 defun；at-text 已覆盖同类能力。

## 与上一轮的关系

- 上一轮 HTTP 抓取：1222 主题 / 25 帖详情，2 帖被挑战拦截；
- 本轮 Playwright：30 帖详情，挑战帖已补齐，113010 需登录权限；
- 没有出现新的需求主题；变化主要是更多老帖正文可读，确认论坛代码仍以 Gu_xl/秋枫等作者的私有函数库为主，直接复用价值有限。

## 建议

1. `at-stat` Excel 导入可补“按 Excel 页面设置分页 / 表格颜色随层或随块”，对应 101879。
2. 把 `*error*` 保存/恢复模板做成 lint 规则或代码片段，对应 100616。
3. 拓扑多边形核心源码仍不可见，暂不据此实现。
4. 局部放大、Block→Lisp 仍是明确缺口，但需要重写而非复制。
