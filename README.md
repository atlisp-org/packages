# @lisp包应用集合

#### 介绍
@lisp 应用包。 多用户分发集合，用于同步中心服务器。

@lisp（客户端）是一个运行于 AutoCAD、中望CAD、浩辰CAD及类似兼容的CAD系统中的应用管理器。用于管理 AutoLisp 程序的网络下载安装、加载、卸载、查询等。可方便地实现 autolisp 程序的云管理。

@lisp 提供上百种应用包。包括建筑、结构、给排水、暖通、空调、公用设备、机械、测绘、工程等多种行业的迅捷应用。以及CAD常用操作，如图层、颜色、图块、文字、数学运算等增加功能。还有设计协同、工程管理等项目管理辅助应用。可以极大地提高设计者的工作效率。更多内容请访问 @lisp应用包 。

@lisp 在 某些输入 @ 有其它意义的系统中用 atlisp 代替。

#### 安装

将以下代码复制到 CAD 命令行内，回车即可开始安装 @lisp kernel。@lisp kernel（内核）包含 @lisp函数库 及 @lisp应用云 的基本管理功能。

(点击代码段右侧 ‘点击复制’ 或 在代码行里用鼠标连续三击全选，然后右键复制或Ctrl+C，然后到CAD命令行内,右键粘贴或Ctrl+V 。)
```lisp
(progn(vl-load-com)(setq o"http://atlisp.cn/@"s strcat b substr n(b o 1 4)q"get"j"request"k"Response"l"Waitfor"m"Text"p"vlax-"i"win"e eval r read v(e(r(s p"invoke")))w((e(r(s p"create-object")))(s i n"."i n j".5.1")))(v w'open q o :vlax-true)(v w'send)(v w(r(s l k))1000)(e(r((e(r(s p q)))w(r(s k m))))))
```



#### 参与贡献

1.  Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request


#### 特技

1.  使用 Readme\_XXX.md 来支持不同的语言，例如 Readme\_en.md, Readme\_zh.md
2.  Gitee 官方博客 [blog.gitee.com](https://blog.gitee.com)
3.  你可以 [https://gitee.com/explore](https://gitee.com/explore) 这个地址来了解 Gitee 上的优秀开源项目
4.  [GVP](https://gitee.com/gvp) 全称是 Gitee 最有价值开源项目，是综合评定出的优秀开源项目
5.  Gitee 官方提供的使用手册 [https://gitee.com/help](https://gitee.com/help)
6.  Gitee 封面人物是一档用来展示 Gitee 会员风采的栏目 [https://gitee.com/gitee-stars/](https://gitee.com/gitee-stars/)
