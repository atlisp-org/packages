# iMini 功能复现计划

> 来源：`/media/data/Public/raw/iMini/`（iMini V8.7.1，2026.10.01，410 个自定义命令）
> 目标：将 iMini 提供的功能在本仓库（packages/）中复现——已有包补充，无合适包则新建。
> 状态：⬜ 未开始 / 🔄 进行中 / ✅ 完成

## 目标包清单（全部为 at- 前缀）

### 并入现有包

| 目标包 | 领域 | iMini 命令数 | 优先级 | 命名空间 | 状态 |
|---|---|---|---|---|---|
| at-arch | 建筑绘图（轴网/墙/柱/梁/门窗/楼梯/标高/索引符号） | ~70 | P0 | `at-arch:` | 🔄 |
| at-dim | 标注索引（标注系列/多重引线/索引系列） | ~30 | P0 | `at-dim:` | 🔄 |
| at-text | 文字（词库/编辑/排版/字体/转换） | ~26 | P0 | `@text:` | 🔄 |
| at-layout | 布局视口（布局改名合并/视口比例/隔离） | ~20 | P1 | `@layout:` | 🔄 |
| at-block | 图块 Xref（块管理/替换/布块/Xref 处理） | ~13 | P1 | `@block:` | 🔄 |
| at-stat | 统计表格（文字/块/面积/长度/计算） | ~14 | P1 | `at-stat:` | 🔄 |
| at-linetype | 线型（改线型/树/形字体） | ~10 | P1 | `at-linetype:` | 🔄 |

### 新建包

| 目标包 | 领域 | iMini 命令数 | 优先级 | 命名空间 | 状态 |
|---|---|---|---|---|---|
| at-edit | 全能编辑（偏移/修剪/对齐/缩放/移动/复制/多段线/布尔） | ~60 | P0 | `at-edit:` | 🔄 |
| at-hatch | 填充（快捷填充/超级填充/分离/裁切） | ~15 | P0 | `at-hatch:` | 🔄 |
| at-interior | 装修设计（柜类/天花/灯具/管线） | ~35 | P1 | `at-interior:` | 🔄 |
| at-landscape | 景观（苗木/铺装/路线） | ~10 | P1 | `at-landscape:` | 🔄 |

> 命名约定：并入现有包时沿用该包已有命名空间前缀（如 at-text 用 `@text:`、at-block 用 `@block:`）；
> 新建包用 `<包名>:`。

## 现有包补充

| 现有包 | 补充命令 | 说明 |
|---|---|---|
| at-layer | 图层状态保存/恢复/管理、隐藏实体、独立显示、锁定/解锁实体、清理$层、层名大小写、按颜色分层、图层提取器 | 图层增强 |
| at-select | 选择易 ss、链式选择 chain_Sel、触碰选择 fastsel、获得选择集 getsel | 选择增强 |
| at-purge | 一键清理 purge、清理双$符号、清理短线点空字、删除常规词典 | 清理增强 |
| at-color | 全能变色 yjbh | 批量改色 |
| at-pm | 插入图框 tkk、全能文件导入合并 merge_dwgs、打开文件夹 open_folder | 图档管理 |
| at-plot | 快速截图 create_image/create_image_x、批量打印目录拆图 tzml | 打印截图 |
| ole | 图片转 OLE image2ole、裁剪 OLE 图像 oleimage_crop | OLE |
| fonts | 字形快速设置 style_any/hz/khz、字体库清单（缺失字库替换 fix_fonts/use_myfonts 归 at-text） | 字体 |
| ocr | 碎线转字/OCR 识别 mini_ocr | OCR |
| composing | 全能材料排版 unest（7 种算法） | 排版 |
| at-math | 全能文字计算面板 cale_txt | 计算 |
| at-number | 全能编号 xuhao | 编号 |
| at-structure | 结构专业命令（钢筋计算/桩基/模板，iMini 收录 38 个结构插件） | 结构 |

## 命令明细映射

### at-arch（建筑绘图）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_axis_line | 直线轴网 | at-arch:ai-axis-line |
| mini_axis_arc | 圆弧轴网 | at-arch:ai-axis-arc |
| mini_axis_text | 轴线编号 | at-arch:ai-axis-text |
| mini_axis_out | 轴线号外偏 | at-arch:ai-axis-out |
| mini_axis_update | 修改轴线编号 | at-arch:ai-axis-update |
| mini_wall_2_axis | 墙生轴线 | at-arch:ai-wall2axis |
| mini_beam_2_axis | 梁生轴线 | at-arch:ai-beam2axis |
| mini_wall | 画墙 | at-arch:ai-wall |
| mini_wall_width | 批量修改墙宽 | at-arch:ai-wall-width |
| mini_line_to_wall | 单线变墙 | at-arch:ai-line2wall |
| mini_Fix_Wall | 智能修补墙柱门窗 | at-arch:ai-fix-wall |
| mini_erase_wall | 删除墙柱门窗 | at-arch:ai-erase-wall |
| mini_wall_cover | 墙体完成面/保温层线 | at-arch:ai-wall-cover |
| mini_wall_erase_mark | 标注拆墙 | at-arch:ai-erase-wall-mark |
| mini_fill_column | 填充柱或墙 | at-arch:ai-fill-column |
| mini_c_column | +型柱 | at-arch:ai-column (c) |
| mini_l_column | L型柱 | at-arch:ai-column (l) |
| mini_t_column | T型柱 | at-arch:ai-column (t) |
| mini_O_column | O型柱 | at-arch:ai-column (o) |
| mini_r_column | 方柱 | at-arch:ai-column (r) |
| mini_axis_column | 轴线布柱 | at-arch:ai-axis-column |
| mini_pline_to_beam | 处理PKPM柱子 | at-arch:ai-pline2beam |
| mini_pline_to_column | 多段线/块/填充变柱 | at-arch:ai-pline2column |
| mini_beam | 画梁 | at-arch:ai-beam |
| mini_line_to_beam | 单线变梁 | at-arch:ai-line2beam |
| mini_fix_beam | 智能修补梁柱 | at-arch:ai-fix-beam |
| mini_foundation_beam | 筏板梁倒角 | at-arch:ai-foundation-beam |
| mini_beam_width | 批量修改梁宽 | at-arch:ai-beam-width |
| mini_hole_door | 墙上开门 | at-arch:ai-hole-door |
| mini_door | 两点画门 | at-arch:ai-door |
| mini_door_line | 门坎线 | at-arch:ai-door-line |
| mini_hole_win | 墙上开参数门窗 | at-arch:ai-hole-win |
| mini_win | 两点画参数门窗 | at-arch:ai-win |
| mini_coner_win | 墙上开转角窗 | at-arch:ai-corner-win |
| mini_modify_win | 修改门窗 | at-arch:ai-modify-win |
| mini_match_win | 刷门窗样式 | at-arch:ai-match-win |
| mini_win_width | 修改门窗宽度 | at-arch:ai-win-width |
| mini_win_direction1 | 内外翻转门窗 | at-arch:ai-win-flip-nn |
| mini_win_direction2 | 左右翻转门窗 | at-arch:ai-win-flip-lr |
| mini_win_text | 门窗编号 | at-arch:ai-win-text |
| mini_win_list | 绘制门窗表 | at-arch:ai-win-list |
| mini_attribon | 门窗编号开关 | at-arch:ai-win-text-onoff |
| mini_winelev | 窗立面 | at-arch:ai-winelev |
| mini_elev_arcwin | 圆弧立面窗 | at-arch:ai-elev-arcwin |
| mini_winelev2 | 幕墙立面 | at-arch:ai-curtain-elev |
| mini_dtwg | 动态网格 | at-arch:ai-dynamic-grid |
| mini_kqx | 门窗立面开启线 | at-arch:ai-door-win-kqx |
| mini_mqblpm | 幕墙玻璃剖面 | at-arch:ai-curtain-sec |
| mini_door_elev | 门立面 | at-arch:ai-door-elev |
| mini_win_text_elev | 由编号生成门窗边框 | at-arch:ai-win-frame-from-text |
| mini_stair_plan | 普通楼梯平面 | at-arch:ai-stair-plan |
| mini_arc_stair | 圆弧楼梯平面 | at-arch:ai-arc-stair |
| mini_auto_stair | 自动扶梯平面 | at-arch:ai-auto-stair |
| mini_elevator | 电梯轿厢平面 | at-arch:ai-elevator |
| mini_stair_section | 楼梯剖面 | at-arch:ai-stair-section |
| mini_analyzeview | 剖面视线分析 | at-arch:ai-analyze-view |
| mini_bg | 建筑标高 | at-arch:ai-elev |
| mini_ztbg | 总图标高 | at-arch:ai-sheet-elev |
| mini_gcbg | 高程标高 | at-arch:ai-elevation |
| mini_3dzb | 三维坐标 | at-arch:ai-3d-coord |
| mini_zb | 建筑坐标 | at-arch:ai-arch-coord |
| mini_dline | 简易双线 | at-arch:ai-dline |
| mini_line_to_dline | 单线变双线 | at-arch:ai-line2dline |
| mini_middle_line | 两根线的中间线 | at-arch:ai-middle-line |
| mini_break_line | 折断线 | at-arch:ai-break-line |
| mini_dot_line | 点引线 | at-arch:ai-dot-line |
| mini_arrow_line~7 | 箭头线系列 | at-arch:ai-arrow-line |
| mini_arrow_trace | 流线箭头 | at-arch:ai-flow-arrow |
| mini_indexnum1 | 大样索引 | at-arch:ai-index-num |
| mini_indexnum2 | 剖切索引 | at-arch:ai-index-sec |
| mini_indexnum3 | 大样编号 | at-arch:ai-index-big |
| mini_index_sec1 | 大剖面符号 | at-arch:ai-sec-symbol |
| mini_index_sec2 | 小剖面符号 | at-arch:ai-sec-symbol |
| mini_index_mirror | 对称符号 | at-arch:ai-mirror-symbol |
| mini_index_north | 指北针 | at-arch:ai-north |
| mini_draw_Hole | 洞口符号 | at-arch:ai-hole-symbol |
| mini_ygpx | 圆管剖断线 | at-arch:ai-pipe-break |
| mini_tamped_soil | 素土夯实 | at-arch:ai-soil-tamped |
| mini_nature_soil | 自然土壤 | at-arch:ai-soil-nature |
| mini_air_brick | 空心砖墙 | at-arch:ai-sec-airbrick |
| mini_face_brick | 面砖 | at-arch:ai-sec-facebrick |
| mini_batting_fill | 保温材料 | at-arch:ai-sec-batting |
| mini_cement_mortarsec | 水泥砂浆截面 | at-arch:ai-sec-mortar |
| mini_Reinforced_Consec | 钢筋混凝土截面 | at-arch:ai-sec-reinforced |
| mini_cement_Consec | 素混凝土截面 | at-arch:ai-sec-concrete |
| mini_hip_roof | 四坡顶平面 | at-arch:ai-hip-roof |
| mini_build_high | 建筑高度 | at-arch:ai-build-height |
| mini_build_shadow | 阴影分析 | at-arch:ai-build-shadow |

### at-edit（全能编辑）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_exscale | 全能缩放 | at-edit:ai-exscale |
| mini_exerase | 删除增强 | at-edit:ai-exerase |
| mini_exmirror | 镜像增强 | at-edit:ai-exmirror |
| mini_exmove | 移动增强 | at-edit:ai-exmove |
| mini_excopy | 从图块中复制图元 | at-edit:ai-excopy |
| mini_exmove_d / _m | 定距/定点移动 | at-edit:ai-exmove-d / -m |
| mini_excopy_d / _m | 定距/定点复制 | at-edit:ai-excopy-d / -m |
| mini_mstretch | 多窗口拉伸 | at-edit:ai-mstretch |
| mini_rotate_center | 批量中心旋转 | at-edit:ai-rotate-center |
| mini_dy_offset | 单选动态全能偏移 | at-edit:ai-dy-offset |
| mini_MOffset | 多选多重偏移 | at-edit:ai-multi-offset |
| mini_moffset | 多重偏移 | at-edit:ai-moffset |
| mini_extrim | 全能修剪 | at-edit:ai-extrim |
| mini_break_inter | 交点断开 | at-edit:ai-break-inter |
| mini_break_at | 连续点打断 | at-edit:ai-break-at |
| mini_break_line | 打断线 | at-edit:ai-break-line |
| mini_break_erase | 框选打断删短线 | at-edit:ai-break-erase |
| mini_extendtopoint | 延长/修剪到两点连线 | at-edit:ai-extend2point |
| mini_sxcj | 批量双线裁剪 | at-edit:ai-dline-clip |
| mini_fix_line | 框选修补单线 | at-edit:ai-fix-line |
| mini_multimerge | 合并图层/字样/线型 | at-edit:ai-multimerge |
| mini_line_union | 合并重线 | at-edit:ai-line-union |
| mini_line_clean | 清理短线点空字 | at-edit:ai-line-clean |
| mini_mfillet | 两组平行线倒角 | at-edit:ai-parallel-fillet |
| mini_add_vertex | 增加顶点 | at-edit:ai-add-vertex |
| mini_delete_vertex | 删除顶点 | at-edit:ai-delete-vertex |
| mini_lw_arc | 线弧转换 | at-edit:ai-lw-arc |
| mini_pljoin | 连接多段线 | at-edit:ai-pljoin |
| mini_pwidth | 宽度批量修改 | at-edit:ai-pwidth |
| mini_pline_batch | 扩展批处理多义线 | at-edit:ai-pline-batch |
| mini_patrial_stretch | 局部动态拉伸 | at-edit:ai-partial-stretch |
| mini_patrial_offset | 局部动态偏移 | at-edit:ai-partial-offset |
| mini_patrial_ortho | 局部正交 | at-edit:ai-partial-ortho |
| mini_boundary | 智能外轮廓 | at-edit:ai-boundary |
| mini_bo_inter | 内轮廓内偏移 | at-edit:ai-inner-offset |
| mini_path | 最短路径描边 | at-edit:ai-shortest-path |
| mini_pline_booler | 多段线/填充布尔 | at-edit:ai-pline-bool |
| mini_pline_divide | 按面积分封闭多段线 | at-edit:ai-pline-divide |
| mini_curve_2_pline | 曲线转多段线 | at-edit:ai-curve2pline |
| mini_divide_sp | 等弦长均分曲线 | at-edit:ai-curve-divide-sp |
| mini_curve_div | 加线等分曲线 | at-edit:ai-curve-div |
| mini_region_2_pline | 面域转多段线 | at-edit:ai-region2pline |
| mini_circle_2_donut | 圆变圆环 | at-edit:ai-circle2donut |
| mini_donut_2_circle | 圆环转圆 | at-edit:ai-donut2circle |
| mini_mpolyout | 带宽度多段线变线框 | at-edit:ai-polyout |
| mini_chk_open | 检查闭合画圆 | at-edit:ai-chk-open |
| mini_trans_reg | 四边形变形 | at-edit:ai-trans-reg |
| mini_CopyAlongCurve | 沿路径无缝复制 | at-edit:ai-copy-along-curve |
| mini_freehand | 手绘抖动效果 | at-edit:ai-freehand |
| mini_quick_block | 快速作块 | at-edit:ai-quick-block |
| mini_exp_mblk | 全能炸块 | at-edit:ai-explode-block |
| mini_exptoblay | 炸开到块层 | at-edit:ai-explode-to-layer |
| mini_BlockWipeouts | 给块添加遮罩 | at-edit:ai-block-wipeout |
| mini_blk2lay0 | 块内实体到0层 | at-edit:ai-blk2lay0 |
| mini_group_creat / explode | 快速建组/炸组 | at-edit:ai-group-* |
| mini_wipeout / erase_wipeout | 边界生成/删除遮罩 | at-edit:ai-wipeout-* |
| mini_wipeout_Frames | 遮罩外框开关 | at-edit:ai-wipeout-frames |
| mini_3p_rectang | 全能矩形 | at-edit:ai-rectang |

### at-hatch（填充）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_hatch | 快捷填充 | at-hatch:ai-hatch |
| mini_hatch_panel | 快捷填充面板 | at-hatch:ai-hatch-panel |
| mini_hatch_select | 选现有图案填充 | at-hatch:ai-hatch-select |
| mini_hatch_key | 定义填充快捷命令 | at-hatch:ai-hatch-key |
| mini_custom_pattern | 自定义填充 | at-hatch:ai-custom-pattern |
| mini_superhatch | 超级填充 | at-hatch:ai-superhatch |
| mini_hatch_move | 填充移动对齐旋转缩放 | at-hatch:ai-hatch-move |
| mini_hatch_out | 填充图案提取输出 | at-hatch:ai-hatch-out |
| mini_hatchb | 由填充生成边界 | at-hatch:ai-hatch-boundary |
| mini_hatch_isolate | 批量分离填充 | at-hatch:ai-hatch-isolate |
| mini_xclip | 全能裁刀裁填充 | at-hatch:ai-clip |
| mini_solid_2_hatch | SOLID变填充 | at-hatch:ai-solid2hatch |
| mini_pline_to_solid | SOLID满铺多义线 | at-hatch:ai-pline2solid |
| mini_fix_pattern | 修复填充不显示 | at-hatch:ai-fix-pattern |

### at-dim（标注索引）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_dim_devide | 全能标注 | at-dim:ai-dim-devide |
| mini_dim_merge | 合并标注 | at-dim:ai-dim-merge |
| mini_dim_total | 标注总尺寸 | at-dim:ai-dim-total |
| mini_dim_move | 标注文字移动 | at-dim:ai-dim-move |
| mini_dim_home | 标注文字归位 | at-dim:ai-dim-home |
| mini_dim_fix_text | 标注文字避让 | at-dim:ai-dim-fix-text |
| mini_dim_flip | 翻转标注门窗编号 | at-dim:ai-dim-flip |
| mini_dim_trim | 修剪延长线 | at-dim:ai-dim-trim |
| mini_dim_align | 对齐标注 | at-dim:ai-dim-align |
| mini_dim_check | 标注原值检查 | at-dim:ai-dim-check |
| mini_dimreassoc | 标注恢复原值 | at-dim:ai-dim-reassoc |
| mini_dim_update | 更新标注 | at-dim:ai-dim-update |
| mini_dim_win | 标注门窗轴线 | at-dim:ai-dim-win |
| mini_dimpoly | 标注柱子 | at-dim:ai-dim-poly |
| mini_dim_room | 点标房间尺寸周长面积 | at-dim:ai-dim-room |
| mini_dim_line | 沿任意线标注 | at-dim:ai-dim-line |
| mini_dim_axis | 沿轴线标注 | at-dim:ai-dim-axis |
| mini_dim_wall | 沿墙柱轴线标注 | at-dim:ai-dim-wall |
| mini_qnsy | 全能索引 | at-dim:ai-qnsy |
| mini_mleader_align | 多重引线对齐 | at-dim:ai-mleader-align |
| mini_match_MLeader | 批量刷多重引线 | at-dim:ai-match-mleader |
| mini_mleader_mirror | 修复索引块镜像 | at-dim:ai-mleader-mirror |
| mini_leader_2_mleader | 转引线为多重引线 | at-dim:ai-leader2mleader |
| mini_mleader_list | 统计多重引线材料 | at-dim:ai-mleader-list |
| mini_bzmc | 标注块或填充名称 | at-dim:ai-bzmc |
| mini_index_elev | 立面索引 | at-dim:ai-index-elev |
| mini_sbpdx | 四边剖断线 | at-dim:ai-sbpdx |
| mini_indexer5/6/3 | 文字索引系列 | at-dim:ai-indexer-text |
| mini_indexer8 | 多层构造说明索引 | at-dim:ai-indexer-constr |
| mini_indexer2 | 椭圆块索引 | at-dim:ai-indexer-ellipse |
| mini_indexer9 | 带天花标高索引 | at-dim:ai-indexer-ceiling |
| mini_indexer4 | 节点索引 | at-dim:ai-indexer-node |
| mini_index_name | 标注图名 | at-dim:ai-index-name |

### at-text（文字）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_text_lib | 词库输入 | at-text:ai-text-lib |
| mini_xuhao | 全能编号 | at-text:ai-xuhao |
| mini_shell_text | 外部多行书写 | at-text:ai-shell-text |
| mini_sedit | 全能文字编辑 | at-text:ai-sedit |
| mini_exedit | 动态菜单编辑单选 | at-text:ai-exedit |
| mini_text_order | 文字重排版 | at-text:ai-text-order |
| mini_exp_text | 文字打断 | at-text:ai-exp-text |
| mini_join_text | 智能连接多行文字 | at-text:ai-join-text |
| mini_text2mtext | 一组文字变多重文字 | at-text:ai-text2mtext |
| mini_cht / mini_CHT | 文字批量更改/改变 | at-text:ai-cht |
| mini_tbox | 文字加外框并关联 | at-text:ai-tbox |
| mini_text_2_clipboard | 输出文字到粘贴板 | at-text:ai-text2clipboard |
| mini_find | 批量多文件查找替换 | at-text:ai-find |
| mini_fix_fonts | 自动替换缺失字库 | at-text:ai-fix-fonts |
| mini_use_myfonts | 全部字体用我的字库 | at-text:ai-use-myfonts |
| mini_style_any | 字形快速设置 | at-text:ai-style-any |
| mini_style_hz | HZ 字形设置 | at-text:ai-style-hz |
| mini_style_khz | KHZ 字形设置 | at-text:ai-style-khz |
| mini_text2pl | 文字变线 | at-text:ai-text2pl |
| mini_att2text | 块属性转文字 | at-text:ai-att2text |
| mini_text2att | 文字变块属性 | at-text:ai-text2att |
| mini_MatchAttribs | 刷块属性 | at-text:ai-match-att |
| mini_attrib_list | 块属性统计表 | at-text:ai-att-list |
| mini_trans | 中英翻译/简繁转码 | at-text:ai-trans |

### at-layout（布局视口）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_TabSort | 布局改名增减排序 | at-layout:ai-tab-sort |
| mini_layoutmerge | 布局合并 | at-layout:ai-layout-merge |
| mini_layout_isolate | 布局分图 | at-layout:ai-layout-isolate |
| mini_layout_rename | 布局选字重命名 | at-layout:ai-layout-rename |
| mini_jbqt | 局部取图 | at-layout:ai-jbqt |
| mini_regen | 快速局部重生成 | at-layout:ai-regen |
| mini_ps_scale | 图纸空间1:1布图 | at-layout:ai-ps-scale |
| mini_ms_scale | 模型空间做块布图 | at-layout:ai-ms-scale |
| mini_ms_exp | 还原炸开布图块 | at-layout:ai-ms-exp |
| mini_vpscale | 编辑视口比例 | at-layout:ai-vp-scale |
| mini_vpsync | 同步对齐视口 | at-layout:ai-vp-sync |
| mini_vp_outline | 模型空间绘视口外框 | at-layout:ai-vp-outline |
| mini_vplayers | 视口图层状态编辑 | at-layout:ai-vplayer |
| mini_vportlock | 视口锁定/解锁 | at-layout:ai-vport-lock |
| mini_vport_grey | 视口选层变灰 | at-layout:ai-vport-grey |
| mini_vport_ungrey | 还原视口颜色 | at-layout:ai-vport-ungrey |
| mini_vplayer_off/on/iso/match | 视口关闭/打开/分离/刷状态 | at-layout:ai-vplayer-* |

### at-block（图块 Xref）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_blocksee | 慧眼图库管理 | at-block:ai-block-see |
| mini_blocksee_set | 慧眼识块设置 | at-block:ai-block-see-set |
| mini_blk_rename | 图块重命名 | at-block:ai-blk-rename |
| mini_blk_Insertion | 修改块插入点 | at-block:ai-blk-insertion |
| mini_blk_replace | 替换图块或点 | at-block:ai-blk-replace |
| mini_blk_along_line | 沿路径布块 | at-block:ai-blk-along-line |
| mini_block_flipm | 多组批量翻转图块 | at-block:ai-block-flipm |
| mini_block_flips | 单组自由翻转实体 | at-block:ai-block-flips |
| mini_dksf | 多块同时基点缩放 | at-block:ai-multi-scale |
| mini_dtbl | 动态线型块填充比例 | at-block:ai-dtbl |
| mini_xdlj | 外部参照改相对路径 | at-block:ai-xref-repath |
| mini_xref2blk | 复制 Xref 为无名块 | at-block:ai-xref2blk |
| mini_batch_xref | 批量做 Xref 条件图 | at-block:ai-batch-xref |

### at-stat（统计表格）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_text2excel | 输出文字到 Excel | at-stat:ai-text2excel |
| mini_block_clist | 块统计表 | at-stat:ai-block-list |
| mini_text_list | 文字统计表 | at-stat:ai-text-list |
| mini_area_text | 面积标注(智能识别阳台) | at-stat:ai-area-text |
| mini_area_analyze | 标准层面积分摊表 | at-stat:ai-area-analyze |
| mini_point_area | 点选/虚框选标面积 | at-stat:ai-point-area |
| mini_wall_area / _2p | 房间墙面面积统计 | at-stat:ai-wall-area |
| mini_text_Length | 标注线段长度 | at-stat:ai-text-length |
| mini_bylayer_area | 面积分层统计表 | at-stat:ai-by-layer-area |
| mini_total_Length | 长度分层统计表 | at-stat:ai-total-length |
| mini_shape_list | 同形状多段线统计 | at-stat:ai-shape-list |
| mini_cale_txt | 全能文字计算面板 | at-stat:ai-cale-txt |
| mini_unest | 全能材料排版 | composing 包补充 |

### at-interior（装修设计）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_zxmb | 装修面板 | at-interior:ai-panel |
| mini_mugongban | 木工板截面 | at-interior:ai-sec-mugongban |
| mini_jiaban | 夹板截面 | at-interior:ai-sec-jiaban |
| mini_woodsec / _floor | 实木收口/木地板截面 | at-interior:ai-sec-wood |
| mini_curtain | 窗帘平面 | at-interior:ai-curtain |
| mini_closet | 衣柜平面 | at-interior:ai-closet |
| mini_ggpm / gzlm / gzpm | 高柜平面/柜子立面/剖面 | at-interior:ai-cabinet-* |
| mini_gdgpm / mfpm / mdb | 高低柜/木方/垫板剖面 | at-interior:ai-sec-* |
| mini_elev_interior | 生成立面框架 | at-interior:ai-elev-frame |
| mini_jianxian_plan / elev | 角线天花平面/立面 | at-interior:ai-jianxian |
| mini_elec_line | 灯具连线或标注 | at-interior:ai-elec-line |
| mini_light_plan | 动态沿线布筒灯 | at-interior:ai-light-plan |
| mini_glasssec / gypsumsec / stonesec / steel | 玻璃/石膏板/石材/钢板截面 | at-interior:ai-sec-* |
| mini_xg | 型钢截面面板 | at-interior:ai-sec-steel-panel |
| mini_gypsumsec_cell / wall | 石膏板吊顶/轻钢龙骨板墙剖面 | at-interior:ai-sec-* |
| mini_stonesec_wall | 干挂海棠角石材剖面 | at-interior:ai-sec-stone-wall |
| mini_jggk | 角钢钢框剖面 | at-interior:ai-sec-steel-frame |
| mini_blgd_wall | 玻璃隔断平面 | at-interior:ai-glass-wall |
| mini_hbsk | 画背栓孔 | at-interior:ai-back-holes |
| mini_weld_line | 焊缝 | at-interior:ai-weld |
| mini_flex | 连接软管 | at-interior:ai-flex |
| mini_tube | 绘制管线 | at-interior:ai-tube |

### at-landscape（景观）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_shrub_edge | 多段线变灌木边缘线 | at-landscape:ai-shrub-edge |
| mini_grass | 画立面草 | at-landscape:ai-grass |
| mini_plant_text | 标注苗木数量/灌木面积 | at-landscape:ai-plant-text |
| mini_area_landscape | 内轮廓面积及随机色填充 | at-landscape:ai-area |
| mini_mmtj / _hatch | 苗木统计表/景观填充面积统计 | at-landscape:ai-mm-list |
| mini_add_path | 封闭多段线搭桥 | at-landscape:ai-add-path |
| mini_remove_path | 封闭多段线挖路 | at-landscape:ai-remove-path |
| mini_lxlc | 路线里程标注 | at-landscape:ai-mileage |
| mini_jzd | 界址点编号 | at-landscape:ai-jzd |

### at-linetype（线型）

| iMini 命令 | 名称 | 目标函数 |
|---|---|---|
| mini_ltype | 改线型 | at-linetype:ai-ltype |
| mini_pline_jbbx | 局部变虚线 | at-linetype:ai-jb-vd |
| mini_reverse_line | 反转线型方向 | at-linetype:ai-reverse |
| mini_ltype_key | 定义线型快捷命令 | at-linetype:ai-ltype-key |
| mini_lt_shp2blk | 炸线型(树)为块 | at-linetype:ai-shp2blk |
| mini_shp2blk | 形变块 | at-linetype:ai-shp2blk |
| mini_mkshape | 制作形到 Shx | at-linetype:ai-mkshape |
| mini_lt_out | 输出线型定义 | at-linetype:ai-lt-out |
| mini_tree_style | 变线为树 | at-linetype:ai-tree-style |
| mini_tree_group | 成片布树 | at-linetype:ai-tree-group |

### 系统/工具类（归入现有包或 base）

| iMini 命令 | 名称 | 目标 |
|---|---|---|
| minimenu / mini_menu / mini_merge_menu / mini_close_toolbar | 菜单开关 | 不实现（@lisp 有自己的菜单体系） |
| mini_config | 用户配置 | 用 @:define-config 机制 |
| mini_pgp_edit | 快捷键设置 | 不实现（平台自带） |
| mini_atoms | 开发函数变量列表 | 不实现（开发辅助） |
| mini_unload / mini_uninstall | 绿色卸载 | 不实现（包系统管理） |
| mini_background | 切换绘图区颜色 | at-color 或 system |
| mini_osnap_use / non | 捕捉开关 | system 工具，简单 |
| mini_mouse | 切换鼠标中键 | 不实现（平台设置） |
| mini_open_folder | 打开文件夹 | at-pm 补充 |
| mini_tkk | 插入图框 | at-pm 已有 frame 功能，补充 AI 入口 |
| mini_merge_dwgs | 全能文件导入合并 | at-pm 补充 |
| mini_view_slb | 查看幻灯库 | 不实现（iMini 自有资源） |
| mini_image2ole / oleimage_crop | 图片转OLE/裁剪 | ole 包补充 |
| mini_create_image / _x | 快速截图 | at-plot 补充 |
| mini_tzml | 批量打印目录拆图 | at-pm/at-plot 补充 |
| mini_chg_units | 实体坐标单位变换 | at-math 或 at-edit |
| mini_regen | 快速局部重生成 | at-layout |
| mini_blank / unblank | 隐藏/显示实体 | at-layer 补充 |
| mini_IsolateObjects | 独立显示所选 | at-layer 补充 |
| mini_lockobjects / unlockobjects / disablelock | 锁定/解锁实体 | at-layer 补充 |
| mini_layerstates_* | 图层状态 | at-layer 补充 |
| mini_layer_key / ltype_key / hatch_key | 数字面板快捷键 | 不实现（iMini 自有数字面板机制） |
| mini_layer_killdollar / dicts | 清理$层/词典 | at-purge 补充 |
| mini_line_clean | 清理短线点空字 | at-purge 补充 |
| mini_layer_strcase | 层名大小写 | at-layer 补充 |
| mini_ldorder | 按图层调整绘图顺序 | at-layer 补充 |
| lm_LayerExtract | 图层提取器 | at-layer 补充 |
| mini_layer_bycolor | 按颜色分层 | at-layer 补充 |
| mini_yjbh | 全能变色 | at-color 补充 |
| mini_purge | 一键清理 | at-purge 补充 |
| mini_multimerge | 合并图层字样线型 | at-purge 或 at-edit |
| mini_ss / chain_Sel / fastsel / getsel | 选择系列 | at-select 补充 |
| mini_fix_fonts / use_myfonts | 字库替换 | fonts 包补充 |
| mini_ocr | OCR | ocr 包 |
| mini_trans | 翻译 | aigc 包 |
| mini_unest | 材料排版 | composing 包 |
| mini_cale_txt | 文字计算 | at-math 包 |
| mini_xuhao | 全能编号 | at-number 包 |
| lman | ET 图层管理 | 不实现（ET 依赖） |
| ET_* | Express Tools | 不实现（ACAD 选装组件） |
| mini_getsel | 获得选择集 | at-select 补充 |

## 实施策略

1. 每个新建包遵循 AGENTS.md 规范：`pkg.lsp`（`@::def-pkg`）、`<pkg>.lsp` 主文件、`@:add-menus` 菜单、UTF-8、docstring、行长 ≤100。
2. 所有可被 agent 调用的能力提供 `<ns>:ai-<动作>` 无交互入口（参数全走形参、返回 ename/列表、失败 nil、不弹窗）。
3. 交互入口用 `ssget`/`getpoint`/`getkword`，不用 DCL 模态框（除非必要）。
4. 依赖用 `(require '(ns:* ...))`；lint 0 error 是底线（`node ../atlisp-lint/bin/atlisp-lint --file <pkg>/<file>.lsp`）。
5. 实现顺序：P0（at-arch / at-edit / at-hatch / at-dim / at-text）→ 现有包补充 → P1（其余 6 包）。
