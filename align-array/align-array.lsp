;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这是使用开发工具 dev-tools 自动创建的程序源文件 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 定义配置项 'align-array:first 用于 应用包 align-array 的 第一个配置项 first 
;;(@:define-config 'align-array:first "我是配置项 align-array:first 的值" "这个配置项的用途说明。")
;; (@:get-config 'align-array:first) ;; 获取配置顶的值
;; (@:set-config 'align-array:first  "新设的值") ;; 设置配置顶的值
;; 向系统中添加菜单 
(@:add-menu "创意绘图" "对齐排列" "(align-array:align-array)" )
;;==========================================================================代码说明
;;单体对齐、等距排列插件/补充图框类别选择/ST-2020.03。
;;双向对齐不做排列，水平：以左下角x值由左到右排列，垂直：以左下角y值由下到上排列。
;;编码思路：1，利用院长外包围9点函数找到合适移动基点。2，根据对齐形式计算移动终点。3，移动命令完成最后步骤。
;;图框、方框和手工操作则利用对角点作为ssget 框选点，全选图纸内容后再做移动，排列，注意边框不要重叠，否则会错位！
;;排图纸说明：图框为块的选2操作，默认都是同名块，点选图框右键可全选，不同名块勾选异名块，单独点选；图框不为块是方框的选3，默认同层同色矩形框可以快速全选，不共性的打钩单独点选；如果连方框都没有直接用4 手动框选图纸内容，没选一次画一个矩形框，选完右键继续，勾选边框比会自动采用标准图纸比例框。
;; /ST-2020.03.06更新。
;;1，增加插图框程序，根据矩形框插入指定图框块，自动缩放居中，因每种图框款式不一，需后期手动微调位置，选取操作与上面矩形框排列一样。默认图框文件是 D：\A2图框MR.dwg 左下角为插入点，要按照标准A2图幅（594x420mm）尺寸来做，不然缩放比例不对的；
;;2，完善了dcl对话框程序，可记忆上次数值；
;;3，补充处理因坐标系非世界系出错代码；
;;4，有群组的时候框选类型会错位，未解决。
;;==========================================================================
;; /ST-2020.03.09更新。
;;1,处理了群组单体排列的问题；2,增加本图图框插入；
;;3,增加阵列,只做了从左到右从上到下排列；
;;4,增加单体速选.
;;==========================================================================
;; /ST-2020.03.11更新。
;;1,增加 自动/手选 顺序排列开关；2,增加复制阵列(自带那种)；
;;==========================================================================
;; /ST-2020.03.13更新。选取方式增加自动识别图框。
;;用于异名图框块，或者非块的矩形外框图框的速选，要全对象计算对比，大型图纸尽量不要使用，使用布局图纸空间的应该问题不大。
;;==========================================================================
;; /ST-2020.03.18更新。自动识别增加速选：直接框选生成边框，大型图慎用。

;;==========================================================================对话框程式代码（不熟不多说）
(defun align-array:align-array (/ DCL dd ucs)
  (if (= (getvar "WORLDUCS" ) 0) (if (tblsearch "ucs" "ucs_old") (command "ucs" "na" "s" "ucs_old" "y" "ucs" "w") 
				     (command "ucs" "na" "s" "ucs_old" "ucs" "w")) (setq ucs 1))
  (setq DCL (load_dialog (make-dcl-pl))) 
  (new_dialog "rect01" DCL) 

  (if (not cp_xx) (setq cp_xx 0))
  (if (not cp_dx) (setq cp_dx 100))
  (if (not YMK_TMP) (setq YMK_TMP 0))
  (if (not BGX_TMP) (setq BGX_TMP 0))
  (if (not CKB_TMP) (setq CKB_TMP 0))
  (if (not SX_TMP) (setq SX_TMP 0))
  (if (not zdSX_TMP) (setq zdSX_TMP 0))
  (if (not LB_TMP) (setq LB_TMP 1))
  
  (if (not tukuang_TMP) (setq tukuang_TMP 1) ) 	
  (if (not t_lujing) (setq t_lujing "D:\\A2图框MR.dwg"))
  
  (if cp_dx1 (setq cp_xx 1 cp_dx cp_dx1 cp_dx1 nil) );判断手动点距离是否有
  (if tk_BLfile  (setq t_lujing tk_BLfile))
  
  (if (not RB_zhenlieFS) (progn (SETQ RB_zhenlieFS 1)));阵列方式默认
  
					;变灰判断
  (if (or (= cp_xx 1) (/= LB_TMP 1)) (mode_tile "row_sxdq" 1))
  (if  (= cp_xx 0)  (mode_tile "cp_d" 1))
  (if  (or (= tukuang_TMP 1) (= tukuang_TMP 2))  (progn (mode_tile "k_lujing" 1) (mode_tile "k_liulan" 1)) )
  
  (if (= RB_zhenlieFS 1) (progn (mode_tile "k_shushu" 1)  (mode_tile "k_tog_zhongdian" 0)) (progn (fuzhizhenlie) (mode_tile "k_shushu" 0)  (mode_tile "k_tog_zhongdian" 1)))
  
  
  
  (if (not t_hengju) (setq t_hengju 100))
  (if (not t_hengshu) (setq t_hengshu 3))
  (if (not t_shuju) (setq t_shuju 100))
  (if (not t_shushu) (setq t_shushu 3))
  (if (not RB_zhenliejianju) (setq RB_zhenliejianju 1))
  
  (if t_hengju_dx (setq t_hengju t_hengju_dx t_hengju_dx nil));手动选间距赋值函数变量，自清空
  (if t_shuju_dx (setq t_shuju t_shuju_dx t_shuju_dx nil))
  
  (if (not RB_paixuFS) (setq RB_paixuFS 1));排序方式  
  
  (if (not t_tog_zhongdian) (setq t_tog_zhongdian 0));阵列基点默认0-左下角
  
  
  (setdate)
  ;;水平
  (action_tile "dtpl1" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 1)")
  (action_tile "dtpl2" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 2)")
  (action_tile "dtpl3" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 3)")
  ;;垂直
  (action_tile "dtpl4" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 4)")
  (action_tile "dtpl5" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 5)")
  (action_tile "dtpl6" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 6)")
  ;;双向
  (action_tile "dtpl7" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 7)")
  (action_tile "dtpl8" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 8)")
  (action_tile "dtpl9" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 9)")
  (action_tile "dtpl10" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 10)")
  (action_tile "dtpl11" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 11)")
  (action_tile "dtpl12" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 12)")
  (action_tile "dtpl13" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 13)")
  (action_tile "dtpl14" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 14)")
  (action_tile "dtpl15" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 15)")
  ;;重排	
  (action_tile "dxjj" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 16)"); 排列点选距离
  (action_tile "k_liulan" "(S_RECT) (S_PGON) (S_toggle) (setq t_moren 0) (done_dialog 17)");浏览
  (action_tile "k_ctk" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 18)");插图框确认键
  
  (action_tile "k_zlqueding" "(S_RECT) (S_PGON) (S_toggle) (if  (or (= t_hengshu 0) (= t_shushu 0)) (alert \"输入数据有误！\")  (done_dialog 19))");阵列确定按键 
  
  (action_tile "k_bt_dianxuan" "(S_RECT) (S_PGON) (S_toggle) (done_dialog 20)");阵列点选距离
  
					;--------------------------------------------------------------------------------非done项目
  (ACTION_TILE "xlb2" "(MODE_TILE \"row_sxdq\" 1)");变灰	
  (ACTION_TILE "xlb3" "(MODE_TILE \"row_sxdq\" 1)");变灰
  (ACTION_TILE "xlb4" "(MODE_TILE \"row_sxdq\" 1)");变灰
  (ACTION_TILE "xlb5" "(MODE_TILE \"row_sxdq\" 1)");变灰
  (ACTION_TILE "xlb1" "(S_RECT) (if (= cp_xx 0) (MODE_TILE \"row_sxdq\" 0))");恢复	
  (action_tile "cp_x" "(S_RECT) (if (= cp_xx 1) (progn (mode_tile \"cp_d\" 0) (mode_tile \"row_sxdq\" 1)) (progn (mode_tile \"cp_d\" 1) (if (= xlb1 1) (mode_tile \"row_sxdq\" 0))))")
  (action_tile "k_moren" "(S_RECT) (mode_tile \"k_lujing\" 1) (mode_tile \"k_liulan\" 1)")
  (action_tile "k_tuzhong" "(S_RECT) (mode_tile \"k_lujing\" 1) (mode_tile \"k_liulan\" 1)")
  (action_tile "k_chawenjian" "(S_RECT) (mode_tile \"k_lujing\" 0) (mode_tile \"k_liulan\" 0)")
					;(action_tile "k_zlqueding" "(S_RECT) (S_PGON) (S_toggle) (if  (= t_hengshu 0) (alert \"输入数据有误！\") )")
  (action_tile "k_rb_chongpaizhenlie" "(S_RECT) (chongpaizhenlie) (mode_tile \"k_shushu\" 1) (mode_tile \"k_rb_zuoxia\" 0) (mode_tile \"k_tog_zhongdian\" 0) (mode_tile \"k_paixuFS\" 0)")
  (action_tile "k_rb_fuzhizhenlie" "(S_RECT) (fuzhizhenlie) (mode_tile \"k_shushu\" 0) (mode_tile \"k_rb_zuoxia\" 1) (mode_tile \"k_tog_zhongdian\" 1) (mode_tile \"k_paixuFS\" 1)")
					;--------------------------------------------------------------------------------

  (setq dd (start_dialog ))
  (cond
    
    ((= dd 1) (cond ((= LB_TMP 1) (if (= cp_xx 1) (S_PLcx 0 7 9 0) (S_DQcx 0 9)));单体
		    ((= LB_TMP 2) (if (= cp_xx 1) (tukuang_BK 0 x 7 9 0) (tukuang_BK 0 9 x x x)));图框块
		    ((= LB_TMP 3) (if (= cp_xx 1) (juxing_WL 0 x 7 9 0) (juxing_WL 0 9 x x x)));矩形框
		    ((= LB_TMP 4) (if (= cp_xx 1) (select_SG 0 x 7 9 0) (select_SG 0 9 x x x)));手工选
		    ((= LB_TMP 5) (if (= cp_xx 1) (zidongshibie_WL 0 x 7 9 0) (zidongshibie_WL 0 9 x x x)));自动识别
		    )) 		
    ((= dd 2) (cond ((= LB_TMP 1) (if (= cp_xx 1) (S_PLcx 0 4 6 0) (S_DQcx 0 5)));单体
		    ((= LB_TMP 2) (if (= cp_xx 1) (tukuang_BK 0 x 4 6 0) (tukuang_BK 0 5 x x x)));图框块
		    ((= LB_TMP 3) (if (= cp_xx 1) (juxing_WL 0 x 4 6 0) (juxing_WL 0 5 x x x)));矩形框
		    ((= LB_TMP 4) (if (= cp_xx 1) (select_SG 0 x 4 6 0) (select_SG 0 5 x x x)));手工选
		    ((= LB_TMP 5) (if (= cp_xx 1) (zidongshibie_WL 0 x 4 6 0) (zidongshibie_WL 0 5 x x x)));自动识别
		    ))
    
    ((= dd 3) (cond ((= LB_TMP 1) (if (= cp_xx 1) (S_PLcx 0 1 3 0) (S_DQcx 0 1)));单体
		    ((= LB_TMP 2) (if (= cp_xx 1) (tukuang_BK 0 x 1 3 0) (tukuang_BK 0 1 x x x)));图框块
		    ((= LB_TMP 3) (if (= cp_xx 1) (juxing_WL 0 x 1 3 0) (juxing_WL 0 1 x x x)));矩形框
		    ((= LB_TMP 4) (if (= cp_xx 1) (select_SG 0 x 1 3 0) (select_SG 0 1 x x x)));手工选
		    ((= LB_TMP 5) (if (= cp_xx 1) (zidongshibie_WL 0 x 1 3 0) (zidongshibie_WL 0 1 x x x)));自动识别
		    ))
    
    
    
    ((= dd 4) (cond ((= LB_TMP 1) (if (= cp_xx 1) (S_PLcx 1 1 7 (* 0.5 pi)) (S_DQcx 1 1)));单体
		    ((= LB_TMP 2) (if (= cp_xx 1) (tukuang_BK 1 x 1 7 (* 0.5 pi)) (tukuang_BK 1 1 x x x)));图框块
		    ((= LB_TMP 3) (if (= cp_xx 1) (juxing_WL 1 x 1 7 (* 0.5 pi)) (juxing_WL 1 1 x x x)));矩形框
		    ((= LB_TMP 4) (if (= cp_xx 1) (select_SG 1 x 1 7 (* 0.5 pi)) (select_SG 1 1 x x x)));手工选
		    ((= LB_TMP 5) (if (= cp_xx 1) (zidongshibie_WL 1 x 1 7 (* 0.5 pi)) (zidongshibie_WL 1 1 x x x)));自动识别
		    ))
    
    ((= dd 5) (cond ((= LB_TMP 1) (if (= cp_xx 1) (S_PLcx 1 2 8 (* 0.5 pi)) (S_DQcx 1 5)));单体
		    ((= LB_TMP 2) (if (= cp_xx 1) (tukuang_BK 1 x 2 8 (* 0.5 pi)) (tukuang_BK 1 5 x x x)));图框块
		    ((= LB_TMP 3) (if (= cp_xx 1) (juxing_WL 1 x 2 8 (* 0.5 pi)) (juxing_WL 1 5 x x x)));矩形框
		    ((= LB_TMP 4) (if (= cp_xx 1) (select_SG 1 x 2 8 (* 0.5 pi)) (select_SG 1 5 x x x)));手工选	
		    ((= LB_TMP 5) (if (= cp_xx 1) (zidongshibie_WL 1 x 2 8 (* 0.5 pi)) (zidongshibie_WL 1 5 x x x)));自动识别	
		    ))
    
    ((= dd 6) (cond ((= LB_TMP 1) (if (= cp_xx 1) (S_PLcx 1 3 9 (* 0.5 pi)) (S_DQcx 1 9)));单体
		    ((= LB_TMP 2) (if (= cp_xx 1) (tukuang_BK 1 x 3 9 (* 0.5 pi)) (tukuang_BK 1 9 x x x)));图框块
		    ((= LB_TMP 3) (if (= cp_xx 1) (juxing_WL 1 x 3 9 (* 0.5 pi)) (juxing_WL 1 9 x x x)));矩形框
		    ((= LB_TMP 4) (if (= cp_xx 1) (select_SG 1 x 3 9 (* 0.5 pi)) (select_SG 1 9 x x x)));手工选	
		    ((= LB_TMP 5) (if (= cp_xx 1) (zidongshibie_WL 1 x 3 9 (* 0.5 pi)) (zidongshibie_WL 1 9 x x x)));自动识别
		    ))	
    
    ((= dd 7) (S_DQcx 2 7))
    ((= dd 8) (S_DQcx 2 4))
    ((= dd 9) (S_DQcx 2 1))
    ((= dd 10) (S_DQcx 2 8))
    ((= dd 11) (S_DQcx 2 5))
    ((= dd 12) (S_DQcx 2 2))
    ((= dd 13) (S_DQcx 2 9))
    ((= dd 14) (S_DQcx 2 6))
    ((= dd 15) (S_DQcx 2 3))
    
    ((= dd 16) (S_RECT2)) ;;排列点选间距
    ((= dd 17) (setq tk_BLfile (getfiled "选择图框文件" "C:/Users/Administrator/Desktop/" "dwg" 16)) (c:ent_DQPL))
    ((= dd 18) (cond  ((= tukuang_TMP 1) (chatukuang "D:\\A2图框MR.dwg"))
		      ((= tukuang_TMP 2) (chatukuang_tuzhong))
		      ((= tukuang_TMP 3) (chatukuang t_lujing))))
    
    ((= dd 19)  (if (= RB_zhenlieFS 1) (if (= LB_TMP 1) (cond  ((= RB_zhenliejianju 1) ( zhenlie_DT t_hengju t_shuju t_hengshu 1))
							       ((= RB_zhenliejianju 2) ( zhenlie_DT t_hengju t_shuju t_hengshu 2)))
					   (cond  ((= RB_zhenliejianju 1) ( zhenlie_SS t_hengju t_shuju t_hengshu 1))
						  ((= RB_zhenliejianju 2) ( zhenlie_SS t_hengju t_shuju t_hengshu 3)))
					   );if1
		    (zhenlie_FUZHI t_shushu  t_hengshu  t_shuju  t_hengju)
		    );if2
     
     
     )
    ((= dd 20)	(S_RECT3))	;;阵列点选间距					
    
    
    );cond
  (if (not ucs) (command "ucs" "na" "r" "ucs_old"))
  (princ)
  )
;;==========================================================================辅助函数
;;提取勾选键、排距数值
(DEFUN S_RECT()
  (SETQ cp_dx (ABS (ATOF (GET_TILE "cp_d"))) ;间距值
	cp_xx (atoi (GET_TILE "cp_x")) ;重排开关值
	xlb1  (atoi (GET_TILE "xlb1")));类别开关值
					;t_moren  (atoi (GET_TILE "k_moren"));默认图框开关值
  (COND ((= (GET_TILE "k_moren") "1") (SETQ tukuang_TMP 1))
        ((= (GET_TILE "k_tuzhong") "1") (SETQ tukuang_TMP 2))
	((= (GET_TILE "k_chawenjian") "1") (SETQ tukuang_TMP 3))
	);图框类别     
  (setq t_hengju (ATOF (GET_TILE "k_hengju")) 
	t_hengshu (atoi (GET_TILE "k_hengshu"))
        t_shuju (ATOF (GET_TILE "k_shuju"))
	t_shushu (atoi (GET_TILE "k_shushu"))
	)
  (COND ((= (GET_TILE "k_rb_zhongzhong") "1") (SETQ RB_zhenliejianju 1))
        ((= (GET_TILE "k_rb_bianbian") "1") (SETQ RB_zhenliejianju 2))
	
	);阵列间距方式 
  (COND ((= (GET_TILE "k_rb_zidong") "1") (SETQ RB_paixuFS 1))
        ((= (GET_TILE "k_rb_xuanxu") "1") (SETQ RB_paixuFS 2))
	
	);排序方式
  (COND ((= (GET_TILE "k_rb_chongpaizhenlie") "1") (SETQ RB_zhenlieFS 1))
        ((= (GET_TILE "k_rb_fuzhizhenlie") "1") (SETQ RB_zhenlieFS 2))
	
	);阵列方式 t_tog_zhongdian
  
  (setq t_tog_zhongdian (atoi (GET_TILE "k_tog_zhongdian")));阵列对齐基点
  
  )
(defun S_RECT2 (/ pdx1);;选点定距函数(排列)
  (setq cp_dx1 (distance (setq pdx1(getpoint)) (getpoint pdx1)))
  (c:ent_DQPL)
  )
(defun S_RECT3 (/ pdx1);;选点定距函数(阵列); 
  (setq p1(getpoint "\n选取1点：") p2 (getcorner p1"\n选取2点(X差=横距，Y差=竖距)："))
  (setq t_hengju_dx (abs (- (car p1) (car p2))) t_shuju_dx (abs (- (cadr p1) (cadr p2))))
  (c:ent_DQPL)
  )
;; 院长函数 / 外包盒9点坐标；在此程序中发挥关键作用
(defun ss9pt (ss n / ss i s1 ll rr box ptn a p1 p2 p3 p4 p5 p6 p7 p8 p9)

  (progn ss
	 (setq i -1)
	 (repeat (sslength ss)
		 (setq s1 (ssname ss (setq i (1+ i))))
		 (vla-GetBoundingBox (vlax-ename->vla-object s1) 'll 'rr)
		 (setq box (list (vlax-safearray->list ll) (vlax-safearray->list rr))
		       ptn (append box ptn)
		       )
		 )
	 (setq a  (mapcar '(lambda (x) (apply 'mapcar (cons x ptn))) (list 'min 'max))
	       p1 (car a)
	       p9 (cadr a)
	       p5 (mapcar '(lambda (x y) (* (+ x y) 0.5)) p1 p9)
	       p2 (list (car p5) (cadr p1))
	       p3 (list (car p9) (cadr p1))
	       p4 (list (car p1) (cadr p5))
	       p6 (list (car p9) (cadr p5))
	       p7 (list (car p1) (cadr p9))
	       p8 (list (car p5) (cadr p9))
	       )
	 (nth (- n 1) (list p1 p2 p3 p4 p5 p6 p7 p8 p9))
	 )
  )
;;选择集=>>名表
(defun SStoLST (ss / i entname lst)
  (setq i -1)
  (if ss
      (while (setq entname (ssname ss(setq i(1+ i))))
	(setq lst (cons entname lst))))
  (reverse lst)
  )

(defun setdate ();;设置dcl各个值 
  (set_tile "cp_x" (rtos cp_xx 2 2))
  (set_tile "cp_d" (rtos cp_dx 2 2))
  (set_tile "toggle_ymk" (rtos YMK_TMP 2 2))
  (set_tile "toggle_bgx" (rtos BGX_TMP 2 2))
  (set_tile "toggle_ckb" (rtos CKB_TMP 2 2)) 
  (set_tile "toggle_sx" (rtos SX_TMP 2 2))
  (set_tile "toggle_sx2" (rtos zdSX_TMP 2 2))
  
  (cond  ((= LB_TMP 1) (set_tile "xlb1" "1"))
	 ((= LB_TMP 2) (set_tile "xlb2" "1"))
	 ((= LB_TMP 3) (set_tile "xlb3" "1"))
	 ((= LB_TMP 4) (set_tile "xlb4" "1"))
	 ((= LB_TMP 5) (set_tile "xlb5" "1"))
	 )
  (cond  ((= tukuang_TMP 1) (set_tile "k_moren" "1"))
	 ((= tukuang_TMP 2) (set_tile "k_tuzhong" "1"))
	 ((= tukuang_TMP 3) (set_tile "k_chawenjian" "1"))		     
	 )
  (set_tile "k_lujing" t_lujing)

  (set_tile "k_hengju" (rtos t_hengju 2 2))
  (set_tile "k_hengshu" (rtos t_hengshu 2 0))
  (set_tile "k_shuju" (rtos t_shuju 2 2))
  (set_tile "k_shushu" (rtos t_shushu 2 0))
  (cond  ((= RB_zhenliejianju 1) (set_tile "k_rb_zhongzhong" "1"))
	 ((= RB_zhenliejianju 2) (set_tile "k_rb_bianbian" "1"))		    
	 );阵列间距方式
  (cond  ((= RB_paixuFS 1) (set_tile "k_rb_zidong" "1"))
	 ((= RB_paixuFS 2) (set_tile "k_rb_xuanxu" "1"))		    
	 );排序方式
  (cond  ((= RB_zhenlieFS 1) (set_tile "k_rb_chongpaizhenlie" "1"))
	 ((= RB_zhenlieFS 2) (set_tile "k_rb_fuzhizhenlie" "1"))		    
	 );阵列方式
  (set_tile "k_tog_zhongdian" (rtos t_tog_zhongdian 2 2));阵列对齐基点-默认左下角
  
  )

(defun fuzhizhenlie ();点复制阵列选项变灰处理 
  (mode_tile "k_no1" 1)	
  (mode_tile "k_no2" 1)
  (mode_tile "k_shuipingduiqi" 1)
  (mode_tile "k_chuizhiduiqi" 1)
  (mode_tile "k_dengju,dianxuan" 1)
					;(mode_tile "k_zlqueding" 1)
  (mode_tile "row_sxdq" 1)
  )
(defun chongpaizhenlie ();点重排阵列选项变亮处理 
  (S_PGON) (S_RECT)	
  (mode_tile "k_no1" 0)	
  (mode_tile "k_no2" 0)
  (mode_tile "k_shuipingduiqi" 0)
  (mode_tile "k_chuizhiduiqi" 0)
  (mode_tile "k_dengju,dianxuan" 0)	
					;(mode_tile "k_zlqueding" 0)
  (if (and (= LB_TMP 1) (= cp_xx 0)) (mode_tile "row_sxdq" 0))
  )
;;==========================================================================主处理函数
;;单体对齐主程序,/ H-S=0：水平 / H-S=1：垂直 / H-S=2:双向 / 9NB:移动基点 /
(defun S_DQcx (H-S 9NB / ang entdate entname gpname gpname_lst gpss_lst gx_list i nb nb0 p0 p1 p1a p2 pick_date ss ss_gp ss_gp_temp ss-9 ssall sslst sslst_px)
  (command "undo" "be");撤销开始点，对于批量操作做好撤销设置，不然一步步后退很麻烦。
  (prompt "\n选择对象:"); ssget 后面不能带操作说明，在其前可以用 prompt 先行提示。

  
  (if (= SX_TMP 1)
      (progn (setq  pick_date (entget (car (entsel "\n点选源对象:"))) GX_list '())
	     
	     (if (= (cdr (assoc 0 pick_date)) "INSERT")  
		 (setq  GX_list (list (assoc 0 pick_date) (assoc 2 pick_date)));图块类
		 (progn (setq  GX_list (list (assoc 0 pick_date) (assoc 8 pick_date))) (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list))));其他类
		 
		 );;收集共性组码
	     
	     (prompt "\n选择要处理对象 / 全选<空格>：")
	     (if (not (setq ssall (ssget GX_list))) (setq ssall (ssget "x"  GX_list)));;共性选集(同层、同色)
	     
	     (sssetfirst nil ssall)
	     (setq  ss_gp (ssadd) ss (ssadd))
	     
	     );progn
      
      (setq ssall (ssget) ss_gp (ssadd) ss (ssadd)));if ;ss_gp:全部属于群组成员的选集
  
  ;;判断群组
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
  (setq i -1 GPname_lst '()) 
  (while (setq entname (ssname ssall (setq i (1+ i))))	  
    (if (= (cdr (assoc 102 (setq entdate (entget entname)))) "{ACAD_REACTORS") (progn (setq GPname (cdr (assoc 330 entdate))) (setq ss_gp (ssadd entname ss_gp))) (setq ss (ssadd entname ss)))
    (if (not (member GPname GPname_lst)) (setq GPname_lst (cons GPname GPname_lst)));群组名的表
    );while 
					;(sssetfirst nil ss);亮显ss
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
  ;; 单体ss选集处理
  (setq p0 (getpoint "\n选择对齐基点<退出>: "));getpoint 等很多函数后面可以接说明，\n 换行号，命令行会换一行再提示。
  (setq i -1);为了配合下面 ssname 函数设的变量，第一个对象编号为 0，所以设为 -1。
  (while (setq entname (ssname ss (setq i (1+ i))));while 对选集全部对象做循环处理，也可以用 repeat 函数来做。
    (setq ss-9 (ssadd) ss-9 (ssadd entname ss-9))
    (setq  p1 (ss9pt ss-9 9NB));9NB是此函数参数之一，就是对象的9点之一（移动基点p1），根据对齐形式选取。
    (cond ((= H-S 0) (setq p2 (list (car p1) (cadr p0))));H-S也是参数之一，根据他来计算移动的目标点 p2
	  ((= H-S 1) (setq p2 (list (car p0) (cadr p1))))
	  ((= H-S 2) (setq p2 p0));双向对齐时，选的点p0就是目标点p2
	  )
    (command "MOVE" entname "" "non" p1 "non" p2);移动命令完成
    );while
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
  ;;群组对象处理：
  (if ss_gp (progn 
	      (setq GPss_lst '());GPss_lst-各组对象选集 列表 
	      (foreach x GPname_lst
		       (setq i -1 ss_gp_temp (ssadd)) ;ss_gp_temp-临时选集 
		       (while (setq entname (ssname ss_gp (setq i (1+ i))))      
			 (if (equal (cdr (assoc 330  (entget entname))) x) (setq ss_gp_temp (ssadd entname ss_gp_temp)))				
			 );while
		       (setq GPss_lst (cons ss_gp_temp GPss_lst))			
		       );foreach1																			
	      
	      (foreach x GPss_lst
		       
		       (if (/= (sslength x) 0)
			   (progn (setq  p1 (ss9pt x 9NB)) 
				  (cond ((= H-S 0) (setq p2 (list (car p1) (cadr p0)))) 
					((= H-S 1) (setq p2 (list (car p0) (cadr p1))))
					((= H-S 2) (setq p2 p0)) 
					)
				  (command "MOVE" x "" "non" p1 "non" p2) ))
		       
		       );foreach2	
	      
	      );progn
      );if
					; ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼	
  (command "undo" "e");撤销结束点，与开始点之间的操作一步撤销完成
  (princ);这个是标配吧
  );结束
;;==========================================================================
;;单体排列主程序,/ H-S=0：水平 / H-S=1：垂直 / ang:水平0，垂直90° / NB:对象移动基点 / NB0: 移动目标点(选取合适的9点之一来计算)
(defun S_PLcx (H-S NB NB0 ang / entname i p0 p1a ss sslst sslst_px ss-9)
  (command "undo" "be");同上
  
  (if (= SX_TMP 1)
      (progn (setq  pick_date (entget (car (entsel "\n点选源对象:"))) GX_list '())
	     
	     (if (= (cdr (assoc 0 pick_date)) "INSERT")  
		 (setq  GX_list (list (assoc 0 pick_date) (assoc 2 pick_date)));图块类
		 (progn (setq  GX_list (list (assoc 0 pick_date) (assoc 8 pick_date))) (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list))));其他类
		 
		 );;收集共性组码
	     
	     (prompt "\n选择要处理对象 / 全选<空格>：")
	     (if (not (setq ssall (ssget GX_list))) (setq ssall (ssget "x"  GX_list)));;共性选集(同层、同色)
	     
	     (sssetfirst nil ssall)
	     (setq  ss_gp (ssadd) ss (ssadd))
	     
	     );progn
      
      (setq ssall (ssget) ss_gp (ssadd) ss (ssadd)));if ;ss_gp:全部属于群组成员的选集
  
  ;;判断群组
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼	 
  (setq i -1 GPname_lst '()) 
  (while (setq entname (ssname ssall (setq i (1+ i))))	  
    (if (= (cdr (assoc 102 (setq entdate (entget entname)))) "{ACAD_REACTORS") (progn (setq GPname (cdr (assoc 330 entdate))) (setq ss_gp (ssadd entname ss_gp))) (setq ss (ssadd entname ss)))
    (if (not (member GPname GPname_lst)) (setq GPname_lst (cons GPname GPname_lst)));群组名的表
    );while
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
  ;; 单体ss选集处理		
  (setq p0 (getpoint "\n选择对齐基点<退出>: "));排列的起点
  (setq sslst (SStoLST ss));;选集转成表以便下面进行排序
					;等距排列其实跟上面对齐操作原理一样，但是要对选集对象排序一下，不然会乱，因为这里的移动终点是根据前对象计算的，而对齐只要根据自己点位计算就行。
  
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  (if (= RB_paixuFS 1) (cond ((= H-S 0) (setq sslst_px (vl-sort sslst '(lambda (a b) (< (car (car (enbox a))) (car (car (enbox b))))))));;横排时按对象1点的x值小到大排序，得到新表
		             ((= H-S 1) (setq sslst_px (vl-sort sslst '(lambda (a b) (< (cadr (car (enbox a))) (cadr (car (enbox b))))))));;竖排时按对象1点的y值小到大排序，得到新表
	                     );cond  自动排列
      
      (setq sslst_px sslst);;按选择顺序排列
      );if
  
  
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■	
  (setq i 0);这里循环设的i是0，和上面-1是一样效果，都是从编号0开始
  (repeat (length sslst_px);这里用了repeat函数，注意这里对象是表，对比一下上面 while 的对象是选集，会发现原理一样，但用到的函数不一样
	  (setq entname (nth i sslst_px)) 
	  (setq ss-9 (ssadd) ss-9 (ssadd entname ss-9)
		p1a (ss9pt ss-9 NB));移动基点
	  
	  (command "MOVE" entname "" "non" p1a "non" p0);移动命令
					;(if (not cp_dx1) (setq cp_dx1 cp_dx));这是对话框的一些代码
	  
	  (setq p0 (polar (ss9pt ss-9 NB0) ang cp_dx) i (1+ i))	;这里是计算下一个对象的移动终点(p0重新赋值，循环使用)
	  );repeat
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
  ;;群组对象处理：
  (if ss_gp (progn 	 																	
	      (setq GPss_lst '());GPss_lst-各组对象选集 列表 
	      
	      (foreach x GPname_lst
		       (setq i -1 ss_gp_temp (ssadd)) ;ss_gp_temp-临时选集 
		       (while (setq entname (ssname ss_gp (setq i (1+ i))))      
			 (if (equal (cdr (assoc 330  (entget entname))) x) (setq ss_gp_temp (ssadd entname ss_gp_temp)))				
			 );while
		       (if (/= (sslength ss_gp_temp) 0) (setq GPss_lst (cons ss_gp_temp GPss_lst)))			
		       );foreach1																			
	      
	      (cond ((= H-S 0) (setq sslst_px (vl-sort GPss_lst '(lambda (a b) (< (car (ss9pt a 1)) (car (ss9pt b 1)))))));;横排时按对象1点的x值小到大排序，得到新表
		    ((= H-S 1) (setq sslst_px (vl-sort GPss_lst '(lambda (a b) (< (cadr (ss9pt a 1)) (cadr (ss9pt b 1)))))));;竖排时按对象1点的y值小到大排序，得到新表
	            );cond
	      (princ GPss_lst	) (princ)
	      (setq i 0) 
	      (repeat (length sslst_px)
		      (setq entname (nth i sslst_px))			               
		      (if (/= (sslength entname) 0)      
			  (progn (setq  p1a (ss9pt entname NB)) 		
		                 (command "MOVE" entname "" "non" p1a "non" p0) 
				 (setq p0 (polar (ss9pt entname NB0) ang cp_dx)));progn
			  );if
		      (setq i (1+ i))
		      );repeat														
	      );progn
      );if
					;▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼			
  (command "undo" "e");同上
					;(setq cp_dx1 nil);这是对话框的
  (princ)
  )


;; 更新补充代码0303 ◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆

;;==========================================================================辅助函数
(DEFUN S_PGON () ;;类别选择函数
  (COND ((= (GET_TILE "xlb1") "1") (SETQ LB_TMP 1))
	((= (GET_TILE "xlb2") "1") (SETQ LB_TMP 2))
	((= (GET_TILE "xlb3") "1") (SETQ LB_TMP 3))
	((= (GET_TILE "xlb4") "1") (SETQ LB_TMP 4))
	((= (GET_TILE "xlb5") "1") (SETQ LB_TMP 5))
	)	
  )

(DEFUN S_toggle () ;;速选、异名块、不共性、长宽比 勾选开关函数 
  (COND ((= (GET_TILE "toggle_ymk") "1") (SETQ YMK_TMP 1))
	((= (GET_TILE "toggle_ymk") "0") (SETQ YMK_TMP 0))
	
	) 
  (COND ((= (GET_TILE "toggle_bgx") "1") (SETQ BGX_TMP 1))
	((= (GET_TILE "toggle_bgx") "0") (SETQ BGX_TMP 0))
	)
  (COND ((= (GET_TILE "toggle_ckb") "1") (SETQ CKB_TMP 1))
	((= (GET_TILE "toggle_ckb") "0") (SETQ CKB_TMP 0))
	)
  (COND ((= (GET_TILE "toggle_sx") "1") (SETQ SX_TMP 1))
	((= (GET_TILE "toggle_sx") "0") (SETQ SX_TMP 0))
	)
  (COND ((= (GET_TILE "toggle_sx2") "1") (SETQ zdSX_TMP 1))
	((= (GET_TILE "toggle_sx2") "0") (SETQ zdSX_TMP 0))
	)
  )

(defun enbox (ename / ll ur);单体外框对角点函数 
  (vla-getboundingbox (vlax-ename->vla-object ename) 'll 'ur)
  (mapcar 'vlax-safearray->list (list ll ur)))

(defun DQ_BK (SS_lst H-S 9NB / i p0 p1 p2 ss-name ss-9);选集类别-对齐函数
  (setq p0 (getpoint "\n选择对齐基点<退出>: "))
  (command "-layer" "u" (setq suo_str (layer_suo_str)) "");开锁
  (setq i -1)	
  (while (setq ss-name (car (nth (setq i (1+ i)) SS_lst)));提取表元素中的 选集
    (setq ss-9 (ssadd) ss-9 (ssadd (caddr (nth i SS_lst)) ss-9))
    (setq  p1 (ss9pt ss-9 9NB));9NB是此函数参数之一，就是对象的9点之一（移动基点p1），根据对齐形式选取。
    (cond ((= H-S 0) (setq p2 (list (car p1) (cadr p0))));H-S也是参数之一，根据他来计算移动的目标点 p2
	  ((= H-S 1) (setq p2 (list (car p0) (cadr p1))))
	  ((= H-S 2) (setq p2 p0));双向对齐时，选的点p0就是目标点p2
	  )
    (command "MOVE" ss-name "" "non" p1 "non" p2);移动命令完成
    );while 	
  (command "-layer" "lo"  suo_str  "");回上锁	
  )

(defun PL_BK (SS_lst H-S NB NB0 ang / i p0 p1a ss-name ss-9);选集类别-排列函数
  (setq p0 (getpoint "\n选择对齐基点<退出>: "))
  (command "-layer" "u" (setq suo_str (layer_suo_str)) "");开锁	
  (setq i -1)	
  (while (setq ss-name (car (nth (setq i (1+ i)) SS_lst)));提取表元素中的 选集
    (setq ss-9 (ssadd) ss-9 (ssadd (caddr (nth i SS_lst)) ss-9))
    (setq  p1a (ss9pt ss-9 NB))
    
    (command "MOVE" ss-name "" "non" p1a "non" p0);移动命令完成
    
					;(if (not cp_dx1) (setq cp_dx1 cp_dx));这是对话框的一些代码
    
    (setq p0 (polar (ss9pt ss-9 NB0) ang cp_dx))	;这里是计算下一个对象的移动终点(p0重新赋值，循环使用)
    );while 
  (command "-layer" "lo"  suo_str  "");回上锁	
  (princ)	
  )

(defun mid_pt (p1 p2);;中心函数
  (mapcar'*(mapcar'+ p1 p2)'(0.5 0.5 0.5))
  )

(defun ckb_rec (p1 p2 / p1x p1y p2x p2y p3 rec ss-9);;手工选择勾选长宽比处理矩形函数
  (setq p1x (car p1) p1y (cadr p1)
	p2x (car p2) p2y (cadr p2)
	)
  (if (<= (abs (/ (- p2x p1x) 420)) (abs (/ (- p1y p2y) 297)))
      (setq p3 (polar (polar p1 0 (abs (* (/ (- p1y p2y) 297) 420))) (* 1.5 pi) (abs(- p1y p2y))))
      (setq p3  (polar (polar p1 0 (abs(- p2x p1x))) (* 1.5 pi) (abs (/ (* (- p2x p1x) 297) 420))))	
      );if	
  (command "RECTANG" "non" p1 "non" p3 "CHANGE" (setq rec (entlast)) "" "p" "c" 224 "")	
  (setq ss-9 (ssadd) ss-9 (ssadd rec ss-9))	
  (command "MOVE" rec "" "non" (ss9pt ss-9 5) "non" (mid_pt p1 p2))
  rec	
  )

(defun layer_suo_str (/ laylst_suo lays_jh str);;上锁图层名字字符串组合
  (setq lays_jh (vla-get-layers (vla-get-activedocument (vlax-get-Acad-Object))));文档图层集合
  (vlax-for x lays_jh
	    (if (= (vla-get-lock x) :vlax-true)		
		(SETQ laylst_suo (APPEND laylst_suo (LIST (vla-get-Name x)))));上锁的图层名表
	    )
  (setq str "")
  (foreach x laylst_suo
	   (setq str (strcat "," x str))
	   )
  (substr str 2)
  )

(defun chatukuang (tk_blfile / entname gx_list i pick_date pt scale ss_tk ss-9 ss-9a );;插图框主函数
  (command "undo" "be")
					;(setq tk_BLfile (getfiled "选择图框文件" "" "dwg" 8))
  (setvar "INSUNITS" 4)
  (setq  pick_date (entget (car (entsel "\n点选矩形框:"))) GX_list '() GX_list (list (assoc 0 pick_date) (assoc 8 pick_date) (assoc 70 pick_date))) (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list)))
  (prompt "\n选择要处理对象 / 全选<空格>：")
  (if (not (setq ss_tk (ssget GX_list))) (setq ss_tk (ssget "x"  GX_list)))
  (setq i -1) 
  (while (setq entname (ssname ss_tk (setq i (1+ i))))
    (setq ss-9 (ssadd) ss-9 (ssadd entname ss-9) pt (ss9pt ss-9 1))
    (if (<= (/ (distance (ss9pt ss-9 1) (ss9pt ss-9 3)) (distance (ss9pt ss-9 1) (ss9pt ss-9 7))) (/ 594. 420)) (setq scale (/ (distance (ss9pt ss-9 1) (ss9pt ss-9 7)) 420.))  (setq scale (/ (distance (ss9pt ss-9 1) (ss9pt ss-9 3)) 594.)));if
    (command "INSERT" tk_BLfile "non" pt scale "" 0  "move" (setq ss-9a (ssadd) ss-9a (ssadd (entlast) ss-9a)) "" "non" (ss9pt ss-9a 5) "non" (ss9pt ss-9 5))
    (entdel entname)
    );while	
  (command "undo" "e")
  (princ)
  )

(defun chatukuang_tuzhong (/ entname gx_list i pick_date pt5a pt5b scale ss_tk ss-9 tukuang_block);;插图框主函数-图中块
  (command "undo" "be")
  (setq tukuang_block (ssget ":s" (list '(0 . "insert"))) pt5a (ss9pt tukuang_block 5))
  (sssetfirst nil tukuang_block) 
  (setq  pick_date (entget (car (entsel "\n点选矩形框:"))) GX_list '() GX_list (list (assoc 0 pick_date) (assoc 8 pick_date) (assoc 70 pick_date))) 
  (sssetfirst nil nil)
  (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list)))
  (prompt "\n选择要处理对象 / 全选<空格>：")
  (if (not (setq ss_tk (ssget GX_list))) (setq ss_tk (ssget "x"  GX_list))) 
  (setq i -1) 
  (while (setq entname (ssname ss_tk (setq i (1+ i))))
    (setq ss-9 (ssadd) ss-9 (ssadd entname ss-9) pt5b (ss9pt ss-9 5) scale (/ (distance (ss9pt ss-9 1) (ss9pt ss-9 9)) (distance (ss9pt tukuang_block 1) (ss9pt tukuang_block 9) )))		
    (command "COPY" tukuang_block "" "non" pt5a  "non" pt5b  "SCALE" (entlast) "" "non" pt5b  scale)
    (entdel entname)
    );while	
  (command "undo" "e")
  (princ)
  
  
  
  
  )


					;==========================================================================主处理函数

;;块图框处理主程序
(defun tukuang_BK (H-S 9NB NB NB0 ang / all_lst all_lst_px entname i ss_tk suo_str tk+tz tk+tz+jdpt_lst tkname) 
  (command "undo" "be")
  (if (= YMK_TMP 0)
      (progn (setq tkname (cdr (assoc 2 (entget (setq entname (car (entsel "\n点选图框块:")))))))
	     (prompt "\n选择要处理对象 / 全选<空格>：")
	     (if (not (setq ss_tk (ssget (list (cons 0 "INSERT") (cons 2 tkname))))) (setq ss_tk (ssget"x"  (list (cons 0 "INSERT") (cons 2 tkname)))));;图框块选集(同名块)
	     )
      (setq ss_tk (ssget (list (cons 0 "INSERT"))));图框块选集(异名块)
      );if 
  (sssetfirst nil ss_tk)
  (setq i -1 tk+tz+jdpt_lst '() ALL_lst '()) 
  (while (setq entname (ssname ss_tk (setq i (1+ i))))
    
    (setq 	tk+tz (ssget "c" (car (enbox entname)) (cadr (enbox entname))));单张图纸选集
    (setq tk+tz+jdpt_lst	(list tk+tz (car (enbox entname)) entname));;单张图纸选集+左下角点+本图框名 的表
    (setq ALL_lst (cons tk+tz+jdpt_lst ALL_lst));;各自小表合并做大表
    
    );while
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■	
  (if (= RB_paixuFS 1) (cond ((= H-S 0) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (car (cadr a)) (car (cadr b)))))));;横排时按对象1点的x值小到大排序，得到新表
		             ((= H-S 1) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))));;竖排时按对象1点的y值小到大排序，得到新表
	                     );cond
      (setq ALL_lst_px (reverse ALL_lst));选择顺序排列
      );if
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■	
  (if (= cp_xx 0) (DQ_BK ALL_lst_px H-S 9NB) (PL_BK ALL_lst_px H-S NB NB0 ang))	
  (command "undo" "e")
  (princ)	
  )

					;--------------------------------------------------------------------------------
;;矩形框处理主程序
(defun juxing_WL (H-S 9NB NB NB0 ang / all_lst all_lst_px entname gx_list i pick_date ss_tk suo_str tk+tz tk+tz+jdpt_lst) 
  (command "undo" "be")
  (if (= BGX_TMP 0)
      (progn (setq  pick_date (entget (car (entsel "\n点选矩形框:"))) GX_list '() GX_list (list (assoc 0 pick_date) (assoc 8 pick_date) (assoc 70 pick_date))) (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list)))
	     (prompt "\n选择要处理对象 / 全选<空格>：")
	     (if (not (setq ss_tk (ssget GX_list))) (setq ss_tk (ssget "x"  GX_list)));;矩形框选集(同层、同色)
	     )
      (setq ss_tk (ssget (list (cons 0 "LWPOLYLINE") (cons 70 1))));矩形框选集(不共性)
      );if 
  (sssetfirst nil ss_tk)

  (setq i -1 tk+tz+jdpt_lst '() ALL_lst '()) 
  (while (setq entname (ssname ss_tk (setq i (1+ i))))
    
    (setq 	tk+tz (ssget "c" (car (enbox entname)) (cadr (enbox entname))));单张图纸选集
    (setq tk+tz+jdpt_lst	(list tk+tz (car (enbox entname)) entname));;单张图纸选集+左下角点+本图框名 的表
    (setq ALL_lst (cons tk+tz+jdpt_lst ALL_lst));;各自小表合并做大表
    
    );while
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  (if (= RB_paixuFS 1) (cond ((= H-S 0) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (car (cadr a)) (car (cadr b)))))));;横排时按对象1点的x值小到大排序，得到新表
		             ((= H-S 1) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))));;竖排时按对象1点的y值小到大排序，得到新表
	                     );cond
      (setq ALL_lst_px (reverse ALL_lst));选择顺序排列
      );if
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  (if (= cp_xx 0) (DQ_BK ALL_lst_px H-S 9NB) (PL_BK ALL_lst_px H-S NB NB0 ang))	
  (command "undo" "e")
  (princ)	
  )

					;--------------------------------------------------------------------------------
;;手工选择处理主程序
(defun select_SG (H-S 9NB NB NB0 ang / all_lst all_lst_px entname i p1 p2 rec ss_tk suo_str tk+tz tk+tz+jdpt_lst)
  (command "undo" "be")
  (setq ss_tk (ssadd))
  (prompt "\n每框选一次为一张图，右键结束选择：")	
  (if (= CKB_TMP 0)
      (while (setq p1 (getpoint "\n点1："))
	(setq p2 (getcorner p1 "\n点2："))
	(command "RECTANG" "non" p1 "non" p2 "CHANGE" (setq rec (entlast)) "" "p" "c" 224 "")
	(setq ss_tk (ssadd rec ss_tk))
	);while
      (while (setq p1 (getpoint "\n点1："))
	(setq p2 (getcorner p1 "\n点2："))
	(setq ss_tk (ssadd (ckb_rec p1 p2) ss_tk))
	)		
      );if
  
  (sssetfirst nil ss_tk)	
  (setq i -1 tk+tz+jdpt_lst '() ALL_lst '()) 
  (while (setq entname (ssname ss_tk (setq i (1+ i))))
    
    (setq 	tk+tz (ssget "c" (car (enbox entname)) (cadr (enbox entname))));单张图纸选集
    (setq tk+tz+jdpt_lst	(list tk+tz (car (enbox entname)) entname));;单张图纸选集+左下角点+本图框名 的表
    (setq ALL_lst (cons tk+tz+jdpt_lst ALL_lst));;各自小表合并做大表
    
    );while
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  (if (= RB_paixuFS 1) (cond ((= H-S 0) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (car (cadr a)) (car (cadr b)))))));;横排时按对象1点的x值小到大排序，得到新表
		             ((= H-S 1) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))));;竖排时按对象1点的y值小到大排序，得到新表
	                     );cond
      (setq ALL_lst_px (reverse ALL_lst));选择顺序排列
      );if
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■	
  (if (= cp_xx 0) (DQ_BK ALL_lst_px H-S 9NB) (PL_BK ALL_lst_px H-S NB NB0 ang))		
  (command "undo" "e")
  (princ)	
  )

					;--------------------------------------------------------------------------------
;;自动识别图框画矩形框-处理主程序
(defun zidongshibie_WL (H-S 9NB NB NB0 ang / all_lst all_lst_px entname gx_list i pick_date ss_tk suo_str tk+tz tk+tz+jdpt_lst) 
  (command "undo" "be")
  
  (setq ss_tk (maketukuangxian) ss_tk (quchongfu ss_tk));;自动识别图框画框
  (sssetfirst nil ss_tk)	
  
  (setq i -1 tk+tz+jdpt_lst '() ALL_lst '()) 
  (while (setq entname (ssname ss_tk (setq i (1+ i))))
    
    (setq 	tk+tz (ssget "c" (car (enbox entname)) (cadr (enbox entname))));单张图纸选集
    (setq tk+tz+jdpt_lst	(list tk+tz (car (enbox entname)) entname));;单张图纸选集+左下角点+本图框名 的表
    (setq ALL_lst (cons tk+tz+jdpt_lst ALL_lst));;各自小表合并做大表
    
    );while
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  (if (= RB_paixuFS 1) (cond ((= H-S 0) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (car (cadr a)) (car (cadr b)))))));;横排时按对象1点的x值小到大排序，得到新表
		             ((= H-S 1) (setq ALL_lst_px (vl-sort ALL_lst '(lambda (a b) (< (cadr (cadr a)) (cadr (cadr b)))))));;竖排时按对象1点的y值小到大排序，得到新表
	                     );cond
      (setq ALL_lst_px (reverse ALL_lst));选择顺序排列
      );if
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  (if (= cp_xx 0) (DQ_BK ALL_lst_px H-S 9NB) (PL_BK ALL_lst_px H-S NB NB0 ang))	
  (command "undo" "e")
  (if (= LB_TMP 5) (command "ERASE" ss_tk ""))
  (princ)	
  )



;; 更新增加阵列代码0309 ◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆◆

(defun zhenlie_PX (lst / zlpx_list);从左到右从上到下排序
  (setq ZLPX_list (vl-sort (vl-sort lst '(lambda (s1 s2) (> (cadadr s1) (cadadr s2)))) 
			   '(lambda (s3 s4) (if(equal (cadadr s3) (cadadr s4) 0.6)(< (caadr s3) (caadr s4))))
			   )
	
	)
  ZLPX_list
  )


(defun zuo>>you_pl (lst pt d flag / ent_pt entname entname1 i);从左到右横向单排子函数
  (command "undo" "be") 
  (setq i -1)
  (repeat (length lst)
	  (cond ((= flag 1) (setq entname (car (nth  (setq i (1+ i)) lst)) ent_pt (cadr (nth i lst))))
		((= flag 2) (setq entname (car (nth  (setq i (1+ i)) lst)) ent_pt (cadr (nth i lst))))
		((= flag 3) (setq entname (car (nth  (setq i (1+ i)) lst)) ent_pt (cadr (nth i lst))) (setq entname1 (caddr (nth i lst))))
		
		);cond
	  (command "move" entname "" "non" ent_pt  "non" pt)
	  (cond ((= flag 1) (setq pt (polar pt 0 d))) ;中>中
		((= flag 2) (setq pt (polar pt 0 (+ d (- (car (cadr (enbox entname))) (car (car (enbox entname))))) )))	;边>边(单体)
		((= flag 3) (setq pt (polar pt 0 (+ d (- (car (cadr (enbox entname1))) (car (car (enbox entname1))))) )))	;边>边(选集)
		
		);cond
	  )
  (command "undo" "e") 
  )

(defun zhenlie_DT (ZL_w ZL_h ZL_nb JJ_x / all_lst all_lst_px entname entname+jdpt_lst enx gx_list i ii iii p0 pick_date ss ssall x_lst);;单体阵列主程序

  (command "undo" "be") 
					;==========================================================================
					;单体选择方式处理
  (cond   ((= LB_TMP 1)
	   (if (= SX_TMP 1)		    
	       (progn (setq  pick_date (entget (car (entsel "\n点选源对象:"))) GX_list '())
		      
		      (if (= (cdr (assoc 0 pick_date)) "INSERT")  
			  (setq  GX_list (list (assoc 0 pick_date) (assoc 2 pick_date)));图块类
			  (progn (setq  GX_list (list (assoc 0 pick_date) (assoc 8 pick_date))) (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list))));其他类
			  
			  );;收集共性组码
		      
		      (prompt "\n选择要处理对象 / 全选<空格>：")
		      (if (not (setq ssall (ssget GX_list))) (setq ssall (ssget "x"  GX_list)));;共性选集(同层、同色)
		      
		      (sssetfirst nil ssall)
		      (setq    ss ssall)
		      
		      );progn							
	       (setq ssall (ssget)  ss ssall)))
	  );cond		
					;==========================================================================		
					;(setq ss (ssget))
  (setq i -1 entname+jdpt_lst '() ALL_lst '()) 
  (if (= t_tog_zhongdian 0)
      (progn (while (setq entname (ssname ss (setq i (1+ i))))			  
	       (setq entname+jdpt_lst	(list entname (car (enbox entname))));;单对象名+左下角点 的小表
	       (setq ALL_lst (cons entname+jdpt_lst ALL_lst));;各自小表合并做大表      		
	       );while
	     (if (= RB_paixuFS 1) 
		 (setq ALL_lst_px (zhenlie_PX ALL_lst));;自动排序后的总表
		 (setq ALL_lst_px (reverse ALL_lst));选择顺序排序
		 );if	
	     (setq p0 (getpoint "\n选择阵列起点<退出>: "))
	     
	     (setq ii -1 iii 0) 	
	     (repeat (fix (+ (/ (length ALL_lst_px) (float ZL_nb)) 0.9))
		     (setq X_lst '())
		     (repeat ZL_nb 
			     (if  (setq enx (nth  (setq ii (1+ ii)) ALL_lst_px))
				  (setq X_lst  (cons enx X_lst))
				  )
			     );repeat1
		     (setq X_lst (reverse X_lst))
		     (cond ((= JJ_x 1) (zuo>>you_pl X_lst p0 ZL_w 1) (setq p0 (polar p0  (* 1.5 pi) ZL_h)));;处理一个小横排再计算下一个小横排基点--中>中 的距离
			   ((= JJ_x 2) (zuo>>you_pl X_lst p0 ZL_w 2) (if  (nth  (setq iii  (+ iii ZL_nb)) ALL_lst_px) (setq p0 (polar p0  (* 1.5 pi) (+ ZL_h (- (cadr (cadr (enbox (car (nth  iii ALL_lst_px))))) (cadr (car (enbox (car (nth  iii ALL_lst_px)))))))))));边>边 的距离
			   
			   );cond			
		     );repeat2
	     );progn-左下角基点
      
      
      (progn (while (setq entname (ssname ss (setq i (1+ i))))
	       
	       (setq ss-9 (ssadd) ss-9 (ssadd entname ss-9))	 
	       (setq entname+jdpt_lst	(list entname (ss9pt ss-9 5)));;单对象名+中心点 的小表
	       (setq ALL_lst (cons entname+jdpt_lst ALL_lst));;各自小表合并做大表      		
	       );while
	     (if (= RB_paixuFS 1) 
		 (setq ALL_lst_px (zhenlie_PX ALL_lst));;自动排序后的总表
		 (setq ALL_lst_px (reverse ALL_lst));选择顺序排序
		 );if	
	     (setq p0 (getpoint "\n选择阵列起点<退出>: "))
	     
	     (setq ii -1 iii 0) 	
	     (repeat (fix (+ (/ (length ALL_lst_px) (float ZL_nb)) 0.9))
		     (setq X_lst '() H0 (/ (- (cadr (cadr (enbox (car (nth  iii ALL_lst_px))))) (cadr (car (enbox (car (nth  iii ALL_lst_px)))))) 2))
		     (repeat ZL_nb 
			     (if  (setq enx (nth  (setq ii (1+ ii)) ALL_lst_px))
				  (setq X_lst  (cons enx X_lst))
				  )
			     );repeat1
		     (setq X_lst (reverse X_lst))
		     (cond ((= JJ_x 1) (zuo>>you_pl X_lst p0 ZL_w 1) (setq p0 (polar p0  (* 1.5 pi) ZL_h)));;处理一个小横排再计算下一个小横排基点--中>中 的距离
			   ((= JJ_x 2) (zuo>>you_pl X_lst p0 ZL_w 2) (if  (nth  (setq iii  (+ iii ZL_nb)) ALL_lst_px) (setq p0 (polar p0  (* 1.5 pi) (+ (+ ZL_h (/ (- (cadr (cadr (enbox (car (nth  iii ALL_lst_px))))) (cadr (car (enbox (car (nth  iii ALL_lst_px)))))) 2) )  H0)))));边>边 的距离
			   
			   );cond			
		     );repeat2
	     );progn-中心基点
      
      
      )
  (command "undo" "e")
  (princ)		
  )

(defun zhenlie_SS (ZL_w ZL_h ZL_nb JJ_x / all_lst all_lst_px entname enx gx_list i ii iii p0 p1 p2 pick_date rec ss ss_tk tk+tz tk+tz+jdpt_lst tkname x_lst);;选集阵列主程序
  (command "undo" "be") 
					;==========================================================================
					;分类别选择处理
  (cond   ((= LB_TMP 2)
	   (if (= YMK_TMP 0)
	       (progn (setq tkname (cdr (assoc 2 (entget (setq entname (car (entsel "\n点选图框块:")))))))
		      (prompt "\n选择要处理对象 / 全选<空格>：")
		      (if (not (setq ss_tk (ssget (list (cons 0 "INSERT") (cons 2 tkname))))) (setq ss_tk (ssget"x"  (list (cons 0 "INSERT") (cons 2 tkname)))));;图框块选集(同名块)
		      )
	       (setq ss_tk (ssget (list (cons 0 "INSERT"))));图框块选集(异名块)
	       );if 
	   (setq ss ss_tk)
	   (sssetfirst nil ss)											
	   );图块类别
	  
	  ((= LB_TMP 3)   
	   (if (= BGX_TMP 0)
	       (progn (setq  pick_date (entget (car (entsel "\n点选矩形框:"))) GX_list '() GX_list (list (assoc 0 pick_date) (assoc 8 pick_date) (assoc 70 pick_date))) (if (assoc 62 pick_date) (setq GX_list (cons  (assoc 62 pick_date) GX_list)))
		      (prompt "\n选择要处理对象 / 全选<空格>：")
		      (if (not (setq ss_tk (ssget GX_list))) (setq ss_tk (ssget "x"  GX_list)));;矩形框选集(同层、同色)
		      )
	       (setq ss_tk (ssget (list (cons 0 "LWPOLYLINE") (cons 70 1))));矩形框选集(不共性)
	       );if 
	   (setq ss ss_tk)
	   (sssetfirst nil ss)	
	   );矩形框类别
	  
	  ((= LB_TMP 4)    
	   (setq ss_tk (ssadd))
	   (prompt "\n每框选一次为一张图，右键结束选择：")	
	   (if (= CKB_TMP 0)
	       (while (setq p1 (getpoint "\n点1："))
		 (setq p2 (getcorner p1 "\n点2："))
		 (command "RECTANG" "non" p1 "non" p2 "CHANGE" (setq rec (entlast)) "" "p" "c" 224 "")
		 (setq ss_tk (ssadd rec ss_tk))
		 );while
	       (while (setq p1 (getpoint "\n点1："))
		 (setq p2 (getcorner p1 "\n点2："))
		 (setq ss_tk (ssadd (ckb_rec p1 p2) ss_tk))
		 )		
	       );if
	   (setq ss ss_tk)
	   (sssetfirst nil ss)	
	   );手工框选类别	
	  
	  ((= LB_TMP 5)   
					;(maketukuangxian)
	   (setq ss (maketukuangxian) ss (quchongfu ss))
	   (sssetfirst nil ss)	
	   );自动识别类别
	  
	  );cond
					;==========================================================================		
					;(setq ss (ssget))
  (setq i -1 tk+tz+jdpt_lst '() ALL_lst '()) 
  (while (setq entname (ssname ss (setq i (1+ i))))
    (setq 	tk+tz (ssget "c" (car (enbox entname)) (cadr (enbox entname))));图框,图纸内容选集		
    (setq tk+tz+jdpt_lst	(list tk+tz (car (enbox entname)) entname));;图框,图纸内容选集+左下角点+图框图元名 的单张小表
    (setq ALL_lst (cons tk+tz+jdpt_lst ALL_lst));;各自小表合并做大表      		
    );while
  
  (if (= RB_paixuFS 1) 
      (setq ALL_lst_px (zhenlie_PX ALL_lst));;自动排序后的总表
      (setq ALL_lst_px (reverse ALL_lst));选择顺序排序
      );if
  (setq p0 (getpoint "\n选择阵列起点<退出>: "))
  (command "-layer" "u" (setq suo_str (layer_suo_str)) "");开锁
  (setq ii -1 iii 0) 	
  (repeat (fix (+ (/ (length ALL_lst_px) (float ZL_nb)) 0.999))
	  (setq X_lst '())
	  (repeat ZL_nb 
		  (if  (setq enx (nth  (setq ii (1+ ii)) ALL_lst_px))
		       (setq X_lst  (cons enx X_lst))
		       )
		  );repeat1
	  (setq X_lst (reverse X_lst))
	  (cond ((= JJ_x 1) (zuo>>you_pl X_lst p0 ZL_w 1) (setq p0 (polar p0  (* 1.5 pi) ZL_h)));;处理一个小横排再计算下一个小横排基点--中>中 的距离
		((= JJ_x 3) (zuo>>you_pl X_lst p0 ZL_w 3) (if (nth  (setq iii (+ iii ZL_nb)) ALL_lst_px) (setq p0 (polar p0  (* 1.5 pi) (+ ZL_h (- (cadr (cadr (enbox (caddr (nth  iii ALL_lst_px))))) (cadr (car (enbox (caddr (nth  iii ALL_lst_px)))))))))));边>边 的距离			
		);cond			
	  );repeat2	
  (command "undo" "e")
  (command "-layer" "lo"  suo_str  "")
  (if (= LB_TMP 5) (command "ERASE" ss "")) 
  (princ)		
  )

(defun zhenlie_FUZHI (ZL_nby ZL_nbx ZL_h ZL_w / lastent pt0 pt1 ss);;复制阵列主程序
  (command "undo" "be")	
  (setq ss (ssget) pt0 (ss9pt ss 5) pt1 (getpoint "\n选取阵列起点/原位<空格>："))
  (if (= RB_zhenliejianju 2) (setq ZL_h (if (>= ZL_h 0) (+ ZL_h (distance (ss9pt ss 1) (ss9pt ss 7))) (- ZL_h (distance (ss9pt ss 1) (ss9pt ss 7)))) ZL_w (if (>= ZL_w 0) (+ ZL_w (distance (ss9pt ss 1) (ss9pt ss 3))) (- ZL_w (distance (ss9pt ss 1) (ss9pt ss 3)))))) 	 
  (if pt1 (command "move" ss "" "non" pt0 "non" pt1))	
  (command "ARRAY" ss "" "r" ZL_nby ZL_nbx ZL_h ZL_w)
  (command "undo" "e")
  (princ)	
  )


(defun maketukuangxian (/ bound e entx gx_list i lastent lst pick_date rects ss ss_last);;自动识别图框画矩形框子函数

  (if (= zdSX_TMP 0) (progn (setq  pick_date (entget (car (entsel "\n点选图框最外边框:"))) GX_list '() GX_list (list (assoc 0 pick_date) (assoc 8 pick_date) ))
			    (setq ss (ssget GX_list)));progn
      (setq ss (ssget '((0 . "LWPOLYLINE,INSERT"))))
      );if
  
  (repeat (setq i (sslength ss))
	  (setq e (ssname ss (setq i (1- i))))
	  (setq lst (cons (ebox e) lst)) ;_提取边界对角点，不生产矩形
	  )
  (setq lst (vl-sort lst '(lambda (x1 x2) (> (area x1) (area x2))))) ;_按面积大小排序
  (while lst
    (setq rects (cons (car lst) rects)) ;_矩形对角点集
    (setq bound (pt4 (car lst))) ;_矩形边界
    (setq lst (vl-remove-if '(lambda (x) (and (PtInPoly (car x) bound) (PtInPoly (cadr x) bound))) (cdr lst))) ;_移除大矩形边界内的小矩形
    )
  (setq lastent (entlast) ss_last (ssadd))
  (mapcar '(lambda (x) (command "rectang" (car x) (cadr x))) rects) ;_批量生成矩形
  (while (setq entx (entnext lastent))
    (setq ss_last (ssadd entx ss_last) lastent entx)
    )
  
  (command "CHANGE" ss_last "" "p" "c" 224 "")
  ss_last 
  )

(defun ebox (e / pa pb)
  (and (= 'ename (type e)) (setq e (vlax-ename->vla-object e)))
  (vlax-invoke-method e 'GetBoundingBox 'pa 'pb)
  (setq pa (trans (vlax-safearray->list pa) 0 1)
        pb (trans (vlax-safearray->list pb) 0 1)
	)
  (list pa pb)
  )
(defun area (pts) (apply '* (cdr (reverse (apply 'mapcar (cons '- pts)))))) ;_求面积
(defun pt4 (pt2)
  (list (car pt2) (list (caadr pt2) (cadar pt2)) (cadr pt2) (list (caar pt2) (cadadr pt2)))
  ) ;_对角点生成四角点
(defun PtInPoly (pt pts)
  (equal pi
         (abs
          (apply '+ (mapcar '(lambda (x y) (rem (- (angle pt x) (angle pt y)) pi)) (cons (last pts) pts) pts))
          )
         1e-6
	 )
  ) ;_点是否在凸多边形内（角度法）

(defun quchongfu (ss / ent ent1 i ii pt pt1) 
  (setq i -1 ss1 (ssadd)) 
  (while (setq ent (ssname ss (setq i (1+ i))))		
    (setq DJ_lst (ebox ent))	  
    (setq ii i) 
    (while (setq ent1 (ssname ss (setq ii (1+ ii))))
      (setq DJ_lst1 (ebox ent1))
      (if (equal DJ_lst DJ_lst1  0.01) (setq ss1 (ssadd ent1 ss1)))		
      );while	 
    );while
  (command "ERASE" ss1 "")
  ss
  )



;;;=================================================================*
;;;生成日期：20200318.175218
;;;本文件由程序自动生成。                                           *
;;;程序生成完成后需将主代码“*.lsp”文件中的语句中的     *
;;; (load_dialog 双引号*.Dcl双引号)改为(load_dialog (make-dcl)) 方可用            *
;;;修改后的代码可编辑到主LISP程序后方运行                                                                 *
;;;=================================================================*
;;;为能让多个有本程序生成的DCL.lsp可以同时使用，生成程序后应将对话框名 (make-dcl)改名   *
;;;供需修改两处地方，一处为加载的地方(load_dialog (？？？-make-dcl)) ，另一处为       *
;;;对话框主程序名(defun ？？？-make-dcl    ，一定要一致                      *
;;;示例：(make-dcl)                                                 *
(defun make-dcl-pl  (/ lst_str str file f)
  (setq lst_str '(
		  "/*★★★★★ListDCL @ fsxm.mjtd.com★★★★★*/"
		  ""
		  "rect01:dialog {"
		  "    label = \"【对齐/排列/图框排版】ST0318\" ;"
		  "    :spacer {}"
		  "    :row {"
		  "        :boxed_column {"
		  "            key = \"k_paixuFS\" ;"
		  "            label = \"排序方式\" ;"
		  "            :radio_button {"
		  "                key = \"k_rb_zidong\" ;"
		  "                label = \"自动\" ;"
		  "            }"
		  "            :radio_button {"
		  "                key = \"k_rb_xuanxu\" ;"
		  "                label = \"选序\" ;"
		  "            }"
		  "        }"
		  "        :boxed_column {"
		  "            label = \"选取方式\" ;"
		  "            :row {"
		  "                key = \"k_no1\" ;"
		  "                :spacer {"
		  "                    width = 1.5 ;"
		  "                }"
		  "                :radio_button {"
		  "                    key = \"xlb1\" ;"
		  "                    label = \"单体\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :radio_button {"
		  "                    key = \"xlb2\" ;"
		  "                    label = \"块图框\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :radio_button {"
		  "                    key = \"xlb3\" ;"
		  "                    label = \"矩形框\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :radio_button {"
		  "                    key = \"xlb4\" ;"
		  "                    label = \"手工选\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :radio_button {"
		  "                    key = \"xlb5\" ;"
		  "                    label = \"自动\" ;"
		  "                }"
		  "            }"
		  "            :row {"
		  "                key = \"k_no2\" ;"
		  "                :spacer {"
		  "                    width = 1.5 ;"
		  "                }"
		  "                :toggle {"
		  "                    key = \"toggle_sx\" ;"
		  "                    label = \"速选\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :toggle {"
		  "                    key = \"toggle_ymk\" ;"
		  "                    label = \"异名块\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :toggle {"
		  "                    key = \"toggle_bgx\" ;"
		  "                    label = \"非共性\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :toggle {"
		  "                    key = \"toggle_ckb\" ;"
		  "                    label = \"边框比\" ;"
		  "                }"
		  "                :spacer {}"
		  "                :toggle {"
		  "                    key = \"toggle_sx2\" ;"
		  "                    label = \"速选\" ;"
		  "                }"
		  "            }"
		  "        }"
		  "    }"
		  "    :image {"
		  "        color = 1 ;"
		  "        height = 0.12 ;"
		  "    }"
		  "    :spacer {}"
		  "    :row {"
		  "        :column {"
		  "            :row {"
		  "                :row {"
		  "                    key = \"row_sxdq\" ;"
		  "                    label = \"双向对齐\" ;"
		  "                    :column {"
		  "                        :button {"
		  "                            key = \"dtpl7\" ;"
		  "                            label = \"╔\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"dtpl8\" ;"
		  "                            label = \"╠\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"dtpl9\" ;"
		  "                            label = \"╚\" ;"
		  "                        }"
		  "                    }"
		  "                    :column {"
		  "                        :button {"
		  "                            key = \"dtpl10\" ;"
		  "                            label = \"╦\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"dtpl11\" ;"
		  "                            label = \"╬\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"dtpl12\" ;"
		  "                            label = \"╩\" ;"
		  "                        }"
		  "                    }"
		  "                    :column {"
		  "                        :button {"
		  "                            key = \"dtpl13\" ;"
		  "                            label = \"╗\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"dtpl14\" ;"
		  "                            label = \"╣\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"dtpl15\" ;"
		  "                            label = \"╝\" ;"
		  "                        }"
		  "                    }"
		  "                }"
		  "                :column {"
		  "                    key = \"k_shuipingduiqi\" ;"
		  "                    label = \"水平对齐\" ;"
		  "                    :button {"
		  "                        key = \"dtpl1\" ;"
		  "                        label = \"┳\" ;"
		  "                    }"
		  "                    :button {"
		  "                        key = \"dtpl2\" ;"
		  "                        label = \"━\" ;"
		  "                    }"
		  "                    :button {"
		  "                        key = \"dtpl3\" ;"
		  "                        label = \"┻\" ;"
		  "                    }"
		  "                }"
		  "                :column {"
		  "                    key = \"k_chuizhiduiqi\" ;"
		  "                    label = \"垂直对齐\" ;"
		  "                    :button {"
		  "                        key = \"dtpl4\" ;"
		  "                        label = \"┣\" ;"
		  "                    }"
		  "                    :button {"
		  "                        key = \"dtpl5\" ;"
		  "                        label = \"┃\" ;"
		  "                    }"
		  "                    :button {"
		  "                        key = \"dtpl6\" ;"
		  "                        label = \"┫\" ;"
		  "                    }"
		  "                }"
		  "            }"
		  "            :spacer {"
		  "                height = 0.5 ;"
		  "            }"
		  "            :row {"
		  "                height = 1 ;"
		  "                key = \"k_dengju,dianxuan\" ;"
		  "                :column {"
		  "                    :toggle {"
		  "                        alignment = centered ;"
		  "                        key = \"cp_x\" ;"
		  "                        label = \"等距重排\" ;"
		  "                    }"
		  "                }"
		  "                :column {"
		  "                    :edit_box {"
		  "                        alignment = top ;"
		  "                        key = \"cp_d\" ;"
		  "                        label = \"间距:\" ;"
		  "                    }"
		  "                }"
		  "                :column {"
		  "                    :button {"
		  "                        alignment = top ;"
		  "                        key = \"dxjj\" ;"
		  "                        label = \"点选定距\" ;"
		  "                    }"
		  "                }"
		  "                :spacer {}"
		  "            }"
		  "        }"
		  "        :column {"
		  "            :row {"
		  "                label = \"阵列\" ;"
		  "                :column {"
		  "                    :row {"
		  "                        :text {"
		  "                            label = \"方式:\" ;"
		  "                        }"
		  "                        :radio_button {"
		  "                            key = \"k_rb_chongpaizhenlie\" ;"
		  "                            label = \"重排阵列\" ;"
		  "                        }"
		  "                        :radio_button {"
		  "                            key = \"k_rb_fuzhizhenlie\" ;"
		  "                            label = \"复制阵列\" ;"
		  "                        }"
		  "                    }"
		  "                    :row {"
		  "                        :edit_box {"
		  "                            key = \"k_hengju\" ;"
		  "                            label = \"横距:\" ;"
		  "                        }"
		  "                        :edit_box {"
		  "                            key = \"k_hengshu\" ;"
		  "                            label = \"个数:\" ;"
		  "                        }"
		  "                    }"
		  "                    :row {"
		  "                        :edit_box {"
		  "                            key = \"k_shuju\" ;"
		  "                            label = \"竖距:\" ;"
		  "                        }"
		  "                        :edit_box {"
		  "                            key = \"k_shushu\" ;"
		  "                            label = \"个数:\" ;"
		  "                        }"
		  "                    }"
		  "                    :spacer {}"
		  "                    :row {"
		  "                        :text {"
		  "                            label = \"算法:\" ;"
		  "                        }"
		  "                        :radio_button {"
		  "                            key = \"k_rb_zhongzhong\" ;"
		  "                            label = \"点<->点\" ;"
		  "                        }"
		  "                        :spacer {}"
		  "                        :radio_button {"
		  "                            key = \"k_rb_bianbian\" ;"
		  "                            label = \"边<->边\" ;"
		  "                        }"
		  "                        :spacer {}"
		  "                    }"
		  "                    :row {"
		  "                        :text {"
		  "                            label = \"基点:\" ;"
		  "                        }"
		  "                        :toggle {"
		  "                            key = \"k_tog_zhongdian\" ;"
		  "                            label = \"中点\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"k_bt_dianxuan\" ;"
		  "                            label = \"选距\" ;"
		  "                        }"
		  "                        :button {"
		  "                            key = \"k_zlqueding\" ;"
		  "                            label = \"确定\" ;"
		  "                        }"
		  "                    }"
		  "                }"
		  "            }"
		  "        }"
		  "    }"
		  "    :spacer {}"
		  "    :column {"
		  "        label = \"插图框\" ;"
		  "        :row {"
		  "            :spacer {}"
		  "            :radio_button {"
		  "                key = \"k_moren\" ;"
		  "                label = \"默认图块\" ;"
		  "            }"
		  "            :radio_button {"
		  "                key = \"k_tuzhong\" ;"
		  "                label = \"图中选块\" ;"
		  "            }"
		  "            :radio_button {"
		  "                key = \"k_chawenjian\" ;"
		  "                label = \"插块文件 >>>\" ;"
		  "            }"
		  "            :button {"
		  "                key = \"k_liulan\" ;"
		  "                label = \"浏览\" ;"
		  "            }"
		  "            :button {"
		  "                alignment = right ;"
		  "                key = \"k_ctk\" ;"
		  "                label = \"插图框\" ;"
		  "            }"
		  "        }"
		  "        :text {"
		  "            alignment = left ;"
		  "            key = \"k_lujing\" ;"
		  "        }"
		  "        :spacer {}"
		  "    }"
		  "    :row {"
		  "        :text {"
		  "            value = \"注:1,图框不能交叉.  2, 插入的图框文件尺寸:594x420mm.\" ;"
		  "        }"
		  "        cancel_button;"
		  "    }"
		  "}"
		  )
	)
  (setq file (vl-filename-mktemp "DclTemp.dcl"))
  (setq f (open file "w"))
  (foreach str lst_str
	   (princ "\n" f)
	   (princ str f)
	   )
  (close f)
  ;;返回
  file
  )
