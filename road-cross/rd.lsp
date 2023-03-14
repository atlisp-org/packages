
;;-------------------------------------------------------------------------
;;作者:lee50310  日期:2020年 10月25日 开始撰写 
;;               日期:2020年 11月08日 增加画线找寻路宽程式 draw_line_obj.lsp
;;               日期:2020年 11月14日 修改测
;;               日期:2020年 11月16日 修改测试完成
;;               日期:2020年 11月22日 修正 cad2010无法执行,在实体图过慢
;;               日期:2020年 11月28日 修正 line 段开程式有时不能断开问题 
;;               日期:2020年 12月15日 修正 修正绘好后回前画面 
;;程式功能: 指令 rd 道路绘制  路宽滑鼠第1,2点决定
;;              srd 道路绘制  路宽键盘输入设定
;;		  	    sld 画一线取得道路宽(想在既有道路间补上一条路  若补上的路横跨三条大小不等的路 执行wed指令 画一条穿越这三条路的线 让电脑计算取得这三个路宽值 再绘制横跨的路)
;;			    std 参数设定 设定 倒圆角 默认值 3 ,  寻找道路两交点距离为路宽 度误差值  默认值 0.02
;;
;; 画制方式 滑鼠点 第一点  ,点第二(宽度) , 点第三点.....  按空白键结束

;;------------------------------------------------------------------------------
;;绘制新道路
;;道路宽由1,2点拉出 绘制道路
(defun road-cross:rd()
  
  (setq sno 1 gen 1)          ;sno=1 道路宽由滑鼠1,2点拉出距离决定 
  (Draw_the_road)
  
  );end_defun_rd 
;;-----------------------------------------------------------------------------
;;绘制新道路
					;输入道路宽,绘制道路
(defun road-cross:srd()
  
  (setq sno 2 gen 1)           ;sno=2 输入道路宽 
  (Draw_the_road)
  
  );end_defun_srd
;;-------------------------------------------------------------------------
;;画线补路
(defun road-cross:sld()

  (setq gen 2 )               ;;表画线补路
  (drl)                       ;;在既有道路上划一线取线内找到的路宽值
  (Draw_the_road)             ;;绘制道路
  (c:prd)                     ;;列出找到的路宽度直 
  )

					;----------------------------------------------------------------------------
;;道路路宽由滑鼠点击荧幕1 ,2 点距离决定
(defun draw_rd()
  
  (princ)
  (princ "\n  <<<   点击开始绘制路面 画制方式: 道路宽(按滑鼠左键)由滑鼠1,2点决定 第3点拉出方向及距离 ,距离确定按滑鼠左键,完成道路按空白键    >>>")
  (setq pt1 (getpoint "\n道路绘制 第一点"))                       ;第一点
  (setq pt2 (getpoint pt1 "\n道路绘制 第二点" ))                  ;第二点
  (setq ww (distance pt1 pt2))                           ;1,2点距离为mline宽度
  
  (setq pt3  (polar pt1 (angle pt1 pt2) (* ww 0.5) ))	 ;第三点
  
  ww                                                     ;宽度值回传
  
  );end_defun_draw_rd 
;;--------------------------------------------------------------------
;;道路 路宽输入设定
(defun draw_srd( )
  
  (princ)
  (princ "\n  <<<   开始绘制路面 画制方式: 请输入路宽 按Enter 后 (按滑鼠左键)点击图面拉出方向及距离 ,距离确定按滑鼠左键,完成道路按空白键    >>>\n")
  (if (= sw nil) (setq sw 10  ) )                                    ;宽度初设 10         
  (setq ww (getreal (strcat "默认宽度按[Space/Enter] 或 输入路宽<" (rtos sw) ">:")))
  (if (= ww nil)
      (progn
        (setq ww sw )
        (princ (strcat "\n<<<   ***   路宽=" (rtos ww 2 1)   "  倒圆角值=" (rtos kng 2 1) "  滑鼠点击荧幕 开始画 ***   >>>"))
        )
      (progn
        (setq sw ww)
        (princ (strcat "\n<<<   ***   路宽=" (rtos ww 2 1)   "  倒圆角值=" (rtos kng 2 1) "  滑鼠点击荧幕 开始画 ***   >>>"))
        )
      )
  
  (setq pt3 (getpoint "\n 道路绘制 第一点"))                ;第一点
  ww                                              ;宽度值回传
  
  )  ;end_defun_draw_srd 
;;---------------------------------------------------------------------------
;;参数输入设定
;;寻找道路宽容许误差范围值设定
(
 defun c:std()
 (princ "\n ***   寻找道路宽容许误差设定   ***")     
 (if (= zng nil) (setq zng 0.02 ) )                 ;默认误差值 0.02         
 (setq z% (getreal (strcat "\n 默认误差值按 Enter 或 输入误差值 < " (rtos zng 2 2) " >:")))
 (if (= z% nil)
     (setq z% zng )        
     (setq zng z%)
     )
 

 ;;道路线倒圆角输入设定
 
 (princ "\n ***   道路线倒圆角值设定   ***")     
 (if (= eng nil) (setq eng 3 ) )                 ;倒圆角初设 3         
 (setq kng (getint (strcat "\n 默认倒圆角按 Enter 或 输入倒圆角 < " (rtos eng) " >:")))
 (if (= kng nil)
     (setq kng eng )        
     (setq eng kng)
     )
 
 )  ;end_defun_std
					;------------------------------------------------------------------------------------
;; 参数设定
;;寻找道路宽容许误差范围值设定
(defun stda()
  
  (if (= zng nil) (setq zng 0.02 ) )             ;默认误差值 0.02         
  
  (if (= z% nil)
      (setq z% zng )        
      (setq zng z%)
      )
  
  ;;---------------------
  ;;道路线倒圆角输入设定
  
  (if (= eng nil) (setq eng 3 ) )                 ;倒圆角初设 3         
  
  (if (= kng nil)
      (setq kng eng )        
      (setq eng kng)
      )
  
  )  ;end_defun_std
					;------------------------------------------------------------------------------------	   

(defun Draw_the_road (/ dic vl ov LastEntity  ss s1 blk en mb1 mb2 xel )

  (vl-load-com)
  (SaveVars (list "osmode" "cmdecho" "orthomode" "DIMDEC"))  ;;储存目前的系统变数
  (setvar "osmode" 0)		                       ;; 关闭目标捕捉状态
  (setvar "orthomode" 0)                            ;; 关闭垂直正交  
  (setvar "WHIPARC" 1)                               ;;打开真圆度
  
  
  (if (equal (tblobjname "LAYER" "地形地貌,HTE") nil)   ;确定当前是否有地形地貌的层，如没有则执行下面操作
      (progn
	(BF-Ent-MakeLayer "HDE" 1 "Continuous" )        ;新建HDE的层，颜色为1，线型为Continuous
	(BF-Ent-MakeLayer "地形地貌" 8 "Continuous" )   ;新建地形地貌的层，颜色为8，线型为Continuous
	(vl-cmdf "-layer" "_lw" 0.05 "地形地貌" "")     ;设置此层线宽为0.05，vl-cmdf功能同command
	)
      (BF-ent-ActiveLayer "地形地貌")                 ;;;
      )

  (BF-ent-ActiveLayer "地形地貌")                    ;;"地形地貌"层设目前
  
  (set_mline)                                        ;; 设定mline线型

  (cond ((= sno 1)(stda)(setq ww (draw_rd)))                  ;;;(取倒圆角值及误差值)   (道路宽由1,2点拉出 绘制道路)
        ((= sno 2)(stda)(setq ww (draw_srd)))                 ;;;(取倒圆角值及误差值)   (输入道路宽,绘制道路)
        ((t       (stda)(setq ww (draw_rd))))                 ;;;(取倒圆角值及误差值)   (若其他值 道路宽由1,2点拉出 绘制道路 )
	)
  
  (command "_mline"  "S"  ww "")                      ;设定mline宽度
  
  
  
  (mapcar 'setvar vl ov)
  (setq vl '("CMDECHO" "PEDITACCEPT" "QAFLAGS")
        ov (mapcar 'getvar vl))
  (setq LastEntity (entlast))
  
  
  (command "_.mline" pt3)                              
  
  (while (= 1 (logand 1 (getvar 'CMDACTIVE)))
    
    (command pause)
    
    );;end_while
  
  (princ)
  (princ "\n<.....程式执行中请稍后..........>")
  
  (setq en (entlast))
  (obj_min_box en)                                           ;物件最小包围框 p1, p2 点
  (scal_x p1 p2 1.4)                                         ;将包围框的两点对角距离 放大1.4倍 ,对角两点: pt1 pt2
  (command "zoom" "w" pt1 pt2)                               ;将视窗缩放至物体大小 


  (mapcar 'setvar vl ov)
  
  (setq en (entlast))
  (command "_.chprop" en ""  "_layer" "地形地貌" "")    ;将物件更换到 "地形地貌" 层
  
  (command "_EXPLODE" en  "" )                          ;;炸开
  (setq sal  (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE")))) 
  ;;因取到的 sal 有时含中心线,有时未含中心线 故下个步骤需再对sal做判断一次
  (if (= (ssget "_P" (list (cons 8 "地形地貌")(cons 62 1))) nil) (setq t2 1) (setq t2 0))  ;判断 sal是否有含中心线 t2=1 未含 ,t2=0 有含
  (setq ss3 (ssget "C" pt1 pt2 ))                        ;;选取两侧道路线+中心线
  (command "_EXPLODE" ss3  "" )                          ;;炸开
  (setq ss3 (ssget "C" pt1 pt2 ))                        ;;再选一次
  (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE")))) ;;从ss3中再过滤只取线 *****
  (setq ss1 (ssget "_P" (list (cons 8 "地形地貌")(cons 62 1))))    ;选取中心线	
  
  ;; 注:当绘制新路或补路(无横跨道路) SAL 与 SS4 集合物数量相等 , 补一条路(有横跨道路)   SS4 > SAL
  ;;    因此判别两集合物数量大小可确认当下做何种动作
  ;;    SS4 > SAL 补一条路(有横跨道路) 则 设 ben=2  ,  否则 SS4 = SAL 绘制新路或补路(无横跨道路) ben=1
  (if (> (sslength ss4) (sslength sal)) (setq ben 2) (setq ben 1))		
  
  
  (cond 
    
    ((= ben 1)
     
     
     (setq ss (DEL_OBJ sal ss4))    ;;从 ss3选集中  删除内含sal集合物,剩其他道路线           *****
     
     (setq ssc (DEL_OBJ ss1 sal))                                 ;;纯道路外框双线 		                                               
     
     (setq ss3 (ssget "C" pt1 pt2 ))                        ;;选取两侧道路线+中心线
     (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE")))) ;;从ss3中再过滤只取线(因弧执行断开时会错误)
     (setq ss2 (DEL_OBJ ss1 ss4))                           ;;路两侧道路集合物 (无中心线)
     
     )
    
    ((= ben 2)	
     
     (setq ss7 (DEL_OBJ sal ss4))    ;;从 ss4选集中  删除内含sal集合物,剩其他道路线 (注:若sal (t2=0)有含中心线这里会被去除 )         *****
     (if (= t2 1)(setq ss (DEL_OBJ ss1 ss7))(setq ss ss7))  ;;若 t2=1 需去除中心线
     
     (setq ssc (DEL_OBJ ss1 sal))                                 ;;纯道路外框双线 
     

     ;;----------------------------------------------找出与补路有相交的道路线	 
     
     
     (progn
       (setq  obj_a nil) 
       (setq eel (tt ssc ss))                                ;;求出道路的双线与其他线 是否有相交
       (vl-cmdf "_draworder" ssc "" "b")                     ;;将纯道路外框双线至于最下方 (参数 "b" 最下方 )
       
       
       (setq  obj_a (ssadd))                                 ;;由交点串列找到物体 
       (foreach  x  eel
		 (setq mm (ssname (ssget "C" x x ) 0))
		 (setq obj_a (ssadd  mm obj_a))		               ;;物体加入选集内
		 
	         )
       
       )

     ;;---------------------------------------------	找出与补路有相交的道路线 存obj_a
     
     (setq ss2 (add_obj obj_a ssc))                          ;; 将与他交集的道路 加入纯道路线 集合物中
     
     )
    ) 
  
  
  
  (break_all ss2)                                        ;;将两侧道路线断开
  
  (sel_obj ss1)                                               ;求中线 前段起始点 pd1 及末段结束点 pd2		
  (setq pbel  (list pd1 pd2))
  
  
  (setq ss3 (ssget "C" pt1 pt2 ))                        ;;因扣除及断开后 ss3,ss2 被清除
  (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE")))) ;;从ss3中再过滤只取线	*****	
  (setq ss2 (DEL_OBJ ss1 ss4))                           ;;再一次取得 段开后 SS2 物件
  
  
  
  ;;这边由中心线 cent 交点数判断  (cent > 3)是绘制新道路 还是(cent < 3)在绘好的路面 从两侧路面中拉出一条连接路	
  (setq cent (Take_inters ss1 ) )                         ;中心线交点 
  
  
  (cond 
    ((= gen 1)                                             ;;(gen/=2 创建新路)    (gen=2 画线取距离值)
     (if (and (/= cent nil) (> (length cent) 3))          ;;绘制新路线(cent \= nil)
	 (Add_route_processing)                              ;;绘制新道路找出去除路中不必要线段的选取点                                                        
         (Pull_a_route)                                      ;;两路之间拉出一条路找出去除路中不必要线段的选取点                                  
	 
	 ) ;end_if
     )	
    
    
    ((= gen 2)(Pull_a_route))                                ;;在既有的道路上画线取各路宽值
    
    )	
  
  
  (command "erase" ss1 "")                               ;删除道路中心线
  
  ;;------------------------------------------
  ;; tel :存放中心线与道路线共同交点串列	例: tel=((3330.46 497.587 0.0) (3383.14 527.11 0.0) (3360.46 444.06 0.0) (3413.14 473.583 0.0))
  ;;道路线与中心线会在起始，结束及十字路口线的4个中点产生交点 ,因此找到这些交点便可将道路内不需要的线去除

  (del_line tel)                                        ;依串列点找路中对应的的线段 并去除之
  
  (setq ss3 (ssget "C" pt1 pt2 ))                       ;;选取两侧道路线+中心线                   
  (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE")))) ;;从ss3中再过滤只取线	****
  (pj ss4)                                              ;;把ss3选择集内 line 线接合为 pline线 ****
  
  (sel_Fillet "地形地貌" kng)                            ;只有指定的层才 倒圆角=kng   函程式( sel_Fillet "依据层名"  倒圆角值)
  
  (command "zoom" "p")                                  ;;显示前一画面范围

  (RestoreVars)                                          ;;还原储存的系统变数
  (princ)
  (princ (strcat "\n<<< 道路宽 = " (rtos ww 2 1) " >>> <<< 倒圆角值 = " (rtos kng 2 1)" >>>"))
  (princ)  
  )  ;end_defun_Draw_the_road

(defun road-cross:help ()
  (@:help "<<<  绘制路面  执行指令:rd (路宽由滑鼠拉出1,2点) 指令:srd (路宽由键盘输入)  执行指令:sld 画线找路宽 再两路间补一条路 指令:std  倒圆角,寻路宽误差值 参数设定  >>>")
  (princ))



					;******************************************************************************************************************************************************************
					;******************************************************************************************************************************************************************

;;<<<<****  处理绘制新道路后 去除路中所有不必要线的选择串列点  ****>>>-----start
(defun Add_route_processing()

  (progn		
    (setq rd_no 1) 
    (setq road (Take_inters ss2 ) )                         ;两侧道路线交点        
    (setq ss3 (ssget "C" pt1 pt2 ))                                                            
    (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE")))) ;;从ss3中再过滤只取线	  ****
    (setq rocen(Take_inters ss4 ) )                         ;两侧道路线+中心线所形成的交点    ****
    
    
    (setq sel (remove_ab cent rocen))                      ;去除中心线剩两侧道路线+其他交点
    (setq tel (remove_ab road sel))                        ;再去两侧道路线交点所剩下的点 就是在十字路口要去除的交点
    (setq  vet (ssadd))
    (foreach  x  pbel
	      (setq mm (ssname (ssget "C" x x ) 0))
	      (setq vet (ssadd  mm vet))		               ;;将头尾两端的封口线加入选集内
	      (command "erase" vet "")                      ;;删除头尾线                     
	      )
    
    );end_progn  
  );end_defun_Add_route_processing

;;<<<<****  处理绘制新道路后 去除路中所有不必要线的选择串列点  ****>>>-----end
					;------------------------------------------------------------
;;<<<<****  适用在两路之间拉出一条路 去除路中所有不必要线的选择串列点       ****>>>-----start
(defun Pull_a_route()


  (progn                                           
    
    (setq rd_no 2) 
    
    (vl-cmdf "_draworder" ss1 "" "b")                         ;;将物体至于最下方 (参数 "b" 最下方 )
    ;;说明 将中心线放置于两头尾线的下方 这样下个步骤才能确保选到头尾线	  
    
    (foreach  x  pbel                                      ;;pbel=头尾端串列点 ,由点去选择到实际线
	      (setq mm (ssname (ssget "C" x x ) 0))
	      (setq ss1 (ssadd  mm ss1))		               ;;将头尾两端的封口线也加入中心线选集内
	      )
    

    (setq ss3 (ssget "C" pt1 pt2 ))                         ;;选取两侧道路线+中心线                    ;;
    (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE"))))       ;;过滤只取线
    (setq ss2 (DEL_OBJ ss1 ss4))                           ;;路两侧道路集合物由  ss3 扣除 ss1 所得 ***
    
    (break_all ss2)                                        ;;把所有交点处线断开
    (setq ss3 (ssget "C" pt1 pt2 ))                        ;;因扣除及断开后 ss3,ss2 被清除  @@@
    (setq ss4 (ssget "_P" (list (cons 8 "地形地貌")(cons 0 "LINE"))))       ;;过滤只取线            ****
    (setq ss2 (DEL_OBJ ss1 ss4))                           ;;故需再作依次取得 SS2 物件 @@@@         ****
    
    
    (setq tel (tt ss1 ss2))                                 ;;求出路中心线与道路的双线 所产生的交点串列(这些交点可当做去除物件选取点)
    
    (setq qel (find_rd  tel))                               ;在两路中 拉出单一道路判断中间横越时是否有十字路口  
					;若有一处则 点串列 tel 需补上路口中心上下垂直两端点 以此类推
    (if (/= qel nil)(setq tel qel))                        ;有十字路口 则 将qel存入tel 否则 tel维持不变
    
    
    );end_progn         

  );end_defun_Pull_a_route

;;<<<<****  适用在两路之间拉出一条路 去除路中所有不必要线的选择串列点       ****>>>-----end


;;-----------------------------------------------------------------------
;; 适用在两路之间拉出一条路 
;; 从 e1 线集合物找出与 e2 线集合物 的所有相交点
(defun tt(e1 e2 / i n1 n2 e1 e2 obj1 obj2 ipt)

  (setq lst nil)

  (setq i   0 
        n1  (sslength e1)
        n2  (sslength e2)
	
	)
  (repeat n1 
	  (setq obj1 (vlax-ename->vla-object (ssname e1 i)))
	  (setq j 0)
	  (repeat n2 
		  (setq obj2 (vlax-ename->vla-object (ssname e2 j))
			ipt  (vlax-variant-value (vla-intersectwith obj1 obj2 0))
			)
		  (if (> (vlax-safearray-get-u-bound ipt 1) 0)
		      (progn
			(setq ipt (vlax-safearray->list ipt))
			(while (> (length ipt) 0)
			  (setq lst (cons (list (car ipt) (cadr ipt) (caddr ipt)) lst) ipt (cdddr ipt))
			  )
			)
		      )
		  (setq j (+ j 1))
		  )
	  (setq i (+ i 1))
	  )
  
  (setq lst (Remove_Dup lst))      ;;去除重复项 
  lst                        ;;回应点串值
  )

					;----------------------------------- 
;;取出中心线段的起始与终止点        
(defun get_cen_line( bb / i ev )

  (setq  i 0  kel nil)
  (repeat (sslength bb)
          (setq ev (cdr(assoc 10 (entget (ssname bb i )))))
          (setq kel (cons ev kel))
	  (setq ev (cdr(assoc 11 (entget (ssname bb i )))))
          (setq kel (cons ev kel))
	  
	  (setq i(+ i 1))

	  )

  kel                                ;;回应点串值
  
  );end_defun

;;----------------------------------
;;在点串列中给 p点值 找出最接近的点
(defun get_start_point(p vv / p s d n)
  (foreach x vv

	   (if (not d)
               (setq d (distance p x)
                     s x
		     )
               (if
		(< (setq n (distance p  x)) d)
		(setq d n
                      s x
		      )
		)
               )
	   
	   )
  s                                        ;回应找到接近点
  );end_defun


;;-------------------------------------------------------------------
;;取出集合线第一条起始点,最后一条结束点
(defun sel_obj(v1)
  (setq nb nil)  
  (repeat (setq i(sslength v1))
	  (setq na (ssname v1 (setq i (1- i))))
	  (setq nb (cons na nb))
	  
	  )
  (setq pd2 (cdr(assoc 11 (entget(car nb)))))
  (setq pd1 (cdr(assoc 10 (entget(car (reverse nb))))))
  )

;;-------------------------------------------------------------------
;;取得 milne,pline图元的 顶点串列
;; pline n=10
;; mline n=11
;;例 (setq en (entlast)
;;  (peek en 10)

(defun peek(vr n / obj)

  (setq ptx nil)
  (setq obj (vlax-ename->vla-object vr))
  (progn
    (setq ptx (mapcar 'cdr
		      (vl-remove-if-not '(lambda (x) (= (car x) n)) (entget  vr) )
		      )
	  )
    
    );end_progn
  ptx                    ;回应点串列值
  )

;;-------------------------------------------------------------------
;;指定项v1 加入选择集 v2中
(defun add_obj(v1 v2)
  
  (repeat (setq i(sslength v1))
	  (setq je (ssname v1 (setq i (1- i))))
	  (setq v2 (ssadd je v2))
	  
	  )
  v2
  )

;;-------------------------------------------------------------------
;;指定项v1 从选择集v2中 移除
(defun del_obj(v1 v2)
  
  (repeat (setq i(sslength v1))
	  (setq de (ssname v1 (setq i (1- i))))
	  (setq v2 (ssdel de v2))
	  
	  )
  v2
  )

;;-------------------------------------------------------------------
;;指定项从串列中移除
(defun del_line(v)
  
  (foreach x1   v
           (setq ee (ssget "C" x1 x1 ))
	   (command "erase" ee "")
	   )
  )

;;-------------------------------------------------------------------
;;指定项从串列中移除
(defun remove_ab(kk ee)
  (setq wel ee)
  (foreach x   kk
           (setq wel (vl-remove x wel))
	   )
  wel
  )
;;--------------------------------------------------------------------
					;新建图层 颜色 线形
(defun BF-Ent-MakeLayer  (v1 v2 v3 )  

  (vl-cmdf "-layer" "_N" v1 "_C"  v2 v1 "_L" v3 v1 "")

  )
;;--------------------------------------------------------------------
;;设为工作层
(defun BF-ent-ActiveLayer (v1)  ;设为工作层
  (vl-cmdf "_.-LAYER" "_S" v1 "")
  )



;;--------------------------------------------------------------------
;;倒圆角 
;;;;对指定图层所选取范围内的 pline 线 进行倒圆角处理。
;;;;例:  (sel_Fillet "地形地貌" 3)
;;
(defun sel_Fillet ( v1 v2 / bn sel ent)  
  (and
   (or (/= 4
           (logand 4
                   (cdr (assoc 70 (entget (tblobjname "LAYER" v1))))
		   )
           )
       (alert (strcat "图层名称< " v1 ">已锁定！ 解锁后再试一次"))
       )
   )
  (setq ssP (ssget "C" pt1 pt2))
  (setq bn -1
        sel (ssget "_P"
                   (list '(0 . "LWPOLYLINE")
                         (cons 8 v1)
                         (cons 410 (getvar 'CTAB))
			 )
		   )
	)
  (setvar 'FILLETRAD v2)
  (repeat (setq i(sslength sel))
	  (setq ent (ssname sel (setq i (1- i))))
	  (command "._fillet" "_P" ent  )
	  
	  )
  (command "-purge" "z")  ;删除长度为0的线
  )
;;-------------------------------------------------------------------

;;取得物体面积值并获得最小包围框
;; 例:(setq s1 (entlast)
;;     (chack_obj_area)
(defun chack_obj_area ()

  (command "area" "o" s1)                                      ;计算物体面积
  (setq obj_area (getvar "area"))                              ;取得物体面积值
  (setq txt_area (rtos obj_area 2 2))
  
  ;;求画出多边形外框的中心点
  (setq ptlist nil)
  (vla-GetBoundingBox (vlax-ename->vla-object s1) 'll 'rr)  ;得到物件的包围盒
  (setq box (list (vlax-safearray->list ll) (vlax-safearray->list rr)))  ;取矩形框 box=(左下点,右上点)
  (setq ptlist (append box ptlist))        
  (setq ssbox (mapcar '(lambda (x) (apply 'mapcar (cons x ptlist))) (list 'min 'max))) ;;依(左下点,右上点)两点求出矩形框四个对角点
  
  (setq p (apply 'mapcar (cons (function (lambda (a b) (/ (+ a b) 2))) ssbox))) ;;求出矩型包围框中心点，p点
  

  )

;;--------------------------
(defun LM:EntnexttoEnd ( e )
  (if (setq e (entnext e))
      (cons e (LM:EntnexttoEnd e))
      )
  )
;;-----------------------------------------------------------------
;;将物体的包围框放大 
;;因物体取最小包围框还无法含盖住，故需将含盖距离提高1.4倍左右 才能含盖所选的物件
;;  物体圈选范围  参数 pw:中心点 v1,v2物体最小包围框的两点(距离) ，v3:要放大的倍数 
;; 所求对角两点: pt1 pt2
(defun  scal_x (v1 v2 v3)
  
  (setq ang (angle v1 v2))
  (setq md   (* (distance v1 v2 ) 0.5))                                ;;求中线长度
  (setq pw   (polar v1 (angle v1 v2) md))                              ;;求中点
  (setq ds1  (* (distance v1 v2) v3))                                  ;; 
  (setq pt1  (polar pw   (angle v1 v2) (* ds1 0.5)))                   ;;因左右都放大故 ds1除2
  (setq pt2  (polar pw   (angle v2 v1) (* ds1 0.5)))
  

  )



;;-----------------------------------------------------------------------------
;;线接合为pline线
(defun PJ (ss / cmde peac ); = Polyline Join

  (setq
   cmde (getvar 'cmdecho)
   peac (getvar 'peditaccept)
   ); end setq
  (setvar 'cmdecho 0)
  (setvar 'peditaccept 1)
  (if ss
      (if (= (sslength ss) 1)
	  (command "_.pedit" ss "_join" "_all" "" ""); then
	  (command "_.pedit" "_multiple" ss "" "_join" "0.0000001" ""); else
	  ); end inner if
      ); end outer if
  (setvar 'cmdecho cmde)
  (setvar 'peditaccept peac)
  (princ)
  ); end defun

					;----------------------------------------------------------------------
;;求出物体最小包围框 (左下点p1,p2右上点)
(defun obj_min_box(en)
  (vla-getboundingbox (vlax-ename->vla-object en) 'p1 'p2)         ;;根据启始块物件名求出最小边界框 左下p1,右上p2
  (setq p1 (vlax-safearray->list p1) p2 (vlax-safearray->list p2)) ;;将启始块的p1,p2 转为实际的两点(左下点p1,p2右上点)
  (setq pp (list p1 p2))
  )
;;--------------------------------------------------------------------
;;恢复ML原设定
(defun stml ()
  (setvar "cmlstyle" "STANDARD")
  )

;;------------------------------------------------------------------------------------ 
;;载入相关程式
(defun load_Sub_lsp()
  ;; (setvar "SECURELOAD" 0)                            ;;关闭安载入档案安全考量提示讯息   0:关闭 1:打开
  (load "./DATA/draw_line_obj.lsp")                  ;;载入画线取路宽程式      执行指令: (drl)   回应取回的路宽串列值: uel
  (load "./DATA/set_mline2.lsp")                     ;;载入mline线型设定程式   执行指令: (set_mline)
  (load "./data/Take_inters.lsp")                    ;取两线相交点          执行指令: (Take_inters ss )  回应值: 交点串列 lst 中
  (load "./data/find_rd.lsp") 	                   ;在两路中 拉出单一道路判断中间横越时是否有十字路口 
					;若有一处则 点串列 tel 需补上路口中心上下垂直两端点 以此类推
					;执行: (find_rd)  会用到的点串列 tel   回应: tel (若有十字路口tel会补上下垂直点)													  
  (load "./data/breakall.LSP")                       ;将选的line线断开程式  执行指令: (break_all ss1) 
					; 例: (setq ss1 (ssget "X" '((0 . "LINE")(62 . 0))))  (breakall SS1)  则自动执行
  ;;(setvar "SECURELOAD" 1)                            ;;打开安载入档案安全考量提示讯息   0:关闭 1:打开
  (setq load_chack 1)                                ;已载入相关lisp 程式                                   
  ) 

					;-------------------------------------------------------------------------------------
					; 用途: 储存目前的系统变数
					; 作者: Tom 2002/10/01
					; 用法: (SaveVars (list "cmdecho" .... )) 将要储存的系统变数列于 list 中
					; 注意事项: 此功能建议只使用于 C: 开头的指令功能
					; 产生公共变数: #lSysVars
					;-------------------------------------------------------------------------------------
(defun SaveVars( lVars / zVar )
  (setq #lSysVars '())
  (foreach zVar lVars
	   (setq #lSysVars (append #lSysVars (list (cons zVar (getvar zVar)))))
	   )
  )

					;-------------------------------------------------------------------------------------
					; 用途: 还原储存的系统变数
					; 作者: Tom 2002/10/01
					; 用法: (RestoreVars)
					; 注意事项: 此功能建议只使用于 C: 开头的指令功能
					; 使用公共变数: #lSysVars  产生于 sample.lsp (SaveVars)
					;-------------------------------------------------------------------------------------
(defun RestoreVars( / aVar )
  (foreach aVar #lSysVars
	   (setvar (car aVar) (cdr aVar))
	   )
  )

					;-------------------------------------------------------------------------------------

					;******************************************************************************************************************************************************************
					;******************************************************************************************************************************************************************

					;-------------------------------------------------------------------------------------
;;BY LEE50310  日期 109 / 10/ 28日
;;使用于道路绘制
;;mline 复线型式
;; 1.双线实线两端头尾垂直封闭
;; 2.有中心线 实线 ,颜色 红色  (ps:中心现颜色红色勿更改 程式搜寻中心线依据)
;; 3.中心线实(Continuous) 线勿更改成虚线(HIDDEN) 原因虚线在做两线相交,交点判断若线刚好穿越虚线中空处会造成误判成无交点     
;;----------------------------------------------------------------------------------
(defun set_mline(/ mlDict mlList)
  (if (= nil (member (cons 3 "VENP")(dictsearch (namedobjdict) "ACAD_MLINESTYLE"))) 
      (progn
	(setq mlDict (cdr(assoc -1(dictsearch (namedobjdict) "ACAD_MLINESTYLE"))))
	(setq mlList (list'(0 . "MLINESTYLE")
	                  '(102 . "{ACAD_REACTORS")
			  '(102 . "}")
                          '(100 . "AcDbMlineStyle")
                          (cons 2  "VENP")         ;双线的名称
			  (cons 70  272)           ;双线 0 头尾不闭合 , 272 头尾闭合线
                          (cons 3  "路面线绘制")   ;线型式的描述
                          (cons 51  1.5708)        ;起始点 角度90度
                          (cons 52  1.5708)        ;终止点 角度90度
                          (cons 71  3)
                          (cons 49  0.5)           ;上底线偏移
                          (cons 62  256)           ;上底线颜色
                          (cons 6  "BYLAYER")      ;上底线线型
                          (cons 49  0.0)           ;中线偏移
                          (cons 62  1)             ;颜色:中心线红色
                          (cons 6  "Continuous")   ;线型:实线
                          (cons 49  -0.5)          ;下底线偏移         
                          (cons 62  256)           ;下底线颜色
                          (cons 6  "BYLAYER")      ;下底线线型
			  ); end list
	      ); end setq
	(dictadd mlDict "VENP" (entmakex mlList))
	); end progn
      ); end if
  (setvar "cmlstyle" "VENP")           ;;复线型式 名称 "VENP" 设为目前
  ); end defun


;;***********************************************************************************************************************************************
;;***********************************************************************************************************************************************

					;--------------------------------------------------------------------------
;;线交点断开程式  (取自国外网站)
;;(支援线型 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
;;
;;-------------------------------------------------------------------------

;;;=======================[ BreakObjects.lsp ]=============================
;;; Author: Copyright?2006-2019 Charles Alan Butler 
;;; Contact @  www.TheSwamp.org    
;;; Version:  2.3  June 6,2019
;;; Purpose: Break All selected objects
;;;    permitted objects are lines, lwplines, plines, splines,
;;;    ellipse, circles & arcs 
;;;                            
;;;  Function  c:BreakAll -      Break all objects selected with each other


(setq Brics (wcmatch  (getvar 'acadver) "*BricsCAD*" )) ; test for BricsCAD 06.06.19

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;               M A I N   S U B R O U T I N E                   
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun break_with (ss2brk ss2brkwith self Gap / cmd intpts lst masterlist ss ssobjs
                   onlockedlayer ssget->vla-list list->3pair GetNewEntities oc
                   get_interpts break_obj GetLastEnt LastEntInDatabase ss2brkwithList
                   )
  ;; ss2brk     selection set to break
  ;; ss2brkwith selection set to use as break points
  ;; self       when true will allow an object to break itself
  ;;            note that plined will break at each vertex
  ;;
  ;; return list of enames of new objects
  
  (vl-load-com)
  
  (princ "\nCalculating Break Points, Please Wait.\n")

  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;;                S U B   F U N C T I O N S                      
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ;;  return T if entity is on a locked layer
  (defun onlockedlayer (ename / entlst)
    (setq entlst (tblsearch "LAYER" (cdr (assoc 8 (entget ename)))))
    (= 4 (logand 4 (cdr (assoc 70 entlst))))
    )
  
  ;;  return a list of objects from a selection set
  ;;  (defun ssget->vla-list (ss)
  ;;  (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss ))))
  ;;)
  (defun ssget->vla-list (ss / i ename allobj) ; this is faster, changed in ver 1.7
    (setq i -1)
    (while (setq  ename (ssname ss (setq i (1+ i))))
      (setq allobj (cons (vlax-ename->vla-object ename) allobj))
      )
    allobj
    )
  
  ;;  return a list of lists grouped by 3 from a flat list
  (defun list->3pair (old / new)
    (while (setq new (cons (list (car old) (cadr old) (caddr old)) new)
                 old (cdddr old)))
    (reverse new)
    )
  
  ;;=====================================
  ;;  return a list of intersect points  
  ;;=====================================
  (defun get_interpts (obj1 obj2 / iplist)
    (if (not (vl-catch-all-error-p
              (setq iplist (vl-catch-all-apply
                            'vlax-safearray->list
                            (list
                             (vlax-variant-value
                              (vla-intersectwith obj1 obj2 acextendnone)
                              ))))))
	iplist
	)
    )


  ;;========================================
  ;;  Break entity at break points in list  
  ;;========================================
  ;;   New as per version 1.8 [BrkGap] --- This subroutine has been re-written
  ;;  Loop through the break points breaking the entity
  ;;  If the entity is not a closed entity then a new object is created
  ;;  This object is added to a list. When break points don't fall on the current 
  ;;  entity the list of new entities are searched to locate the entity that the 
  ;;  point is on so it can be broken.
  ;;  "Break with a Gap" has been added to this routine. The problem faced with 
  ;;  this method is that sections to be removed may lap if the break points are
  ;;  too close to each other. The solution is to create a list of break point pairs 
  ;;  representing the gap to be removed and test to see if there i an overlap. If
  ;;  there is then merge the break point pairs into one large gap. This way the 
  ;;  points will always fall on an object with one exception. If the gap is too near
  ;;  the end of an object one break point will be off the end and therefore that 
  ;;  point will need to be replaced with the end point.
  ;;    NOTE: in ACAD2000 the (vlax-curve-getdistatpoint function has proven unreliable
  ;;  so I have used (vlax-curve-getdistatparam in most cases
  (defun break_obj (ent brkptlst BrkGap / brkobjlst en enttype maxparam closedobj
                    minparam obj obj2break p1param p2param brkpt2 dlst idx brkptS
                    brkptE brkpt result GapFlg result ignore dist tmppt
                    #ofpts 2gap enddist lastent obj2break stdist
                    )
    (or BrkGap (setq BrkGap 0.0)) ; default to 0
    (setq BrkGap (/ BrkGap 2.0)) ; if Gap use 1/2 per side of break point
    
    (setq obj2break ent
          brkobjlst (list ent)
          enttype   (cdr (assoc 0 (entget ent)))
          GapFlg    (not (zerop BrkGap)) ; gap > 0
          closedobj (vlax-curve-isclosed obj2break)
	  )
    ;; when zero gap no need to break at end points, not closed
    (if (and (zerop Brkgap)(not closedobj)) ; Revision 2.2
	(setq spt (vlax-curve-getstartpoint ent)
              ept (vlax-curve-getendpoint ent)
              brkptlst (vl-remove-if '(lambda(x) (or (< (distance x spt) 0.0001)
                                                  (< (distance x ept) 0.0001)))
                                     brkptlst)
	      )
	)
    (if brkptlst
	(progn
	  ;;  sort break points based on the distance along the break object
	  ;;  get distance to break point, catch error if pt is off end
	  ;; ver 2.0 fix - added COND to fix break point is at the end of a
	  ;; line which is not a valid break but does no harm
	  (setq brkptlst (mapcar '(lambda(x) (list x (vlax-curve-getdistatparam obj2break
						      ;; ver 2.0 fix
						      (cond ((vlax-curve-getparamatpoint obj2break x))
							    ((vlax-curve-getparamatpoint obj2break
											 (vlax-curve-getclosestpointto obj2break x))))))
				   ) brkptlst))
	  ;; sort primary list on distance
	  (setq brkptlst (vl-sort brkptlst '(lambda (a1 a2) (< (cadr a1) (cadr a2)))))
	  
	  (if GapFlg ; gap > 0
	      ;; Brkptlst starts as the break point and then a list of pairs of points
	      ;;  is creates as the break points
	      (progn
		;;  create a list of list of break points
		;;  ((idx# stpoint distance)(idx# endpoint distance)...)
		(setq idx 0)
		(foreach brkpt brkptlst
			 
			 ;; ----------------------------------------------------------
			 ;;  create start break point, then create end break point    
			 ;;  ((idx# startpoint distance)(idx# endpoint distance)...)  
			 ;; ----------------------------------------------------------
			 (setq dist (cadr brkpt)) ; distance to center of gap
			 ;;  subtract gap to get start point of break gap
			 (cond
			   ((and (minusp (setq stDist (- dist BrkGap))) closedobj )
			    (setq stdist (+ (vlax-curve-getdistatparam obj2break
								       (vlax-curve-getendparam obj2break)) stDist))
			    (setq dlst (cons (list idx
						   (vlax-curve-getpointatparam obj2break
									       (vlax-curve-getparamatdist obj2break stDist))
						   stDist) dlst))
			    )
			   ((minusp stDist) ; off start of object so get startpoint
			    (setq dlst (cons (list idx (vlax-curve-getstartpoint obj2break) 0.0) dlst))
			    )
			   (t
			    (setq dlst (cons (list idx
						   (vlax-curve-getpointatparam obj2break
									       (vlax-curve-getparamatdist obj2break stDist))
						   stDist) dlst))
			    )
			   )
			 ;;  add gap to get end point of break gap
			 (cond
			   ((and (> (setq stDist (+ dist BrkGap))
				    (setq endDist (vlax-curve-getdistatparam obj2break
									     (vlax-curve-getendparam obj2break)))) closedobj )
			    (setq stdist (- stDist endDist))
			    (setq dlst (cons (list idx
						   (vlax-curve-getpointatparam obj2break
									       (vlax-curve-getparamatdist obj2break stDist))
						   stDist) dlst))
			    )
			   ((> stDist endDist) ; off end of object so get endpoint
			    (setq dlst (cons (list idx
						   (vlax-curve-getpointatparam obj2break
									       (vlax-curve-getendparam obj2break))
						   endDist) dlst))
			    )
			   (t
			    (setq dlst (cons (list idx
						   (vlax-curve-getpointatparam obj2break
									       (vlax-curve-getparamatdist obj2break stDist))
						   stDist) dlst))
			    )
			   )
			 ;; -------------------------------------------------------
			 (setq idx (1+ IDX))
			 ) ; foreach brkpt brkptlst
		

		(setq dlst (reverse dlst))
		;;  remove the points of the gap segments that overlap
		(setq idx -1
		      2gap (* BrkGap 2)
		      #ofPts (length Brkptlst)
		      )
		(while (<= (setq idx (1+ idx)) #ofPts)
		  (cond
		    ((null result) ; 1st time through
		     (setq result (list (car dlst)) ; get first start point
			   result (cons (nth (1+(* idx 2)) dlst) result))
		     )
		    ((= idx #ofPts) ; last pass, check for wrap
		     (if (and closedobj (> #ofPts 1)
			      (<= (+(- (vlax-curve-getdistatparam obj2break
								  (vlax-curve-getendparam obj2break))
				       (cadr (last BrkPtLst))) (cadar BrkPtLst)) 2Gap))
			 (progn
			   (if (zerop (rem (length result) 2))
			       (setq result (cdr result)) ; remove the last end point
			       )
			   ;;  ignore previous endpoint and present start point
			   (setq result (cons (cadr (reverse result)) result) ; get last end point
				 result (cdr (reverse result))
				 result (reverse (cdr result)))
			   )
			 )
		     )
		    ;; Break Gap Overlaps
		    ((< (cadr (nth idx Brkptlst)) (+ (cadr (nth (1- idx) Brkptlst)) 2Gap))
		     (if (zerop (rem (length result) 2))
			 (setq result (cdr result)) ; remove the last end point
			 )
		     ;;  ignore previous endpoint and present start point
		     (setq result (cons (nth (1+(* idx 2)) dlst) result)) ; get present end point
		     )
		    ;; Break Gap does Not Overlap previous point 
		    (t
		     (setq result (cons (nth (* idx 2) dlst) result)) ; get this start point
		     (setq result (cons (nth (1+(* idx 2)) dlst) result)) ; get this end point
		     )
		    ) ; end cond stmt
		  ) ; while
		
		;;  setup brkptlst with pair of break pts ((p1 p2)(p3 p4)...)
		;;  one of the pair of points will be on the object that
		;;  needs to be broken
		(setq dlst     (reverse result)
		      brkptlst nil)
		(while dlst ; grab the points only
		  (setq brkptlst (cons (list (cadar dlst)(cadadr dlst)) brkptlst)
			dlst   (cddr dlst))
		  )
		)
	      )
	  ;;   -----------------------------------------------------

	  ;; (if (equal  a ent) (princ)) ; debug CAB  
	  
	  (foreach brkpt (reverse brkptlst)
		   (if GapFlg ; gap > 0
		       (setq brkptS (car brkpt)
			     brkptE (cadr brkpt))
		       (setq brkptS (car brkpt)
			     brkptE brkptS)
		       )
		   ;;  get last entity created via break in case multiple breaks
		   (if brkobjlst
		       (progn
			 (setq tmppt brkptS) ; use only one of the pair of breakpoints
			 ;;  if pt not on object x, switch objects
			 (if (not (numberp (vl-catch-all-apply
					    'vlax-curve-getdistatpoint (list obj2break tmppt))))
			     (progn ; find the one that pt is on
			       (setq idx (length brkobjlst))
			       (while (and (not (minusp (setq idx (1- idx))))
					   (setq obj (nth idx brkobjlst))
					   (if (numberp (vl-catch-all-apply
							 'vlax-curve-getdistatpoint (list obj tmppt)))
					       (null (setq obj2break obj)) ; switch objects, null causes exit
					       t
					       )
					   )
				 )
			       )
			     )
			 )
		       )
		   ;; 	;| ;; ver 2.0 fix - removed this code as there are cases where the break point
		   ;; ;; is at the end of a line which is not a valid break but does no harm
		   ;; (if (and brkobjlst idx (minusp idx)
		   ;; 	    (null (alert (strcat "Error - point not on object"
		   ;; 				 "\nPlease report this error to"
		   ;; 				 "\n   CAB at TheSwamp.org"))))
		   ;;     (exit)
		   ;;     )
		   ;; |;
		   ;; (if (equal (if (null a)(setq a (car(entsel"\nTest Ent"))) a) ent) (princ)) ; debug CAB  -------------

		   ;;  Handle any objects that can not be used with the Break Command
		   ;;  using one point, gap of 0.000001 is used
		   (setq closedobj (vlax-curve-isclosed obj2break))
		   (if GapFlg ; gap > 0
		       (if closedobj
			   (progn ; need to break a closed object
			     (setq brkpt2 (vlax-curve-getPointAtDist obj2break
								     (- (vlax-curve-getDistAtPoint obj2break brkptE) 0.00001)))
			     (command "._break" obj2break "_non" (trans brkpt2 0 1)
				      "_non" (trans brkptE 0 1))
			     (and (= "CIRCLE" enttype) (setq enttype "ARC"))
			     (setq BrkptE brkpt2)
			     )
			   )
		       ;;  single breakpoint ----------------------------------------------------
		       ;;  ;|(if (and closedobj ; problems with ACAD200 & this code
		       ;; 		 (not (setq brkptE (vlax-curve-getPointAtDist obj2break
		       ;; 							      (+ (vlax-curve-getDistAtPoint obj2break brkptS) 0.00001))))
		       ;; 		 )
		       ;; (setq brkptE (vlax-curve-getPointAtDist obj2break
		       ;; 						    (- (vlax-curve-getDistAtPoint obj2break brkptS) 0.00001)))
		       
		       ;; 	    )|;
		       (if (and closedobj 
				(not (setq brkptE (vlax-curve-getPointAtDist obj2break
									     (+ (vlax-curve-getdistatparam obj2break
													   ;;(vlax-curve-getparamatpoint obj2break brkpts)) 0.00001))))
													   ;; ver 2.0 fix
													   (cond ((vlax-curve-getparamatpoint obj2break brkpts))
														 ((vlax-curve-getparamatpoint obj2break
																	      (vlax-curve-getclosestpointto obj2break brkpts))))) 0.00001)))))
			   (setq brkptE (vlax-curve-getPointAtDist obj2break
								   (- (vlax-curve-getdistatparam obj2break
												 ;;(vlax-curve-getparamatpoint obj2break brkpts)) 0.00001)))
												 ;; ver 2.0 fix
												 (cond ((vlax-curve-getparamatpoint obj2break brkpts))
												       ((vlax-curve-getparamatpoint obj2break
																    (vlax-curve-getclosestpointto obj2break brkpts))))) 0.00001)))
			   )
		       ) ; endif
		   
		   ;; (if (null brkptE) (princ)) ; debug
		   
		   (setq LastEnt (GetLastEnt))
		   (command "._break" obj2break "_non" (trans brkptS 0 1) "_non" (trans brkptE 0 1))
		   (and *BrkVerbose* (princ (setq *brkcnt* (1+ *brkcnt*))) (princ "\r"))
		   (and (= "CIRCLE" enttype) (setq enttype "ARC"))
		   (if (and (not closedobj) ; new object was created
			    (not (equal LastEnt (entlast))))
		       (setq brkobjlst (cons (entlast) brkobjlst))
		       )
		   )
	  )
	) ; endif brkptlst
    
    ) ; defun break_obj

  ;;====================================
  ;;  CAB - get last entity in datatbase
  (defun GetLastEnt ( / ename result )
    (if (setq result (entlast))
	(while (setq ename (entnext result))
	  (setq result ename)
	  )
	)
    result
    )
  ;;===================================
  ;;  CAB - return a list of new enames
  (defun GetNewEntities (ename / new)
    (cond
      ((null ename) (alert "Ename nil"))
      ((eq 'ENAME (type ename))
       (while (setq ename (entnext ename))
         (if (entget ename) (setq new (cons ename new)))
	 )
       )
      ((alert "Ename wrong type."))
      )
    new
    )

  
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;;         S T A R T  S U B R O U T I N E   H E R E              
  ;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  (setq LastEntInDatabase (GetLastEnt))
  (if (and ss2brk ss2brkwith)
      (progn
	(setq oc 0
              ss2brkwithList (ssget->vla-list ss2brkwith))
	(if (> (* (sslength ss2brk)(length ss2brkwithList)) 5000)
            (setq *BrkVerbose* t)
	    )
	(and *BrkVerbose*
             (princ (strcat "Objects to be Checked: "
			    (itoa (* (sslength ss2brk)(length ss2brkwithList))) "\n")))
	;;  CREATE a list of entity & it's break points
	(foreach obj (ssget->vla-list ss2brk) ; check each object in ss2brk
		 (if (not (onlockedlayer (vlax-vla-object->ename obj)))
		     (progn
		       (setq lst nil)
		       ;; check for break pts with other objects in ss2brkwith
		       (foreach intobj  ss2brkwithList
				(if (and (or self (not (equal obj intobj)))
					 (setq intpts (get_interpts obj intobj))
					 )
				    (setq lst (append (list->3pair intpts) lst)) ; entity w/ break points
				    )
				(and *BrkVerbose* (princ (strcat "Objects Checked: " (itoa (setq oc (1+ oc))) "\r")))
				)
		       (if lst
			   (setq masterlist (cons (cons (vlax-vla-object->ename obj) lst) masterlist))
			   )
		       )
		     )
		 )

	
	(and *BrkVerbose* (princ "\nBreaking Objects.\n"))
	(setq *brkcnt* 0) ; break counter
	;;  masterlist = ((ent brkpts)(ent brkpts)...)
	(if masterlist
            (foreach obj2brk masterlist
		     (break_obj (car obj2brk) (cdr obj2brk) Gap)
		     )
	    )
	)
      )
  ;;==============================================================
  (and (zerop *brkcnt*) (princ "\nNone to be broken."))
  (setq *BrkVerbose* nil)
  (GetNewEntities LastEntInDatabase) ; return list of enames of new objects
  )
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      E N D   O F    M A I N   S U B R O U T I N E             
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;           M A I N   S U B   F U N C T I O N S                 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;======================
;;  Redraw ss with mode 
;;======================
(defun ssredraw (ss mode / i num)
  (setq i -1)
  (while (setq ename (ssname ss (setq i (1+ i))))
    (redraw (ssname ss i) mode)
    )
  )

;;===========================================================================
;;  get all objects touching entities in the sscross                         
;;  limited obj types to "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"
;;  returns a list of enames
;;===========================================================================
(defun gettouching (sscros / ss lst lstb lstc objl)
  (and
   (setq lstb (vl-remove-if 'listp (mapcar 'cadr (ssnamex sscros)))
         objl (mapcar 'vlax-ename->vla-object lstb)
	 )
   (setq
    ss (ssget "_A" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE")
                         (cons 410 (getvar "ctab"))))
    )
   (setq lst (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
   (setq lst (mapcar 'vlax-ename->vla-object lst))
   (mapcar
    '(lambda (x)
      (mapcar
       '(lambda (y)
         (if (not
              (vl-catch-all-error-p
               (vl-catch-all-apply
                '(lambda ()
                  (vlax-safearray->list
                   (vlax-variant-value
                    (vla-intersectwith y x acextendnone)
                    ))))))
             (setq lstc (cons (vlax-vla-object->ename x) lstc))
             )
         ) objl)
      ) lst)
   )
  lstc
  )



;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;          E N D   M A I N    F U N C T I O N S                 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



;;===============================================
;;   Break all objects selected with each other  
;;===============================================
(defun Break_All ( ss / cmd ss NewEnts AllEnts tmp)

  (command "_.undo" "_begin")
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (or Bgap (setq Bgap 0)) ; default

  (setq tmp 0.0000001 Bgap tmp)

  ;;  get objects to break

  (setq NewEnts (Break_with ss ss nil Bgap) ; ss2break ss2breakwith (flag nil = not to break with self)
					; AllEnts (append NewEnts (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
        )

  (setvar "CMDECHO" cmd)
  (command "_.undo" "_end")
  (princ)
  )

					;******************************************************************************************************************************************************************
					;******************************************************************************************************************************************************************

					;------------------------------------------------------------------------------------- 
;; BY LEE50310  日期 109 / 11/ 08日
;;
;;在既有的道路上画线取距离值(路宽值)
;;需用到函数 (tt v1 v2) 求 v1与v2 交点串列
;;           (sort-x-min-li pcl))  将点串列 做 x点由小排到大 
;; 
;;画线取路宽 
					;------------------------------------------------------------------------------------- 
(defun drl( / sst ssa ssb pc1 n i m1 m2 d1)
  
  (princ "\n<<<  在既有的道路上画线取各路宽值   >>>")
  
  ;;       (setvar "orthomode" 1)                                       ;打开垂直正交
  (setq px1 (getpoint "\n画线取得路宽  第一点: "))				
  (setq px2 (getpoint px1 "\n画线取得路宽  第二点: "))
  (grdraw px1 px2 2 1)
  (redraw)
  
  ;;		(setvar "orthomode" 0)                                           ;关闭垂直正交  
  (setq sst (ssget "_f" (list px1 px2) ))                          ;画线取物
  
  (command "line" px1 px2 "")
  (setq ssb (ssadd ))
  (setq ssb (ssadd (entlast) ssb))                                  ;将图元转选集

  (setq pcl (tt ssb sst))                                           ;求 ssb与ssa交点串列



  (setq n(length pcl))
  (if (/= (rem n 2) 0)                                                    ;判断 n值是否为偶数
      (setq n(1- n))                                                  ;因道路两线都是偶数成对 若为奇数将值-1
      ) ;end_if
  
  (setq pcl (sort-x-min-li pcl))                                 ;做 x点由小排到大	
  (setq i 0 uel nil)
  (while (/= i n)
    
    (setq m1 (nth i pcl))
    (setq m2 (nth (1+ i) pcl))
    (setq d1 (read(rtos(distance m1 m2) 2 2 )))                    ;距离取到小数点第一位
    (setq uel (cons d1 uel))                                       ;距离值串列 
    (setq i (+ i 2))                                               ;水平两条路为例 i=(0,1)为道路 (1,2)为路与路间距需跳过 故 i+2 ,(2,3)为道路
    
    ) ;end_while
  
  (setq uel (Remove_Dup uel))                                    ;;去除重复项 
  (setq uel (reverse uel))                                       				
  
  (command "erase" ssb "")
  
  ) ;end_defun

;;------------------------------------------------------------------
;;列出找到的路宽值
(defun road-cross:prd()  
  (if (= (length uel) 0)                 
      (setq uel (cons ww uel))          ; 若uel串列为空值将目前道路绘制路宽设给uel
      ) 
  (setq nn (length uel) ke "") 
  (foreach x uel
	   (setq ke (strcat ke (rtos x 2 2) "(m)  "))                
	   )
  
  (setq taw (strcat "\n找到共" (rtos nn )  "条路,路宽值分别为 = " ke))
  (princ  taw )
  (prin1)
  )
;;----------------------------------------------------------------------

					;******************************************************************************************************************************************************************
					;******************************************************************************************************************************************************************

					;------------------------------------------------------------------------------------- 
;; BY LEE50310  日期 109 / 11/ 08日
;;在两路中 拉出单一道路判断中间横越时是否有十字路口 若有一处则 点串列 vel 需补上路口中心上下垂直两端点 以此类推 
;;方便后续 vel 点串列 消除十字路口线水平线时也能消除垂直线
;;找寻两点距离宽 判定为路宽 是依据 假设道路路宽定义范围  1.5m <= 路宽 <= 20m 之间
					;------------------------------------------------------------------------------------- 
(defun find_rd(vel)

  (if (> (length vel) 7)              ;若外拉的一条道路[(中心+头尾=6点交点)]线 交点值须有8 点已上表示中间有横越到路产生十字路口   

      (progn
	
	(setq zeb (nth+ 4 vel 0))  ;因点串列 vel 前4点为头尾线交点 故从第5点以后开始找寻 可能产生十字路口的任2点
        (setq zeb (sort-x-min-li zeb))  ;串列点需做 x点由小排到大		 
	

	(if (= gen nil)(progn (setq gen 1)(setq ww (getdist "\n 选取路宽两点距离或输入既有道路宽: "))))

        (setq   mel nil yel nil  rel nil)
	
	
	(cond ((= gen 1)                                                                              ;gen=1 路宽值就是原本绘制的道路宽
               
               (setq rd_min (- ww (* ww z%))  rd_max (+ ww (* ww z%)))                 ; 容许误差范围 
	       (Vertical_point)                                                        ;找到两点十字路宽加入上下垂直点 
	       )
	      
	      ((= gen 2)                                                                             ;gen=2 有执行过画线找既有路宽 路宽值存于uel
	       (if (= (length uel) 0)                 
		   (setq uel (cons ww uel))                                                       ; 若uel串列为空值将目前道路绘制路宽设给uel
		   )				
	       (foreach  x  uel                                                                ;例:找到三条路宽 uel=(10.2 15.6 20)
			 (setq rd_min (- x (* x z%))  rd_max (+ x (* x z%)))                     ;在交点串列中找两点距离为路宽时加入容许误差范围寻找
					;例: 实际路宽 20  在 19.6 >= 找两点距离路宽值<= 20.4	的范围内,都被认定是道路宽					
			 (Vertical_point)                                                        ;依找到两交点路宽就是十字路口需在交点串列中补上下垂直点
					;交点串列中有路宽水平两点及垂值两点 就可由这四点去消除十字路口,口字的四条线
			 )
	       
	       )
	      );end_cond
	(setq mel (Remove_Dup mel))                                ;;去除重复项         @@@@
	;; (setq mel (sort-x-min-li mel))                           ;做 x点由小排到大	   @@@@
	(setq vel (append vel mel))                                ;将十字路口补上垂直点
	vel	                                              ;回应值	
	
	);end_progn
      
      
      );end_if
  
  )
					;-----------------------------------------------------------------------------
					;找到两点十字路口的宽度距离,在距离的中点加入上下垂直点
(defun Vertical_point ( / c1 c2 wt)
  (setq i 0)
  (while (/= i (- (length zeb) 1) )
    
    (setq c1 (nth i zeb))
    (setq c2 (nth (+ i 1) zeb ))				 
    (setq wt (distance c1 c2))
    
    (if (setq vv(and (>= wt rd_min )(<= wt rd_max )))           ; 例: 1.5m <= 路宽 <= 20m
	(progn                                        ;找到两点有十字路宽
	  (prod_line c1 c2)
	  (setq wt (distance c1 c2))
	  (setq mel (cons pe1 mel))             ;将上垂直点加入
	  (setq mel (cons pe2 mel))             ;将下垂直点加入
	  
	  );end_progn
	
        );end_if	
    (setq i (+ i 1))
    
    );end_while
  
  );end_progn
					;----------------------------------------------------------
					;依水平路宽两点求出中点上下垂直点
(defun  prod_line(v1 v2 / w g pm wh )
  (setq pe1 nil pe2 nil)  
  (setq u (/ pi 180))                            ;弪度值与度转换 
  (setq w (*(distance v1 v2) 0.5))
  (setq g (angle v1 v2))
  (setq pm (polar v1 g w))                       ;水平中点
  (setq wh (* ww 0.5))                           ;垂直宽*0.5 	
  
  (setq pe1 (polar pm (+ g (* pi  0.5)) wh))     ;垂直点
  (setq pe2 (polar pm (+ g (* pi -0.5)) wh))     ;垂直点
  
  );end_defun

;;------------------------------------------------------------
;;  第n加 函数
;;  CAB  10/15/2005
;; 返回从第n个位置开始的子列表，然后
;; 由num指定的项目数
;;使用范例:
;;  (nth+ 0 '(1 2 3 4 5) 2)  ;回应> (1 2)  说明 从串列数的 第0位开始取2位 
;;  (nth+ 3 '(1 2 3 4 5) 2)  ;回应> (4 5)  说明 从串列数的 第3位开始取2位 
;;  (nth+ 4 '(1 2 3 4 5) 5)  ;回应> (5)    
;;  (nth+ 6 '(1 2 3 4 5) 2)) ;回应> nil    
;;  (nth+ 2 '(1 2 3 4 5) 0)) ;回应> (3 4 5) 说明 从串列数的 第2位开始取到最后 
;;例 vel = ((5646.96 539.269 0.0) (5646.64 542.96 0.0) (5570.21 536.454 0.0) (5570.52 532.763 0.0) (5639.97 540.533 0.0) (5572.3 534.773 0.0))
;;要取得vel点串列第4项到最后的数
;; 执行: (NTH+ 4 vel 0)      ;回应> ((5639.97 540.533 0.0) (5572.3 534.773 0.0))
;;------------------------------------------------------------

(defun nth+ (idx               ; start position 0 = first item
             lst               ; list of items
             num               ; number of items to return
					;    0= all remaining items
             / newlst)
  (and (or (numberp num)       ; catch non numbers
           (setq num 0))       ; force all
       (zerop num)             ; if zero
       (setq num (length lst)) ; all
       )
  (repeat num
	  (setq newlst (cons (nth idx lst) newlst)
		idx (1+ idx))
	  )
  (reverse (vl-remove nil newlst))
  )

;;----------------------------------------------------------
;;阶层排序
					;例: 点串列 zeb = ((1703.87 1298.93 0.0) (1879.27 1296.67 0.0) (1683.09 1299.2 0.0) (1558.82 1300.81 0.0))
					; 1. X左->右  由小到大
					;    (sort-x-min-li zeb)
					;            回应:((1558.82 1300.81 0.0) (1683.09 1299.2 0.0) (1703.87 1298.93 0.0) (1879.27 1296.67 0.0))

(defun sort-x-min-li (lst)
  (vl-sort lst '(lambda (x y) (< (car x)(car y) ) ))
  ) ;x由小到大


(defun sort-x-max-li (lst)
  (vl-sort lst '(lambda (x y) (> (car x)(car y) ) ))
  ) ;x由大到小


(defun sort-y-min-li (lst)
  (vl-sort lst '(lambda (x y) (< (cadr x)(cadr y) ) ))
  ) ;Y由小到大


(defun sort-y-max-li (lst)
  (vl-sort lst '(lambda (x y) (> (cadr x)(cadr y) ) ))
  ) ;Y由大到小


;;******************************************************************************************************************************************************************
					;******************************************************************************************************************************************************************

;;----------------------------------------------------------------------------------
;;
;;取交点+去除重复点 
;;
;;-----------------------------------------------------------------------------------


(defun Take_inters ( ss / ipt  n n1 n2 obj1 obj2)
  (setq lst nil)

  (setq n  (sslength ss)
        n1 0
	)
  (while (< n1 (1- n))
    (setq obj1 (vlax-ename->vla-object (ssname ss n1))
          n2   (1+ n1)
	  )
    (while (< n2 n)
      (setq obj2 (vlax-ename->vla-object (ssname ss n2))
            ipt  (vlax-variant-value (vla-intersectwith obj1 obj2 0))
	    )
      (if (> (vlax-safearray-get-u-bound ipt 1) 0)
          (progn
            (setq ipt (vlax-safearray->list ipt))
            (while (> (length ipt) 0)
              (setq lst (cons (list (car ipt) (cadr ipt) (caddr ipt)) lst) ipt (cdddr ipt))
              )
            )
	  )
      (setq n2 (1+ n2))
      )
    (setq n1 (1+ n1))
    )
  
  (setq lst (Remove_Dup lst))      ;;去除重复项 
  lst                              ;;回应值
  )
;;------------------------------------------------------------
;; 删除表重复元素 
;;使用方式: (_RemoveDuplicates  lst)
;;

(defun Remove_Dup ( lst / foo index )
  (defun foo (x)
    (cond
      ((vl-position x index))
      ((null (setq index (cons x index))))
      )
    )
  (vl-remove-if
   'foo
   lst
   )
  )

;;******************************************************************************************************************************************************************
					;******************************************************************************************************************************************************************


