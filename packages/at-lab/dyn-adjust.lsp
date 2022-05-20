;;;duotu007 ver1.0 2012/9/6(原著)
;;;1028695446   ver2.0 2019/4/4(修改)
(defun @lab:dyn-adjust (/ bl_update bl_update_time_batch boolean_typeofss ent error_do main_process main_process_batch ss_first text_update color text textent)
  ;;智能加载子函数库
  ;; (if (/= (TYPE test_load ) 'SUBR) 	;判断testload函数是否已被加载，如果是，表示已经加载了子函数文件夹
  ;;     (PROGN
  ;;      (c:add) 	;加载子函数文件夹
  ;;      )
  ;;   )
  ;;启用错误处理之后，导致无法先选择后执行，具体原因还不得而知。
  ;;(error_init 'error_do 1)
  (defun error_do()
    (redraw)
    (if text (entdel text));删除临时文字
    (princ)
    )
  (defun bl_update(oba bl);;子函数,更新比例为绝对值bl
    (cond
     ((or (= oba "REGION") (= oba "LWPOLYLINE") (= oba "LINE") (= oba "CIRCLE") (= oba "ARC"))
      (vla-put-LinetypeScale obj bl);;设定线型比例
      )
     ((= oba "HATCH")
      (vla-put-PatternScale obj bl);;设定填充比例
      )
     ((= oba "INSERT")
      (vla-put-xscalefactor obj bl);;设定图块比例x
      (vla-put-yscalefactor obj bl);;设定图块比例y
      (vla-put-zscalefactor obj bl);;设定图块比例z
      )
     ((or (= oba "TEXT")(= oba "MTEXT"))
      (vla-put-Height obj bl)
      )
     ((wcmatch oba "*DIMENSION")
      (vla-put-ScaleFactor obj bl)
      )
     )
    )
  (defun bl_update_time_batch(list_ent list_bl bl / oba);;子函数,对选择集内的图元，批量更新比例为原始比例的bl倍
    (setq oba (cdr (assoc 0 (entget (nth 0 list_ent)))))
    (cond
     ((or (= oba "REGION") (= oba "LWPOLYLINE") (= oba "LINE") (= oba "CIRCLE") (= oba "ARC"))
      
      (mapcar '(lambda (x y) (vla-put-LinetypeScale (vlax-ename->vla-object x) (* y bl))) list_ent list_bl)
      )
     ((= oba "HATCH")
      
      (mapcar '(lambda (x y) (vla-put-PatternScale (vlax-ename->vla-object x) (* y bl))) list_ent list_bl)
      )
     ((= oba "INSERT")
      
      (mapcar '(lambda (x y) (vla-put-xscalefactor (vlax-ename->vla-object x) (* y bl))) list_ent (nth 0 list_bl))
      (mapcar '(lambda (x y) (vla-put-yscalefactor (vlax-ename->vla-object x) (* y bl))) list_ent (nth 1 list_bl))
      (mapcar '(lambda (x y) (vla-put-zscalefactor (vlax-ename->vla-object x) (* y bl))) list_ent (nth 2 list_bl))
      
      )
     ((or (= oba "TEXT")(= oba "MTEXT"))
      (mapcar '(lambda (x y) (vla-put-Height (vlax-ename->vla-object x) (* y bl))) list_ent list_bl)
      )
     ((wcmatch oba "*DIMENSION")
					;(vla-put-ScaleFactor obj bl)
      (mapcar '(lambda (x y) (vla-put-ScaleFactor (vlax-ename->vla-object x) (* y bl))) list_ent list_bl)
      )
     )
    )
  (defun text_update(str / );;子函数,更新临时文字
    (if color(princ)(setq color 1))
    
					;(setq str (rtos bl))
    (if text;文字显示
	(progn
	  (setq textent (subst (cons 1 str)   (assoc 1  textent) textent))
	  (setq textent (subst (cons 62 color)(assoc 62 textent) textent))
	  (setq textent (subst (cons 40 (/ (getvar "viewsize") 30))(assoc 40 textent) textent))
					;鼠标移动的时候，更新坐标，使其跟随鼠标移动；敲击键盘的时候，不更新坐标
	  (if (= a 5) (setq textent (subst (cons 10 aa)(assoc 10 textent) textent)))
					;(if flag_dynamic (setq textent (subst (cons 10 aa)(assoc 10 textent) textent)));启用动态比例
	  (entmod textent)
	  );第二遍已有,修改内容
      (progn
	(entmake (list '(0 . "TEXT")
		       (cons 1 str)
					;鼠标移动的时候，更新坐标，使其跟随鼠标移动；其他情况，坐标为图元的坐标
		       (if (= a 5) (cons 10 aa)(cons 10 pt0))
					;(if flag_dynamic (cons 10 aa)(cons 10 pt0))
		       (cons 40 (/ (getvar "viewsize") 30));;字体大小,同视图比例相关
		       (cons 41 0.7) ;;字高
		       (cons 50 0);;字旋转角度
		       (cons 62 color)
		       )
		 )
	(setq text (entlast) textent (entget text))
	);第一遍文字不存在先生成
      )
    )
					;判断选择集是否均为同一图元类型
  (defun boolean_typeOfSs(ss str_type / ent flag index oba)
    (setq flag T)
    (setq index 0)
    (while (and flag (setq ent (ssname ss index)))
      (setq oba (cdr (assoc 0 (entget ent))))
      (if (not
	   (if (= str_type "*DIMENSION")
	       (wcmatch oba "*DIMENSION")
	     (= oba str_type)
	     )
	   )
	  (setq flag nil)
	)
      (setq index (1+ index))
      )
    flag
    )
					;针对单一图元的处理流程
  (defun main_process(ent pt0 / a aa bl color elist flag_circulate flag_dynamic flag_secondclick mouse oba obj point_base)
    (setq elist (entget ent))
    (setq oba (cdr (assoc 0 elist)))
    (setq obj (vlax-ename->vla-object ent))
    (setq flag_dynamic nil);;默认启用动态比例
    (setq flag_secondClick nil) ;第二次鼠标左键，结束程序
    (cond
					;获得标注的全局比例
     ((wcmatch oba "*DIMENSION")
      (if (= (setq bl (vla-get-ScaleFactor (vlax-ename->vla-object ent))) nil) (setq bl 1))
      )
     ((= oba "TEXT") 			 (if (= (setq bl (cdr (assoc 40 elist))) nil) (setq bl 1)))	;取文字高度值作为实际比例
     ((= oba "MTEXT") 			 (if (= (setq bl (cdr (assoc 40 elist))) nil) (setq bl 1)))
     ((= oba "REGION")      (if (= (setq bl (cdr (assoc 48 elist))) nil) (setq bl 1)))
     ((= oba "LWPOLYLINE")  (if (= (setq bl (cdr (assoc 48 elist))) nil) (setq bl 1)))
     ((= oba "LINE")        (if (= (setq bl (cdr (assoc 48 elist))) nil) (setq bl 1)))
     ((= oba "CIRCLE")      (if (= (setq bl (cdr (assoc 48 elist))) nil) (setq bl 1)))
     ((= oba "ARC")         (if (= (setq bl (cdr (assoc 48 elist))) nil) (setq bl 1)))
     ((= oba "HATCH")       (setq bl (cdr (assoc 41 elist)))(setq flag_dynamic nil));;注意,这要关闭,因为初始不能为0
     ((= oba "INSERT")      (setq bl (cdr (assoc 41 elist)))(setq flag_dynamic nil));;注意,这要关闭,因为初始不能为0
     (t (alert "\n选择错误..."))
     )
    (if bl
	(progn
	  (setq flag_circulate T)
	  (while flag_circulate
	    (setq mouse (grread T 12 0))
	    (setq a (car mouse) aa (cadr mouse))
	    (cond
					;按键d或者D字高增加一倍
	     ((and (= 2 a) (or (= 100 aa) (= 68 aa)))	;2表示键盘输入,'(2 100)表示d键,'(2 68)表示D键
	      (setq flag_dynamic nil);;关闭动态比例
	      (setq bl (* bl 2))
	      
	      (bl_update oba bl)
	      
	      (text_update (rtos bl));;更新文字
	      
	      )
					;按键w或者W字高缩小一倍
	     ((and (= 2 a) (or (= 120 aa) (= 88 aa)))	;2表示键盘输入,'(2 120)表示x键,'(2 88)表示X键
	      (setq flag_dynamic nil);;关闭动态比例
	      (setq bl (/ bl 2))
					;(if(= oba "INSERT")(if (< 0 (1- bl))(setq bl (1- bl)))(setq bl (/ bl 2)));;块单独区分,加减更适用
					;(if (< bL 0.01)(setq bL 0.01));;防止比例为太杂****************************************
	      (bl_update oba bl)
	      (text_update (rtos bl));;更新文字
	      )
					;按键e或者E指定比例
	     ((and (= 2 a) (or (= 101 aa) (= 69 aa)))
	      (setq flag_dynamic nil);;关闭动态比例
	      (setq bl (getreal "\n指定缩放比例:"))
	      (bl_update oba bl)
	      (text_update (rtos bl));;更新文字
	      )
	     ((and(= a 5) flag_dynamic);鼠标移动和启用动态比例
	      (redraw)
	      (grdraw point_base aa 1);画向量
	      (setq bl(distance point_base aa))
	      (cond
	       ((and(< 0 bl)(< bl 0.1))
		(setq bl(* (fix (/ bl 0.01)) 0.01) color 1);规范0~1之间取值,模数=0.1
		)
	       ((and(<= 0.1 bl)(< bl 1))
		(setq bl(* (fix (/ bl 0.1)) 0.1) color 2);规范0~1之间取值,模数=0.1
		)
	       ((and(<= 1 bl)(< bl 10))
		(setq bl(* (fix (/ bl 0.5)) 0.5) color 3);规范1~10之间取值,模数=0.5
		)
	       ((and(<= 10 bl)(< bl 20))
		(setq bl(fix bl) color 4);规范10~20之间取值,模数=1
		)
	       ((and(<= 20 bl)(< bl 100))
		(setq bl(* (fix (/ bl 5)) 5) color 4);规范20~100之间取值,模数=5
		)
	       ((<= 100 bl)
		(setq bl(* (fix (/ bl 10)) 10) color 6);规范20~100之间取值,模数=10
		)
	       ((= 0 bl) (setq bl 1 color 6))
	       )
					;(if (/= bl_last bl);;当比例有变化
					;	(progn
					;		(setq bl_last bl)
					;		(bl_update oba bl);;更新比例
					;	)
					;)
	      (bl_update oba bl);;更新比例
	      (text_update (rtos bl));;更新文字
	      )
	     ((and(= a 5) (not flag_dynamic));鼠标移动,不启用动态比例
	      (text_update (strcat "当前比例：" (rtos bl 2 2) "\n【放大(D)/缩小(X)/指定(E)/动态(左键)/退出(空格)】"));;更新文字
	      )
	     ((= a 3)	;鼠标左键,启用动态比例
	      (setq flag_dynamic T)
	      (setq point_base aa)
	      (if (= flag_secondClick nil)	;识别第二次点击鼠标左键
		  (setq flag_secondClick T)
		(setq flag_circulate nil)
		)
	      )
	     ((or
	       (= 25 a) (= 11 a) ;右键
	       (and (= a 2) (= aa 13));回车
	       (and (= a 2) (= aa 32));或空格
	       )
	      (setq flag_circulate nil)
	      )
	     )
	    )
	  )
      (alert "\n比例不能为0")
      )
    (redraw)
    (if text (entdel text));删除临时文字
    (princ)
    )
					;对选择集的批量处理，以相对的缩放倍数为基准
  (defun main_process_batch(ss pt0 / a aa bl color flag_circulate flag_dynamic flag_secondclick list_bl list_ent mouse oba point_base)
    (setq list_ent (pickset_2list ss))
    
    (setq oba (cdr (assoc 0 (entget (nth 0 list_ent)))))
    (cond
     ((or (= oba "REGION") (= oba "LWPOLYLINE") (= oba "LINE") (= oba "CIRCLE") (= oba "ARC"))
      
      
      (setq list_bl (mapcar '(lambda (x) (vla-get-LinetypeScale(vlax-ename->vla-object x))) list_ent) )
      )
     ((= oba "HATCH")
      
      
      (setq list_bl (mapcar '(lambda (x) (vla-get-PatternScale(vlax-ename->vla-object x))) list_ent) )
      )
     ((= oba "INSERT")
      
      (setq list_bl (list 
		     (mapcar '(lambda (x) (vla-get-xscalefactor(vlax-ename->vla-object x))) list_ent)
		     (mapcar '(lambda (x) (vla-get-yscalefactor(vlax-ename->vla-object x))) list_ent)
		     (mapcar '(lambda (x) (vla-get-zscalefactor(vlax-ename->vla-object x))) list_ent)
		     ))
      
      )
     ((or (= oba "TEXT")(= oba "MTEXT"))
      (setq list_bl (mapcar '(lambda (x) (vla-get-Height(vlax-ename->vla-object x))) list_ent) )
      )
     ((wcmatch oba "*DIMENSION")
					;(vla-put-ScaleFactor obj bl)				
      (setq list_bl (mapcar '(lambda (x) (vla-get-ScaleFactor(vlax-ename->vla-object x))) list_ent) )
      )
     )
    
    
    (setq flag_dynamic nil);;默认启用动态比例
    (setq flag_secondClick nil) ;第二次鼠标左键，结束程序
    (setq bl 1)
    (setq flag_circulate T)
    (while flag_circulate
      (setq mouse (grread T 12 0))
      (setq a (car mouse) aa (cadr mouse))
      (cond
					;按键q或者Q字高增加一倍
       ((and (= 2 a) (or (= 100 aa) (= 68 aa)))	;2表示键盘输入,'(2 100)表示d键,'(2 68)表示D键
	(setq flag_dynamic nil);;关闭动态比例
	(setq bl (* bl 2))
	
	(bl_update_time_batch list_ent list_bl bl)
	(text_update (rtos bl));;更新文字
	)
					;按键w或者W字高缩小一倍
       ((and (= 2 a) (or (= 120 aa) (= 88 aa)))	;2表示键盘输入,'(2 120)表示x键,'(2 88)表示X键
	(setq flag_dynamic nil);;关闭动态比例
	(setq bl (/ bl 2))
					;(if(= oba "INSERT")(if (< 0 (1- bl))(setq bl (1- bl)))(setq bl (/ bl 2)));;块单独区分,加减更适用
					;(if (< bL 0.01)(setq bL 0.01));;防止比例为太杂****************************************
	(bl_update_time_batch list_ent list_bl bl)
	(text_update (rtos bl));;更新文字
	)
					;按键e或者E指定比例
       ((and (= 2 a) (or (= 101 aa) (= 69 aa)))
	(setq flag_dynamic nil);;关闭动态比例
	(setq bl (getreal "\n指定缩放比例:"))
	(bl_update_time_batch list_ent list_bl bl)
	(text_update (rtos bl));;更新文字
	)
       ((and(= a 5) flag_dynamic);鼠标移动和启用动态比例
	(redraw)
	(grdraw point_base aa 1);画向量
	(setq bl(distance point_base aa))
	(cond
	 ((and(< 0 bl)(< bl 0.1))
	  (setq bl(* (fix (/ bl 0.01)) 0.01) color 1);规范0~1之间取值,模数=0.1
	  )
	 ((and(<= 0.1 bl)(< bl 1))
	  (setq bl(* (fix (/ bl 0.1)) 0.1) color 2);规范0~1之间取值,模数=0.1
	  )
	 ((and(<= 1 bl)(< bl 10))
	  (setq bl(* (fix (/ bl 0.5)) 0.5) color 3);规范1~10之间取值,模数=0.5
	  )
	 ((and(<= 10 bl)(< bl 20))
	  (setq bl(fix bl) color 4);规范10~20之间取值,模数=1
	  )
	 ((and(<= 20 bl)(< bl 100))
	  (setq bl(* (fix (/ bl 5)) 5) color 4);规范20~100之间取值,模数=5
	  )
	 ((<= 100 bl)
	  (setq bl(* (fix (/ bl 10)) 10) color 6);规范20~100之间取值,模数=10
	  )
	 ((= 0 bl) (setq bl 1 color 6))
	 )
	(bl_update_time_batch list_ent list_bl bl);;更新比例
	(text_update (rtos bl));;更新文字
	)
       ((and(= a 5) (not flag_dynamic));鼠标移动,不启用动态比例
	(text_update (strcat "当前变化倍数：" (rtos bl 2 2) "\n【放大(D)/缩小(X)/指定(E)/动态(左键)/退出(空格)】"));;更新文字
	)
       ((= a 3)	;鼠标左键,启用动态比例
	(setq flag_dynamic T)
	(setq point_base aa)
	(if (= flag_secondClick nil)	;识别第二次点击鼠标左键
	    (setq flag_secondClick T)
	  (setq flag_circulate nil)
	  )
	)
       ((or
	 (= 25 a) (= 11 a) ;右键
	 (and (= a 2) (= aa 13));回车
	 (and (= a 2) (= aa 32));或空格
	 )
	(setq flag_circulate nil)
	)
       )
      )
    (redraw)
    (if text (entdel text));删除临时文字
    (princ)
    )
  
  
  (if (setq ss_first (ssget "I"))
					;预先选择的情况
      (progn
					;有这一句，才不影响后续使用ssget函数
	(sssetfirst nil)
	(if (= 1 (sslength ss_first))
	    (progn
	      (main_process (ssname ss_first 0) '(0 0 0))
	      )
	  (progn
	    (if (or
		 (boolean_typeOfSs ss_first "*DIMENSION")
		 (boolean_typeOfSs ss_first "TEXT")
		 (boolean_typeOfSs ss_first "MTEXT")
		 (boolean_typeOfSs ss_first "REGION")
		 (boolean_typeOfSs ss_first "LWPOLYLINE")
		 (boolean_typeOfSs ss_first "LINE")
		 (boolean_typeOfSs ss_first "CIRCLE")
		 (boolean_typeOfSs ss_first "ARC")
		 (boolean_typeOfSs ss_first "HATCH")
		 (boolean_typeOfSs ss_first "INSERT")
		 )
		(main_process_batch ss_first '(0 0 0))
	      (main_process (ssname ss_first 0) '(0 0 0))
	      )
	    )
	  )
	)
					;非预先选择的情况
    (progn
      (if (setq ent (entsel "\n选择要修改比例的图元[线型比例][块插入比例][填充比例]/<退出>..."))
	  (progn
					;第一个参数为图元名，第二个参数为鼠标的选择坐标位置
	    (main_process (car ent) (cadr ent))
	    )
	)
      )
    )
  
  
					;(error_end)
  )
