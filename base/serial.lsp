;;;获取mac地址-32位系统用
(defun-q hdinfo:get-mac (/ i mac s str svr wmi)
  "获取mac地址，不一定有用。"
  (vl-load-com)
  (setq wmi (vlax-create-object "WbemScripting.SWbemLocator"))
  (setq svr (vlax-invoke wmi 'ConnectServer))
  (setq str "Select * from Win32_NetworkAdapterConfiguration Where IPEnabled=TRUE")
  (setq mac (vlax-invoke svr 'ExecQuery str))
  (vlax-for i mac
	    (setq s (cons (vlax-get i 'macAddress) s))
	    )
  (vlax-release-object mac)
  (vlax-release-object svr)
  (vlax-release-object wmi)       
  (car s)               
  )
;获取硬盘序列号
(defun-q hdinfo:get-hd-serial (/ lccon lox objw ret serx sn)
  "获取硬盘序列号,不一定有用。"
  (setq serx '())
  (if (setq objw (vlax-create-object "wbemscripting.swbemlocator"))
      (progn
	(setq lccon (vlax-invoke objw 'connectserver "." "\\root\\cimv2"  ""  ""  ""  ""  128 nil))
	(setq lox (vlax-invoke lccon 'execquery "select serialnumber,tag from win32_physicalmedia"))
	(vlax-for item lox (setq serx (cons (list (vlax-get item 'tag) (vlax-get item 'serialnumber)) serx)))
	(vlax-release-object lox)
	(vlax-release-object lccon)
	(vlax-release-object objw)
	)
    )
  serx
  )
(defun-q hdinfo:get-cpuid (/ Vlist VObj lcom lExecQuery item)
  "获取CPU ID,不一定有用。"
  (vl-load-com)
  (setq Vlist '())
  (if (setq VObj (vlax-create-object "wbemscripting.swbemlocator"))
      (progn
	(SETQ lcom (VLAX-INVOKE
		    VObj       'ConnectServer     "."
		    "\\root\\cimv2"  ""     ""
		    ""       ""  128     nil
		    ) ;_ 结束VLAX-INVOKE
	      ) ;_ 结束SETQ
	(setq lExecQuery
	      (vlax-invoke
	       lcom
	       'ExecQuery
	       ;;"Select * From Win32_BIOS"
	       "Select * from Win32_Processor"
	       ) ;_ 结束vlax-invoke
	      ) ;_ 结束setq
	(vlax-for item lExecQuery
		  (setq Vlist (vlax-get item 'ProcessorId) ;_ 结束cons
			) ;_ 结束setq
		  ) ;_ 结束vlax-for
	(vlax-release-object lExecQuery)	
	(vlax-release-object lcom)
	(vlax-release-object Vobj)
	)
    )
  Vlist
  )
