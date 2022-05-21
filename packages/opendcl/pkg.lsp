;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; base -- @lisp 基础工具包
;;; Author: VitalGG<vitalgg@gmail.com>
;;; Description: 基于 AutoLisp/VisualLisp 开发的绘图工具集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(@:def-pkg '((:name . "opendcl") ;; storage name 
	     (:full-name . "OpenDCL运行时库") ;; real name 
	     (:version . "1.0.15")
	     (:author . "VitalGG")
	     (:email . "vitalgg@gmail.com")
	     (:description . "根据CAD运行环境，下载相匹配的OpenDCL运行时支持库和语言包,不用下载全部文件。")
	     (:category . "Lib") ;; 分类
	     (:compatible .  ("acad"))
	     (:opensource . 1) ;; 是否开源
	     (:licenses . "GPL") ;; 开源许可
	     (:url . "http://atlisp.cn") ;; 主页
	     (:files . ("opendcl"
		       ))))


;; Local variables:
;; coding: gb2312
;; End: 
