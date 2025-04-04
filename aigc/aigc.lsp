(@:add-menus
 '("AIGC"
   ("*生成内容" "(aigc:gen-content)")
   ("本地部署" "(aigc:Local-deployment)")
   ("下载模型" "(aigc:pull-models)")
   ))
(if (null aigc:*models*)
    (setq aigc:*models*
	  (list "gemma3:1b"
		"gemma3:4b"
		"deepseek-r1:1.5b"
		"deepseek-r1:7b"
		"qwen2.5:0.5b"
		"qwen2.5:1.5b"
		"qwen2.5:3b"
		"qwen2.5:7b"
		"qwen2.5-coder:0.5b"
		"qwen2.5-coder:1.5b"
		"qwen2.5-coder:3b"
		"qwen2.5-coder:7b"
		"opencoder:1.5b"
		"starcoder2:3b"
		"starcoder2:7b"
		"codegemma:2b"
		"codegemma:7b"
		)))
(defun aigc:gen-content () 
  (@:help "选择一个文本，根据文本的条件和要求生成内容（测试版）")
  (@:prompt "请选择一个文本:")
  (if 
    (and 
      (setq req (car (pickset:to-list (ssget ":S" '((0 . "*text"))))))
      (setq str-req (text:remove-fmt (text:get-mtext req))))
   
    (progn 
      (@:load-module 'aibot)
      (setq req-box (entity:getbox req 0))
      (setq fontsize (entity:getdxf req 40))

      (setq ent-mtext (entity:make-mtext 
                        "{\\C2;正在生成内容 .... }"
                        (polar 
                          (car req-box)
                          (* 1.5 pi)
                          fontsize)
                        fontsize
                        (* fontsize 40)
                        (* fontsize 40 3)))
      (entity:putdxf ent-mtext 62 3)
      (redraw ent-mtext)
      (if (setq response (@:aigc str-req)) 
        (progn 
          (setq response (text:from-markdown response))
          (vla-put-textstring (e2o ent-mtext) response))
        (progn 
     (@:prompt "生成内容被 中断。")
    (entdel ent-mtext))))))
(defun aigc:Local-deployment ()
  (@::help '("将LLM大语言模型部署到本地电脑上"
	     "因需要下载1G左右的安装包，首次运行需耐心等待."))
  (if (member "ollama.exe"(sys:list-process-name))
      (@::prompt"本地Ollama已在运行中。")
      (if (findfile (strcat (getenv "userprofile")"\\OllamaSetup.exe"))
	  (if (> (vl-file-size (strcat (getenv "userprofile")"\\OllamaSetup.exe"))
		 1e9)
	      (progn
		(@::prompt "安装Ollama")
		(startapp (strcat (getenv "userprofile")"\\OllamaSetup.exe"))
		)
	      (@::prompt "正在下载，请等下载完成后再试"))
	  (progn
	    (@::prompt "本地没有运行Ollama。开始下载，您可以进行其它工作，等下载完成后安装使用。")
	    ;;(@::cmd "shell" "winget install ollama.ollama"))
	    (@::down-by-powershell
	     "https://ollama.com/download/OllamaSetup.exe"
	     (strcat (getenv "userprofile")"\\OllamaSetup.exe")
	     (@::timestamp))
	    )))
  (princ)
  )
(defun aigc:pull-models ()
  (@::help '("下载LLM 大语言模型"))
  (setq models (list "gemma3:1b"
		     "gemma3:4b"
		     "deepseek-r1:1.5b"
		     "deepseek-r1:7b"
		     "qwen2.5:0.5b"))
		     
  (if (member "ollama.exe"(sys:list-process-name))
      (progn
	(foreach model
		 (ui:select-multi "请选择要下载的模型"
				  aigc:*models*)
		 (@::cmd "shell" (strcat "ollama pull " model))))
      (if (ui:confirm "本地没有运行Ollama,是否部署?")
	  (aigc:Local-deployment))
      ))
      
