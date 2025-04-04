(@:add-menus
 '("AIGC"
   ("*��������" "(aigc:gen-content)")
   ("���ز���" "(aigc:Local-deployment)")
   ("����ģ��" "(aigc:pull-models)")
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
  (@:help "ѡ��һ���ı��������ı���������Ҫ���������ݣ����԰棩")
  (@:prompt "��ѡ��һ���ı�:")
  (if 
    (and 
      (setq req (car (pickset:to-list (ssget ":S" '((0 . "*text"))))))
      (setq str-req (text:remove-fmt (text:get-mtext req))))
   
    (progn 
      (@:load-module 'aibot)
      (setq req-box (entity:getbox req 0))
      (setq fontsize (entity:getdxf req 40))

      (setq ent-mtext (entity:make-mtext 
                        "{\\C2;������������ .... }"
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
     (@:prompt "�������ݱ� �жϡ�")
    (entdel ent-mtext))))))
(defun aigc:Local-deployment ()
  (@::help '("��LLM������ģ�Ͳ��𵽱��ص�����"
	     "����Ҫ����1G���ҵİ�װ�����״����������ĵȴ�."))
  (if (member "ollama.exe"(sys:list-process-name))
      (@::prompt"����Ollama���������С�")
      (if (findfile (strcat (getenv "userprofile")"\\OllamaSetup.exe"))
	  (if (> (vl-file-size (strcat (getenv "userprofile")"\\OllamaSetup.exe"))
		 1e9)
	      (progn
		(@::prompt "��װOllama")
		(startapp (strcat (getenv "userprofile")"\\OllamaSetup.exe"))
		)
	      (@::prompt "�������أ����������ɺ�����"))
	  (progn
	    (@::prompt "����û������Ollama����ʼ���أ������Խ���������������������ɺ�װʹ�á�")
	    ;;(@::cmd "shell" "winget install ollama.ollama"))
	    (@::down-by-powershell
	     "https://ollama.com/download/OllamaSetup.exe"
	     (strcat (getenv "userprofile")"\\OllamaSetup.exe")
	     (@::timestamp))
	    )))
  (princ)
  )
(defun aigc:pull-models ()
  (@::help '("����LLM ������ģ��"))
  (setq models (list "gemma3:1b"
		     "gemma3:4b"
		     "deepseek-r1:1.5b"
		     "deepseek-r1:7b"
		     "qwen2.5:0.5b"))
		     
  (if (member "ollama.exe"(sys:list-process-name))
      (progn
	(foreach model
		 (ui:select-multi "��ѡ��Ҫ���ص�ģ��"
				  aigc:*models*)
		 (@::cmd "shell" (strcat "ollama pull " model))))
      (if (ui:confirm "����û������Ollama,�Ƿ���?")
	  (aigc:Local-deployment))
      ))
      
