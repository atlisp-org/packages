Set fs = CreateObject("Scripting.FileSystemObject")

Set ws = CreateObject("WScript.Shell")

Path = fs.GetParentFolderName(WScript.ScriptFullName)

ws.RegWrite "HKEY_CURRENT_USER\Software\InkPaint Computing\PSK\" _
            & "\Install Path", Path

Set fs = Nothing : Set ws = Nothing

MsgBox "ÉèÖÃÂ·¾¶Îª:" _
       & vbLf & Path, _
       vbInformation, "PSK"