if (-not ( Get-Command winget -errorAction SilentlyContinue)) {
    Add-AppxPackage -RegisterByFamilyName -MainPackage Microsoft.DesktopAppInstaller_8wekyb3d8bbwe
}
if (-not ( Get-Command python3 -errorAction SilentlyContinue)) {
    winget install python.python.3.13
}

if (-not ( Get-Command python3 -errorAction SilentlyContinue)) {
    Read-Host -Prompt "自动安装 Python3 失败.无法执行本功能。"
} else {
    if ( Get-Command pip3 -errorAction SilentlyContinue) {
	pip3 install -U atlisp-ole
	Read-Host "安装完成，按任意键退出"
    } else {
	Read-Host -Prompt "PATH中没有发现 pip3 包管理器.无法执行本功能。"
    }
}

