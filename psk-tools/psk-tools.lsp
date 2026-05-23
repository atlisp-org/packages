;; Ω‚—π
(if (and (null (findfile "packages/psk-tools/config/profiles/default/layer-configration.csv"))
	 (findfile (strcat (@:package-path "psk-tools") "psk-tools.zip")))
    (@:unzip (strcat (@:package-path "psk-tools") "psk-tools.zip") "packages/psk-tools"))
(vl-registry-write "HKEY_CURRENT_USER\\Software\\InkPaint Computing\\PSK" "Install Path" (vl-filename-directory (@:package-path "psk-tools")))

(@:load "psk-tools/functions")
(@:load "psk-tools/psk")
