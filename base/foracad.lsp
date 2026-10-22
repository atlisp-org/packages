;; AutoCAD 平台专用处理
;; 注意: condition.lsp / format.lsp 已在 pkg.lsp 的 :files 列表中,
;; 编译 base-whole 时会自动合并加载, 此处不可再显式 @:load,
;; 否则在 AutoCAD 上会二次执行 define-config, 触发
;; "Config redefined: BASE:SYSVAR" 警告 (2026-02 修复)
