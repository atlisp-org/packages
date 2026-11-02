;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @lisp 二维码生成（qrencode）
;; 依赖 bin/QRencodeForLisp.exe（Windows）生成矩阵，再用多段线绘制到 DWG。
;; AI 入口 qrencode:ai-make 不弹窗、不下载、不取点，失败返回 nil。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require '(entity:* block:*))

(@::define-config 'qrencode:scale 100 "二维码绘制比例。")

(@::add-menu "文本2" "生成二维码" "(qrencode:draw)")

(defun qrencode:exe ()
  "QRencodeForLisp.exe 全路径，不存在返回 nil。"
  "文件路径或 nil"
  "(qrencode:exe)"
  (findfile (strcat @::*prefix* "bin\\QRencodeForLisp.exe")))

(defun qrencode:mkpline (pt col scale)
  "在 pt 画一个二维码模块（宽度 = scale），返回 ename。"
  "ename 或 nil"
  "(qrencode:mkpline '(0 0) 0 100)"
  (entmake (list '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbPolyline")
                 (cons 420 col)
                 (cons 90 2)
                 (cons 10 pt)
                 (cons 10 (polar pt 0 scale))
                 (cons 43 scale)))
  (entlast))

(defun qrencode:qr-data (str / wscript stdout wsreturn outstr exe)
  "调用 QRencodeForLisp.exe，返回二维码矩阵列表；失败 nil。"
  "列表或 nil"
  "(qrencode:qr-data \"hello\")"
  (setq exe (qrencode:exe))
  (if (null exe)
      nil
      (progn
        (setq wscript (vl-catch-all-apply
                       'vlax-get-or-create-object (list "WScript.Shell")))
        (if (or (null wscript) (vl-catch-all-error-p wscript))
            nil
            (progn
              (setq wsreturn
                    (vl-catch-all-apply
                     'vlax-invoke
                     (list wscript 'exec
                           (strcat "\"" exe "\" \"" str "\""))))
              (if (or (null wsreturn) (vl-catch-all-error-p wsreturn))
                  nil
                  (progn
                    (setq stdout (vlax-get-property wsreturn 'StdOut))
                    (setq outstr (vlax-invoke stdout 'Readall))
                    (if (and (stringp outstr) (> (strlen outstr) 0))
                        (setq outstr (vl-catch-all-apply
                                      'read (list outstr)))
                        (setq outstr nil))
                    (if (or (vl-catch-all-error-p outstr) (null outstr))
                        nil
                        outstr))))))))

(defun qrencode:render (str pt / lst scale ents ptbase pt-ins blkname)
  "把字符串的二维码绘制到 pt（左下角），返回块参照 ename；失败 nil。"
  "块参照 ename 或 nil"
  "(qrencode:render \"hello\" '(0 0))"
  (setq scale (@::get-config 'qrencode:scale))
  (if (or (null scale) (not (numberp scale)) (<= scale 0.0))
      (setq scale 100.0))
  (setq lst (qrencode:qr-data str))
  (if (null lst)
      (progn (princ "qrencode: 无法生成二维码（缺少 exe 或调用失败）") nil)
      (progn
        (setq ents nil)
        (setq ptbase pt)
        (setq pt-ins (polar ptbase (* 0.5 pi) (* scale 0.5)))
        (setq ents
              (cons (entity:make-rectangle
                     pt-ins
                     (polar (polar ptbase (* 1.75 pi)
                                   (* scale (length (car lst)) (sqrt 2.0)))
                            (* 0.5 pi) (* scale 0.5)))
                    ents))
        (foreach n lst
          (setq pt ptbase)
          (foreach k n
            (setq ents (cons (qrencode:mkpline
                              pt
                              (if (= k 1) 0 16777215)
                              scale)
                             ents))
            (setq pt (polar pt 0 scale)))
          (setq ptbase (polar ptbase (* pi 1.5) scale)))
        (setq ents (vl-remove nil ents))
        (setq blkname (strcat "qr-" (@::timestamp)))
        (if (entity:block ents blkname pt-ins)
            (block:insert blkname "" pt-ins 0 1)
            nil))))

(defun qrencode:ai-make (str pt)
  "AI 调用入口：在 pt 绘制二维码，返回块参照 ename；失败 nil。
  不弹窗、不下载、不调用 getpoint。
  示例: (qrencode:ai-make \"https://atlisp.cn\" '(0 0))"
  "块参照 ename 或 nil"
  "(qrencode:ai-make \"hello\" '(0 0))"
  (if (and str pt (qrencode:exe))
      (qrencode:render str pt)
      (progn
        (princ (if (qrencode:exe)
                   "qrencode:ai-make 参数不能为空"
                   "qrencode:ai-make 缺少 bin/QRencodeForLisp.exe"))
        nil)))

(defun qrencode:make (str / pt)
  "交互：输入绘制点后生成二维码（exe 缺失时尝试下载）。"
  "块参照 ename 或 nil"
  "(qrencode:make \"hello\")"
  (if (null (qrencode:exe))
      (progn
        (@::down-file "bin/QRencodeForLisp.exe")
        (alert "正在下载 QRencode，请稍候...")
        (sleep 10)))
  (if (setq pt (getpoint "请输入二维码绘制位置:"))
      (qrencode:render str pt)
      nil))

(defun qrencode:draw (/ str)
  "选择文字生成二维码（交互入口）。"
  "块参照 ename 或 nil"
  "(qrencode:draw)"
  (@::help "选择文字生成QR二维码")
  (setq str (text:get-mtext (car (entsel "请选择一个文本:"))))
  (if str
      (qrencode:make (text:remove-fmt str))
      (progn (princ "not select text") nil)))
