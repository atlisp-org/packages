;;; 视图截图
;;; 用文档 GetImage 方法把当前视图/指定区域保存为位图。
;;; 注意：部分 CAD 版本不支持 GetImage 方法，此时返回 nil 并打印原因。

(@:add-menu "通用打印" "视图截图" "(@plot:screenshot)")

(defun @plot:view-window (/ view w h)
  "取当前视图窗口 (左下点 右上点)；读不到视图参数时退回图形范围。"
  (if (setq view (getvar "view"))
      (progn
        (setq w (car (cdddr view)))
        (setq h (cadr (cdddr view)))
        (list (list (caddr view) (cadddr view) 0)
              (list (+ (caddr view) w) (+ (cadddr view) h) 0)))
      (list (getvar "extmin") (getvar "extmax"))))

(defun @plot:ai-screenshot (out-file region / doc fmt ext w h pw ph win res)
  "AI 调用入口：视图截图保存为图片，成功返回 t，失败返回 nil。
out-file: 输出图片完整路径（.png/.jpg/.bmp）
region: 截图窗口 (左下点 右上点) 模型坐标；nil 用当前视图
示例: (@plot:ai-screenshot \"D:/shot.png\" nil)"
  (if (null out-file)
      (progn (princ "\n[at-plot] 截图：未指定输出文件。") nil)
      (progn
  (setq ext (strcase (vl-filename-extension out-file)))
  (setq fmt (cond
              ((or (= ext ".JPG") (= ext ".JPEG")) "JPG")
              ((= ext ".BMP") "BMP")
              (t "PNG")))
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (if (null region)
      (setq region (@plot:view-window)))
  (setq w (- (car (cadr region)) (car (car region))))
  (setq h (- (cadr (cadr region)) (cadr (car region))))
  (if (<= w 0.0)
      (setq w 1.0))
  (setq pw 1024.0)
  (setq ph (max 1.0 (round (* pw (/ h w)))))
  (setq win (vlax-make-variant
             (vlax-safearray-fill
              (vlax-make-safearray vlax-vbDouble '(0 . 3))
              (list (car (car region)) (cadr (car region))
                    (car (cadr region)) (cadr (cadr region))))))
  (setq res (vl-catch-all-apply
             'vlax-invoke
             (list doc 'GetImage out-file fmt (fix pw) (fix ph) win)))
  (if (vl-catch-all-error-p res)
      (progn
        (princ (strcat "\n[at-plot] 截图失败（此 CAD 可能不支持 GetImage 方法）: "
                       (vl-catch-all-error-message res)))
        nil)
    t))))

(defun @plot:screenshot (/ file)
  "交互：把当前视图保存为图片。"
  (@:help "将当前视图截图保存为 PNG/JPG 图片。")
  (setq file (getfiled "选择图片保存路径"
                       (system:dir (@:get-config '@plot:export-path))
                       "png" 8))
  (if file
      (@plot:ai-screenshot file nil))
  (princ))
