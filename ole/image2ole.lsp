;; 图片转 OLE / 裁剪 OLE 图像
;; iMini 功能复现：image2ole（图片转 OLE 插入）、oleimage_crop（裁剪 OLE/图像）
;; 注：部分 CAD（如 ZWCAD）无 startundo/endundo，统一用 UNDO Begin/End 命令实现。

(@:add-menu "ole图像" "图片转OLE" "(ole:image2ole)")
(@:add-menu "ole图像" "裁剪图像" "(ole:ole-crop)")

(defun ole:create-ole (ms img-file pt / res)
  "尝试以 OLE 方式插入图片（部分 CAD 提供 vla-CreateOLEObject）。"
  "成功返回 OLE vla-object；失败返回 nil 并降级说明"
  "示例: (ole:create-ole ms \"C:/img/1.png\" '(0 0 0))"
  (setq res
        (vl-catch-all-apply
          'vla-CreateOLEObject
          (list ms img-file (point:to-ax pt))))
  (if (vl-catch-all-error-p res)
      (progn
        (princ "image2ole: vla-CreateOLEObject 不可用，降级为光栅图像插入。")
        nil)
    res))

(defun ole:add-raster (ms img-file pt scale / res)
  "以光栅图像方式插入图片（vla-AddRaster）。"
  "成功返回光栅图像 vla-object；失败返回 nil"
  "示例: (ole:add-raster ms \"C:/img/1.png\" '(0 0 0) 1.0)"
  (setq res (vl-catch-all-apply
              'vla-AddRaster
              (list ms img-file (point:to-ax pt) scale 0.0)))
  (if (vl-catch-all-error-p res) nil res))

(defun ole:ai-image2ole (img-file pt scale / doc ms obj)
  "AI 入口：将图片文件作为 OLE/图像对象插入到指定点，优先 OLE，失败降级光栅图像。
img-file: png/jpg 等图片文件路径。pt: 插入点（默认 0,0,0）。scale: 缩放比例（默认 1.0）。"
  "插入对象 ename；失败返回 nil"
  "示例: (ole:ai-image2ole \"C:/img/1.png\" '(0 0 0) 1.0)"
  (if (null img-file)
      (progn (princ "image2ole: 未提供图片路径。") nil)
    (if (null (findfile img-file))
        (progn (princ (strcat "image2ole: 找不到文件 " img-file)) nil)
      (progn
        (if (null pt) (setq pt '(0 0 0)))
        (if (null scale) (setq scale (@::get-config 'ole:scale)))
        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
              ms (vla-get-ModelSpace doc))
        (setq obj (ole:create-ole ms img-file pt))
        (if (null obj)
            (setq obj (ole:add-raster ms img-file pt scale)))
        (if obj
            (progn (vla-update obj)
                   (vlax-vla-object->ename obj))
          (progn (princ "image2ole: 插入失败。") nil))))))

(defun ole:image2ole (/ img-file pt scale res)
  "将图片文件作为 OLE/图像对象插入到指定点。"
  "插入对象 ename；失败 nil"
  "示例: 运行 ole:image2ole 后按提示选择图片与插入点"
  (if (setq img-file (getfiled "选择图片文件" "" "png" 8))
      (progn
        (setq pt (getpoint "\n插入点<0,0,0>:"))
        (if (null pt) (setq pt '(0 0 0)))
        (setq scale (getreal "\n缩放比例<1.0>:"))
        (if (null scale) (setq scale 1.0))
        (command "_.UNDO" "Begin")
        (setq res (ole:ai-image2ole img-file pt scale))
        (command "_.UNDO" "End")
        res))
  (princ))

(defun ole:clip-raster (obj corner1 corner2 / x1 y1 x2 y2 pts sa res)
  "按矩形对角点裁剪光栅图像（设置 ClipBoundary 为矩形四点）。"
  "裁剪后图像 ename；失败 nil"
  "示例: (ole:clip-raster (e2o (car (entsel))) '(0 0 0) '(100 100 0))"
  (setq x1 (min (car corner1) (car corner2))
        y1 (min (cadr corner1) (cadr corner2))
        x2 (max (car corner1) (car corner2))
        y2 (max (cadr corner1) (cadr corner2)))
  (setq pts (list x1 y1 x2 y1 x2 y2 x1 y2))
  (setq sa (vlax-make-safearray vlax-vbDouble '(0 . 7)))
  (vlax-safearray-fill sa pts)
  (setq res (vl-catch-all-apply 'vla-put-ClipBoundary
                                 (list obj (vlax-make-variant sa))))
  (if (vl-catch-all-error-p res)
      (progn
        (princ (strcat "ole-crop: 裁剪失败 "
                       (vl-catch-all-error-message res)))
        nil)
    (vlax-vla-object->ename obj)))

(defun ole:fit-ole (obj corner1 corner2 / x1 y1 x2 y2 w h res)
  "OLE 无法裁剪，改为按矩形区域重新调整位置与大小（等效重新插入）。"
  "调整后 OLE ename；失败 nil"
  "示例: (ole:fit-ole (e2o (car (entsel))) '(0 0 0) '(100 100 0))"
  (setq x1 (min (car corner1) (car corner2))
        y1 (min (cadr corner1) (cadr corner2))
        x2 (max (car corner1) (car corner2))
        y2 (max (cadr corner1) (cadr corner2))
        w (- x2 x1)
        h (- y2 y1))
  (setq res
        (vl-catch-all-apply
          '(lambda ()
             (vla-put-LockAspectRatio obj :vlax-false)
             (vla-put-width obj w)
             (vla-put-height obj h)
             (vla-put-InsertionPoint obj (point:to-ax (list x1 y1 0.0)))
             (vla-update obj))))
  (if (vl-catch-all-error-p res)
      (progn
        (princ (strcat "ole-crop: 调整失败 "
                       (vl-catch-all-error-message res)))
        nil)
    (vlax-vla-object->ename obj)))

(defun ole:ai-ole-crop (ent corner1 corner2 / obj name)
  "AI 入口：按矩形边界裁剪 IMAGE/OLE。IMAGE 用 ClipBoundary 裁剪；OLE 不支持裁剪，改为按矩形区域调整大小。
ent: IMAGE 或 OLE 对象 ename。corner1/corner2: 裁剪矩形对角点。"
  "裁剪后对象 ename；失败返回 nil"
  "示例: (ole:ai-ole-crop (car (entsel)) '(0 0 0) '(100 100 0))"
  (if (or (null ent) (null corner1) (null corner2))
      (progn (princ "ole-crop: 参数缺失。") nil)
    (progn
      (setq obj (vl-catch-all-apply 'e2o (list ent)))
      (if (or (null obj) (vl-catch-all-error-p obj))
          (progn (princ "ole-crop: 无效对象。") nil)
        (progn
          (setq name (vla-get-ObjectName obj))
          (cond
            ((= name "AcDbRasterImage")
             (ole:clip-raster obj corner1 corner2))
            ((vl-string-search "AcDbOle" name)
             (princ "ole-crop: OLE 对象不支持裁剪，已按矩形区域调整位置与大小。")
             (ole:fit-ole obj corner1 corner2))
            (t
             (princ (strcat "ole-crop: 不支持的对象类型 " name))
             nil)))))))

(defun ole:ole-crop (/ ent p1 p2 res)
  "裁剪 IMAGE/OLE 图像到矩形边界。"
  "裁剪后对象 ename；失败 nil"
  "示例: 运行 ole:ole-crop 后按提示选择图像并框选范围"
  (if (setq ent (car (entsel "\n选择要裁剪的图像/OLE:")))
      (progn
        (setq p1 (getpoint "\n裁剪范围第一角点:"))
        (if p1
            (setq p2 (getcorner p1 "\n裁剪范围对角点:")))
        (if (and p1 p2)
            (progn
              (command "_.UNDO" "Begin")
              (setq res (ole:ai-ole-crop ent p1 p2))
              (command "_.UNDO" "End")
              res)
          (princ "ole-crop: 未指定裁剪范围。"))))
  (princ))
