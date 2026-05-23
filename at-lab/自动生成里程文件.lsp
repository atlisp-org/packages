;;; 自动生成里程文件.lsp
;;; 作者：wosyuwu（优化版使用 at-lib）
;;; 功能：从CASS生成里程文件
;;; 使用方法：在CAD中输入 SS4 使用

(defun c:ss4 (/ poly ss data i si ins_pt param filename f sn elevations avg-elevation)
  (vl-load-com)

  (prompt "\n** 自动生成里程文件")

  ;; 1. 选择并验证多段线
  (if (not (setq poly (car (entsel "\n选择多段线: "))))
    (princ "\n未选择对象.")

    (if (not (wcmatch (cdr (assoc 0 (entget poly))) "*POLYLINE"))
      (alert "必须选择多段线!")

      (progn
        (setq poly (vlax-ename->vla-object poly))

        ;; 2. 选择GC200图块
        (if (not (setq ss (ssget "_X" '((0 . "INSERT") (2 . "gc200") (8 . "GCD")))))
          (alert "未找到GC200图块!")

          (progn
            ;; 3. 收集数据
            (setq data '())
            (setq i 0)
            (repeat (sslength ss)
              (setq si (ssname ss i))
              (setq ins_pt (cdr (assoc 10 (entget si))))
              (setq ins_pt (trans ins_pt si 1))
              (setq ins_pt (point:2d->3d ins_pt))

              (if (setq param (vlax-curve-getparamatpoint poly ins_pt))
                (setq data (cons (list (vlax-curve-getdistatparam poly param) (caddr ins_pt)) data))
              )
              (setq i (1+ i))
            )

            ;; 4. 排序（使用无损排序）
            (setq data (list:sort data (function (lambda (a b) (< (car a) (car b))))))

            ;; 5. 写入文件
            (if (not (setq filename (getfiled "保存里程文件" "" "HDM" 1)))
              (princ)

              (if (not (setq f (open filename "w")))
                (alert "无法创建文件!")

                (progn
                  (write-line "BEGIN" f)
                  (foreach item data
                    (write-line (strcat (m:rtos (car item) 3) "," (m:rtos (cadr item) 3)) f)
                  )
                  (close f)

                  ;; 6. 显示统计信息
                  (setq sn (length data))
                  (if (> sn 0)
                    (progn
                      (setq elevations (mapcar (function cadr) data))
                      (setq avg-elevation (/ (apply '+ elevations) sn))
                      (alert
                        (strcat
                          "已获取 " (itoa sn) " 个高程点\n"
                          "平均高程: " (m:rtos avg-elevation 3) "\n"
                          "最低高程: " (m:rtos (m:minlist elevations) 3) "\n"
                          "最高高程: " (m:rtos (m:maxlist elevations) 3) "\n"
                          "里程文件已保存至:\n" filename
                        )
                      )
                    )
                    (alert "未找到有效高程点!")
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (princ)
)
