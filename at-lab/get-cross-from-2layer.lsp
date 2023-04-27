(@:add-menu "@试验室" "层间线交点" '(@lab:get-cross-from-2layer))
(defun @lab:get-cross-from-2layer (/ layer1 layer2 pt1 pt2 ents1 ents2 res)
  (@:help '("求两个图层的线的交点形成圆"
            " 操作：选择第一个图层，然后提示选择第二个图层，然后选择图纸范围，范围内的图层一和图层二的线的交点形成圆形"))
  ;; 取第一、二个图层名
  (setq layer1 (entity:get-layer (car (entsel))))
  (setq layer2 (entity:get-layer (car (entsel))))
  ;; 取图纸范围
  (setq pt1 (getpoint "取图纸范围的第一个点"))
  ;; 为了直观些，我们使用 getcorner 函数
  (setq pt2 (getcorner pt1 "取图纸范围的第二个点"))

  ;; 需求中要求不需要同一图层的曲线
  (setq res nil)
  (foreach ent1 (pickset:to-list
                 (ssget "c" pt1 pt2  (list (cons 0 (@:get-config '@curve:types))(cons 8 layer1))))
           (foreach ent2 (pickset:to-list
                          (ssget "c" pt1 pt2  (list (cons 0 (@:get-config '@curve:types))(cons 8 layer2))))
                    (setq res (append res
                                      (curve:inters ent1 ent2 acextendnone)
                                      ))))
  ;; 去掉空值
  (setq res (vl-remove nil res))
  ;; 画圆,半径 50
  (entity:make-circle res 50)
  )
