(@:add-menu "@������" "����߽���" '(@lab:get-cross-from-2layer))
(defun @lab:get-cross-from-2layer (/ layer1 layer2 pt1 pt2 ents1 ents2 res)
  (@::prompt '("������ͼ����ߵĽ����γ�Բ"
            " ������ѡ���һ��ͼ�㣬Ȼ����ʾѡ��ڶ���ͼ�㣬Ȼ��ѡ��ͼֽ��Χ����Χ�ڵ�ͼ��һ��ͼ������ߵĽ����γ�Բ��"))
  ;; ȡ��һ������ͼ����
  (setq layer1 (entity:get-layer (car (entsel))))
  (setq layer2 (entity:get-layer (car (entsel))))
  ;; ȡͼֽ��Χ
  (setq pt1 (getpoint "ȡͼֽ��Χ�ĵ�һ����"))
  ;; Ϊ��ֱ��Щ������ʹ�� getcorner ����
  (setq pt2 (getcorner pt1 "ȡͼֽ��Χ�ĵڶ�����"))

  ;; ������Ҫ����Ҫͬһͼ�������
  (setq res nil)
  (foreach ent1 (pickset:to-list
                 (ssget "c" pt1 pt2  (list (cons 0 (@:get-config '@curve:types))(cons 8 layer1))))
           (foreach ent2 (pickset:to-list
                          (ssget "c" pt1 pt2  (list (cons 0 (@:get-config '@curve:types))(cons 8 layer2))))
                    (setq res (append res
                                      (curve:inters ent1 ent2 acextendnone)
                                      ))))
  ;; ȥ����ֵ
  (setq res (vl-remove nil res))
  ;; ��Բ,�뾶 50
  (entity:make-circle res 50)
  )
