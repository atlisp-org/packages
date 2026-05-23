
;; PORTS 格式如下每一项为 (管口id, WCS位置, 方向, 特征尺寸)
;; 特征尺寸 
;; ((10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0) (11 (45632.5 13384.1 0.0) (1.0 0.0) 0.0))

(defun psk-port-id (port)
  (car port)
)

(defun psk-port-pos (port)
  (cadr port)
)
;; (psk-port-dir '(10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0))
;; 4.71239
(defun psk-port-dir (port)
  (caddr port)
)

(defun psk-port-angle (port)
  (angle '(0. 0.) (psk-port-dir port))
)

(defun psk-port-size (port)
  (cadddr port)
)

;; (psk-ports-sort '((10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0) (11 (45632.5 13384.1 0.0) (1.0 0.0) 0.0)) '(45632.5 13384.1 0.0))
;; ((11 3032.96 3824.84 0.0) (10 2707.08 3191.4 0.0))
;; 返回格式((靠近指定点的管口) (剩余管口1) (剩余管口2) ...)
(defun psk-ports-sort (ports p / e e1 e2)
  (mapcar
    (function cdr)
    (vl-sort
      (mapcar (function cons)
	      (mapcar (function (lambda (e) (distance p e)))
		      (mapcar 'psk-port-pos ports)
	      )
	      ports
      )
      (function (lambda (e1 e2) (< (car e1) (car e2))))
    )
  )
)

;; 将管口尺寸编码以保存
;; 矩形风管保存为组码1010的点，使用前两位表示管口的宽及高
;; 圆管用一个1040的浮点数表示 DN或管径
(defun psk-port-packsize (size)
  (if (vl-consp size)
    (cons 1010 size)
    (cons 1040 size)
  )
)

(defun psk-port-packpos	(pos)
  (if (equal pos '(0 0 0) 1e-6)
    (cons 1010 pos)
    (cons 1013 pos)
  )
)

;; (setq en (car (entsel)))
;;;(p-xdata-set en "PSK-PORT" (psk-port-pack '("CD" (0 0 0) (1 0 0) 20)))
;; (psk-port-pack '("CD" (0 0 0) (1 0 0) 20))
;; ((1000 . "CD") (1010 0 0 0) (1013 1 0 0) (1040 . 20) (1004 . "00"))
(defun psk-port-pack (lst /)
  (list	(cons 1000 (psk-port-id lst))
	(psk-port-packpos (psk-port-pos lst))
	(cons 1013 (psk-port-dir lst))
	(psk-port-packsize (psk-port-size lst))
	(cons 1004 "") ;_ 结束标记
  )
)


(defun psk-ports-pack (lst)
  (apply (function append)
	 (mapcar (function (lambda (e) (psk-port-pack e))) lst)
  )
)
;; (psk-ports-unpack '((1000 . "CD") (1010 0 0 0) (1013 1 0 0) (1040 . 20) (1004 . "00") (1000 . "REF") (1013 150 0 0) (1013 1 0 0) (1040 . 20) (1004 . "00")))
;; (("CD" (0 0 0) (1 0 0) 20) ("REF" (0 0 0) (1 0 0) 20))
(defun psk-ports-unpack	(xdata / p r)
  (while xdata
    (while (and xdata (not (equal (car xdata) '(1004 . ""))))
      (setq p	  (cons (cdar xdata) p)
	    xdata (cdr xdata)
      )
    )
    (setq r	(cons (reverse p) r)
	  p	nil
	  xdata	(cdr xdata)
    )
  )
  (reverse r)
)

;; (psk-ports-fetch en)
(defun psk-ports-fetch (en /)
  (psk-ports-unpack (p-xdata-get en "PSK-PORT"))
)

;; (psk-ports-store en '(("CD" (0 0 0) (1 0 0) 20) ("REF" (0 0 0) (1 0 0) 20)))
(defun psk-ports-store (en ports /)
  (p-xdata-set en "PSK-PORT" (psk-ports-pack ports))
)