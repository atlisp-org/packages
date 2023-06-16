;;;相关矩阵函数;;;----------------------------------------------------;
;;; 向量的模(长度)                                       ;
;;; Vector Norm - Lee Mac                               ;
;;; Args: v - vector in R^n                               ;
;;;----------------------------------------------------;
(defun matrix:norm ( v )
  "向量的模(长度)"
  (sqrt (apply '+ (mapcar '* v v)))
)

;;;----------------------------------------------------;
;;; 向量乘标量(系数)                                       ;
;;; Vector x Scalar - Lee Mac                               ;
;;; Args: v - vector in R^n, s - real scalar               ;
;;;----------------------------------------------------;
(defun matrix:vxs ( v s )
  "向量乘标量(系数)"
  (mapcar '(lambda ( n ) (* n s)) v)
)

;;;----------------------------------------------------;
;;; 单位向量                                               ;
;;; Unit Vector - Lee Mac                              ;
;;; Args: v - vector in R^n                               ;
;;;----------------------------------------------------;
(defun matrix:unit ( v )
  "单位向量"
  ( (lambda ( n )
      (if (equal 0.0 n 1e-14)
        nil
        (matrix:vxs v (/ 1.0 n))
      )
    )
    (matrix:norm v)
  )
)

;;;----------------------------------------------------;
;;; 向量的点积                                         ;
;;; matrix:vxv Returns the dot product of 2 vectors       ;
;;;----------------------------------------------------;
(defun matrix:vxv (v1 v2)
  "向量的点积"
  (apply '+ (mapcar '* v1 v2))
)

;;;----------------------------------------------------;
;;; 两向量的叉积                                       ;
;;; Vector Cross Product - Lee Mac                       ;
;;; Args: u,v - vectors in R^3                               ;
;;;----------------------------------------------------;
(defun matrix:v^v ( u v )
  "两向量的叉积"
  (list
    (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
    (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
    (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
  )
)

;;;----------------------------------------------------;
;;; 矩阵转置                                           ;
;;; matrix:trp Transpose a matrix -Doug Wilson-           ;
;;;----------------------------------------------------;
(defun matrix:trp (m)
  "矩阵转置"
  (apply 'mapcar (cons 'list m))
)

;;;----------------------------------------------------;
;;; 向量的矩阵变换(向量乘矩阵)                         ;
;;; Matrix x Vector - Vladimir Nesterovsky             ;
;;; Args: m - nxn matrix, v - vector in R^n            ;
;;;----------------------------------------------------;
(defun matrix:mxv (m v)
  "向量的矩阵变换(向量乘矩阵) "
  (mapcar (function (lambda (r) (apply '+ (mapcar '* r v)))) m)
)

;;;----------------------------------------------------;
;;; 点到矩阵的变换                                     ;
;;;----------------------------------------------------;
(defun matrix:mxp (m p)
  "点到矩阵的变换"
  (reverse (cdr (reverse (matrix:mxv m (append p '(1.0))))))
)

;;;----------------------------------------------------;
;;; 矩阵相乘                                           ;
;;; matrix:mxm Multiply two matrices -Vladimir Nesterovsky;
;;;----------------------------------------------------;
(defun matrix:mxm (m q)
  "矩阵相乘"
  (mapcar (function (lambda (r) (matrix:mxv (matrix:trp q) r))) m)
)
