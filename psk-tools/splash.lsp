(defun psk-file-version (/ d)
  (setq d (vl-file-systime (psk-get-filename "psk.fas")))
  (strcat (p-string-right (itoa (car d)) 1)
          (p-number-padding (cadr d) 2)
          (p-number-padding (nth 3 d) 2)
  )
)

(princ
  (strcat "\n" $psk-about "." (psk-file-version) " ря╪сть")
)
(princ)