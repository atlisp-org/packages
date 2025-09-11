(defun pdb-ctl-disable (ctl)
  (mode_tile ctl 1)
)

(defun pdb-ctl-enable (ctl)
  (mode_tile ctl 0)
)

(defun pdb-text-focus (ctl)
  (mode_tile ctl 2) ;_ Set focus to tile
)

(defun pdb-text-select (ctl)
  (mode_tile ctl 3) ;_Select edit box contents
)



(defun pdb-list-fill (ctl texts /)
  (start_list ctl)
  (mapcar (function add_list) texts)
  (end_list)
)

(defun pdb-list-update (ctl idx text /)
  (start_list ctl 1 idx) ;_ 修改指定位置上的数据
  (add_list text)
  (end_list)
)

(defun pdb-list-append (ctl text /)
  (start_list ctl 2)
  (add_list text)
  (end_list)
)

(defun pdb-list-clear (ctl /)
  (start_list ctl)
  (end_list)
)

(defun pdb-list-select (ctl idx)
  (if (numberp idx)
    (setq idx (itoa idx))
  )
  (set_tile ctl idx)
)

(defun pdb-list-getselect (ctl)
  (atoi (get_tile ctl))
)




(defun pdb-text-set (ctl text)
  (set_tile ctl text)
)

(defun pdb-text-get (ctl)
  (get_tile ctl)
)



;; (pdb-dlg-show (mt-getpath "\\textedit.dcl") "mt_textreplaced" nil nil)
;; (pdb-dlg-show (mt-getpath "\\textedit.dcl") "mt_textreplaced" nil nil)

(defun pdb-dlg-show (dclfile dclname init ondlg / dlgrt id rt)
  (setq	id (load_dialog dclfile)
	rt 2
  )
  (while (and (>= id 0)
	      (>= rt 2)
	      (new_dialog dclname id)
	 )
    (progn
      ;; TODO
;;;      (if (cdr init)
;;;	(vl-catch-all-apply (car init) (cdr init))
	(if (or	(= 'usubr (type init)) ;_ 加载lsp
		(= 'subr (type init)) ;_ 编译后fas
	    )
	  (vl-catch-all-apply (function init))
	)
;;;      )

      (setq rt (start_dialog))
      (if ondlg
	(setq dlgrt (vl-catch-all-apply (function ondlg) (list rt)))
      )
    )
  )
  (unload_dialog id)
  dlgrt
)