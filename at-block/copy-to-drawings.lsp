;;; copy-to-drawings.lsp
;;; 将当前图纸中选定的对象复制到其他图纸文件，无需在编辑器中打开目标图纸。
;;; 支持 dwg/dwt/dws 格式，自动处理图层、线型及布局的导入。
;;; 基于 Lee Mac 的 Copy2Drawings v1.3 重构，适配 @block: 命名空间。
;;;----------------------------------------------------------------------
;;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com
;;;  Refactored for @block: namespace
;;;----------------------------------------------------------------------

(defun c:copy-to-drawings ( / *error* _getitem acd app dbx doc dwl err inc lst msg sel tab var vrs )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type dbx)) (not (vlax-object-released-p dbx)))
            (vlax-release-object dbx)
        )   
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )

    (defun _getitem ( col itm )
        (if (not (vl-catch-all-error-p (setq itm (vl-catch-all-apply 'vla-item (list col itm)))))
            itm
        )
    )

    (setq app (vlax-get-acad-object)
          acd (vla-get-activedocument app)
          tab (if (= 1 (getvar 'cvport)) (getvar 'ctab) "Model")
          cnt 0
    )
    (cond
        (   (not
                (and
                    (setq sel (ssget (list '(0 . "~VIEWPORT") (cons 410 tab))))
                    (setq lst (@block:copy:getfiles "选择要复制到的图纸" "" "dwg;dwt;dws"))
                )
            )
            (princ "\n*取消*")
        )
        (   (progn
                (setq dbx
                    (vl-catch-all-apply 'vla-getinterfaceobject
                        (list (setq app (vlax-get-acad-object))
                            (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                                "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                            )
                        )
                    )
                )
                (or (null dbx) (vl-catch-all-error-p dbx))
            )
            (prompt "\n无法连接 ObjectDBX 接口。")
        )
        (   t
            (vlax-for doc (vla-get-documents app)
                (setq dwl (cons (cons (strcase (vla-get-fullname doc)) doc) dwl))
            )
            (repeat (setq inc (sslength sel))
                (setq var (cons (vlax-ename->vla-object (ssname sel (setq inc (1- inc)))) var))
            )
            (setq var
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray vlax-vbobject (cons 0 (1- (length var)))) var
                    )
                )
            )
            (foreach dwg lst
                (if
                    (or (setq doc (cdr (assoc (strcase dwg) dwl)))
                        (and (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx dwg))))
                             (setq doc dbx)
                        )
                    )
                    (progn
                        (vla-copyobjects acd var
                            (vla-get-block
                                (cond
                                    (   (_getitem (vla-get-layouts doc) tab))
                                    (   (vla-add  (vla-get-layouts doc) tab))
                                )
                            )
                        )
                        (vla-saveas doc dwg)
                        (setq cnt (1+ cnt))
                    )
                    (princ (apply 'strcat (cons "\n无法连接文件: " (cdr (fnsplitl dwg)))))
                )
            )
            (setq msg
                (if (< 0 cnt)
                    (strcat "\n"
                        (itoa (sslength sel))
                        (if (= 1 (sslength sel))
                            " 个对象"
                            " 个对象"
                        )
                        " 复制到 " (itoa cnt)
                        (if (= 1 cnt)
                            " 张图纸。"
                            " 张图纸。"
                        )
                    )
                    ""
                )
            )
            (if (< 0 (setq err (- (length lst) cnt)))
                (setq msg
                    (strcat msg
                        "\n无法复制到 " (itoa err)
                        (if (= 1 err)
                            " 张图纸。"
                            " 张图纸。"
                        )
                    )
                )
            )
            (princ msg)
            (if (= 'vla-object (type dbx))
                (vlax-release-object dbx)
            )
        )
    )
    (princ)
)

;;------------------------=={ Get Files Dialog }==----------------------;;
;;                                                                      ;;
;;  An analog of the 'getfiled' function for multiple file selection.   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2012  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Arguments:                                                          ;;
;;  msg - [str/nil] Dialog box label; 'Select Files' if nil or "".      ;;
;;  def - [str/nil] Default directory; dwgprefix if nil or "".          ;;
;;  ext - [str/nil] File extension filter (e.g. "dwg;lsp"); "*" if nil  ;;
;;----------------------------------------------------------------------;;
;;  Returns:  List of selected files, else nil                          ;;
;;----------------------------------------------------------------------;;
;;  Version 1.6    -    2016-03-21                                      ;;
;;----------------------------------------------------------------------;;

(defun @block:copy:getfiles ( msg def ext / *error* dch dcl des dir dirdata lst rtn )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch)) (< 0 dch))
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\n错误: " msg))
        )
        (princ)
    )    
    
    (if
        (and
            (setq dcl (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open dcl "w"))
            (progn
                (foreach x
                   '(
                        "lst : list_box"
                        "{"
                        "    width = 40.0;"
                        "    height = 20.0;"
                        "    fixed_width = true;"
                        "    fixed_height = true;"
                        "    alignment = centered;"
                        "    multiple_select = true;"
                        "}"
                        "but : button"
                        "{"
                        "    width = 20.0;"
                        "    height = 1.8;"
                        "    fixed_width = true;"
                        "    fixed_height = true;"
                        "    alignment = centered;"
                        "}"
                        "getfiles : dialog"
                        "{"
                        "    key = \"title\"; spacer;"
                        "    : row"
                        "    {"
                        "        alignment = centered;"
                        "        : edit_box { key = \"dir\"; label = \"文件夹:\"; }"
                        "        : button"
                        "        {"
                        "            key = \"brw\";"
                        "            label = \"浏览\";"
                        "            fixed_width = true;"
                        "        }"
                        "    }"
                        "    spacer;"
                        "    : row"
                        "    {"
                        "        : column"
                        "        {"
                        "            : lst { key = \"box1\"; }"
                        "            : but { key = \"add\" ; label = \"添加文件\"; }"
                        "        }"
                        "        : column {"
                        "            : lst { key = \"box2\"; }"
                        "            : but { key = \"del\" ; label = \"移除文件\"; }"
                        "        }"
                        "    }"
                        "    spacer; ok_cancel;"
                        "}"
                    )
                    (write-line x des)
                )
                (setq des (close des))
                (< 0 (setq dch (load_dialog dcl)))
            )
            (new_dialog "getfiles" dch)
        )
        (progn
            (setq ext (if (= 'str (type ext)) (@block:copy:str->lst (strcase ext) ";") '("*")))
            (set_tile "title" (if (member msg '(nil "")) "选择文件" msg))
            (set_tile "dir"
                (setq dir
                    (@block:copy:fixdir
                        (if (or (member def '(nil "")) (not (vl-file-directory-p (@block:copy:fixdir def))))
                            (getvar 'dwgprefix)
                            def
                        )
                    )
                )
            )
            (setq lst (@block:copy:updatefilelist dir ext nil))
            (mode_tile "add" 1)
            (mode_tile "del" 1)

            (action_tile "brw"
                (vl-prin1-to-string
                   '(if (setq tmp (@block:copy:browseforfolder "" nil 512))
                        (setq lst (@block:copy:updatefilelist (set_tile "dir" (setq dir tmp)) ext rtn)
                              rtn (@block:copy:updateselected dir rtn)
                        )                              
                    )
                )
            )

            (action_tile "dir"
                (vl-prin1-to-string
                   '(if (= 1 $reason)
                        (setq lst (@block:copy:updatefilelist (set_tile "dir" (setq dir (@block:copy:fixdir $value))) ext rtn)
                              rtn (@block:copy:updateselected dir rtn)
                        )
                    )
                )
            )

            (action_tile "box1"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm tmp )
                            (if (setq itm (mapcar '(lambda ( n ) (nth n lst)) (read (strcat "(" $value ")"))))
                                (if (= 4 $reason)
                                    (cond
                                        (   (equal '("..") itm)
                                            (setq lst (@block:copy:updatefilelist (set_tile "dir" (setq dir (@block:copy:updir dir))) ext rtn)
                                                  rtn (@block:copy:updateselected dir rtn)
                                            )
                                        )
                                        (   (vl-file-directory-p (setq tmp (@block:copy:checkredirect (strcat dir "\\" (car itm)))))
                                            (setq lst (@block:copy:updatefilelist (set_tile "dir" (setq dir tmp)) ext rtn)
                                                  rtn (@block:copy:updateselected dir rtn)
                                            )
                                        )
                                        (   (setq rtn (@block:copy:sort (append rtn (mapcar '(lambda ( x ) (strcat dir "\\" x)) itm)))
                                                  rtn (@block:copy:updateselected dir rtn)
                                                  lst (@block:copy:updatefilelist dir ext rtn)
                                            )
                                        )
                                    )
                                    (if (vl-every '(lambda ( x ) (vl-file-directory-p (strcat dir "\\" x))) itm)
                                        (mode_tile "add" 1)
                                        (mode_tile "add" 0)
                                    )
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "box2"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if (setq itm (mapcar '(lambda ( n ) (nth n rtn)) (read (strcat "(" $value ")"))))
                                (if (= 4 $reason)
                                    (setq rtn (@block:copy:updateselected dir (vl-remove (car itm) rtn))
                                          lst (@block:copy:updatefilelist dir ext rtn)
                                    )
                                    (mode_tile "del" 0)
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "add"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if
                                (setq itm
                                    (vl-remove-if 'vl-file-directory-p
                                        (mapcar '(lambda ( n ) (nth n lst)) (read (strcat "(" (get_tile "box1") ")")))
                                    )
                                )
                                (setq rtn (@block:copy:sort (append rtn (mapcar '(lambda ( x ) (strcat dir "\\" x)) itm)))
                                      rtn (@block:copy:updateselected dir rtn)
                                      lst (@block:copy:updatefilelist dir ext rtn)
                                )
                            )
                            (mode_tile "add" 1)
                            (mode_tile "del" 1)
                        )
                    )
                )
            )

            (action_tile "del"
                (vl-prin1-to-string
                   '(
                        (lambda ( / itm )
                            (if (setq itm (read (strcat "(" (get_tile "box2") ")")))
                                (setq rtn (@block:copy:updateselected dir (@block:copy:removeitems itm rtn))
                                      lst (@block:copy:updatefilelist dir ext rtn)
                                )
                            )
                            (mode_tile "add" 1)
                            (mode_tile "del" 1)
                        )
                    )
                )
            )
         
            (if (zerop (start_dialog))
                (setq rtn nil)
            )
        )
    )
    (*error* nil)
    rtn
)

(defun @block:copy:listbox ( key lst )
    (start_list key)
    (foreach x lst (add_list x))
    (end_list)
    lst
)

(defun @block:copy:listfiles ( dir ext lst )
    (vl-remove-if '(lambda ( x ) (member (strcat dir "\\" x) lst))
        (cond
            (   (cdr (assoc dir dirdata)))
            (   (cdar
                    (setq dirdata
                        (cons
                            (cons dir
                                (append
                                    (@block:copy:sortlist (vl-remove "." (vl-directory-files dir nil -1)))
                                    (@block:copy:sort
                                        (if (member ext '(("") ("*")))
                                            (vl-directory-files dir nil 1)
                                            (vl-remove-if-not
                                                (function
                                                    (lambda ( x / e )
                                                        (and
                                                            (setq e (vl-filename-extension x))
                                                            (setq e (strcase (substr e 2)))
                                                            (vl-some '(lambda ( w ) (wcmatch e w)) ext)
                                                        )
                                                    )
                                                )
                                                (vl-directory-files dir nil 1)
                                            )
                                        )
                                    )
                                )
                            )
                            dirdata
                        )
                    )
                )
            )
        )
    )
)

(defun @block:copy:checkredirect ( dir / itm pos )
    (cond
        (   (vl-directory-files dir) dir)
        (   (and
                (=  (strcase (getenv "UserProfile"))
                    (strcase (substr dir 1 (setq pos (vl-string-position 92 dir nil t))))
                )
                (setq itm
                    (cdr
                        (assoc (substr (strcase dir t) (+ pos 2))
                           '(
                                ("my documents" . "Documents")
                                ("my pictures"  . "Pictures")
                                ("my videos"    . "Videos")
                                ("my music"     . "Music")
                            )
                        )
                    )
                )
                (vl-file-directory-p (setq itm (strcat (substr dir 1 pos) "\\" itm)))
            )
            itm
        )
        (   dir   )
    )
)

(defun @block:copy:sort ( lst )
    (apply 'append
        (mapcar '@block:copy:sortlist
            (vl-sort
                (@block:copy:groupbyfunction lst
                    (lambda ( a b / x y )
                        (and
                            (setq x (vl-filename-extension a))
                            (setq y (vl-filename-extension b))
                            (= (strcase x) (strcase y))
                        )
                    )
                )
                (function
                    (lambda ( a b / x y )
                        (and
                            (setq x (vl-filename-extension (car a)))
                            (setq y (vl-filename-extension (car b)))
                            (< (strcase x) (strcase y))
                        )
                    )
                )
            )
        )
    )
)

(defun @block:copy:sortlist ( lst )
    (mapcar (function (lambda ( n ) (nth n lst)))
        (vl-sort-i (mapcar '@block:copy:splitstring lst)
            (function
                (lambda ( a b / x y )
                    (while
                        (and
                            (setq x (car a))
                            (setq y (car b))
                            (= x y)
                        )
                        (setq a (cdr a)
                              b (cdr b)
                        )
                    )
                    (cond
                        (   (null x) b)
                        (   (null y) nil)
                        (   (and (numberp x) (numberp y)) (< x y))
                        (   (numberp x))
                        (   (numberp y) nil)
                        (   (< x y))
                    )
                )
            )
        )
    )
)

(defun @block:copy:groupbyfunction ( lst fun / tmp1 tmp2 x1 )
    (if (setq x1 (car lst))
        (progn
            (foreach x2 (cdr lst)
                (if (fun x1 x2)
                    (setq tmp1 (cons x2 tmp1))
                    (setq tmp2 (cons x2 tmp2))
                )
            )
            (cons (cons x1 (reverse tmp1)) (@block:copy:groupbyfunction (reverse tmp2) fun))
        )
    )
)

(defun @block:copy:splitstring ( str )
    (
        (lambda ( l )
            (read
                (strcat "("
                    (vl-list->string
                        (apply 'append
                            (mapcar
                                (function
                                    (lambda ( a b c )
                                        (cond
                                            (   (member b '(45 46 92))
                                                (list 32)
                                            )
                                            (   (< 47 b 58)
                                                (list b)
                                            )
                                            (   (list 32 34 b 34 32))
                                        )
                                    )
                                )
                                (cons nil l) l (append (cdr l) '(( )))
                            )
                        )
                    )
                    ")"
                )
            )
        )
        (vl-string->list (strcase str))
    )
)

(defun @block:copy:browseforfolder ( msg dir flg / err fld pth shl slf )
    (setq err
        (vl-catch-all-apply
            (function
                (lambda ( / app hwd )
                    (if (setq app (vlax-get-acad-object)
                              shl (vla-getinterfaceobject app "shell.application")
                              hwd (vl-catch-all-apply 'vla-get-hwnd (list app))
                              fld (vlax-invoke-method shl 'browseforfolder (if (vl-catch-all-error-p hwd) 0 hwd) msg flg dir)
                        )
                        (setq slf (vlax-get-property fld 'self)
                              pth (@block:copy:fixdir (vlax-get-property slf 'path))
                        )
                    )
                )
            )
        )
    )
    (if slf (vlax-release-object slf))
    (if fld (vlax-release-object fld))
    (if shl (vlax-release-object shl))
    (if (vl-catch-all-error-p err)
        (prompt (vl-catch-all-error-message err))
        pth
    )
)

(defun @block:copy:full->relative ( dir path / p q )
    (setq dir (vl-string-right-trim "\\" dir))
    (cond
        (   (and
                (setq p (vl-string-position 58  dir))
                (setq q (vl-string-position 58 path))
                (/= (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            path
        )
        (   (and
                (setq p (vl-string-position 92  dir))
                (setq q (vl-string-position 92 path))
                (= (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            (@block:copy:full->relative (substr dir (+ 2 p)) (substr path (+ 2 q)))
        )
        (   (and
                (setq q (vl-string-position 92 path))
                (= (strcase dir) (strcase (substr path 1 q)))
            )
            (strcat ".\\" (substr path (+ 2 q)))
        )
        (   (= "" dir)
            path
        )
        (   (setq p (vl-string-position 92 dir))
            (@block:copy:full->relative (substr dir (+ 2 p)) (strcat "..\\" path))
        )
        (   (@block:copy:full->relative "" (strcat "..\\" path)))
    )
)

(defun @block:copy:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (@block:copy:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

(defun @block:copy:updatefilelist ( dir ext lst )
    (@block:copy:listbox "box1" (@block:copy:listfiles dir ext lst))
)

(defun @block:copy:updateselected ( dir lst )
    (@block:copy:listbox "box2" (mapcar '(lambda ( x ) (@block:copy:full->relative dir x)) lst))
    lst
)

(defun @block:copy:updir ( dir )
    (substr dir 1 (vl-string-position 92 dir nil t))
)

(defun @block:copy:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

(defun @block:copy:removeitems ( itm lst / idx )
    (setq idx -1)
    (vl-remove-if '(lambda ( x ) (member (setq idx (1+ idx)) itm)) lst)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: copy-to-drawings.lsp | 基于 Copy2Drawings v1.3 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: 命令: copy-to-drawings ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;