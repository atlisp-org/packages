;;
;;  Overkill.lsp - Overlaping object checker/fixer.
;;                    
;;
;;  Copyright (R) 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
 
;|
 
;; Overlaping vector checker/fixer.
 
;; The idea is to find overlapping objects and:
;;  1. remove the un-needed/hidden objects.
;;  2. modify partially overlapping objects such that they no longer overlap.
 
;; Supported objects will include: 
;;  LINES, POLYLINES, LWPOLYLINES, ARCS, and CIRCLES.
 
;; All objects will be broken down into simple line and arc representations
;; nd placed in a master list.
 
;; The master list will contain sublists of the form:
;; ((type gen-data) otherData ename)
 
;; Line data Example:
;;   (0   (m b)          (p1 p2) ename)
 
;;  -  Type is 0 for a line.
;;  -  The general data consists of 'm' and 'b' which are the 
;;     slope and y-intersection respectively. The 'm' and 'b' 
;;     elements are found in the equation of a line: y=mx+b
;;  -  The specific data for a line is the two endpoints.
;;  -  Finally an ename 
 
;;    NOTE: If the line is vertical (undefined slope) then 'm' will be nil 
;;          and 'b' will actually be the x-axis intersection value.
;;          i.e. (nil 1.0) is the equation of the line x=1.0 
 
;; Arc Data Example:
;;  (1 (cent radius) (startAng endAng) ename)
 
;;  -  Type is 1 for an arc (includes circles)
;;  -  General data is center point and radius
;;  -  specific data is comprised of start and end angles respectively.
;;  -  ename
 
 
;; Using the (type gen-data) element of each sublist we can m-assoc to 
;; extract a list of all objects that have the potential to overlap. 
;; From here we can loop through the set checking each object against
;; the others to determine which objects overlap with others.
;; compare
;;  object1 and object2 
;;   ...    and object3 
;;   ...    and object4
;;   ...    and object5
 
;; If object1 completely contains object2 then delete object2
;; If object1 partially overlaps with object2 modify object2
 
 
 
;; acet-ss-remove-dups
;; 0   type
;; 8   layer
;; 6   linetype
;; 62  color
;; 370 lineweight
;; 390 plotstyle
 
 
 
;; |;
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:overkill ( / lst )
 (acet-error-init 
  (list '("cmdecho" 0)
         T
  )
 )
 (if (setq lst (acet-overkill-ui nil))
     (acet-overkill2 lst)
 );if
 (acet-error-restore)
);defun c:overkill
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:-overkill ( / lst )
 (acet-error-init 
  (list '("cmdecho" 0)
         T
  )
 )
 (if (setq lst (acet-overkill-ui T)) ;force command line
     (acet-overkill2 lst)
 );if
 (acet-error-restore)
);defun c:-overkill
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;If the cmdline arg is true then command will be command line line driven.
;
(defun acet-overkill-ui ( cmdline / ss lst )
 
 (if (and (setq ss (ssget "_:l"))
          (setq ss (car (acet-ss-filter (list ss nil T))))
     );and
     (progn
      (princ "\n")
      (if (or cmdline
              (= 4 (logand 4 (getvar "cmdactive")))
              (= 0 (getvar "cmddia"))
          );or
          (setq lst (acet-overkill-ui-cmd))
          (setq lst (acet-overkill-ui-dlg))
      );if
      (if lst
          (setq lst (cons ss lst))
      );if
     );progn then
 );if
);defun acet-overkill-ui
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-ui-cmd ( / ans )
 
 (setq ans T)
 (while ans
  (acet-overkill-print-modes)
  (initget "Ignore Fuzz Plines parTial Endtoend") ;; force non-negative entry 4+128
  (setq ans (getkword "\nEnter an option to change [Ignore/Fuzz/Plines/parTial/Endtoend] <done>: "))
  (cond
   ((= ans "Ignore")   (acet-overkill-ui-cmd-ignore))
   ((= ans "Fuzz")     (acet-overkill-ui-cmd-fuz))
   ((= ans "Plines")   (acet-overkill-ui-cmd-plines))
   ((= ans "parTial")  (acet-overkill-ui-cmd-partial))
   ((= ans "Endtoend") (acet-overkill-ui-cmd-endtoend))
  );cond close
 );while
 (list
  (max (acet-overkill-fuz-get) 1.0e-08)
  (acet-overkill-ignore-get)
  (acet-overkill-no-plines-get)
  (acet-overkill-no-partial-get)
  (acet-overkill-no-endtoend-get)
 );list
);defun acet-overkill-ui-cmd
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;prompts for fuz value and returns the fuz as a floating point number.
;
(defun acet-overkill-ui-cmd-fuz ( / def fuz ans )
 (setq def (acet-overkill-fuz-get))
 (if (not def)
     (setq def 0.000001)
 );if
 (initget 4) ;; no negative fuz
 (setq ans (getdist
            (acet-str-format "\nSpecify Fuzz for numeric comparisons <%1>: "
                             (acet-rtos2 def)
            )
           );getdist
 );setq
 (if (not ans)
     (setq ans def)
 );if
 (acet-overkill-fuz-set ans)
 ans 
);defun acet-overkill-ui-cmd-fuz
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;prompts for properties to ignore and returns the ignore list (list of common group codes to ignore
;during object comparisons)
;
(defun acet-overkill-ui-cmd-ignore ( / def fuz ans lst a x anslst lst2 lst3 n ignore )
 
 (setq ignore (acet-overkill-ignore-get)) 
 (acet-overkill-print-ignore)
 
 (setq lst '((8 "LAYER") 
             (6 "LTYPE")
             (62 "COLOR")
             (370 "LWEIGHT") 
             (390 "PLOTSTYLE")
            )
 );setq
 ;; build a list of strings
 (foreach x lst
   (setq lst2 (cons (cadr x) lst2));setq then
 );foreach
 (setq lst2 (cons "." lst2)
       lst2 (reverse lst2)
 ) ;add a dot as valid entry (used to specify none)
 
 (princ "\nSpecify properties to ignore when comparing objects... ")  
 (setq ans (acet-ui-m-get-names 
            (list nil	;; no spaces
                  "\n[layer,ltype,color,lweight,plotstyle, * (for all)] or \".\" for none <default>: "
                  lst2	;; valid entries
            );list
           );acet-ui-m-get-names
 );setq
 (if ans
     (progn
      ;; now convert back to a list of group codes and save it.
      (setq  lst (mapcar 'reverse lst)
            lst2 nil
      );setq
      (foreach x ans
       (if (setq a (assoc x lst))
           (setq lst2 (cons (cadr a) lst2));setq then
       );if
      );foreach
      (setq lst2 (reverse lst2))
      (acet-overkill-ignore-set lst2)
     );progn then
 );if
 
 (acet-overkill-ignore-get)
);defun acet-overkill-ui-cmd-ignore
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-ui-cmd-plines (/ def ans a )
 (setq def (acet-overkill-no-plines-get))
 (if (= def 1)
     (setq def "No")
     (setq def "Yes")
 );if
 (initget "Yes No")
 (setq ans (getkword (acet-str-format "\nOptimize segments within plines <%1>: " def)))
 (if (not ans)
     (setq ans def)
 );if
 (if (= ans "No")
     (progn
      (acet-overkill-no-plines-set T)
      (setq a T)
     );progn
     (acet-overkill-no-plines-set nil)
 );if
 a
);defun acet-overkill-ui-cmd-plines
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-ui-cmd-partial (/ def ans a )
 (setq def (acet-overkill-no-partial-get))
 (if (= def 1)
     (setq def "No")
     (setq def "Yes")
 );if
 (initget "Yes No")
 (setq ans (getkword (acet-str-format "\nCombine co-linear objects that partially overlap <%1>: " def)))
 (if (not ans)
     (setq ans def)
 );if
 (if (= ans "No")
     (progn
       (acet-overkill-no-partial-set T)
       (setq a T)
     );progn
     (acet-overkill-no-partial-set nil)
 );if
 a
);defun acet-overkill-ui-cmd-partial
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-ui-cmd-endtoend (/ def ans a )
 (setq def (acet-overkill-no-endtoend-get))
 (if (= def 1)
     (setq def "No")
     (setq def "Yes")
 );if
 (initget "Yes No")
 (setq ans (getkword (acet-str-format "\nCombine co-linear objects when aligned end to end <%1>: " def)))
 (if (not ans)
     (setq ans def)
 );if
 (if (= ans "No")
     (progn
       (acet-overkill-no-endtoend-set T)
       (setq a T)
     );progn
     (acet-overkill-no-endtoend-set nil)
 );if
 a
);defun acet-overkill-ui-cmd-endtoend
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-print-ignore ( / ignore lst a x )
 (setq ignore (acet-overkill-ignore-get))
 (setq lst '((8 "Layer") 
             (6 "Linetype")
             (62 "Color")
             (370 "Lineweight") 
             (390 "Plotstyle")
            )
 );setq
 (if ignore
     (princ "\nIGNORE=")
     (princ "\nIGNORE=none")
 );if
 (foreach x ignore
  (if a
      (princ ",")
  );if
  (if (setq a (assoc x lst))
      (princ (cadr a))
  );if
 );foreach 
);defun acet-overkill-print-ignore
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-print-modes ( / fuz ignore no-plines no-partial no-endtoend x a lst )
 
 (setq          fuz (acet-overkill-fuz-get)
          no-plines (acet-overkill-no-plines-get)
         no-partial (acet-overkill-no-partial-get)
        no-endtoend (acet-overkill-no-endtoend-get)
 );setq
 (acet-overkill-print-ignore)
 
 (princ "\n")
 (princ (strcat "Fuzz=" (acet-rtos2 fuz)))
 
 (if (not no-plines) 		;; "aint got no back-eyed peas neither!"
     (princ ", Optimize PLINES=Y")
     (princ ", Optimize PLINES=N")
 );if
 (if (not no-partial)
     (princ ", combine PARTIAL overlap=Y")
     (princ ", combine PARTIAL overlap=N")
 );if
 (if (not no-endtoend)
     (princ ", combine ENDTOEND=Y")
     (princ ", combine ENDTOEND=N")
 );if
 
);defun acet-overkill-print-modes
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
(defun acet-overkill-ui-dlg ( / ignore checkit lst flag iv fuz )
 
 (setq ignore (acet-overkill-ignore-get))
 (if (> (setq iv (load_dialog "overkill"));setq
        0
     );test
     (progn
      (if (new_dialog "overkill" iv)
          (progn
           ;; local function validates and returns the list of group codes to ignore and the fuz
           ;; if no errors are found.
           (defun checkit ( / ignore fuz lst no-plines no-partial no-endtoend)
            (if (= (get_tile "layer") "1")
                (setq ignore (cons 8 ignore))
            );if
            (if (= (get_tile "linetype") "1")
                (setq ignore (cons 6 ignore))
            );if
            (if (= (get_tile "color") "1")
                (setq ignore (cons 62 ignore))
            );if
            (if (= (get_tile "lineweight") "1")
                (setq ignore (cons 370 ignore))
            )
            (if (= (get_tile "plotstyle") "1")
                (setq ignore (cons 390 ignore))
            )
 
            (if (= (get_tile "plines") "0")
                (setq no-plines T)
            )
            (if (= (get_tile "partial") "0")
                (setq no-partial T)
            )
            (if (= (get_tile "endtoend") "0")
                (setq no-endtoend T)
            )
 
            (setq fuz (get_tile "fuz"))
            (if (or (not (distof fuz))
                    (< (distof fuz) 0.0)
                );or
                (set_tile "error" "Invalid. Fuzz value must be numeric and non-negative.")
                (setq lst (list (distof fuz)
                                ignore
                                no-plines
                                no-partial
                                no-endtoend
                          );list
                );setq else
            );if    
            lst
           );defun checkit
 
           ;; initialize the tiles
           (if (member 8 ignore)
               (set_tile "layer" "1")
               (set_tile "layer" "0")
           );if
           (if (member 6 ignore)
               (set_tile "linetype" "1")
               (set_tile "linetype" "0")
           );if
           (if (member 62 ignore)
               (set_tile "color" "1")
               (set_tile "color" "0")
           );if
           (if (member 370 ignore)
               (set_tile "lineweight" "1")
               (set_tile "lineweight" "0")
           );if
           (if (member 390 ignore)
               (set_tile "plotstyle" "1")
               (set_tile "plotstyle" "0")
           );if
 
           (set_tile "fuz" (acet-rtos2 (acet-overkill-fuz-get)))
 
           (if (acet-overkill-no-plines-get)
               (set_tile "plines" "0")
               (set_tile "plines" "1")
           );if
           (if (acet-overkill-no-partial-get)
               (set_tile "partial" "0")
               (set_tile "partial" "1")
           );if
           (if (acet-overkill-no-endtoend-get)
               (set_tile "endtoend" "0")
               (set_tile "endtoend" "1")
           );if
           (action_tile "accept" "(if (setq lst (checkit)) (done_dialog 1))")
           (action_tile "cancel" "(done_dialog 0)")
           (action_tile "help" "(acet-help \"OVERKILL\")")
 
 
           (setq flag (start_dialog));setq
 
           (if (= flag 1)
               (progn
                (acet-overkill-fuz-set         (nth 0 lst))
                (acet-overkill-ignore-set      (nth 1 lst))
                (acet-overkill-no-plines-set   (nth 2 lst))
                (acet-overkill-no-partial-set  (nth 3 lst))
                (acet-overkill-no-endtoend-set (nth 4 lst))
                (setq lst (list (max (nth 0 lst) 1.0e-08) ;min uz value of 1.0e-08
                                (nth 1 lst)
                                (nth 2 lst)
                                (nth 3 lst)
                                (nth 4 lst)
                          );list
                );setq
               );progn then
           );if
          );progn then initialize the tiles and activate the dialog box
          (alert "Unable to display dialog box")
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (alert "Unable to load dialog box");else
 );if load
 (if (= flag 1)
     (acet-acad-refresh) ;force AutoCAD to refresh it's window.
 );if
 
 lst
);defun acet-overkill-ui-dlg
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of group codes that corispond to properties that overkill will ignore
;when comparing objects.
;
(defun acet-overkill-ignore-get ( / lst )
 (setq lst (acet-getvar (list "ACET-OVERKILL-IGNORE" 6))) ;; 2+4=look in current profile then in fixed profile
 (if (and lst
          (/= lst "")
     );and
     (setq lst (acet-str-to-list "," lst)
           lst (mapcar 'atoi lst)
     );setq
     (setq lst nil)
 );if
 lst
);defun acet-overkill-ignore-get
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a list of group codes that corispond to properties that overkill will ignore
;and saves the data in the current profile as well as the fixed profile.
;
(defun acet-overkill-ignore-set ( ignore / gcode a )
 (setq a "")
 (foreach gcode ignore
   (setq a (strcat a "," (itoa gcode)))
 );foreach
 (if (/= a "")
     (setq a (substr a 2))
 );if
 (acet-setvar (list "ACET-OVERKILL-IGNORE" a 6)) ;; 2+4=place it in current and fixed profiles
);defun acet-overkill-ignore-set
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default get and set functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-fuz-get ( / fuz )
 (setq fuz (acet-getvar '("ACET-OVERKILL-FUZZ" 1)))
 (if (not fuz)
     (setq fuz 0.000001)
 );if
 fuz
);defun acet-overkill-fuz-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-fuz-set ( fuz / )
 (acet-setvar (list "ACET-OVERKILL-FUZZ" fuz 1))
);defun acet-overkill-fuz-set
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-no-plines-get ( / no-pline )
 (setq no-pline (acet-getvar '("ACET-OVERKILL-NO-PLINES")))
 (if (/= no-pline 1)
     (setq no-pline nil)
 );if
 no-pline
);defun acet-overkill-no-plines-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-no-plines-set ( no-pline / )
 (if no-pline
     (acet-setvar (list "ACET-OVERKILL-NO-PLINES" 1 3))
     (acet-setvar (list "ACET-OVERKILL-NO-PLINES" 0 3))
 );if
);defun acet-overkill-no-plines-set
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-no-partial-get ( / no-partial )
 (setq no-partial (acet-getvar '("ACET-OVERKILL-NO-PARTIAL")))
 (if (/= no-partial 1)
     (setq no-partial nil)
 );if
 no-partial
);defun acet-overkill-no-partial-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-no-partial-set ( no-partial / )
 (if no-partial
     (acet-setvar (list "ACET-OVERKILL-NO-PARTIAL" 1 3))
     (acet-setvar (list "ACET-OVERKILL-NO-PARTIAL" 0 3))
 );if
);defun acet-overkill-no-partial-set
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-no-endtoend-get ( / no-endtoend )
 (setq no-endtoend (acet-getvar '("ACET-OVERKILL-NO-ENDTOEND")))
 (if (/= no-endtoend 1)
     (setq no-endtoend nil)
 );if
 no-endtoend
);defun acet-overkill-no-endtoend-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-no-endtoend-set ( no-endtoend / )
 (if no-endtoend
     (acet-setvar (list "ACET-OVERKILL-NO-ENDTOEND" 1 3))
     (acet-setvar (list "ACET-OVERKILL-NO-ENDTOEND" 0 3))
 );if
);defun acet-overkill-no-endtoend-set
 
;;;functions below are maintained for backward compatibility...
 
(defun acet-overkill (alst)
 (acet-overkill2 alst)
)
(defun acet-overkill-resolve-lines ( lst ss2 fuz no-partial no-endtoend / )
 (acet-overkill-resolve-arcs2 lst ss2 fuz no-partial no-endtoend)
)
(defun acet-overkill-resolve-arcs ( lst ss2 fuz no-partial no-endtoend / )
 (acet-overkill-resolve-arcs2 lst ss2 fuz no-partial no-endtoend)
)
(defun acet-overkill-line-data ( e1 fuz genprops / )
 (acet-overkill-line-data2 e1 fuz genprops)
)
(defun acet-overkill-gen-prop-get ( e1 genprops / )
 (acet-overkill-gen-prop-get2 e1 genprops)
)
(defun acet-rtos (val)
 (acet-rtos2 val)
)
(defun acet-overkill-ss->primitives ( ss fuz ignore )
 (acet-overkill-ss->primitives2 ss fuz ignore)
)


(acet-autoload2	'("OVERKILLSUP.LSP"	(acet-overkill2 alst)))
(acet-autoload2	'("OVERKILLSUP.LSP"	(acet-overkill-gen-prop-get2 e1 genprops)))
(acet-autoload2	'("OVERKILLSUP.LSP"	(acet-overkill-line-data2 e1 fuz genprops)))
(acet-autoload2	'("OVERKILLSUP.LSP"	(acet-overkill-resolve-arcs2 lst ss2 fuz no-partial no-endtoend)))
(acet-autoload2	'("OVERKILLSUP.LSP"	(acet-overkill-ss->primitives2 ss fuz ignore)))
(acet-autoload2	'("OVERKILLSUP.LSP"	(acet-rtos2 val)))
(princ)
