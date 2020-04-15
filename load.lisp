(in-package :cl-user)

;;;loader for a bunch of permutation problems
;;;different solvers using 
;;;backtracking through permutations or
;;;gsat, an iterative repair method 
;;;
;;;Problems include
;;;Einsteins Riddle
;;;Zebra Problem
;;;Mickey problem
;;;Send more money and
;;;Barrel Problem
;;;Tested in ACL, LW, Clisp, sbcl, cmucl. scl (clone of cmucl?), gcl 2.6.6 ansi (does not work in head)
;;;Runs in Corman with patch

#|
(pushnew :fast *features*)
|#

#+:gcl (defparameter *load-truename*  #P"/Users/karstenpoeck/lisp/combine/load.lisp")

(unless (or (and *load-truename* (probe-file *load-truename*))
	    (and *compile-file-pathname* (probe-file *compile-file-pathname*)))
  (Error "Please load this file or adjust *LOAD-TRUENAME* or  *compile-file-pathname*"))

#|

#+(or :gcl :cormanlisp)
(unless (fboundp 'with-compilation-unit)
  (defmacro with-compilation-unit (blah &body body)
    (declare (ignore blah))
    `(progn ,@body)))
|#

#+cormanlisp
(defun compile-file-1 (file &rest bla)
  (declare (ignore bla))
  (values (namestring file) nil nil)
  )


#-cormanlisp
(defun compile-file-1 (file &rest bla)
  (apply #'compile-file file bla))

#+cormanlisp
(require :loop)

(let ((files '("closures" "generic" 
               "backtracking" "gsat-mixin" "gsat-comb" "test"
               "mickey" "einstein" "zebra" "eight-queens"
               "generic-number-puzzle"
               "gsat" "gsat2"
               "sendmoremoney" "sendmoremoney-2" "barrel" "barrel-riddle"
               #+cormanlisp "corman"
               )))
  (with-compilation-unit ()
    (dolist (file files)
      (multiple-value-bind
            (fasl warning failure)
          (let ((input (make-pathname :name file :type "lisp" :defaults (or *load-truename*  *compile-file-pathname*))))
            #-:gcl
            (compile-file-1 
             input
             :verbose t :print nil)
            #+:gcl (compile-file-1 input))
        (declare (ignore warning failure))
        (if (not fasl)
            (error "Couldn't compile ~s~%" file)
          (load fasl :print nil :verbose t))))))

(defun test-all ()
  (test-mickey)
  (test-gsat-mickey)
  (test-einstein)
  (test-gsat-einstein)
  (test-zebra) 
  (test-gsat-zebra)
  (test-sendmoremoney)
  (test-sendmoremoney-no-carries)
  (test-gsat-sendmoremoney) 
  (test-gsat-all-values-sendmoremoney)
  (test-gsat-swapping-sendmoremoney)
  (test-barrel)
  (test-barrel-riddle)
  (test-barrel-gsat)
  (test-barrel-gsat-swapping)
  (test-eight-queens)
  (test-eight-queens-gsat)
  )

(defun test-einstein-multiple ()
  (dotimes (x 100)
    (test-einstein nil)))

(format t 
    (concatenate 'string
      "~%Test with ~%"
      "Backtracking~%"
      "Einsteins Riddle (test-einstein) ~%"
      "The zebra problem (test-zebra) ~%"
      "Mickey Mouse (test-mickey) ~%"
      "SendMoreMoney with Carries (test-sendmoremoney) ~%"
      "SendMoreMoney without Carries (TEST-SENDMOREMONEY-NO-CARRIES) ~%"
      "Barrels (test-barrel) ~%"
      "Barrels Permutation (test-barrel-riddle) ~%"
      "~%Gsat heuristic optimisation~%"
      "Send More Money with Carries all values (TEST-GSAT-SENDMOREMONEY) ~%"
      "Send More Money without Carries all values (TEST-GSAT-ALL-VALUES-SENDMOREMONEY) ~%"
      "Send More Money without Carries swapping (TEST-GSAT-SWAPPING-SENDMOREMONEY) ~%"
      "Barrels all values (TEST-BARREL-GSAT) ~%"
      "Barrels swapping (TEST-BARREL-GSAT-SWAPPING) ~%"
      "Einstein (TEST-GSAT-EINSTEIN) ~%"
      "Zebra (test-gsat-zebra) ~%"
      "Mickey Mouse (TEST-GSAT-MICKEY) ~%"
      "8-Queens (test-eight-queens) ~%"
      "8-Queens Gsat test-eight-queens-gsat ~%"
      "~%or test all with (test-all)~%"
      "~%in (in-package :cl-user)"))

#+no
(test-all)

#|
(test-einstein)
not :fast
Lispworks 4.3.7  The solution in 31612 tries testing 185014 constraints took       0.53 seconds constraints/seconds  348425.61 and is:
ACL 7.0          The solution in 31612 tries testing 185014 constraints took       0.63 seconds constraints/seconds  293207.62 and is:
ECLS  2005-03-06 The solution in 31612 tries testing 185014 constraints took       2.00 seconds constraints/seconds   92507.00 and is:                                                       onstraints/seconds   92507.00 and is:
gcl 2.6.2        The solution in 31612 tries testing 185014 constraints took       3.63 seconds constraints/seconds   50921.28 and is:
CLISP 2005-03-06 The solution in 31612 tries testing 185014 constraints took       3.96 seconds constraints/seconds   46720.71 and is:
Corman 2.5       The solution in 31612 tries testing 185014 constraints took      16.42 seconds constraints/seconds   11268.96 and is:

:fast
Lispworks 4.3.7  The solution in 31612 tries testing 185014 constraints took       0.48 seconds constraints/seconds  384644.49 and is:
ACL 7.0          The solution in 31612 tries testing 185014 constraints took       0.55 seconds constraints/seconds  335778.60 and is:
|#
