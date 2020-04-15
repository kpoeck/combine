(in-package :cl-user)

#-fast
(eval-when
    #-:gcl (:compile-toplevel :execute :load-toplevel)
    #+:gcl (compile eval load)
  (proclaim '(optimize (speed 0) (safety 3) (space 0)(debug 3)(compilation-speed 0)))
  )

#+fast
(eval-when
    #-:gcl (:compile-toplevel :execute :load-toplevel)
    #+:gcl (compile eval load)
  (proclaim '(optimize (speed 3) (safety 0) (space 0)(debug 0)(compilation-speed 0)))
  )

(defun test-backtracking (title solver &optional (print t)(repetitions 1))
  (let ((solution nil)
        (before (get-internal-real-time))
        after)
    (dotimes (x repetitions)
      (setq solution (solve-it solver)))
    (setq after (get-internal-real-time))
    (when print
      (show-result solution SOLVER title (- after before))
      (format t "~2%")
      )
    solution
    )
  )

(defun test-gsat (title solver &optional (print t))
  (let ((solution nil)
        (before (get-internal-real-time))
        after)
    (setq solution (solve-it solver))
    (setq after (get-internal-real-time))
    (when print
      (cond (solution
             (show-result solution SOLVER title (- after before))
             (format t "Iterations:~a~%" (GSAT-SOLVED-ITERATION solver))
             (format t "Solutions Tried:~a~%" (solution-tried solver))
             )
            (t
             (format t "~%Failed:~a~%" title)
             (format t "~%No solution found, in ~a Iterations. Solutions tried:~a Time  ~10,2f seconds ~%"
                     (GSAT-MAX-tries solver)
                     (solution-tried solver)
                     (float (/ (float (- after before)) internal-time-units-per-second)))
             )
            )
      (format t "Worser:~a~%" (GSAT-WORSENED solver))
      (format t "Aborted:~a~%" (GSAT-aborted solver))
      (format t "~2%")
      )
    )
  (values)
  )