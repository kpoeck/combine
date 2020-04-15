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

(defclass backtracking-solver (combinatoric-solver)
  ()
  )

(defmethod solve-it ((solver BACKTRACKING-SOLVER))
  (let ((perm-array (generate-perm-array solver))
        (index -1)
        (limit (problem-size solver))
        (stack-array (make-array (problem-size solver)))
        (partial (generate-empty-solution solver))
        )
    #-gcl (setf (solution-tried solver) 0)
    (unless (PARTIAL-SOLUTION-CORRECT solver partial)
      (error "Empty solution not valid"))
    (loop
      #-gcl (incf (solution-tried solver))
      #+clasp
      (when (zerop (mod (solution-tried solver) 100))
        (princ "."))
      (cond ((PARTIAL-SOLUTION-CORRECT solver partial)
             (incf index)
             (when (= index limit)
               ;hurra
               (return-from solve-it partial)
               )
             ;extend the solution
             
             (setf (svref stack-array index)(svref perm-array index))
             (expand-partial-solution partial (first (svref stack-array index)) index))
            (t
             (loop
               #+no (break "Backtracking")
               (cond ((svref stack-array index)
                      (let ((new (pop (svref stack-array index))))
                        (change-partial-solution partial new index)
                        (return)))
                     (t
                      ; no alternatives in current level, backtrack
                      (forget-partial-solution partial index)
                      (decf index)
                      (when (minusp index)
                        (break "Failed")))))
             )))   
    )
  )

(defmethod generate-perm-array ((solver BACKTRACKING-SOLVER))
  (let ((array (make-array (problem-size solver)))
        (index 0)
        )
    (dolist (domain (all-domains solver))
      (setf (aref array index) (my-permutation solver domain))
      (incf index))
    array))

(defmethod my-permutation ((solver BACKTRACKING-SOLVER) domain)
  (permutation domain))

(defclass RIDDLE-SOLVER (BACKTRACKING-SOLVER)
  ()
  )