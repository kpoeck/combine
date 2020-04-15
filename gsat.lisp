(in-package :cl-user)

;;;GSAT [Selman, Levesque, Mitchell AAAI-92, 440-446]
;;;
;;;For i := 1 to maxTries
;;;    Sol := Randomly generated solution   
;;;    For j:= 1 to Max-Flips
;;;        If correct(Solution) return(Solution)
;;;        For all Vars i
;;;            For all possible values v
;;;                Check&NoteAssignment(i,V) in PossibleChanges
;;;        Apply best improvement (i,V) (Randomly if break even)

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

(defclass gsat-all-values-solver (GSAT-SOLVER-MIXIN NUMERIC-PUZZLE-SOLVER)
  (
   )
  )

(defmethod guess-a-solution ((me gsat-all-values-solver))
  (let (
        (empty (generate-empty-solution me))
        (index 0)
        )
    (dolist (pair (ALL-DOMAINS-EXTENDED me))
      (let ((possible-values (second pair)))
        (EXPAND-PARTIAL-SOLUTION empty (nth (random (length possible-values)) possible-values) index))
      (incf index))
    empty)
  )

(defmethod get-current-value ((partial numeric-puzzle-partial-solution) index)
  (svref (number-array partial) index)
  )

(defmethod solve-it ((solver gsat-all-values-solver))
  (setf (solution-tried solver) 0)
  (setf (GSAT-WORSENED solver) 0)
  (dotimes (x (GSAT-MAX-tries solver))
    (let ((partial (GUESS-A-SOLUTION solver)))
      (dotimes (xx (GSAT-MAX-FLIPS solver))
        (let ((eval (PARTIAL-SOLUTION-EVALUATION SOLVER partial)))
          (when (zerop eval)
            (setf (gsat-solved-iteration solver) x)
            (return-from solve-it partial))
          (let ((best-changes nil)
                (best-eval nil)
                (index 0)
                )
            (dolist (pair (ALL-DOMAINS-EXTENDED SOLVER))
              (let ((possible-values (second pair))
                    (current-value (get-current-value partial index)))
                (dolist (poss POSSIBLE-VALUES)
                  (unless (= poss CURRENT-VALUE)
                    (incf (solution-tried solver))
                    (EXPAND-PARTIAL-SOLUTION partial poss index)
                    (let ((curr (PARTIAL-SOLUTION-EVALUATION solver partial)))
                      (cond ((null best-eval)
                             (setq best-eval curr
                                 best-changes (list (list index poss))))
                            ((= best-eval curr)
                             (push (list index poss) best-changes))
                            ((< curr best-eval)
                             (setq best-eval curr
                                 best-changes (list (list index poss)))
                             )
                            )
                      )
                    )
                  )
                (EXPAND-PARTIAL-SOLUTION partial current-value index)
                (incf index)))
            ;;;apply best change
            (setq best-changes
                  (if (null (rest best-changes))
                      (first best-changes)
                    (nth (random (length best-changes)) best-changes)))
            (when (> BEST-EVAL eval)
              #-no
              (incf (GSAT-WORSENED solver))
              #+no
              (incf (gsat-aborted solver))
              ;;abort the try
              #+no
              (return)
              )
            (EXPAND-PARTIAL-SOLUTION partial (second best-changes)(first best-changes))
            )
          )
        )
      )
    )
  nil
  )