(in-package :cl-user)

;;;GSAT [Selman, Levesque, Mitchell AAAI-92, 440-446]
;;;
;;;For i := 1 to maxTries
;;;    Sol := Randomly generated solution   
;;;    For j:= 1 to Max-Flips
;;;        If correct(Sol) return(Sol)
;;;        For all Vars i
;;;            For all possible values v
;;;                Check&NoteAssignment(i,V) in PossibleChanges
;;;        Apply best improvement (i,V) (Randomly if break even)


;;; Gsat 1'st try works really bad, need more than 1 000 000 tries and not even always works
;;; Idea
;;;     drop the carry constraints
;;;     Swap two values or put one not yet used
;;;     This works better, always finds a solution

;;; Assumes: one single domain for all variables

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

(defclass gsat-solver-swapping (GSAT-SOLVER-MIXIN NUMERIC-PUZZLE-SOLVER)
  (
   )
  )

#-:clisp
(defgeneric the-domain (problem-specification))

(defmethod unused-values-exists ((me GSAT-SOLVER-SWAPPING))
  nil)

(defmethod guess-a-solution ((me GSAT-SOLVER-SWAPPING))
  (let ((empty (generate-empty-solution me))
        (index 0)
        (spec (cs-specification me))
        )
    (dolist (x (GENERATE-ONE-PERMUTATION (the-domain spec) (problem-size me)))
      (EXPAND-PARTIAL-SOLUTION empty x index)
      (incf index)
      )
    empty
    )
  )

(defun generate-one-permutation (domain size)
  (let ((res nil)
        (possible (copy-list domain)))
    (dotimes (x size)
      (let ((new (nth (random (length possible)) possible)))
        (push new res)
        (setq possible (delete new possible))))
    res))

(defmethod give-missing-elements ((me gsat-solver-swapping) PARTIAL)
  ;;; possible values 0 1 2 3 4 5 6 7 8 9
  ;;; check which of these is not used
  (let ((free nil))
    (dolist (x (the-domain (cs-specification me)))
      (dotimes (y (problem-size me) (push x free))
        (when (= x (svref (number-array partial) y))
          (return))))
    free)
  )

(defmethod solve-it ((solver gsat-solver-swapping))
  (setf (solution-tried solver) 0)
  (setf (GSAT-WORSENED solver) 0)
  (dotimes (x (GSAT-MAX-tries solver))
    (let (
          (partial (GUESS-A-SOLUTION solver))
          (last-x -1)
          (last-y -1)
          (last-new-index -1)
          )
      (dotimes (xx (GSAT-MAX-FLIPS solver))
        (let ((eval (PARTIAL-SOLUTION-EVALUATION SOLVER partial)))
          (when (zerop eval)
            (setf (gsat-solved-iteration solver) x)
            (return-from solve-it partial))
          (let ((best-changes nil)
                (best-eval nil)
                (index 0)               
                )
            ;;; try the not used values
            (when (unused-values-exists solver)
              (let ((possible-values (GIVE-MISSING-ELEMENTS solver partial)))
                (dolist (pair (ALL-DOMAINS-EXTENDED SOLVER))
                  (declare (ignore pair))
                  (unless (= index last-new-index)
                    (let ((current-value (get-current-value partial index))
                          )
                      (dolist (poss POSSIBLE-VALUES)
                        (unless (= poss CURRENT-VALUE)
                          (incf (solution-tried solver))
                          (EXPAND-PARTIAL-SOLUTION partial poss index)
                          (let ((curr (PARTIAL-SOLUTION-EVALUATION solver partial)))
                            (cond ((null best-eval)
                                   (setq best-eval curr
                                       best-changes (list (list :new index poss))))
                                  ((= best-eval curr)
                                   (push (list :new index poss) best-changes))
                                  ((< curr best-eval)
                                   (setq best-eval curr
                                       best-changes (list (list :new index poss)))
                                   )
                                  )
                            )
                          )
                        )
                      (EXPAND-PARTIAL-SOLUTION partial current-value index)
                      (incf index))))))
            ;;; try to swap
            (dotimes (x (problem-size solver))
              (dotimes (y (problem-size solver))
                (when (> y x)
                  ;;;swap the values of x and y
                  (unless (and (= x last-x)(= y last-y))
                    (let ((before-a (svref (number-array partial) x))
                          (before-b (svref (number-array partial) y)))
                      (EXPAND-PARTIAL-SOLUTION partial before-b x)
                      (EXPAND-PARTIAL-SOLUTION partial before-a y)
                      ;;; evaluate
                      (let ((curr (PARTIAL-SOLUTION-EVALUATION solver partial)))
                        (cond ((null best-eval)
                               (setq best-eval curr
                                   best-changes (list (list :swap x BEFORE-B y BEFORE-A))))
                              ((= best-eval curr)
                               (push (list :swap x BEFORE-B y BEFORE-A) best-changes))
                              ((< curr best-eval)
                               (setq best-eval curr
                                   best-changes (list (list :swap x BEFORE-B y BEFORE-A)))
                               )
                              )
                        )
                      ;;; reset
                      (EXPAND-PARTIAL-SOLUTION partial before-a x)
                      (EXPAND-PARTIAL-SOLUTION partial before-b y)
                      )
                    )
                  )
                )
              )
            #+no (print `(,BEST-EVAL ,BEST-CHANGES))
            #+no (when (zerop (mod xx 1000))
                   (break "jsd"))
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
            #+no
            (print `(,BEST-EVAL ,BEST-CHANGES))
            #+no (break "Blah")
            (setq last-new-index -1 LAST-X -1 LAST-Y -1)
            (ecase (first best-changes)
              (:new
               (setq last-new-index (SECOND best-changes))
               (EXPAND-PARTIAL-SOLUTION partial (third best-changes)(SECOND best-changes)))
              (:swap
               (setq last-x (second best-changes)
                   last-y (fourth best-changes))
               (EXPAND-PARTIAL-SOLUTION partial (third best-changes)(SECOND best-changes))
               (EXPAND-PARTIAL-SOLUTION partial (fifth best-changes)(fourth best-changes))
               )
              )
            )
          )
        )
      )
    )
  nil
  )
