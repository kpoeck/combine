(in-package :cl-user)

;;;GSAT [Selman, Levesque, Mitchell AAAI-92, 440-446]
;;; applied to Einsteins riddle

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

(defclass gsat-riddle-solver (GSAT-SOLVER-MIXIN combinatoric-solver)
  (
   )
  )

;;; should play a bit with this to incorporate simulated annealing
(defmethod allow-worser-solution ((me gsat-riddle-solver))
  t)

(defmethod guess-a-solution ((me gsat-riddle-solver))
  (let (
        (empty (generate-empty-solution me))
        (index 0)
        )
    (dolist (domain (all-domains me))
      (EXPAND-PARTIAL-SOLUTION empty (generate-random-permutation domain) index)
      (incf index))
      empty
    )
  )

(defun generate-random-permutation (values)
  (let ((res nil)
        (size (length values))
        )
    (dotimes (x size)
      (let ((new (nth (random (length values)) values)))
        (push new res)
        (setq values (remove new values))))
    res))
 
(defmethod variable-size ((me GSAT-RIDDLE-SOLVER))
  (length (get-possible-values me 0)))

(DEFMETHOD get-possible-values ((me GSAT-RIDDLE-SOLVER) index)
  (second (nth index (ALL-DOMAINS-EXTENDED me))))

(defmethod index-to-selector ((me GSAT-RIDDLE-SOLVER) index)
  (first (nth index (ALL-DOMAINS-EXTENDED me)))
  )
   
#+no
(defmethod get-house-element ((solver GSAT-RIDDLE-SOLVER) partial index x)
  (let ((getter (getf (element-property-mapper PARTIAL) (INDEX-TO-SELECTOR solver index)))
        (house (nth x (RIDDLE-ELEMENTS partial))))
    (funcall  getter house)
    )
  )

(defmethod get-house-element ((solver GSAT-RIDDLE-SOLVER) partial index x)
  (let ((key (INDEX-TO-SELECTOR solver index))
        (house (nth x (RIDDLE-ELEMENTS partial))))
    (read-value-for-key house key)))

#+old
(defmethod swap-partial-solution ((partial riddle-partial-solution) INDEX x new-x y new-y)
  (let ((setter (getf (ELEMENT-MAPPER PARTIAL) index))
        (house-x (nth x (RIDDLE-ELEMENTS partial)))
        (house-y (nth y (RIDDLE-ELEMENTS partial))))
    (funcall setter new-x house-x)
    (funcall setter new-y house-y)
    )
  (values)
  )

(defmethod swap-partial-solution ((partial riddle-partial-solution) INDEX x new-x y new-y)
  (let ((house-x (nth x (RIDDLE-ELEMENTS partial)))
        (house-y (nth y (RIDDLE-ELEMENTS partial))))
    (EXPAND-PARTIAL-SOLUTION house-x index new-x)
    (EXPAND-PARTIAL-SOLUTION  house-y index new-y)
    )
  (values)
  )

(defmethod solve-it ((solver GSAT-RIDDLE-SOLVER))
  #-gcl (setf (solution-tried solver) 0)
  (setf (GSAT-WORSENED solver) 0)
  (dotimes (x (GSAT-MAX-tries solver))
    (let (
          (partial (GUESS-A-SOLUTION solver))
          (last-x -1)
          (last-y -1)
          (last-index -1)
          (p-size (problem-size solver))
          (v-size (variable-size solver))
          )
      (dotimes (xx (GSAT-MAX-FLIPS solver))
        (let ((eval (PARTIAL-SOLUTION-EVALUATION SOLVER partial)))
          (when (zerop eval)
            (setf (gsat-solved-iteration solver) x)
            (return-from solve-it partial))
          (let ((best-changes nil)
                (best-eval nil)
                )           
            ;;; try to swap
            (dotimes (index p-size)
              ;;; x = index of parameters in house
              (dotimes (x v-size)
                (dotimes (y v-size)
                  (when (> y x)
                    ;;;swap the values of x and y
                    (unless (and (= x last-x)(= y last-y)(= index last-index))
                      (let ((before-x (get-house-element solver partial index x))
                            (before-y (get-house-element solver partial index y)))
                        #-gcl (incf (solution-tried solver))
                        (swap-partial-solution partial index x before-y y before-x)
                        ;;; evaluate
                        (let ((curr (PARTIAL-SOLUTION-EVALUATION solver partial)))
                          (cond ((null best-eval)
                                 (setq best-eval curr
                                     best-changes (list (list :swap INDEX x BEFORE-y y BEFORE-x))))
                                ((= best-eval curr)
                                 (push (list :swap index x BEFORE-y y BEFORE-x) best-changes))
                                ((< curr best-eval)
                                 (setq best-eval curr
                                     best-changes (list (list :swap INDEX x BEFORE-y y BEFORE-x)))
                                 )
                                )
                          )
                        ;;; reset
                        (swap-partial-solution partial INDEX x before-x y before-y)
                        ))))))
 
            ;;;apply best change
            #+no
            (print `(,BEST-EVAL ,BEST-CHANGES))
            #+no
            (break "s,d2")
            (setq best-changes
                  (if (null (rest best-changes))
                      (first best-changes)
                    (nth (random (length best-changes)) best-changes)))
            (when (> BEST-EVAL eval)
              (cond ((ALLOW-WORSER-SOLUTION solver)
                     (incf (GSAT-WORSENED solver)))
                    (t (incf (gsat-aborted solver))
                       ;;abort the try
                       (return))))
                       
            #+no
            (print `(,BEST-EVAL ,BEST-CHANGES))
            #+no (break "Blah")
            (let ((INDEX (second BEST-CHANGES))
                  (X (third BEST-CHANGES))
                  (x-value (fourth BEST-CHANGES))
                  (y (fifth BEST-CHANGES))
                  (y-value (sixth BEST-CHANGES)))
              (swap-partial-solution partial INDEX x x-value y y-value)
              ;;; avoid looping
              (setq LAST-X x LAST-Y y last-index index)
              )
            )
          )
        )
      )
    )
  nil
  )
