(in-package :cl-user)


;;;I a aware that using min/conflicts or gsat 1-Million queens can be solved easily
;;;This is just proof of concept

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

(defclass eight-queens-riddle-problem (SYMBOLIC-PROBLEM-SPECIFICATION)
  (
   (size :initarg :size :initform 8 :reader eqrp-size)
   )
  )

(defmethod all-domains-extended ((me eight-queens-riddle-problem))
  `(
    (:column ,(loop for x from 0 to (1- (eqrp-size me)) collect x))
    )
  )

(defclass eight-queen-solution-element (solution-element)
  (
   (column :accessor eight-queen-column :initform nil)
   )
  )

(defmethod show-house-result ((me eight-queen-solution-element
                                  ))
  (format t "A Row with ")
  (format t "Column ~10a~%" (eight-queen-column me))
  )

(defclass eight-queen-partial-solution (RIDDLE-PARTIAL-SOLUTION)
  (
   (columns-used   :accessor columns-used)
   (left-columns   :accessor left-columns)
   (right-columns   :accessor right-columns)
   (size :initarg :size :reader eqps-size)
   )
  )

(defmethod initialize-instance :after ((me eight-queen-partial-solution) &rest default-initargs)
  (declare (ignore default-initargs))
  (setf (columns-used me)
    (make-array (eqps-size me) :initial-element 0))
  (setf (left-columns me)
    (make-array (- (* 2 (eqps-size me)) 1) :initial-element 0))
  (setf (right-columns me)
    (make-array (- (* 2 (eqps-size me)) 1) :initial-element 0)))

;;;Two queens (i,j) conflict LeftDown2RightUp if rowi+columni=rowj+columnj
;;;--> diagonal-index = row + column
;;;
;;;Two queens (i,j) conflict LeftUp2RightDown if columni-rowi=columnj-rowj
;;;--> diagonal-index = n-1 + (column-row)

(defmethod partial-solution-class ((solver eight-queens-riddle-problem))
  (find-class 'eight-queen-partial-solution))

(defmethod solution-element-class ((me eight-queens-riddle-problem))
  (find-class 'eight-queen-solution-element))

(defclass column-constraint (riddle-constraints)
  (
   )
  )

(defclass up-diagonal-constraint (riddle-constraints)
  (
   )
  )

(defclass down-diagonal-constraint (riddle-constraints)
  (
   )
  )

(defmethod constraint-holds ((constraint column-constraint) (partial-solution eight-queen-partial-solution))
  (dotimes (x (eqps-size partial-solution) t)
    (when (> (aref (columns-used partial-solution) x) 1)
      (return nil)))
  )

(defmethod constraint-holds ((constraint up-diagonal-constraint) (partial-solution eight-queen-partial-solution))
  (dotimes (x (- (* 2 (eqps-size partial-solution)) 1) t)
    (when (> (aref (left-columns partial-solution) x) 1)
      (return nil)))
  )

(defmethod constraint-holds ((constraint down-diagonal-constraint) (partial-solution eight-queen-partial-solution))
  (dotimes (x (- (* 2 (eqps-size partial-solution)) 1) t)
    (when (> (aref (right-columns partial-solution) x) 1)
      (return nil)))
  )

(defmethod initialize-instance :after ((me eight-queens-riddle-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list
     #+no
     (make-instance 'column-constraint)
     (make-instance 'up-diagonal-constraint)
     (make-instance 'down-diagonal-constraint))))

(defmethod expand-partial-solution ((me eight-queen-partial-solution)
                                    permutation index)
  (declare (ignore index))
  (let ((row 0))
    (reset-all-arrays me)
    (dolist (house (riddle-elements me))
      (let ((value (pop permutation)))
        (setf (eight-queen-column house) value)
        (note-column-used me value)
        (note-down-diagonal-used me row value)
        (note-up-diagonal-used me row value)
        )
      (incf row)
      )
    )
  )

(defmethod reset-all-arrays ((me eight-queen-partial-solution))
  (dotimes (x (length (columns-used me)))
    (setf (aref (columns-used me) x) 0))
  (dotimes (x (length (left-columns me)))
    (setf (aref (left-columns me) x) 0))
  (dotimes (x (length (right-columns me)))
    (setf (aref (right-columns me) x) 0))
  )

(defmethod note-column-used ((me eight-queen-partial-solution) value)
  (incf (aref (columns-used me) value)))

(defmethod note-up-diagonal-used ((me eight-queen-partial-solution) row column)
  (incf (aref (left-columns me) (+ row column))))

(defmethod note-down-diagonal-used ((me eight-queen-partial-solution) row column)
  (incf (aref (right-columns me) (+ (1- (eqps-size me)) (- column row)))))

(defmethod forget-column-used ((me eight-queen-partial-solution) value)
  (decf (aref (columns-used me) value)))

(defmethod forget-up-diagonal-used ((me eight-queen-partial-solution) row column)
  (decf (aref (left-columns me) (+ row column))))

(defmethod forget-down-diagonal-used ((me eight-queen-partial-solution) row column)
  (decf (aref (right-columns me) (+ (1- (eqps-size me)) (- column row)))))


(defmethod FORGET-PARTIAL-SOLUTION ((me eight-queen-partial-solution)
                                    index)
  (declare (ignore index))
  (let ((row 0))
    (dolist (house (riddle-elements me))
      (let ((value (eight-queen-column house)))
        (declare (ignore value))
        (setf (eight-queen-column house) nil)
        )
      (incf row)
      )
    )
  )

(defclass eight-queens-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'eight-queens-riddle-problem)))

(defmethod my-size ((me eight-queens-solver-backtracking))
  (eqrp-size (cs-specification me)))

(defmethod amount-elements ((me eight-queens-solver-backtracking))
  (my-size me))

(defmethod generate-empty-solution ((me eight-queens-solver-backtracking))
  (let ((elements nil)
        (class (solution-element-class (CS-SPECIFICATION me))))
    (dotimes (x (amount-elements me))
      (push (make-instance class) elements))
    (make-instance (partial-solution-class (CS-SPECIFICATION me))
      :riddle-elements elements   
      :size (my-size me)
      )
    )
  )

;;; Test for Zebra Riddle
(defun test-eight-queens (&optional (print t)(size 8))
  (test-backtracking "8 Queens Backtracking"
                     (make-instance 'eight-queens-solver-backtracking
                       :specification (make-instance 'eight-queens-riddle-problem :size size))
                     print)
  )

(defclass eight-queens-solver-gsat (gsat-riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'MICKEY-RIDDLE-PROBLEM)))

(defmethod generate-empty-solution ((me eight-queens-solver-gsat))
  (let ((elements nil)
        (class (solution-element-class (CS-SPECIFICATION me))))
    (dotimes (x (eqrp-size (cs-specification me)))
      (push (make-instance class) elements))
    (make-instance (partial-solution-class (CS-SPECIFICATION me))
      :riddle-elements elements   
      :size (eqrp-size (cs-specification me))
      )
    )
  )

(defmethod READ-VALUE-FOR-KEY ((me EIGHT-QUEEN-SOLUTION-ELEMENT) key)
  (declare (ignore key))
  (eight-queen-column me))

(defmethod EXPAND-PARTIAL-SOLUTION ((me eight-queen-solution-element) index value)
  (unless (= index 0)
    (error "did not expect this index"))
  (setf (eight-queen-column me) value)
  )
  

(defun test-eight-queens-gsat (&optional (print t)(size 8))
  (test-gsat "8 Queens Gsat"
                      (make-instance 'eight-queens-solver-gsat
                        :specification (make-instance 'eight-queens-riddle-problem :size size)
                        :MAX-FLIPS 5000
                        :max-tries 50)
                     print)
  )

#|
(setq der
      (test-backtracking "8 Queens Backtracking" (make-instance 'eight-queens-solver-backtracking) t))

(setq der (generate-empty-solution (make-instance 'eight-queens-solver-backtracking)))
(expand-partial-solution der '(1 7 5 0 2 4 6 3) 0)

(constraint-holds (make-instance 'column-constraint) der)
(constraint-holds (make-instance 'up-diagonal-constraint) der)
(constraint-holds (make-instance 'down-diagonal-constraint) der)
(generate-perm-array (make-instance 'eight-queens-solver-backtracking))
(member '(1 7 5 0 2 4 6 3) (permutation '(0 1 2 3 4 5 6 7)) :test #'equal)


|#
