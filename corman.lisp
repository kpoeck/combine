(in-package :cl-user)

;;;Fixes for cormanlisp

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

#+no
(defclass barrel-solver-backtracking (numeric-puzzle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-solver-backtracking)))

(defmethod initialize-instance :after ((me numeric-puzzle-solver) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'barrel-problem))
  )

#+no
(defclass barrel-solver-gsat (gsat-all-values-solver)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-problem)))

(defmethod initialize-instance :after ((me barrel-solver-gsat) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'barrel-problem))
  )

#+no
(defclass barrel-solver-gsat-swapping (gsat-solver-swapping)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-problem)))

(defmethod initialize-instance :after ((me barrel-solver-gsat-swapping) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'barrel-problem))
  )

#+no
(defclass barrel-riddle-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-riddle-problem)))

(defmethod initialize-instance :after ((me barrel-riddle-solver-backtracking) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'barrel-riddle-problem))
  )

#+no
(defclass sendMoreMoney-no-carries-solver-gsat-all-values (GSAT-ALL-VALUES-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-without-carries)))

(defmethod initialize-instance :after ((me sendMoreMoney-no-carries-solver-gsat-all-values) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'send-money-problem-without-carries))
  )

#+no
(defclass einstein-riddle-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'einstein-riddle-problem)))

(defmethod initialize-instance :after ((me einstein-riddle-solver-backtracking) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'einstein-riddle-problem))
)

#+no
(defclass einstein-riddle-solver-gsat (gsat-riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'einstein-riddle-problem)))

(defmethod initialize-instance :after ((me einstein-riddle-solver-gsat) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'einstein-riddle-problem))

  
  (setf (gsat-max-flips me) 500)
  (setf (gsat-max-tries me) 50)
  (setf (gsat-worsened me) 0)
  (setf (gsat-aborted me) 0))

#+no
(defclass MICKEY-RIDDLE-SOLVER (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'mickey-riddle-problem)))

(defmethod initialize-instance :after ((me MICKEY-RIDDLE-SOLVER) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'mickey-riddle-problem))
)

#+no
(defclass mickey-solver-gsat (gsat-riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'MICKEY-RIDDLE-PROBLEM)))

(defmethod initialize-instance :after ((me mickey-solver-gsat) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'mickey-riddle-problem))

  
  (setf (gsat-max-flips me) 500)
  (setf (gsat-max-tries me) 50)
  (setf (gsat-worsened me) 0)
  (setf (gsat-aborted me) 0)
  )

#+no
(defclass sendMoreMoney-solver-backtracking-no-carries (NUMERIC-PUZZLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-without-carries)))

(defmethod initialize-instance :after ((me sendMoreMoney-solver-backtracking-no-carries) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'send-money-problem-without-carries))
)

#+no
(defclass sendMoreMoney-solver-gsat-swap (gsat-solver-swapping)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-without-carries)))

(defmethod initialize-instance :after ((me sendMoreMoney-solver-gsat-swap) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'send-money-problem-without-carries))
)

#+no
(defclass sendMoreMoney-solver-backtracking-carries (NUMERIC-PUZZLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-carries)))

(defmethod initialize-instance :after ((me sendMoreMoney-solver-backtracking-carries) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'send-money-problem-carries))
)

#+no
(defclass sendMoreMoney-solver-gsat-all-values (GSAT-ALL-VALUES-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-carries)))

(defmethod initialize-instance :after ((me sendMoreMoney-solver-gsat-all-values) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'send-money-problem-carries))
)

#+no
(defclass zebra-riddle-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'zebra-riddle-problem)))

(defmethod initialize-instance :after ((me zebra-riddle-solver-backtracking) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'zebra-riddle-problem))
)

#+no
(defclass zebra-riddle-solver-gsat (GSAT-RIDDLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'ZEBRA-RIDDLE-PROBLEM)))

(defmethod initialize-instance :after ((me zebra-riddle-solver-gsat) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'zebra-riddle-problem))
  )

#+no
(defclass eight-queens-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'eight-queens-riddle-problem)))

(defmethod initialize-instance :after ((me eight-queens-solver-backtracking) &rest args)
  (declare (ignore args))
  (setf (cs-specification me) (make-instance 'eight-queens-riddle-problem))
  )