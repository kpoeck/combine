(in-package :cl-user)

;;; 6 barrels of whine
;;; 1 red, 5 white
;;; the barrels are of 15, 16, 18, 19, 20, 31 liters (or gallons if you like)
;;; the dealer sells all white whine to 2 clients.
;;; one client bought twice as much as the other
;;; how many liters are in the barrel with red whine?

;;; To get a solution, try (test-barrels)
;;; Run tests at least twice to get clos prepared

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

(defclass barrels (solution-element)
  (
   (color :accessor barrels-color :initform nil)
   (gallons :accessor barrels-gallons :initform nil)
  )
  )

(defmethod show-house-result ((me barrels))
  (format t "A Barrel with ")
  (format t "Color ~10a " (barrels-color me))
  (format t "Gallons ~10a ~%"(barrels-gallons me))
  )

(defclass barrel-riddle-problem (SYMBOLIC-PROBLEM-SPECIFICATION)
  ()
  )

(defmethod all-domains-extended ((me barrel-riddle-problem))
  '(
    (:color (:red :white-1 :white-2 :white-3 :white-4 :white-5))
    (:gallons (15 16 18 19 20 31))
    )
  )

(defclass barrel-riddle-constraint (riddle-constraints)
  ()
  )


;;; if a local sum in the second loop = twice the remain, the solution fulfilled
(defmethod constraint-holds ((constraint barrel-riddle-constraint)
                             (barrel-partial-solution partial-solution))
  (let ((total-white 0))
    (dolist (barrel (riddle-elements barrel-partial-solution))
      (let ((color (barrels-color barrel))
            (gallons (barrels-gallons barrel)))
        (when (or (null color)
                  (null gallons))
          (return-from constraint-holds t))
        (unless (eq :red color)
          (incf total-white gallons))))
    #+no (print total-white)
    (let ((subtotal 0))
      (dolist (barrel (riddle-elements barrel-partial-solution) nil)
        (unless (eq :red (barrels-color barrel))
          (incf subtotal (barrels-gallons barrel))
          #+no (print `(subtotal ,subtotal remaining ,(- total-white subtotal)))
          (when (= subtotal (* 2 (- total-white subtotal)))
            (return t)))))))

  
(defmethod initialize-instance :after ((me barrel-riddle-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list (make-instance 'barrel-riddle-constraint))))

(defclass barrel-partial-solution (riddle-partial-solution)
  (
   )
  )

(defmethod partial-solution-class ((me barrel-riddle-problem))
  (find-class 'barrel-partial-solution))

(defmethod solution-element-class ((me barrel-riddle-problem))
  (find-class 'barrels))

#+old
(def-closures
    +barrel-setf-mapper+ 
    (list 0 #'(setf barrels-color)
          1 #'(setf barrels-gallons)
          ))

#+old
(defparameter
    +barrel-setf-mapper+ 
    (list 0 #'PCL::|SETF COMMON-LISP-USER BARRELS-COLOR|
          1 #'PCL::|SETF COMMON-LISP-USER BARRELS-GALLONS|
          ))

#+old
(defmethod element-mapper ((me barrel-partial-solution))
  +barrel-setf-mapper+)

#+old
(def-closures
    +barrel-property-mapper+ 
    (list :color #'barrels-color
          :color #'barrels-gallons
          )
  )

#+old
(defmethod element-property-mapper ((me barrel-partial-solution))
  +barrel-property-mapper+)

(defmethod read-value-for-key ((me barrels) key)
  (ecase key
    (:color (barrels-color me))
    (:gallons (barrels-gallons me))))

(defmethod EXPAND-PARTIAL-SOLUTION ((me barrels)
                                    index value)
  (ecase index
    (0 (setf (barrels-color me) value))
    (1 (setf (barrels-gallons me) value))))

(defclass barrel-riddle-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-riddle-problem)))

(defmethod amount-elements ((me barrel-riddle-solver-backtracking))
  6)

(defmethod generate-empty-solution ((me barrel-riddle-solver-backtracking))
  (let ((elements nil)
        (class (solution-element-class (CS-SPECIFICATION me))))
    (dotimes (x (amount-elements me))
      (push (make-instance class) elements))
    (make-instance (partial-solution-class (CS-SPECIFICATION me))
      :riddle-elements elements   
      )
    )
  )

;;; Test for Barrels Riddle
(defun test-barrel-riddle (&optional (print t))
  (test-backtracking "Barrel Riddle Backtracking" (make-instance 'barrel-riddle-solver-backtracking) print)
  )

#|
(test-backtracking "Barrel Riddle Backtracking" (make-instance 'barrel-riddle-solver-backtracking) t)
(generate-perm-array (make-instance 'barrel-riddle-solver-backtracking))
(setq der (generate-empty-solution (make-instance 'barrel-riddle-solver-backtracking)))
(expand-partial-solution der '(:red :white-1 :white-2 :white-3 :white-4 :white-5) 0)
(expand-partial-solution der '(20 16 19 31 15 18) 1)
(constraint-holds (make-instance 'barrel-riddle-constraint) der)
(solve-it (make-instance 'barrel-riddle-solver-backtracking))

|#
