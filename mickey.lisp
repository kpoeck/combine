(in-package :cl-user)


;;;Mickeys Riddle, see generic.lisp
;;;Mickey Mouse loves Gouda
;;;Mighty Mouse's favorite TV show is Emergency Room
;;;The mouse that lives in the left hole never misses an episode of Seinfeld
;;;Mickey Mouse and Mighty Mouse have one mouse hole between them 
;;;The Simpsons fan does not live on the left of the Brie lover

;;;To get a solution, try (test-mickey)
;;;Run tests at least twice to get clos prepared

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

(defclass mickey-riddle-problem (SYMBOLIC-PROBLEM-SPECIFICATION)
  ()
  )

(defmethod all-domains-extended ((me mickey-riddle-problem))
  '((:name (:mickey :mighty :minny))
    (:tv (:emergency-room :seinfield :simpsons))
    (:cheese (:gouda :brie :emmental))
    )
  )

(defclass mickey-house (solution-element)
  (
   (name :accessor riddle-house-name :initform nil)
   (tv :accessor riddle-house-tv :initform nil)
   (cheese :accessor riddle-house-cheese :initform nil)
   )
  )

(defclass mickey-partial-solution (RIDDLE-PARTIAL-SOLUTION)
  ()
  )

(defmethod read-value-for-key ((me mickey-house) key)
  (ecase key
    (:name (riddle-house-name me))
    (:tv (riddle-house-tv me))
    (:cheese (riddle-house-cheese me))))

(defmethod EXPAND-PARTIAL-SOLUTION ((me mickey-house)
                                    index value)
  (ecase index
    (0 (setf (riddle-house-name me) value))
    (1 (setf (riddle-house-tv me) value))
    (2 (setf (riddle-house-cheese me) value))))


#-:gcl
(def-closures
    +mickey-setf-mapper+ 
    (list 0 #'(setf riddle-house-name)
          1 #'(setf riddle-house-tv)
          2 #'(setf riddle-house-cheese)
          )
  )

#+:gcl
(defparameter +mickey-setf-mapper+ 
    (list 0 #'pcl::|SETF COMMON-LISP-USER RIDDLE-HOUSE-NAME|
          1 #'pcl::|SETF COMMON-LISP-USER RIDDLE-HOUSE-TV|
          2 #'pcl::|SETF COMMON-LISP-USER RIDDLE-HOUSE-CHEESE|
          )
  )

(defmethod element-mapper ((me mickey-partial-solution))
  (error "element-mapper")
  +mickey-setf-mapper+)

(def-closures
    +mickey-property-mapper+ 
    (list :name #'riddle-house-name
          :tv #'riddle-house-tv
          :cheese #'riddle-house-cheese))

(defmethod element-property-mapper ((me mickey-partial-solution))
  (error "element-property-mapper")
  +mickey-property-mapper+)

(defmethod partial-solution-class ((solver mickey-riddle-problem))
  (find-class 'mickey-partial-solution))

(defmethod solution-element-class ((me mickey-riddle-problem))
  (find-class 'mickey-house))

(defmethod show-house-result ((me mickey-house))
  (format t "A house with ")
  (format t "Name ~10a " (riddle-house-name me))
  (format t "Soap ~10a " (riddle-house-tv me))
  (format t "Cheese ~10a~%"(riddle-house-cheese me))
  )

(defmethod initialize-instance :after ((me mickey-riddle-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list 
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :name
       :value-one :mickey
       :SELECTOR-two :cheese
       :value-two :gouda)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :name
       :value-one :MIGHTY
       :SELECTOR-two :tv
       :value-two :emergency-room)
     (make-instance 'POSITION-AND-PROPERTY-CONSTRAINT
       :SELECTOR-ONE :tv
       :VALUE-ONE :seinfield
       :POSITION 0)
     (make-instance 'ABS-DISTANCE-NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :NAME
       :VALUE-ONE :MICKEY
       :SELECTOR-TWO :NAME
       :VALUE-TWO :MIGHTY
       :DISTANCE 2)
     (make-instance 'NEGATED-DIRECTED-DISTANCE-NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :tv
       :VALUE-ONE :SIMPSONS
       :SELECTOR-TWO :CHEESE
       :VALUE-TWO :brie
       :DISTANCE 1)   
     )))

(defclass MICKEY-RIDDLE-SOLVER (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'mickey-riddle-problem)))

;;; Test for Mickeys Riddle
(defun test-mickey (&optional (print t))
  (test-backtracking 
   "Mickey Backtracking" 
   (make-instance 
       'MICKEY-RIDDLE-SOLVER
     ) 
   print)
  )

#|

(test-mickey)
; cpu time (non-gc) 0 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  0 msec user, 0 msec system
; real time  0 msec
; space allocation:
;  183 cons cells, 232 other bytes, 0 static bytes
The solution in 10 tries is:
A house with Name MICKEY     Soap SEINFIELD  Cheese GOUDA     
A house with Name MINNY      Soap SIMPSONS   Cheese BRIE      
A house with Name MIGHTY     Soap EMERGENCY-ROOM Cheese EMMENTAL 
|#

#|

(defparameter *solver* (make-instance 'mickey-riddle-solver))
(defparameter *test* (GENERATE-EMPTY-SOLUTION *solver*))
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

|#

#|
(#(MICKEY GOUDA SEINFELD) #(MINNY BRIE SIMPSONS) #(MIGHTY EMMENTAL ER))

(defparameter *solver* (make-instance 'mickey-riddle-solver))
(defparameter *test* (GENERATE-EMPTY-SOLUTION *solver*))
(EXPAND-PARTIAL-SOLUTION *test* '(:mickey :MINNY :mighty)  0)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)
(EXPAND-PARTIAL-SOLUTION *test* '(:SEINFIELD :SIMPSONS :EMERGENCY-ROOM)  1)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)
(EXPAND-PARTIAL-SOLUTION *test* '(:gouda :brie :emmental)  2)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

|#

(defclass mickey-solver-gsat (gsat-riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'MICKEY-RIDDLE-PROBLEM)))

(defun test-gsat-mickey (&optional (print t))
  (TEST-GSAT
   "Mickey Gsat" 
   (make-instance 'MICKEY-SOLVER-GSAT 
     :MAX-FLIPS 500 
     :max-tries 50) 
   print)
  )

#|
(TEST-GSAT-MICKEY)
|#