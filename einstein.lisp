(in-package :cl-user)

;;;Einsteins Riddle, see generic.lisp
;;;There are five houses in five different colours starting from left to right. 
;;;In each house lives a person of a different nationality. 
;;;These owners all drink a certain type of beverage, 
;;;smoke a certain brand of cigarette and keep a certain type of pet. 
;;;No two owners have the same pet, smoke the same brand or drink the same beverage. 
;;;The question is: WHO OWNS THE FISH??? Hints: 
;;;
;;;The Brit lives in the red house 
;;;The Swede keeps dogs as pets 
;;;The Dane drinks tea 
;;;The green house is on the left of the white house 
;;;The green house's owner drinks coffee 
;;;The person who smokes Pall Mall rears birds 
;;;The owner of the yellow house smokes Dunhill 
;;;The man living in the centre house drinks milk 
;;;The Norwegian lives in the first house 
;;;The person who smokes Marlboro lives next to the one who keeps cats 
;;;The person who keeps horses lives next to the person who smokes Dunhill 
;;;The person who smokes Winfield drinks beer 
;;;The German smokes Rothmans 
;;;The Norwegian lives next to the blue house 
;;;The person who smokes Marlboro has a neigbor who drinks water 

;;;To get a solution, try (test-einstein)
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

(defclass Einstein-Riddle-House (solution-element)
  (
   (nation :accessor riddle-house-nation :initform nil)
   (color :accessor riddle-house-color :initform nil)
   (animal :accessor riddle-house-animal :initform nil)
   (cigarette :accessor riddle-house-cigarette :initform nil)
   (drink :accessor riddle-house-drink :initform nil)
   )
  )

(defmethod show-house-result ((me Einstein-Riddle-House))
  (format t "A house with ")
  (format t "Nation ~10a " (riddle-house-nation me))
  (format t "Color ~10a " (riddle-house-color me))
  (format t "Animal ~10a "(riddle-house-animal me))
  (format t "Cigarette ~10a "(riddle-house-cigarette me))
  (format t "Drink ~10a~%"  (riddle-house-drink me))
  )

(defclass einstein-riddle-problem (SYMBOLIC-PROBLEM-SPECIFICATION)
  ()
  )

(defmethod all-domains-extended ((me einstein-riddle-problem))
  '((:nation (:british :swedish :norwegian :german :danish))
    (:color (:red :green :yellow :blue :white))
    (:animal (:dog :horse :cat :bird :fish))
    (:cigarette (:marlboro :winfield :rothmans :pallmall :dunhill))
    (:drink (:tea :coffee :milk :beer :water))
    )
  )

(defmethod initialize-instance :after ((me einstein-riddle-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list 
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :BRITISH
       :SELECTOR-two :color
       :value-two :red)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :SWEDISH
       :SELECTOR-two :animal
       :value-two :dog)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :danish
       :SELECTOR-two :drink
       :value-two :tea)   
     (make-instance 'DIRECTED-DISTANCE-NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :color
       :VALUE-ONE :green
       :SELECTOR-TWO :color
       :VALUE-TWO :white
       :distance 1)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :color
       :value-one :green
       :SELECTOR-two :drink
       :value-two :coffee)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :animal
       :value-one :bird
       :SELECTOR-two :cigarette
       :value-two :pallmall)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :color
       :value-one :yellow
       :SELECTOR-two :CIGARETTE
       :value-two :dunhill)
     (make-instance 'POSITION-AND-PROPERTY-CONSTRAINT
       :POSITION 2
       :SELECTOR-ONE :drink
       :VALUE-ONE :milk)     
     (make-instance 'POSITION-AND-PROPERTY-CONSTRAINT
       :POSITION 0
       :SELECTOR-ONE :NATION
       :VALUE-ONE :NORWEGIAN)
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :ANIMAL
       :value-one :CAT
       :SELECTOR-two :CIGARETTE
       :value-two :MARLBORO)
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :ANIMAL
       :value-one :HORSE
       :SELECTOR-two :CIGARETTE
       :value-two :DUNHILL)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :CIGARETTE
       :value-one :WINFIELD
       :SELECTOR-two :DRINK
       :value-two :beer)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :german
       :SELECTOR-two :CIGARETTE
       :value-two :ROTHMANS)
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :NORWEGIAN
       :SELECTOR-two :color
       :value-two :blue)
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :CIGARETTE
       :value-one :MARLBORO
       :SELECTOR-two :DRINK
       :value-two :WATER))))

(defclass einstein-partial-solution (riddle-partial-solution)
  (
   )
  )

(defmethod partial-solution-class ((me einstein-riddle-problem))
  (find-class 'einstein-partial-solution))

(defmethod solution-element-class ((me einstein-riddle-problem))
  (find-class 'Einstein-Riddle-House))

#+old
(def-closures
    +einstein-setf-mapper+ 
    (list 0 #'(setf riddle-house-nation)
          1 #'(setf riddle-house-color)
          2 #'(setf riddle-house-animal)
          3 #'(setf riddle-house-cigarette)
          4 #'(setf riddle-house-drink)
          ))

#+old
(defmethod element-mapper ((me einstein-partial-solution))
  +einstein-setf-mapper+)

#+old
(def-closures
    +einstein-property-mapper+ 
    (list :nation #'riddle-house-nation
          :color #'riddle-house-color
          :drink #'riddle-house-drink
          :animal #'riddle-house-animal
          :cigarette #'riddle-house-cigarette))

#+old
(defmethod element-property-mapper ((me EINSTEIN-PARTIAL-SOLUTION))
  +einstein-property-mapper+)

(defmethod read-value-for-key ((me Einstein-Riddle-House) key)
  (ecase key
    (:nation (riddle-house-nation me))
    (:color (riddle-house-color me))
    (:animal (riddle-house-animal me))
    (:cigarette (riddle-house-cigarette me))
    (:drink (riddle-house-drink me))))

(defmethod EXPAND-PARTIAL-SOLUTION ((me Einstein-Riddle-House)
                                    index value)
  (ecase index
    (0 (setf (riddle-house-nation me) value))
    (1 (setf (riddle-house-color me) value))
    (2 (setf (riddle-house-animal me) value))
    (3 (setf (riddle-house-cigarette me) value))
    (4 (setf (riddle-house-drink me) value))))

(defclass einstein-riddle-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'einstein-riddle-problem)))

;;; Test for Einsteins Riddle
(defun test-einstein (&optional (print t))
  (test-backtracking "Einstein Backtracking" (make-instance 'EINSTEIN-RIDDLE-SOLVER-BACKTRACKING) print)
  )

#|
(test-backtracking "Einstein Backtracking" (make-instance 'EINSTEIN-RIDDLE-SOLVER-BACKTRACKING) t)
(defparameter *solver* (make-instance 'einstein-riddle-solver-backtracking))
(defparameter *test* (GENERATE-EMPTY-SOLUTION *solver*))

(EXPAND-PARTIAL-SOLUTION *test* '(:NORWEGIAN :DANISH :BRITISH :GERMAN :SWEDISH) 0)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

(EXPAND-PARTIAL-SOLUTION *test* '(:YELLOW :BLUE :RED :GREEN :white) 1)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

(EXPAND-PARTIAL-SOLUTION *test* '(:CAT :HORSE :BIRD :FISH :DOG) 2)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

(EXPAND-PARTIAL-SOLUTION *test* '(:DUNHILL :MARLBORO :pallmall :ROTHMANS :WINFIELD) 3)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

(EXPAND-PARTIAL-SOLUTION *test* '(:WATER :TEA :MILK :COFFEE :BEER) 4)
(PARTIAL-SOLUTION-CORRECT *solver* *test*)


(time (PARTIAL-SOLUTION-CORRECT *solver* *test*))
|#


#|

(prof:with-profiling (:type :count-only)
  (test-einstein))

(prof:show-call-counts)

(prof:with-profiling (:type :time)
  (test-einstein))

(prof:show-flat-profile)
(prof:show-call-graph)

|#

#|

 (dotimes (x 3)
  #+:allegro
  (mp:without-scheduling 
    (test-einstein))
  #+:lispworks
  (mp:without-preemption
   (test-einstein))
  #-(or :allegro :lispworks)
  (test-einstein)
  )

; cpu time (non-gc) 830 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  830 msec user, 0 msec system
; real time  830 msec
; space allocation:
;  8,468 cons cells, 384 other bytes, 0 static bytes
Test:Einstein Backtracking
The solution in 31612 tries testing 185014 constraints is:
A house with Nation NORWEGIAN  Color YELLOW     Animal CAT        Cigarette DUNHILL    Drink WATER     
A house with Nation DANISH     Color BLUE       Animal HORSE      Cigarette MARLBORO   Drink TEA       
A house with Nation BRITISH    Color RED        Animal BIRD       Cigarette PALLMALL   Drink MILK      
A house with Nation GERMAN     Color GREEN      Animal FISH       Cigarette ROTHMANS   Drink COFFEE    
A house with Nation SWEDISH    Color WHITE      Animal DOG        Cigarette WINFIELD   Drink BEER     
|#

#|
Original Solution
(#S(UNIT NATION NORWEGIAN HOUSE YELLOW ANIMAL CAT CIGARETTE DUNHILL DRINK WATER)
    #S(UNIT NATION DANISH HOUSE BLUE ANIMAL HORSE CIGARETTE MARLBORO DRINK TEA)
    #S(UNIT NATION BRITISH HOUSE RED ANIMAL BIRD CIGARETTE PALLMALL DRINK MILK)
    #S(UNIT NATION GERMAN HOUSE GREEN ANIMAL FISH CIGARETTE ROTHMANS DRINK COFFEE)
    #S(UNIT NATION SWEDISH HOUSE WHITE ANIMAL DOG CIGARETTE WINFIELD DRINK BEER))

; cpu time (non-gc) 770 msec user, 0 msec system
; cpu time (gc)     220 msec user, 0 msec system
; cpu time (total)  990 msec user, 0 msec system
; real time  990 msec
; space allocation:
;  346,860 cons cells, 6,289,576 other bytes, 1136 static bytes
|#


(defclass einstein-riddle-solver-gsat (gsat-riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'einstein-riddle-problem)))

(defun test-gsat-einstein (&optional (print t))
  (TEST-GSAT "Einstein Gsat" (make-instance 'EINSTEIN-RIDDLE-SOLVER-GSAT :MAX-FLIPS 500 :max-tries 50) print)
  )

#|
(TEST-GSAT-EINSTEIN)

(dotimes (x 10)
  (TEST-GSAT-EINSTEIN))

(setq *solver* (make-instance 'GSAT-SOLVER-EINSTEIN))

(GUESS-A-SOLUTION *SOLVER*)

|#

#|
ACL only
Profiler 

(defparameter *der* nil)
(setq *der*
      (let ((solver (make-instance 'gsat-solver-einstein :MAX-FLIPS 500 :max-tries 50))
            )
        (PROF:WITH-PROFILING (:type :time)
          (solve-it solver))))

(PROF:WITH-PROFILING (:type :time)
  (TEST-GSAT-EINSTEIN))

(prof:show-flat-profile)
(prof:show-call-graph)


(setq *der*
      (let ((solver (make-instance 'gsat-solver-einstein :MAX-FLIPS 500 :max-tries 50))
            )
        (PROF:WITH-PROFILING (:type :COUNT-ONLY)
          (solve-it solver))))

(PROF:WITH-PROFILING (:type :count-only)
  (TEST-GSAT-EINSTEIN))

(PROF:SHOW-CALL-COUNTS)


|#