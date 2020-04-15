(in-package :cl-user)


;;;zebra problem see PAIP page 372

;;;% The Englishman lives in the red house
;;;member(h(englishman,H1), Nationalities),
;;;member(h(red,H1), Colours),

;;;% The Spaniard owns the dog
;;;member(h(spaniard,H2), Nationalities),
;;;member(h(dog,H2), Pets),

;;;% The Norwegian lives in the first house on the lleft
;;;member(h(norwegian,1), Nationalities),

;;;% Cools are smoked in the yellow house.
;;;member(h(kools,H3), Cigarettes),
;;;member(h(yellow,H3), Colours),

;;;% The man who smokes Chesterfields lives in the house
;;;% next to the man with the fox.
;;;member(h(chesterfields,H4), Cigarettes),
;;;next(H4, H5),
;;;member(h(fox,H5), Pets),

;;;% The Norwegian lives next to the blue house
;;;member(h(norwegian,H6), Nationalities),
;;;next(H6, H7),
;;;member(h(blue,H7), Colours),

;;;% The Winston smoker owns snails.
;;;member(h(winston,H8), Cigarettes),
;;;member(h(snails,H8), Pets),

;;;% The lucky strike smoker drinks orange juice
;;;member(h(lucky_strike,H9), Cigarettes),
;;;member(h(orange_juice,H9), Drinks),

;;;% The Ukrainian drinks tea
;;;member(h(ukrainian,H10), Nationalities),
;;;member(h(tea,H10), Drinks),

;;;% The Japanese smokes parliaments
;;;member(h(japanese,H11), Nationalities),
;;;member(h(parliaments,H11), Cigarettes),

;;;% Cools are smoked in the house next to the house where the horse is kept.
;;;member(h(kools,H12), Cigarettes),
;;;next(H12, H13),
;;;member(h(horse,H13), Pets),

;;;% Coffee is drunk in the green house
;;;member(h(coffee,H14), Drinks),
;;;member(h(green,H14), Colours),

;;;% The green house is immediately to the right (your right) of the ivory house
;;;member(h(green,H15), Colours),
;;;lleft(H16, H15),
;;;member(h(ivory,H16), Colours),

;;;% Milk is drunk in the middle house.
;;;member(h(milk,3), Drinks).

;;;To get a solution, try (test-zebra)
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

(defclass zebra-riddle-problem (SYMBOLIC-PROBLEM-SPECIFICATION)
  ()
  )

(defmethod all-domains-extended ((me zebra-riddle-problem))
  '((:nation (:english :spanish :ukrainian :norwegian :japanese))
    (:color (:red :green :ivory :yellow :blue))
    (:animal (:dog :snails :fox :horse :zebra))
    (:cigarette (:winston :cools :chesterfield :luckystrike :parliament))
    (:drink (:COFFEE :tea  :milk :orange-juice :water))
    )
  )

(defclass zebra-house (solution-element)
  (
   (nation :accessor zebra-house-NATION :initform nil)
   (color :accessor zebra-house-COLOR :initform nil)
   (animal :accessor zebra-house-animal :initform nil)
   (CIGARETTE :accessor zebra-house-CIGARETTE :initform nil)
   (drink :accessor zebra-house-drink :initform nil)
   )
  )

(defclass zebra-partial-solution (RIDDLE-PARTIAL-SOLUTION)
  ()
  )

#+old
(def-closures +zebra-setf-mapper+ 
    (list 0 #'(setf zebra-house-nation)
          1 #'(setf zebra-house-color)
          2 #'(setf zebra-house-animal)
          3 #'(setf zebra-house-CIGARETTE)
          4 #'(setf zebra-house-drink)
          ))

#+old
(defparameter +zebra-setf-mapper+ 
    (list 0 #'PCL::|SETF COMMON-LISP-USER ZEBRA-HOUSE-NATION|
          1 #'PCL::|SETF COMMON-LISP-USER ZEBRA-HOUSE-COLOR|
          2 #'PCL::|SETF COMMON-LISP-USER ZEBRA-HOUSE-ANIMAL|
          3 #'PCL::|SETF COMMON-LISP-USER ZEBRA-HOUSE-CIGARETTE|
          4 #'PCL::|SETF COMMON-LISP-USER ZEBRA-HOUSE-DRINK|
          ))

#+old
(defmethod element-mapper ((me zebra-partial-solution))
  +zebra-setf-mapper+)

#+old
(def-closures +zebra-property-mapper+ 
    (list :nation #'zebra-house-nation
          :color #'zebra-house-color
          :animal #'zebra-house-animal
          :cigarette #'zebra-house-cigarette
          :drink #'zebra-house-drink
          ))

#+old
(defmethod element-property-mapper ((me zebra-partial-solution))
  +zebra-property-mapper+)

(defmethod read-value-for-key ((me zebra-house) key)
  (ecase key
    (:nation (zebra-house-nation me))
    (:color (zebra-house-color me))
    (:animal (zebra-house-animal me))
    (:cigarette (zebra-house-cigarette me))
    (:drink (zebra-house-drink me))))

(defmethod EXPAND-PARTIAL-SOLUTION ((me zebra-house)
                                    index value)
  (ecase index
    (0 (setf (zebra-house-nation me) value))
    (1 (setf (zebra-house-color me) value))
    (2 (setf (zebra-house-animal me) value))
    (3 (setf (zebra-house-cigarette me) value))
    (4 (setf (zebra-house-drink me) value))))

(defmethod partial-solution-class ((solver zebra-riddle-problem))
  (find-class 'zebra-partial-solution))

(defmethod solution-element-class ((me zebra-riddle-problem))
  (find-class 'zebra-house))

(defmethod show-house-result ((me zebra-house))
  (format t "A house with ")
  (format t "Nation ~10a " (zebra-house-nation me))
  (format t "Color ~10a " (zebra-house-color me))
  (format t "Animal ~10a"(zebra-house-animal me))
  (format t "Cigarette ~15a"(zebra-house-cigarette me))
  (format t "Drink ~10a~%"(zebra-house-drink me))
  )

(defmethod initialize-instance :after ((me zebra-riddle-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list 
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :english
       :SELECTOR-two :COLOR
       :value-two :red)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :SPANISH
       :SELECTOR-two :ANIMAL
       :value-two :DOG)
     (make-instance 'POSITION-AND-PROPERTY-CONSTRAINT
       :SELECTOR-ONE :nation
       :VALUE-ONE :NORWEGIAN
       :POSITION 0)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :CIGARETTE
       :value-one :COOLS
       :SELECTOR-two :COLOR
       :value-two :YELLOW)
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :CIGARETTE
       :VALUE-ONE :chesterfield
       :SELECTOR-TWO :animal
       :VALUE-TWO :fox
       )
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :nation
       :VALUE-ONE :norwegian
       :SELECTOR-TWO :color
       :VALUE-TWO :blue
       )
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :CIGARETTE
       :value-one :WINSTON
       :SELECTOR-two :ANIMAL
       :value-two :SNAILS)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :CIGARETTE
       :value-one :LUCKYSTRIKE
       :SELECTOR-two :DRINK
       :value-two :ORANGE-JUICE)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :UKRAINIAN
       :SELECTOR-two :DRINK
       :value-two :TEA)
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :nation
       :value-one :JAPANESE
       :SELECTOR-two :CIGARETTE
       :value-two :PARLIAMENT)
     (make-instance 'NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :cigarette
       :VALUE-ONE :COOLS
       :SELECTOR-TWO :animal
       :VALUE-TWO :horse
       )
     (make-instance 'TWO-VALUES-IN-HOUSE-CONSTRAINT
       :SELECTOR-ONE :DRINK
       :value-one :COFFEE
       :SELECTOR-two :COLOR
       :value-two :GREEN)
     
     (make-instance 'DIRECTED-DISTANCE-NEIGHBOUR-CONSTRAINT
       :SELECTOR-ONE :color
       :VALUE-ONE :IVORY
       :SELECTOR-TWO :color
       :VALUE-TWO :GREEN
       :DISTANCE 1
       )
     (make-instance 'POSITION-AND-PROPERTY-CONSTRAINT
       :SELECTOR-ONE :drink
       :VALUE-ONE :MILK
       :POSITION 2) 
     )))

(defclass zebra-riddle-solver-backtracking (riddle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'zebra-riddle-problem)))

;;; Test for Zebra Riddle
(defun test-zebra (&optional (print t))
  (test-backtracking "Zebra Backtracking" (make-instance 'zebra-riddle-solver-backtracking) print)
  )

#|
(test-backtracking "Zebra Backtracking" (make-instance 'zebra-riddle-solver-backtracking) t 1000)
(test-zebra)
CL-USER(21): (test-zebra)
; cpu time (non-gc) 220 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  220 msec user, 0 msec system
; real time  220 msec
; space allocation:
;  8,443 cons cells, 384 other bytes, 0 static bytes

The solution in 14903 tries is:
A house with Nation NORWEGIAN  Color YELLOW     Animal FOX       CIGARETTE COOLS          Drink WATER     
A house with Nation UKRAINIAN  Color BLUE       Animal HORSE     CIGARETTE CHESTERFIELD   Drink TEA       
A house with Nation ENGLISH    Color RED        Animal SNAILS    CIGARETTE WINSTON        Drink MILK      
A house with Nation SPANISH    Color IVORY      Animal DOG       CIGARETTE LUCKYSTRIKE    Drink ORANGE-JUICE
A house with Nation JAPANESE   Color GREEN      Animal ZEBRA     CIGARETTE PARLIAMENT     Drink COFFEE    
|#

(defclass zebra-riddle-solver-gsat (GSAT-RIDDLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'ZEBRA-RIDDLE-PROBLEM)))

(defun test-gsat-zebra (&optional (print t))
  (TEST-GSAT "Zebra Gsat" (make-instance 'zebra-riddle-solver-gsat :MAX-FLIPS 500 :max-tries 50) print)
  )

#|

(test-gsat-zebra)

|#