(in-package :cl-user)

;;; 6 barrels of whine
;;; 1 red, 5 blue
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

(defclass barrel-problem (numeric-one-domain-problem-specification)
  ()
  )

(defmethod the-domain ((me barrel-problem))
  '(15 16 18 19 20 31))

(defmethod the-variables ((me barrel-problem))
  '(:red :white-1 :white-2 :white-3 :white-4 :white-5))

(defparameter *barrel-domains*
  '((:red (15 16 18 19 20 31))
    (:white-1 (15 16 18 19 20 31))
    (:white-2 (15 16 18 19 20 31))
    (:white-3 (15 16 18 19 20 31))
    (:white-4 (15 16 18 19 20 31))
    (:white-5 (15 16 18 19 20 31))
    )
  )

(defmethod all-domains-extended ((me barrel-problem))
  *barrel-domains*
  )

(defmethod initialize-instance :after ((me barrel-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list 
     (make-instance 'all-distinct
       :sm-indices (find-variable-indices me (the-variables me)))
     (make-instance 'barrel-constraint)
     )
    )
  )

#|
Number 0 is red whine
Number 1 ..X White double cuantity
Number X+1 .. 5 White single cuantity
1/4
2/3
3/2
4/1

Most probable is 3/2
|#

(defclass barrel-constraint (number-puzzle-constraints)
  (
   )
  )

(defmethod constraint-holds ((constraint barrel-constraint)(ps numeric-puzzle-partial-solution))
  (let ((value-a (svref (number-array ps) 1))
        (value-b (svref (number-array ps) 2))
        (value-c (svref (number-array ps) 3))
        (value-d (svref (number-array ps) 4))
        (value-e (svref (number-array ps) 5))
        )
    (if (and value-a value-b value-c value-d value-e)
        (or 
         (is-double constraint (list value-a) (list value-b value-c value-d value-e))
         (is-double constraint (list value-a value-b) (list value-c value-d value-e))
         (is-double constraint (list value-a value-b value-c) (list  value-d value-e))
         (is-double constraint (list value-a value-b value-c value-d) (list value-e))
         )
      t)))

#+:no
(defmethod is-double ((constraint barrel-constraint) list-a list-b)
  (=  (reduce #'+ list-a)(* 2 (reduce #'+ list-b))))

(defmethod is-double ((constraint barrel-constraint) list-a list-b)
  (flet ((sum-list (a-list)
                   (loop for element in a-list
                         sum element)))
    (=  (sum-list list-a)(* 2 (sum-list list-b))))
  )

(defclass barrel-solver-backtracking (numeric-puzzle-solver)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-problem)))

(defun test-barrel (&optional (print t))
  (test-backtracking "barrel backtracking" (make-instance 'barrel-solver-backtracking) print)
  )

#|
(test-barrel)

Variable RED is:20

Variable WHITE-1 is:16
Variable WHITE-2 is:19
Variable WHITE-3 is:31
Sum = 66
Variable WHITE-4 is:15
Variable WHITE-5 is:18
Sum = 33
|#

(defclass barrel-solver-gsat (gsat-all-values-solver)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-problem)))

(defun test-barrel-gsat (&optional (print t))
  (test-gsat "barrel gsat" (make-instance 'barrel-solver-gsat :max-flips 500 :max-tries 50) print)
  )

#|

(test-barrel-gsat)

|#

(defclass barrel-solver-gsat-swapping (gsat-solver-swapping)
  ()
  (:default-initargs
      :specification (make-instance 'barrel-problem)))

(defun test-barrel-gsat-swapping (&optional (print t))
  (test-gsat "barrel gsat swapping" (make-instance 'barrel-solver-gsat-swapping :max-flips 500 :max-tries 50) print)
  )

#|

(TEST-BARREL-GSAT-SWAPPING)

|#
