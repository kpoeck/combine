#|
http://www.wired.com/2015/06/answer-150-year-old-math-conundrum-brings-mystery/
Fifteen young ladies in a school walk out three abreast for seven days in succession: 
it is required to arrange them daily, so that no two shall walk twice abreast.” 
(By “abreast,” Kirkman meant “in a group,” so the girls are walking out in groups of three, 
and each pair of girls should be in the same group just once.)
|#

#|
Representation
Group-size = 3
Days = 7
Ladies = a b c d e f g h i j k l m n o
Group-names = g1 g2 g3 g4 g5

Array of ladies of Groups
|#

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

(defclass 15-ladies-problem (NUMERIC-ONE-DOMAIN-PROBLEM-SPECIFICATION)
  ()
  )

(defmethod the-domain ((me  15-ladies-problem ))
  (list :g1 :g2 :g3 :g4 :g5)
  )

(defmethod the-variables ((me  15-ladies-problem))
  '(
    :a1 :b1 :c1 :d1 :e1 :f1 :g1 :h1 :i1 :j1 :k1 :l1 :m1 :n1 :o1
    :a2 :b2 :c2 :d2 :e2 :f2 :g2 :h2 :i2 :j2 :k2 :l2 :m2 :n2 :o2
    :a3 :b3 :c3 :d3 :e3 :f3 :g3 :h3 :i3 :j3 :k3 :l3 :m3 :n3 :o3
    :a4 :b4 :c4 :d4 :e4 :f4 :g4 :h4 :i4 :j4 :k4 :l4 :m4 :n4 :o4
    :a5 :b5 :c5 :d5 :e5 :f5 :g5 :h5 :i5 :j5 :k5 :l5 :m5 :n5 :o5
    :a6 :b6 :c6 :d6 :e6 :f6 :g6 :h6 :i6 :j6 :k6 :l6 :m6 :n6 :o6
    :a7 :b7 :c7 :d7 :e7 :f7 :g7 :h7 :i7 :j7 :k7 :l7 :m7 :n7 :o7
       )
  )

(defmethod problem-size ((me 15-ladies-problem))
  (length (the-variables me)))

(defclass ladies-solver-backtracking (NUMERIC-PUZZLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance ' 15-ladies-problem)))

(defmethod  problem-size ((me ladies-solver-backtracking))
  (problem-size (cs-specification me)))

(defmethod all-domains ((me ladies-solver-backtracking))
   (all-domains (cs-specification me)))

(defmethod all-domains ((me 15-ladies-problem))
   (mapcar #'(lambda(var)
               (the-domain me))
           (the-variables me)))

(defmethod all-domains-extended ((me 15-ladies-problem))
  (break "This should not be called")
    )

(defmethod all-variable-names ((me ladies-solver-backtracking))
  (all-variable-names (cs-specification me)))

(defmethod all-variable-names ((me  15-ladies-problem))
  (the-variables me))

(defmethod find-variable-index ((me 15-ladies-problem) name)
  (let ((index 0))
    (dolist (var (the-variables me)(error "Did not found variable"))
      (if (eq name var)
        (return index)
        (incf index)))))

;;;; Constraints
(defclass ladies-constraints (number-puzzle-constraints)
  ()
  )

(defclass group<=3-ladies-constraints (ladies-constraints)
  ((indices :initarg :indices :accessor group<=3-indices)
   (group :initarg :group :accessor group<=3-group)
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint group<=3-ladies-constraints)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((group (group<=3-group constraint))
        (counter 0))
  (dolist (index (group<=3-indices constraint) t)
    (let ((value (svref  (number-array ps) index)))
      (cond ((null value))
            ((eq value group)
             (incf counter)
             (if (= counter 4)
                 (return-from CONSTRAINT-HOLDS nil))))))))


(defmethod initialize-instance :after ((me 15-ladies-problem) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
        (mapcar #'(lambda(group)
                    (make-instance 'group<=3-ladies-constraints
                      :group group
                      :indices (mapcar #'(lambda(var)
                                           (find-variable-index me var))
                                       '(:a1 :b1 :c1 :d1 :e1 :f1 :g1 :h1 :i1 :j1 :k1 :l1 :m1 :n1 :o1))))
                '(:g1 :g2 :g3 :g4 :g5))
        )
  )

(defun test-15-ladies (&optional (print t))
  (test-backtracking "15 ladies Backtracking" (make-instance 'ladies-solver-backtracking ) print)
  )
