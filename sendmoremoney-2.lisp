(in-package :cl-user)

;;;  Send
;;;+ More
;;;=Money

;;;To get a solution, try (test-sendMoreMoney)
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

(defclass send-money-problem-without-carries (NUMERIC-ONE-DOMAIN-PROBLEM-SPECIFICATION)
  ()
  )

(defmethod the-domain ((me SEND-MONEY-PROBLEM-WITHOUT-CARRIES))
  '(0 1 2 3 4 5 6 7 8 9))

(defmethod the-variables ((me SEND-MONEY-PROBLEM-WITHOUT-CARRIES))
  '(:s :e :n :d :m :o :r :y))

(defparameter *send-more-money-domains-2*
  '((:s (0 1 2 3 4 5 6 7 8 9))
    (:e (0 1 2 3 4 5 6 7 8 9))
    (:n (0 1 2 3 4 5 6 7 8 9))
    (:d (0 1 2 3 4 5 6 7 8 9))
    (:m (0 1 2 3 4 5 6 7 8 9))
    (:o (0 1 2 3 4 5 6 7 8 9))
    (:r (0 1 2 3 4 5 6 7 8 9))
    (:y (0 1 2 3 4 5 6 7 8 9))
    )
  )

(defmethod all-domains-extended ((me send-money-problem-without-carries))
  *send-more-money-domains-2*
    )

(defmethod initialize-instance :after ((me send-money-problem-without-carries) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list 
     (make-instance 'index<>value
       :sm-index (FIND-VARIABLE-INDEX me :s)
       :sm-value 0)
     (make-instance 'index<>value
       :sm-index (FIND-VARIABLE-INDEX me :m)
       :sm-value 0)
     (make-instance 'all-distinct
       :sm-indices (FIND-VARIABLE-INDices me '(:s :e :n :d :m :o :r :y)))
     (make-instance 'mega-constraint
       :INDEX-SUM-10000 (FIND-VARIABLE-INDEX me :m)
       :INDEX-SUM-1000 (FIND-VARIABLE-INDEX me :o)
       :INDEX-SUM-100 (FIND-VARIABLE-INDEX me :n)
       :INDEX-SUM-10 (FIND-VARIABLE-INDEX me :e)
       :INDEX-SUM-1 (FIND-VARIABLE-INDEX me :y)
       
       :INDEX-SUMAND-A-1000 (FIND-VARIABLE-INDEX me :s)
       :INDEX-SUMAND-A-100 (FIND-VARIABLE-INDEX me :e)
       :INDEX-SUMAND-A-10 (FIND-VARIABLE-INDEX me :n)
       :INDEX-SUMAND-A-1 (FIND-VARIABLE-INDEX me :d)
      
       :INDEX-SUMAND-B-1000 (FIND-VARIABLE-INDEX me :m)
       :INDEX-SUMAND-B-100 (FIND-VARIABLE-INDEX me :o)
       :INDEX-SUMAND-B-10 (FIND-VARIABLE-INDEX me :r)
       :INDEX-SUMAND-B-1 (FIND-VARIABLE-INDEX me :e)
       )
     (make-instance 'COLUMN=MOD10
       :value-a (FIND-VARIABLE-INDEX me :d)
       :value-b (FIND-VARIABLE-INDEX me :e)
       :mod10sum (FIND-VARIABLE-INDEX me :y))
     (make-instance 'COLUMN=MORE-OR-LESS
       :value-a (FIND-VARIABLE-INDEX me :n)
       :value-b (FIND-VARIABLE-INDEX me :r)
       :mod10sum (FIND-VARIABLE-INDEX me :e))
     (make-instance 'COLUMN=MORE-OR-LESS
       :value-a (FIND-VARIABLE-INDEX me :e)
       :value-b (FIND-VARIABLE-INDEX me :o)
       :mod10sum (FIND-VARIABLE-INDEX me :n))
     (make-instance 'COLUMN=MORE-OR-LESS
       :value-a (FIND-VARIABLE-INDEX me :s)
       :value-b (FIND-VARIABLE-INDEX me :m)
       :mod10sum (FIND-VARIABLE-INDEX me :o))
     (make-instance 'COLUMN=DIV-MORE-OR-LESS
       :value-a (FIND-VARIABLE-INDEX me :s)
       :value-b (FIND-VARIABLE-INDEX me :m)
       :mod10sum (FIND-VARIABLE-INDEX me :m))
     )
    )
  )


(defclass sendMoreMoney-solver-backtracking-no-carries (NUMERIC-PUZZLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-without-carries)))

(defun test-sendMoreMoney-no-carries (&optional (print t))
  (test-backtracking "Send more Money no Carries Backtracking" (make-instance 'sendMoreMoney-solver-backtracking-no-carries) print)
  )

#|
(test-sendMoreMoney-no-carries)
(* 10 10 10 10 10 10 10 10)
10 663 399 tries with the minimal constraints
the same with mod10(d+e)=y (because y is the last variable to be checked, so no added value)
 3 902 766 tries with mod10(n + r) = e or mod10(n + r + 1) = e
 1 072 147 tries with mod10(e + o) = n or mod10(e + o + 1) = n
   562 198 tries with mod10(s + m) = o or mod10(s + m + 1) = o
    84 182 tries with div10(s + m) = m or div10(s + m + 1= = m

|#

#|
(defparameter *solver* (make-instance 'SEND-MONEY-SOLVER-2))
(defparameter *test* (GENERATE-EMPTY-SOLUTION *solver*))
(EXPAND-PARTIAL-SOLUTION *test* 9  (FIND-VARIABLE-INDEX *SOLVER* :s))
(EXPAND-PARTIAL-SOLUTION *test* 5  (FIND-VARIABLE-INDEX *SOLVER* :e))
(EXPAND-PARTIAL-SOLUTION *test* 6  (FIND-VARIABLE-INDEX *SOLVER* :n))
(EXPAND-PARTIAL-SOLUTION *test* 7  (FIND-VARIABLE-INDEX *SOLVER* :d))
(EXPAND-PARTIAL-SOLUTION *test* 1  (FIND-VARIABLE-INDEX *SOLVER* :m))
(EXPAND-PARTIAL-SOLUTION *test* 0  (FIND-VARIABLE-INDEX *SOLVER* :o))
(EXPAND-PARTIAL-SOLUTION *test* 8  (FIND-VARIABLE-INDEX *SOLVER* :r))
(EXPAND-PARTIAL-SOLUTION *test* 2  (FIND-VARIABLE-INDEX *SOLVER* :y))
(PARTIAL-SOLUTION-CORRECT *solver* *test*)

|#

      
(defclass sendMoreMoney-solver-gsat-swap (gsat-solver-swapping)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-without-carries)))

(defmethod UNUSED-VALUES-EXISTS ((me SENDMOREMONEY-SOLVER-GSAT-SWAP))
  t)

(defun test-gsat-swapping-sendMoreMoney (&optional (print t))
  (TEST-GSAT "Send more Money no Carries Gsat swapping" (make-instance 'SENDMOREMONEY-SOLVER-GSAT-SWAP :MAX-FLIPS 500 :max-tries 50) print)
  )

#|
(TEST-GSAT-SWAPPING-SENDMOREMONEY)

(dotimes (x 10)
  (TEST-GSAT-SWAPPING-SENDMOREMONEY))
|#

(defclass sendMoreMoney-no-carries-solver-gsat-all-values (GSAT-ALL-VALUES-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-without-carries)))

(defun test-gsat-all-values-sendMoreMoney (&optional (print t))
  (TEST-GSAT "Send more Money no Carries Gsat all Values" (make-instance 'SENDMOREMONEY-NO-CARRIES-SOLVER-GSAT-ALL-VALUES :MAX-FLIPS 500 :max-tries 50) print)
  )

#|

(test-gsat-all-values-sendMoreMoney)

(dotimes (x 10)
  (TEST-GSAT-ALL-VALUES-SENDMOREMONEY))

|#