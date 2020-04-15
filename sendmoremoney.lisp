(in-package :cl-user)

;;;  Send
;;;+ More
;;;=Money

;;;To get a solution, try (test-sendMoreMoney)
;;;Run tests at least twice to get clos prepared

;;;Value Ordering is important, we get a solution between 23 833 and 11 380 457 tries

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

(defclass send-money-problem-carries (NUMERIC-SEVERAL-DOMAINS-PROBLEM-SPECIFICATION)
  ()
  )

(defparameter *send-more-money-domains*
  '((:s (0 1 2 3 4 5 6 7 8 9))
    (:m (0 1 2 3 4 5 6 7 8 9))
    (:o (0 1 2 3 4 5 6 7 8 9))
    (:c-sm (0 1))
    (:e (0 1 2 3 4 5 6 7 8 9))
    (:n (0 1 2 3 4 5 6 7 8 9))
    (:c-eo (0 1)) 
    (:r (0 1 2 3 4 5 6 7 8 9))
    (:c-nr (0 1))
    (:d (0 1 2 3 4 5 6 7 8 9))
    (:y (0 1 2 3 4 5 6 7 8 9))
    (:c-de (0 1))   
    )
  #+no
  '(
    (:d (0 1 2 3 4 5 6 7 8 9))
    (:e (0 1 2 3 4 5 6 7 8 9))
    (:y (0 1 2 3 4 5 6 7 8 9))
    (:c-de (0 1))
    
    (:n (0 1 2 3 4 5 6 7 8 9))
    (:r (0 1 2 3 4 5 6 7 8 9))
    (:c-nr (0 1))
    
    (:o (0 1 2 3 4 5 6 7 8 9))
    (:c-eo (0 1))
    
    (:s (0 1 2 3 4 5 6 7 8 9))
    (:m (0 1 2 3 4 5 6 7 8 9))
    (:c-sm (0 1))  
    )
  #+no
  '(
    (:c-sm (0 1))
    (:c-eo (0 1))
    (:c-nr (0 1))
    (:c-de (0 1))
    
    (:s (0 1 2 3 4 5 6 7 8 9))
    (:e (0 1 2 3 4 5 6 7 8 9))
    (:n (0 1 2 3 4 5 6 7 8 9))
    (:d (0 1 2 3 4 5 6 7 8 9))
    (:m (0 1 2 3 4 5 6 7 8 9))
    (:o (0 1 2 3 4 5 6 7 8 9))
    (:r (0 1 2 3 4 5 6 7 8 9))
    (:y (0 1 2 3 4 5 6 7 8 9))  
    )
  #+no
  '(   
    (:s (0 1 2 3 4 5 6 7 8 9))
    (:e (0 1 2 3 4 5 6 7 8 9))
    (:n (0 1 2 3 4 5 6 7 8 9))
    (:d (0 1 2 3 4 5 6 7 8 9))
    (:m (0 1 2 3 4 5 6 7 8 9))
    (:o (0 1 2 3 4 5 6 7 8 9))
    (:r (0 1 2 3 4 5 6 7 8 9))
    (:y (0 1 2 3 4 5 6 7 8 9))
    
    (:c-sm (0 1))
    (:c-eo (0 1))
    (:c-nr (0 1))
    (:c-de (0 1))
  
    )
  )

(defmethod all-domains-extended ((me send-money-problem-carries))
  *send-more-money-domains*
    )

(defmethod initialize-instance :after ((me send-money-problem-carries) &rest initargs)
  (declare (ignore initargs))
  (setf (my-constraints me)
    (list 
     (make-instance 'index<>value
       :sm-index (FIND-VARIABLE-INDEX me :s)
       :sm-value 0)
     (make-instance 'index<>value
       :sm-index (FIND-VARIABLE-INDEX me :m)
       :sm-value 0)
     (make-instance 'value=value
       :index-a (FIND-VARIABLE-INDEX me :m)
       :index-b (FIND-VARIABLE-INDEX me :c-sm)
       )
     (make-instance 'all-distinct
       :sm-indices (FIND-VARIABLE-INDices me '(:s :e :n :d :m :o :r :y)))
     (make-instance 'COLUMN=
       :value-a (FIND-VARIABLE-INDEX me :d)
       :value-b (FIND-VARIABLE-INDEX me :e)
       :mod10sum (FIND-VARIABLE-INDEX me :y)
       :div10 (FIND-VARIABLE-INDEX me :c-de))
     (make-instance 'COLUMN=CARRY
       :value-a (FIND-VARIABLE-INDEX me :n)
       :value-b (FIND-VARIABLE-INDEX me :r)
       :mod10sum (FIND-VARIABLE-INDEX me :e)
       :div10right (FIND-VARIABLE-INDEX me :c-de)
       :div10 (FIND-VARIABLE-INDEX me :c-nr))
     (make-instance 'COLUMN=CARRY
       :value-a (FIND-VARIABLE-INDEX me :e)
       :value-b (FIND-VARIABLE-INDEX me :o)
       :mod10sum (FIND-VARIABLE-INDEX me :n)
       :div10right (FIND-VARIABLE-INDEX me :c-nr)
       :div10 (FIND-VARIABLE-INDEX me :c-eo))
     (make-instance 'COLUMN=CARRY
       :value-a (FIND-VARIABLE-INDEX me :s)
       :value-b (FIND-VARIABLE-INDEX me :m)
       :mod10sum (FIND-VARIABLE-INDEX me :o)
       :div10right (FIND-VARIABLE-INDEX me :c-eo)
       :div10 (FIND-VARIABLE-INDEX me :c-sm))
     )))

(defclass sendMoreMoney-solver-backtracking-carries (NUMERIC-PUZZLE-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-carries)))

(defun test-sendMoreMoney (&optional (print t))
  (test-backtracking "Send more Money Carries Backtracking" (make-instance 'sendMoreMoney-solver-backtracking-carries) print)
  )

#|
(test-sendMoreMoney)
CG-USER(23): 
; cpu time (non-gc) 550 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  550 msec user, 0 msec system
; real time  550 msec
; space allocation:
;  862 cons cells, 5,056 other bytes, 0 static bytes
The solution in 23833 tries is:
Variable S is:9
Variable M is:1
Variable O is:0
Variable C-SM is:1
Variable E is:5
Variable N is:6
Variable C-EO is:0
Variable R is:8
Variable C-NR is:1
Variable D is:7
Variable Y is:2
Variable C-DE is:1
CG-USER(24): 
; cpu time (non-gc) 930 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  930 msec user, 0 msec system
; real time  930 msec
; space allocation:
;  20 cons cells, 248 other bytes, 0 static bytes
The solution in 45761 tries is:
Variable D is:7
Variable E is:5
Variable Y is:2
Variable C-DE is:1
Variable N is:6
Variable R is:8
Variable C-NR is:1
Variable O is:0
Variable C-EO is:0
Variable S is:9
Variable M is:1
Variable C-SM is:1
CG-USER(25): 
; cpu time (non-gc) 41,470 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  41,470 msec user, 0 msec system
; real time  41,470 msec
; space allocation:
;  176 cons cells, 408 other bytes, 560 static bytes
The solution in 5172656 tries is:
Variable C-SM is:1
Variable C-EO is:0
Variable C-NR is:1
Variable C-DE is:1
Variable S is:9
Variable E is:5
Variable N is:6
Variable D is:7
Variable M is:1
Variable O is:0
Variable R is:8
Variable Y is:2
CG-USER(26): 
; cpu time (non-gc) 236,290 msec (00:03:56.290) user, 0 msec system
; cpu time (gc)     50 msec user, 0 msec system
; cpu time (total)  236,340 msec (00:03:56.340) user, 0 msec system
; real time  236,340 msec (00:03:56.340)
; space allocation:
;  922 cons cells, 648 other bytes, 560 static bytes
The solution in 11380457 tries is:
Variable S is:9
Variable E is:5
Variable N is:6
Variable D is:7
Variable M is:1
Variable O is:0
Variable R is:8
Variable Y is:2
Variable C-SM is:1
Variable C-EO is:0
Variable C-NR is:1
Variable C-DE is:1
|#

(defclass sendMoreMoney-solver-gsat-all-values (GSAT-ALL-VALUES-SOLVER)
  ()
  (:default-initargs
      :specification (make-instance 'send-money-problem-carries)))

(defun test-gsat-sendMoreMoney (&optional (print t))
  (TEST-GSAT "Send more Money Carries Gsat all values" (make-instance 'SENDMOREMONEY-SOLVER-GSAT-ALL-VALUES :MAX-FLIPS 500 :max-tries 50) print)
  )

#|

(TEST-GSAT-SENDMOREMONEY)

(dotimes (x 10)
  (TEST-GSAT-SENDMOREMONEY))
  
|#