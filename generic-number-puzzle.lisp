(in-package :cl-user)


;;; Unfortunately the possible permutations of domains > 7
;;; do not fit at all in memory
;;; We therefore check the <> condition explicitly

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

(defclass numeric-puzzle-solver (BACKTRACKING-SOLVER)
  ()
  )

(defmethod my-permutation ((solver numeric-puzzle-solver) domain)
  domain)

(defmethod all-variable-names ((solver numeric-puzzle-solver))
  (mapcar #'first (ALL-DOMAINS-EXTENDED solver)))

(defclass numeric-puzzle-partial-solution (PARTIAL-SOLUTION)
  (
   (number-array :initarg :number-array :accessor number-array)
   )
  )

;;; Corman will initiate the array to 0
(defmethod GENERATE-EMPTY-SOLUTION ((me numeric-puzzle-solver))
  (make-instance 'NUMERIC-PUZZLE-PARTIAL-SOLUTION
    :number-array (make-array (problem-size me) :initial-element nil)))


(defmethod EXPAND-PARTIAL-SOLUTION ((partial numeric-puzzle-partial-solution) perm index)
  (setf (svref (number-array partial) index) perm)
  (values)
  )

(defmethod CHANGE-PARTIAL-SOLUTION ((partial numeric-puzzle-partial-solution) perm index)
  (EXPAND-PARTIAL-SOLUTION partial perm index)
  )

(defmethod FORGET-PARTIAL-SOLUTION ((partial numeric-puzzle-partial-solution) index)
  (setf (svref (number-array partial) index) nil)
  )

(defmethod show-result ((me numeric-puzzle-partial-solution) (solver NUMERIC-PUZZLE-SOLVER) title time-units)
  (format t "~%Test:~a~%" title)
  (format t "The solution in ~a tries testing ~a constraints took ~10,2f seconds constraints/seconds ~10,2f and is:~%" 
    (SOLUTION-TRIED solver)
    (cs-constraints-tested solver)
    (float (/ (float time-units) internal-time-units-per-second))
    (if (zerop time-units)
        0
    (/ (float (cs-constraints-tested solver)) (float (/ (float time-units) internal-time-units-per-second))))
    )
  (dolist (var (ALL-VARIABLE-NAMES solver))
    (let ((index (find-variable-index (CS-SPECIFICATION solver) var)))
      (format t "Variable ~a is:~a~%" var (svref (number-array me) index))))
  )

(defclass number-puzzle-constraints (testable-constraint)
  ()
  )

(defclass index&value (number-puzzle-constraints)
  (
   (index :accessor sm-index :initarg :sm-index)
   (value :accessor sm-value :initarg :sm-value)
   )
  )

(defclass index=value (index&value)
  ())

(defclass index<>value (index&value)
  ())

(defmethod CONSTRAINT-HOLDS ((constraint index&value)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((index (sm-index constraint)))
    (let ((cv (svref (number-array ps) index)))
      (if (null cv)
          t
        (compare constraint cv)))))

(defmethod compare ((constraint index=value) cv)
  (= cv (sm-value constraint))
  )

(defmethod compare ((constraint index<>value) cv)
  (not (= cv (sm-value constraint)))
  )

(defclass all-distinct (number-puzzle-constraints)
  (
   (indices :accessor sm-indices :initarg :sm-indices)
   )
  )


;;quadratic effort
(defmethod CONSTRAINT-HOLDS ((constraint all-distinct)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((indices (sm-indices constraint)))
    (dolist (index-a indices t)
      (let ((val-a (svref (number-array ps) index-a)))
        (unless (null val-a)
          (dolist (index-b indices)
            (when (not (= index-a index-b))
              (let ((val-b (svref (number-array ps) index-b)))
                (unless (null val-b)
                  (when (= val-a val-b)
                    (return-from CONSTRAINT-HOLDS nil)))))))))))

(defclass value=value (number-puzzle-constraints)
  (
   (index-a :accessor np-index-a :initarg :index-a)
   (index-b :accessor np-index-b :initarg :index-b)
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint value=value)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((value-a (svref (number-array ps) (np-index-a constraint))))
      (if (null value-a)
          t
        (let ((value-b (svref (number-array ps) (np-index-b constraint))))
          (if (null value-b)
              t
            (= value-a value-b))))))

(defclass column=root (number-puzzle-constraints)
  (
   (value-a :accessor np-value-a :initarg :value-a)
   (value-b :accessor np-value-b :initarg :value-b)
   (mod10sum :accessor np-mod10sum :initarg :mod10sum)
   )
  ) 

(defclass column=div (column=root)
  (
   (div10 :accessor np-div10 :initarg :div10)
   )
  )

(defclass column= (column=div)
  ())

(defmethod CONSTRAINT-HOLDS ((constraint COLUMN=)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((value-a (svref (number-array ps)(np-value-a constraint))))
    (if (null value-a)
        t
      (let ((value-b (svref (number-array ps)(np-value-b constraint))))
        (if (null value-b)
            t
          (let ((mod10sum (svref (number-array ps)(np-mod10sum constraint))))
            (if (null mod10sum)
                t
              (let ((div10 (svref (number-array ps)(np-div10 constraint))))
                (if (null div10)
                    (= (mod (+ value-a value-b) 10) MOD10SUM)
                  (= (+ value-a value-b) (+ MOD10SUM (* div10 10))))))))))))

(defclass column=carry (column=div)
  (
   (div10right :initarg :DIV10RIGHT :accessor np-DIV10RIGHT)
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint COLUMN=CARRY)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((value-a (svref (number-array ps)(np-value-a constraint))))
    (if (null value-a)
        t
      (let ((value-b (svref (number-array ps)(np-value-b constraint))))
        (if (null value-b)
            t
          (let ((DIV10RIGHT (svref (number-array ps)(NP-DIV10RIGHT constraint))))
            (if (null DIV10RIGHT)
                t
              (let ((MOD10SUM (svref (number-array ps)(NP-MOD10SUM constraint))))
                (if (null MOD10SUM)
                    t
                  (let ((div10 (svref (number-array ps)(np-div10 constraint))))
                    (if (null DIV10)
                        (= (mod (+ value-a value-b DIV10RIGHT) 10) MOD10SUM)
                      (= (+ value-a value-b DIV10RIGHT) (+ MOD10SUM (* div10 10))))))))))))))

(defclass mega-constraint (number-puzzle-constraints)
  (
   (index-sum-10000 :accessor np-index-sum-10000 :initarg :index-sum-10000)
   (index-sum-1000 :accessor np-index-sum-1000 :initarg :index-sum-1000)
   (index-sum-100 :accessor np-index-sum-100 :initarg :index-sum-100)
   (index-sum-10 :accessor np-index-sum-10 :initarg :index-sum-10)
   (index-sum-1 :accessor np-index-sum-1 :initarg :index-sum-1)
   (index-sumand-a-1000 :accessor np-index-sumand-a-1000 :initarg :index-sumand-a-1000)
   (index-sumand-a-100 :accessor np-index-sumand-a-100 :initarg :index-sumand-a-100)
   (index-sumand-a-10 :accessor np-index-sumand-a-10 :initarg :index-sumand-a-10)
   (index-sumand-a-1 :accessor np-index-sumand-a-1 :initarg :index-sumand-a-1)
   (index-sumand-b-1000 :accessor np-index-sumand-b-1000 :initarg :index-sumand-b-1000)
   (index-sumand-b-100 :accessor np-index-sumand-b-100 :initarg :index-sumand-b-100)
   (index-sumand-b-10 :accessor np-index-sumand-b-10 :initarg :index-sumand-b-10)
   (index-sumand-b-1 :accessor np-index-sumand-b-1 :initarg :index-sumand-b-1)
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint MEGA-CONSTRAINT)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((index-sum-10000 (svref (number-array ps) (np-index-sum-10000 constraint)))
        (index-sum-1000 (svref (number-array ps) (np-index-sum-1000 constraint)))
        (index-sum-100 (svref (number-array ps) (np-index-sum-100 constraint)))
        (index-sum-10 (svref (number-array ps) (np-index-sum-10 constraint)))
        (index-sum-1 (svref (number-array ps) (np-index-sum-1 constraint)))
        (INDEX-SUMAND-A-1000 (svref (number-array ps) (NP-INDEX-SUMAND-A-1000 constraint)))
        (INDEX-SUMAND-A-100 (svref (number-array ps) (NP-INDEX-SUMAND-A-100 constraint)))
        (INDEX-SUMAND-A-10 (svref (number-array ps) (NP-INDEX-SUMAND-A-10 constraint)))
        (INDEX-SUMAND-A-1 (svref (number-array ps) (NP-INDEX-SUMAND-A-1 constraint)))
        (index-sumand-b-1000 (svref (number-array ps) (NP-index-sumand-b-1000 constraint)))
        (index-sumand-b-100 (svref (number-array ps) (NP-index-sumand-b-100 constraint)))
        (index-sumand-b-10 (svref (number-array ps) (NP-index-sumand-b-10 constraint)))
        (index-sumand-b-1 (svref (number-array ps) (NP-index-sumand-b-1 constraint)))
        )
    (if (and INDEX-SUM-10000 INDEX-SUM-1000 INDEX-SUM-100 INDEX-SUM-10 INDEX-SUM-1
             INDEX-SUMAND-A-1000 INDEX-SUMAND-A-100 INDEX-SUMAND-A-10 INDEX-SUMAND-A-1
             INDEX-SUMAND-B-1000 INDEX-SUMAND-B-100 INDEX-SUMAND-B-10 INDEX-SUMAND-B-1)
        (=
         (+ (* 10000 INDEX-SUM-10000) (* 1000 INDEX-SUM-1000)(* 100 INDEX-SUM-100)(* 10 INDEX-SUM-10) INDEX-SUM-1)
         (+ (* 1000 (+ INDEX-SUMAND-A-1000 INDEX-SUMAND-b-1000))
            (* 100 (+ INDEX-SUMAND-A-100 INDEX-SUMAND-b-100))
            (* 10 (+ INDEX-SUMAND-A-10 INDEX-SUMAND-b-10))
            (+ INDEX-SUMAND-A-1 INDEX-SUMAND-b-1)))
      t)))

(defclass column=mod10 (COLUMN=ROOT)
  (
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint COLUMN=MOD10)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((value-a (svref (number-array ps)(np-value-a constraint))))
    (if (null value-a)
        t
      (let ((value-b (svref (number-array ps)(np-value-b constraint))))
        (if (null value-b)
            t
          (let ((mod10sum (svref (number-array ps)(np-mod10sum constraint))))
            (if (null mod10sum)
                t
              (= (mod (+ value-a value-b) 10) mod10sum))))))))

(defclass column=more-or-less (COLUMN=ROOT)
  (
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint COLUMN=MORE-OR-LESS)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((value-a (svref (number-array ps)(np-value-a constraint))))
    (if (null value-a)
        t
      (let ((value-b (svref (number-array ps)(np-value-b constraint))))
        (if (null value-b)
            t
          (let ((mod10sum (svref (number-array ps)(np-mod10sum constraint))))
            (if (null mod10sum)
                t
              (or (= (mod (+ value-a value-b) 10) mod10sum)
                  (= (mod (+ value-a value-b 1) 10) mod10sum)))))))))

(defclass column=div-more-or-less (COLUMN=ROOT)
  (
   )
  )

(defmethod CONSTRAINT-HOLDS ((constraint column=div-more-or-less)(ps NUMERIC-PUZZLE-PARTIAL-SOLUTION))
  (let ((value-a (svref (number-array ps)(np-value-a constraint))))
    (if (null value-a)
        t
      (let ((value-b (svref (number-array ps)(np-value-b constraint))))
        (if (null value-b)
            t
          (let ((mod10sum (svref (number-array ps)(np-mod10sum constraint))))
            (if (null mod10sum)
                t
              (or
               (multiple-value-bind 
                     (div mod)
                   (floor (+ value-a value-b) 10)
                 (declare (ignore mod))
                 (= MOD10SUM div))
               (multiple-value-bind 
                     (div mod)
                   (floor (+ value-a value-b 1) 10)
                 (declare (ignore mod))
                 (= MOD10SUM div))))))))))



