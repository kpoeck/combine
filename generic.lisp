(in-package :cl-user)

;;; Solution to Einstein Riddle by Dr. Karsten Poeck
;;; inspired by Dr. Edmund Weitz solution
;;; But, no macros, no function generation, CLOS only
;;; In Allegro 6.0 about twice as fast as the original
;;; But more important consumes a lot less memory
;;; Original 445,402 cons cells, 8,186,528 other bytes, 1328 static bytes
;;; This version  8,443 cons cells, 384 other bytes, 0 static bytes
;;; The Backtracking is essentially the same (but no nested dolist)
;;; but it stops after the first solution (return-from)
;;; The Code for the conditions is a bit optimised

;;; Permutation is borrowed from Dr. Edmund Weitz
;;; Works in ACL 6.0, LW 4.1.20, Corman 1.42 and clisp 2.27

;;; Einstein specific code in einstein.lisp
;;; Simpler mickey test in mickey.lisp
;;; Zebra in Zebra.lisp

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

(defun permutation (list)
  (if (null list)
      (list nil)
    (mapcan #'(lambda (first) 
                (mapcar #'(lambda (rest)
                            (cons first rest))
                  (permutation (remove first list :count 1 :test #'eq))))
      list)))

(defclass combine-root ()
  ()
  )

(defclass combinatoric-problem-specification (COMBINE-ROOT)
  (
   (my-constraints :accessor my-constraints :initarg :my-constraints :initform nil)
   )
  )

(defclass numeric-combinatoric-problem-specification (COMBINATORIC-PROBLEM-SPECIFICATION)
  ()
  )

(defclass symbolic-problem-specification (COMBINATORIC-PROBLEM-SPECIFICATION)
  (
   )
  )

(defmethod find-variable-index ((me NUMERIC-COMBINATORIC-PROBLEM-SPECIFICATION) name)
  (let ((index 0))
    (dolist (assoc (all-domains-extended me)(error "Did not found variable"))
      (if (eq name (first assoc))
        (return index)
        (incf index)))))

(defmethod FIND-VARIABLE-INDices ((me NUMERIC-COMBINATORIC-PROBLEM-SPECIFICATION) list)
  (mapcar #'(lambda(was)
              (find-variable-index me was))
    list))

(defclass numeric-one-domain-problem-specification (NUMERIC-COMBINATORIC-PROBLEM-SPECIFICATION)
  ()
  )

(defclass numeric-several-domains-problem-specification (NUMERIC-COMBINATORIC-PROBLEM-SPECIFICATION)
  ()
  )

(defclass partial-solution (combine-root)
  ()
  )

(defclass testable-constraint (combine-root)
  ()
  )

(defmethod constraint-holds ((constraint testable-constraint) (solution partial-solution))
  t)

(defclass combinatoric-solver (combine-root)
  (
   (solution-tried :accessor solution-tried :initform 0)
   (my-specification :initarg :specification :accessor cs-specification)
   (constraints-tested :initform 0 :accessor cs-constraints-tested)
   )
  )

#-:clisp
(defgeneric SOLVE-IT (solver))

(defmethod get-my-constraints ((me combinatoric-solver))
  (my-constraints (CS-SPECIFICATION me))
  )

(defmethod partial-solution-correct ((me combinatoric-solver) (solution partial-solution))
  (dolist (constraint (get-my-constraints me) t)
    (incf (cs-constraints-tested me))
    (unless (constraint-holds constraint solution)
      (return nil))))

(defmethod partial-solution-evaluation ((me COMBINATORIC-SOLVER) (solution partial-solution))
  (let ((val 0))
    (dolist (constraint (get-my-constraints me) t)
      #-gcl (incf (cs-constraints-tested me))
      #+gcl nil
      (unless (constraint-holds constraint solution)
        (incf val)))
  val))

(defmethod all-domains ((me COMBINATORIC-SOLVER))
  (mapcar #'second (all-domains-extended me)))

(defmethod problem-size ((me COMBINATORIC-SOLVER))
  (length (all-domains-extended me)))


#-:clisp
(defgeneric ALL-DOMAINS-EXTENDED (specification-or-solver))

(defmethod all-domains-extended ((me COMBINATORIC-SOLVER))
  (ALL-DOMAINS-EXTENDED (cs-specification me)))

#-:clisp
(defgeneric partial-solution-class (specification))

#-:clisp
(defgeneric solution-element-class (SPECIFICATION))

(defmethod generate-empty-solution ((me COMBINATORIC-SOLVER))
  (let ((elements nil)
        (class (solution-element-class (CS-SPECIFICATION me))))
    (dotimes (x (problem-size me))
      (push (make-instance class) elements))
    (make-instance (partial-solution-class (CS-SPECIFICATION me))
      :riddle-elements elements   
      )
    )
  )

#-:clisp
(defgeneric EXPAND-PARTIAL-SOLUTION (partial permutation index))

#-:clisp
(defgeneric CHANGE-PARTIAL-SOLUTION (PARTIAL permutation index))

#-:clisp
(defgeneric FORGET-PARTIAL-SOLUTION (PARTIAL index))

;;; Specific to Riddle

(defclass riddle-partial-solution (partial-solution)
  (
   (riddle-elements :accessor riddle-elements :initarg :riddle-elements)
   )
  )

(defclass solution-element (combine-root)
  ()
  )

(defmethod show-result ((me riddle-partial-solution) (solver COMBINATORIC-SOLVER) title time-units)
  (format t "~%Test:~a~%" title)
  (format t "The solution in ~a tries testing ~a constraints took ~10,2f seconds constraints/seconds ~10,2f and is:~%" 
    (SOLUTION-TRIED solver)
    (cs-constraints-tested solver)
    (float (/ (float time-units) internal-time-units-per-second))
    (if (zerop time-units)
        0
    (/ (float (cs-constraints-tested solver)) (float (/ (float time-units) internal-time-units-per-second))))
    )
  (dolist (house (riddle-elements me))
    (show-house-result house))
  )
  
  
#-:clisp
(defgeneric element-mapper (partial-solution))

;;; getf may or may not work with numbers
(defun %getf-number (plist number)
  (loop 
    (let ((first (pop plist))
          (second (pop plist)))
      (if (and first second (= first number))
          (return second)
          (unless first
            (return nil))))))

#+old
(defmethod EXPAND-PARTIAL-SOLUTION ((me RIDDLE-PARTIAL-SOLUTION)
                                    permutation index)
  (break "EXPAND-PARTIAL-SOLUTION")
  (let ((setter (%getf-number (element-mapper me) index)))
    (dolist (house (riddle-elements me))
      (funcall setter (pop permutation)  house)
      )
    )
  )

(defmethod EXPAND-PARTIAL-SOLUTION ((me RIDDLE-PARTIAL-SOLUTION)
                                    permutation index)
    (dolist (house (riddle-elements me))
      (EXPAND-PARTIAL-SOLUTION house index (pop permutation))))



(defmethod CHANGE-PARTIAL-SOLUTION ((me RIDDLE-PARTIAL-SOLUTION)
                                    permutation index)
  (EXPAND-PARTIAL-SOLUTION me permutation index)
  )

#+old
(defmethod FORGET-PARTIAL-SOLUTION ((me RIDDLE-PARTIAL-SOLUTION)
                                    index)
  (let ((setter (%getf-number (element-mapper me) index)))
    (dolist (house (riddle-elements me))
      (funcall setter nil  house)
      )
    )
  )

(defmethod FORGET-PARTIAL-SOLUTION ((me RIDDLE-PARTIAL-SOLUTION)
                                    index)
   (dolist (house (riddle-elements me))
     (EXPAND-PARTIAL-SOLUTION house index nil)))

(defclass riddle-constraints (testable-constraint)
  ()
  )

#-:clisp
(defgeneric element-property-mapper (riddle-partial-solution))

(defclass position-and-property-constraint (RIDDLE-CONSTRAINTS)
  (
   (selector-one :initarg :selector-one :accessor riddle-selector-one)
   (value-one :initarg :value-one :accessor riddle-value-one)
   (position :initarg :position :accessor riddle-position)
   )
  )

#+old
(defmethod constraint-holds ((constraint position-and-property-constraint) (RIDDLE-PARTIAL-SOLUTION partial-solution))
  (let ((sel-1 (getf (element-property-mapper RIDDLE-PARTIAL-SOLUTION)  
                     (riddle-selector-one constraint)))
        (house-list (riddle-elements riddle-partial-solution))
        )
    (if (null (funcall sel-1 (first house-list)))
        t
      (let ((index 0)
            (value-1 (riddle-value-one constraint))
            (position (riddle-position constraint)))

        (dolist (house house-list nil)
          (when (eq value-1 (funcall sel-1 house))
            (let ((result (= index POSITION)))
              (return result)))
         (incf index)
          )
        )
      )
    )
  )

(defmethod constraint-holds ((constraint position-and-property-constraint) (RIDDLE-PARTIAL-SOLUTION partial-solution))
  (let ((key-1 (riddle-selector-one constraint))
        (house-list (riddle-elements riddle-partial-solution)))
    (if (null (read-value-for-key (first house-list) key-1))
        t
      (let ((index 0)
            (value-1 (riddle-value-one constraint))
            (position (riddle-position constraint)))
        (dolist (house house-list nil)
          (when (eq value-1 (read-value-for-key house key-1 ))
            (let ((result (= index POSITION)))
              (return result)))
         (incf index))))))

(defclass TWO-VALUES-IN-HOUSE-constraint (riddle-constraints)
  (
   (selector-one :initarg :selector-one :accessor riddle-selector-one)
   (value-one :initarg :value-one :accessor riddle-value-one)
   (selector-two :initarg :selector-two :accessor riddle-selector-two)
   (value-two :initarg :value-two :accessor riddle-value-two)
   )
  )

#+old
(defmethod constraint-holds ((constraint TWO-VALUES-IN-HOUSE-constraint) (RIDDLE-PARTIAL-SOLUTION partial-solution))
  (let ((sel-1 (getf (element-property-mapper RIDDLE-PARTIAL-SOLUTION)  (riddle-selector-one constraint)))
        (sel-2 (getf (element-property-mapper RIDDLE-PARTIAL-SOLUTION) (riddle-selector-two constraint)))
        (house-list (riddle-elements riddle-partial-solution))
        )
    (if (or (null (funcall sel-1 (first house-list)))
            (null (funcall sel-2 (first house-list)))
            )
        t
      (let ((value-1 (riddle-value-one constraint))
            (value-2 (riddle-value-two constraint)))
        (dolist (house house-list nil)
          (when (eq value-1 (funcall sel-1 house))
            (let ((result (eq value-2 (funcall sel-2 house))))
              (return result)))
          )
        )
      )
    )
  )

(defmethod constraint-holds ((constraint TWO-VALUES-IN-HOUSE-constraint) (RIDDLE-PARTIAL-SOLUTION partial-solution))
  (let ((key-1 (riddle-selector-one constraint))
        (key-2 (riddle-selector-two constraint))
        (house-list (riddle-elements riddle-partial-solution))
        )
    (if (or (null (read-value-for-key (first house-list) key-1 ))
            (null (read-value-for-key (first house-list) key-2 )))
        t
        (let ((value-1 (riddle-value-one constraint))
              (value-2 (riddle-value-two constraint)))
          (dolist (house house-list nil)
            (when (eq value-1 (read-value-for-key house key-1 ))
              (let ((result (eq value-2 (read-value-for-key house key-2))))
                (return result))))))))

(defclass generic-neighbour-constraint (riddle-constraints)
  (
   (selector-one :initarg :selector-one :accessor riddle-selector-one)
   (value-one :initarg :value-one :accessor riddle-value-one)
   (selector-two :initarg :selector-two :accessor riddle-selector-two)
   (value-two :initarg :value-two :accessor riddle-value-two)
   )
  )

(defclass NEIGHBOUR-CONSTRAINT (generic-neighbour-constraint)
  ()
  )

(defclass abs-distance-neighbour-constraint (GENERIC-NEIGHBOUR-CONSTRAINT)
  (
    (distance :initarg :distance :accessor riddle-distance :initform nil)
   )
  )

(defclass directed-distance-neighbour-constraint (GENERIC-NEIGHBOUR-CONSTRAINT)
  (
   (distance :initarg :distance :accessor riddle-distance :initform nil)
   )
  )

(defclass negated-directed-distance-neighbour-constraint (GENERIC-NEIGHBOUR-CONSTRAINT)
  (
   (distance :initarg :distance :accessor riddle-distance :initform nil)
   )
  )

#-:clisp
(defgeneric evaluate-distance (NEIGHBOUR-CONSTRAINT pos-a pos-b))

#+old
(defmethod constraint-holds ((constraint generic-neighbour-constraint) (RIDDLE-PARTIAL-SOLUTION partial-solution))
  (let ((sel-1 (getf (element-property-mapper RIDDLE-PARTIAL-SOLUTION) (riddle-selector-one constraint)))
        (sel-2 (getf (element-property-mapper RIDDLE-PARTIAL-SOLUTION) (riddle-selector-two constraint)))
        (house-list (riddle-elements riddle-partial-solution))
        )
    (if (or (null (funcall sel-1 (first house-list)))
            (null (funcall sel-2 (first house-list)))
            )
        t
      (let ((index 0)
            (pos-a nil)
            (pos-b nil)
            (value-1 (riddle-value-one constraint))
            (value-2 (riddle-value-two constraint)))
        (dolist (house house-list nil)
          (when (eq value-1 (funcall sel-1 house))
            (setq pos-a index))
          (when (eq value-2 (funcall sel-2 house))
            (setq pos-b index))
          (when (and pos-a pos-b)
            (return
              (EVALUATE-DISTANCE CONSTRAINT pos-a pos-b)))
          (incf index)
          )
        )
      )
    )
  )

(defmethod constraint-holds ((constraint generic-neighbour-constraint) (RIDDLE-PARTIAL-SOLUTION partial-solution))
  (let ((key-1 (riddle-selector-one constraint))
        (key-2  (riddle-selector-two constraint))
        (house-list (riddle-elements riddle-partial-solution))
        )
    (if (or (null (read-value-for-key (first house-list) key-1))
            (null (read-value-for-key (first house-list) key-2)))
        t
      (let ((index 0)
            (pos-a nil)
            (pos-b nil)
            (value-1 (riddle-value-one constraint))
            (value-2 (riddle-value-two constraint)))
        (dolist (house house-list nil)
          (when (eq value-1 (read-value-for-key house key-1 ))
            (setq pos-a index))
          (when (eq value-2 (read-value-for-key house key-2 ))
            (setq pos-b index))
          (when (and pos-a pos-b)
            (return
              (EVALUATE-DISTANCE CONSTRAINT pos-a pos-b)))
          (incf index)
          )
        )
      )
    )
  )

(defmethod evaluate-distance ((CONSTRAINT NEIGHBOUR-CONSTRAINT) pos-a pos-b)
  (or (= pos-a (1- pos-b))
      (= pos-a (1+ pos-b)))
  )

(defmethod evaluate-distance ((CONSTRAINT abs-distance-neighbour-constraint) pos-a pos-b)
  (= (riddle-distance constraint)
     (abs (- pos-a pos-b)))
  )

(defmethod evaluate-distance ((CONSTRAINT directed-distance-neighbour-constraint) pos-a pos-b)
  (= (riddle-distance constraint)
     (- pos-b POS-A))
  )

(defmethod evaluate-distance ((CONSTRAINT NEGATED-DIRECTED-DISTANCE-NEIGHBOUR-CONSTRAINT) pos-a pos-b)
  (not (= (riddle-distance constraint)
          (- pos-b pos-a)))
  )
