(in-package :cl-user)

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

(defclass gsat-solver-mixin (combine-root)
  (
   (max-flips :initarg :max-flips :accessor gsat-max-flips)
   (max-tries :initarg :max-tries :accessor gsat-max-tries)
   (solved-iteration :accessor gsat-solved-iteration)
   (worsened :accessor gsat-worsened :initform 0)
   (aborted :accessor gsat-aborted :initform 0)
   )
  )