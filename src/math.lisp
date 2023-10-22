(in-package :mlep)

(defparameter *big-decimal-class* (class-of (abcl-big-decimals:make-big-decimal 1)))

(defmacro def-opt (cl-op big-dec-op)
  (let ((fname (read-from-string (format nil "opt~a" cl-op))))
    `(progn
       (defun ,fname (a b)
         #+abcl
         (cond ((and (eq (class-of a) *big-decimal-class*)
                     (eq (class-of b) *big-decimal-class*))
                (,big-dec-op a b))
               ((and (eq (class-of a) *big-decimal-class*)
                     (not (eq (class-of b) *big-decimal-class*)))
                (,big-dec-op a (abcl-big-decimals:make-big-decimal b)))
               ((and (eq (class-of b) *big-decimal-class*)
                     (not (eq (class-of a) *big-decimal-class*)))
                (,big-dec-op b (abcl-big-decimals:make-big-decimal a)))
               (t
                (,big-dec-op (abcl-big-decimals:make-big-decimal a)
                             (abcl-big-decimals:make-big-decimal b))))
         #-abcl (,cl-op a b))
       (export ',fname))))

(mlep::def-opt * abcl-big-decimals:multiply)
(mlep::def-opt + abcl-big-decimals:add)
(mlep::def-opt - abcl-big-decimals:subtract)
(mlep::def-opt / abcl-big-decimals:divide)
