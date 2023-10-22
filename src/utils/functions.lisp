;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :mlep)

(defun step-function (x)
  (if (> x 0) 1 0))

(defun d-step-function (x)
  (if (= x 0) 1 0))

(defun sigmoid (x)
  (/ 1.0d0 (+ 1.0d0 (expt +e+ (- x)))))

(defun d-sigmoid (x)
  (/ (expt +e+ x) (square (+ (expt +e+ x) 1))))

(defun d-tanh (x)
  (- 1.0d0 (square (tanh x))))

(defun leaky-relu (x)
  (if (#-abcl > #+abcl abcl-big-decimals:> x #-abcl 0 #+abcl B0)
      x
      (#-abcl * #+abcl abcl-big-decimals:multiply #-abcl 0.01 #+abcl B"0.01" x)))

(defun d-leaky-relu (x)
  (if (#-abcl > #+abcl abcl-big-decimals:> x #-abcl 0 #+abcl B0)
      #-abcl 1 #+abcl B1
      #-abcl 0.01 #+abcl B"0.01"))

(defparameter *diff-function-dict* (make-hash-table :size 2))
(setf (gethash #'step-function *diff-function-dict*) #'d-step-function)
(setf (gethash #'sigmoid *diff-function-dict*) #'d-sigmoid)
(setf (gethash #'tanh *diff-function-dict*) #'d-tanh)
(setf (gethash #'leaky-relu *diff-function-dict*) #'d-leaky-relu)

(defun diff (func)
  (let ((result (gethash func *diff-function-dict*)))
    (if result result
      (error "No derivative function of ~a is known" func))))
